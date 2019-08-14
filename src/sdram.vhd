-- Copyright (c) 2019 Josh Bassett
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- This module implements a memory controller for a 16Mx16 SDRAM memory module.
entity sdram is
  generic (
    BURST_LENGTH     : std_logic_vector(2 downto 0) := "000"; -- 000=1, 001=2, 010=4, 011=8
    BURST_TYPE       : std_logic_vector(0 downto 0) := "0";   -- 0=sequential, 1=interleaved
    CAS_LATENCY      : std_logic_vector(2 downto 0) := "010"; -- 010=below 100MHz, 011=above 100MHz
    WRITE_BURST_MODE : std_logic_vector(0 downto 0) := "1"    -- 0=burst, 1=single bit
  );
  port (
    -- reset
    reset : in std_logic;

    -- clock signals
    clk : in std_logic;
    cke : in std_logic := '1';

    -- asserted when there is an operation pending
    busy : out std_logic;

    -- asserted when the pending operation has completed
    done : out std_logic;

    -- address
    addr : in std_logic_vector(24 downto 0);

    -- data in
    din : in std_logic_vector(15 downto 0);

    -- data out
    dout : out std_logic_vector(15 downto 0);

    -- read enable
    rden : in std_logic;

    -- write enable
    wren : in std_logic;

    -- SDRAM IO
    sdram_clk   : out std_logic;
    sdram_cke   : out std_logic;
    sdram_cs_n  : out std_logic;
    sdram_ras_n : out std_logic;
    sdram_cas_n : out std_logic;
    sdram_we_n  : out std_logic;
    sdram_ba    : out std_logic_vector(1 downto 0);
    sdram_a     : out std_logic_vector(12 downto 0);
    sdram_dqm   : out std_logic_vector(1 downto 0);
    sdram_dq    : inout std_logic_vector(15 downto 0)
  );
end sdram;

architecture arch of sdram is
  -- commands
  constant CMD_LOAD_MODE    : std_logic_vector(3 downto 0) := "0000"; -- 0
  constant CMD_AUTO_REFRESH : std_logic_vector(3 downto 0) := "0001"; -- 1
  constant CMD_PRECHARGE    : std_logic_vector(3 downto 0) := "0010"; -- 2
  constant CMD_ACTIVE       : std_logic_vector(3 downto 0) := "0011"; -- 3
  constant CMD_WRITE        : std_logic_vector(3 downto 0) := "0100"; -- 4
  constant CMD_READ         : std_logic_vector(3 downto 0) := "0101"; -- 5
  constant CMD_STOP         : std_logic_vector(3 downto 0) := "0110"; -- 6
  constant CMD_NOP          : std_logic_vector(3 downto 0) := "0111"; -- 7
  constant CMD_INHIBIT      : std_logic_vector(3 downto 0) := "1000"; -- 8

  -- mode register
  constant MODE_REGISTER : std_logic_vector(12 downto 0) := "000" & WRITE_BURST_MODE & "00" & CAS_LATENCY & BURST_TYPE & BURST_LENGTH;

  constant COL_WIDTH  : natural := 9;
  constant ROW_WIDTH  : natural := 13;
  constant BANK_WIDTH : natural := 2;

  type state_t is (INIT, IDLE, REFRESH, READ, READ_DONE, WRITE, PRECHARGE);

  -- state signals
  signal state, next_state : state_t;

	-- command signals
  signal cmd, next_cmd : std_logic_vector(3 downto 0);

  -- registers
  signal addr_reg : std_logic_vector(24 downto 0);
  signal din_reg  : std_logic_vector(15 downto 0);
  signal dout_reg : std_logic_vector(15 downto 0);

  -- control signals
  signal start_refresh : std_logic;

  -- aliases to decode the address register
  alias col  : std_logic_vector(COL_WIDTH-1 downto 0) is addr_reg(8 downto 0);
  alias row  : std_logic_vector(ROW_WIDTH-1 downto 0) is addr_reg(22 downto 10);
  alias bank : std_logic_vector(BANK_WIDTH-1 downto 0) is addr_reg(24 downto 23);
begin
  latch_state : process (clk, reset)
  begin
    if reset = '1' then
      state <= INIT;
    elsif rising_edge(clk) then
      state <= next_state;
      cmd   <= next_cmd;
    end if;
  end process;

  fsm : process (state, start_refresh, rden, wren)
  begin
    next_state <= state;

    -- default to a NOP command
    next_cmd <= CMD_NOP;

    case state is
      -- this is the default state, we need to initialise the memory
      when INIT =>
        -- wait 100us
        -- precharge all banks
        -- auto refresh
        -- auto refresh
        -- program mode register
        next_state <= IDLE;

      -- wait for a read/write or refresh operation
      when IDLE =>
        if start_refresh = '1' then
          next_state <= REFRESH;
        elsif rden = '1' then
          next_state <= READ;
          next_cmd   <= CMD_ACTIVE;
        elsif wren = '1' then
          next_state <= WRITE;
          next_cmd   <= CMD_ACTIVE;
        end if;

      -- perform refresh
      when REFRESH =>
        next_state <= IDLE;
        next_cmd   <= CMD_AUTO_REFRESH;

      -- perform read
      when READ =>
        next_state <= READ_DONE;
        next_cmd   <= CMD_READ;

      -- read is finished
      when READ_DONE =>
        next_state <= PRECHARGE;

      -- perform write
      when WRITE =>
        next_state <= PRECHARGE;
        next_cmd   <= CMD_WRITE;

      -- close row
      when PRECHARGE =>
        next_state <= IDLE;
        next_cmd   <= CMD_PRECHARGE;
    end case;
  end process;

  -- latch input signals
  process (clk)
  begin
    if rising_edge(clk) then
      if state = IDLE then
        -- latch the address
        addr_reg <= addr;

        -- latch the input data
        din_reg <= din;
      elsif state = READ_DONE then
        -- latch the SDRAM data
        dout_reg <= sdram_dq;
      end if;
    end if;
  end process;

	-- set SDRAM bank and address
  process (state, addr_reg)
  begin
    sdram_ba <= (others => '0');
    sdram_a  <= (others => '0');

    case state is
      when INIT =>
        sdram_a <= MODE_REGISTER;
      when IDLE =>
        sdram_ba <= bank;
        sdram_a  <= row;
      when READ =>
        sdram_ba <= bank;
        sdram_a  <= "0000" & col;
      when WRITE =>
        sdram_ba <= bank;
        sdram_a  <= "0000" & col;
      when others => null;
    end case;
  end process;

  -- FIXME
  busy <= '0';
  done <= '0';

	-- set SDRAM control signals
  (sdram_cs_n, sdram_ras_n, sdram_cas_n, sdram_we_n) <= cmd;

  -- set SDRAM data bus
  sdram_dq <= din_reg when state = WRITE else (others => 'Z');
  sdram_dqm <= "00";
  sdram_clk <= clk;
  sdram_cke <= '1';

  dout <= dout_reg;
end architecture arch;
