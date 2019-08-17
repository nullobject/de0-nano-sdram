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
    CAS_LATENCY      : std_logic_vector(2 downto 0) := "010"; -- 0=below 100MHz, 011=above 100MHz
    WRITE_BURST_MODE : std_logic_vector(0 downto 0) := "1"    -- 0=burst, 1=single bit
  );
  port (
    -- reset
    reset : in std_logic;

    -- clock signals
    clk : in std_logic;
    cke : in std_logic := '1';

    -- asserted when there is a pending operation
    busy : out std_logic;

    -- asserted when the current operation has completed
    ready : out std_logic;

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

    -- SDRAM interface
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
  constant CMD_LOAD_MODE    : std_logic_vector(3 downto 0) := "0000";
  constant CMD_AUTO_REFRESH : std_logic_vector(3 downto 0) := "0001";
  constant CMD_PRECHARGE    : std_logic_vector(3 downto 0) := "0010";
  constant CMD_ACTIVE       : std_logic_vector(3 downto 0) := "0011";
  constant CMD_WRITE        : std_logic_vector(3 downto 0) := "0100";
  constant CMD_READ         : std_logic_vector(3 downto 0) := "0101";
  constant CMD_STOP         : std_logic_vector(3 downto 0) := "0110";
  constant CMD_NOP          : std_logic_vector(3 downto 0) := "0111";
  constant CMD_INHIBIT      : std_logic_vector(3 downto 0) := "1000";

  -- this value is written to the mode register to configure the memory
  constant MODE : std_logic_vector(12 downto 0) := "000" & WRITE_BURST_MODE & "00" & CAS_LATENCY & BURST_TYPE & BURST_LENGTH;

  -- this value can be pre-pended to the column address to enable auto
  -- precharging during a read/write operation
  constant AUTO_PRECHARGE : std_logic_vector(3 downto 0) := "0010";

  constant COL_WIDTH  : natural := 9;
  constant ROW_WIDTH  : natural := 13;
  constant BANK_WIDTH : natural := 2;

  type state_t is (INIT, IDLE, ACTIVE, READ, READ_WAIT, WRITE, REFRESH, PRECHARGE);

  -- state signals
  signal state, next_state : state_t;

  -- registers
  signal wren_reg : std_logic;
  signal addr_reg : std_logic_vector(24 downto 0);
  signal din_reg  : std_logic_vector(15 downto 0);
  signal dout_reg : std_logic_vector(15 downto 0);

  -- counter
  signal t : natural range 0 to 7;

  -- control signals
  signal read_done : std_logic;

  -- aliases to decode the address register
  alias col  : std_logic_vector(COL_WIDTH-1 downto 0) is addr_reg(8 downto 0);
  alias row  : std_logic_vector(ROW_WIDTH-1 downto 0) is addr_reg(22 downto 10);
  alias bank : std_logic_vector(BANK_WIDTH-1 downto 0) is addr_reg(24 downto 23);
begin
  -- state machine
  fsm : process (state, rden, wren, wren_reg, read_done)
  begin
    next_state <= state;

    case state is
      -- this is the default state, we need to initialise the memory
      when INIT =>
        -- wait 100us
        -- precharge all banks
        -- auto refresh
        -- auto refresh
        -- program mode register
        next_state <= IDLE;

      -- wait for a read/write request
      when IDLE =>
        if rden = '1' or wren = '1' then
          next_state <= ACTIVE;
        end if;

      -- begin the read/write operation
      when ACTIVE =>
        if wren_reg = '1' then
          next_state <= WRITE;
        else
          next_state <= READ;
        end if;

      -- perform a read operation
      when READ =>
        next_state <= READ_WAIT;

      -- wait for the read operation to complete
      when READ_WAIT =>
        if read_done = '1' then
          next_state <= IDLE;
        end if;

      -- perform a write operation
      when WRITE =>
        next_state <= IDLE;

      -- close the row
      when PRECHARGE =>
        next_state <= IDLE;

      -- perform an auto refresh operation
      when REFRESH =>
        next_state <= IDLE;
    end case;
  end process;

  latch_state : process (clk, reset)
  begin
    if reset = '1' then
      state <= INIT;
    elsif rising_edge(clk) then
      state <= next_state;
    end if;
  end process;

  update_counter : process(clk, reset)
  begin
    if reset = '1' then
      t <= 0;
    elsif rising_edge(clk) then
      if state /= next_state then  -- state is changing
        t <= 0;
      else
        t <= t + 1;
      end if;
    end if;
  end process;

  -- Latch the input signals.
  --
  -- Some of the input signals need to be registered, because they are used
  -- during later states.
  latch_input_signals : process (clk)
  begin
    if rising_edge(clk) then
      if state = IDLE then
        addr_reg <= addr;
        din_reg  <= din;
        wren_reg <= wren;
      end if;
    end if;
  end process;

  -- latch the SDRAM data once the read operation has completed
  latch_sdram_data : process (clk)
  begin
    if rising_edge(clk) then
      if read_done = '1' then
        dout_reg <= sdram_dq;
      end if;
    end if;
  end process;

  -- FIXME: set control signals
  busy  <= '1' when state /= IDLE else '0';
  ready <= '1' when state = IDLE else '0';

  -- the read operation is complete after the CAS latency has elapsed
  read_done <= '1' when state = READ_WAIT and t >= unsigned(CAS_LATENCY)-1 else '0';

  -- set SDRAM clock signals
  sdram_clk <= clk;
  sdram_cke <= '1';

  -- set SDRAM bank
  with state select
    sdram_ba <=
      bank            when ACTIVE,
      bank            when READ,
      bank            when WRITE,
      (others => '0') when others;

  -- set SDRAM address
  with state select
    sdram_a <=
      MODE                 when INIT,
      row                  when ACTIVE,
      AUTO_PRECHARGE & col when READ,
      AUTO_PRECHARGE & col when WRITE,
      (others => '0')      when others;

  -- set SDRAM data bus if we're writing, otherwise put it into a high impedance state
  sdram_dqm <= "00";
  sdram_dq <= din_reg when state = WRITE else (others => 'Z');

	-- set SDRAM control signals
  with state select
    (sdram_cs_n, sdram_ras_n, sdram_cas_n, sdram_we_n) <=
      CMD_ACTIVE    when ACTIVE,
      CMD_READ      when READ,
      CMD_WRITE     when WRITE,
      CMD_PRECHARGE when PRECHARGE,
      CMD_NOP       when others;

  -- set output data
  dout <= dout_reg;
end architecture arch;
