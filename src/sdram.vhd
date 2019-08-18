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

  -- the number of words to read in a burst
  constant BURST_LENGTH : unsigned(2 downto 0) := "000"; -- 000=1, 001=2, 010=4, 011=8

  -- the ordering of the accesses within a burst
  constant BURST_TYPE : std_logic_vector(0 downto 0) := "0"; -- 0=sequential, 1=interleaved

  -- the delay in clock cycles, between the start of a read command and the
  -- availability of the output data
  constant CAS_LATENCY : unsigned(2 downto 0) := "010"; -- 010=below 100MHz, 011=above 100MHz

  -- the write burst mode toggles bursting during a write command
  constant WRITE_BURST_MODE : std_logic_vector(0 downto 0) := "1"; -- 0=burst, 1=single

  -- the value written to the mode register to configure the memory
  constant MODE : std_logic_vector(12 downto 0) := (
    "000" &
    WRITE_BURST_MODE &
    "00" &
    std_logic_vector(CAS_LATENCY) &
    BURST_TYPE &
    std_logic_vector(BURST_LENGTH)
  );

  -- The minimum number of clock ticks between each auto refresh command. The
  -- datasheet specifies that the auto refresh command needs to be executed
  -- 8192 times every 64ms.
  constant TICKS_PER_REFRESH : natural := 781;

  constant COL_WIDTH  : natural := 9;
  constant ROW_WIDTH  : natural := 13;
  constant BANK_WIDTH : natural := 2;

  type state_t is (INIT, LOAD_MODE, IDLE, ACTIVE, READ, READ_WAIT, WRITE, REFRESH);

  -- state signals
  signal state, next_state : state_t;

  -- command signals
  signal cmd, next_cmd : std_logic_vector(3 downto 0) := CMD_NOP;

  -- registers
  signal addr_reg  : std_logic_vector(24 downto 0);
  signal din_reg   : std_logic_vector(15 downto 0);
  signal dout_reg  : std_logic_vector(15 downto 0);
  signal wren_reg  : std_logic;
  signal ready_reg : std_logic;

  -- counters
  signal wait_counter    : natural range 0 to 31;
  signal refresh_counter : natural range 0 to 1023;

  -- aliases to decode the address register
  alias col  : std_logic_vector(COL_WIDTH-1 downto 0) is addr_reg(8 downto 0);
  alias row  : std_logic_vector(ROW_WIDTH-1 downto 0) is addr_reg(22 downto 10);
  alias bank : std_logic_vector(BANK_WIDTH-1 downto 0) is addr_reg(24 downto 23);
begin
  -- state machine
  fsm : process (state, cmd, wait_counter, refresh_counter, rden, wren, wren_reg, ready_reg)
  begin
    next_state <= state;

    -- default to a NOP command
    next_cmd <= CMD_NOP;

    case state is
      -- execute the initialisation sequence
      when INIT =>
        if wait_counter = 0 then
          next_cmd <= CMD_PRECHARGE;
        elsif wait_counter = 2 then
          next_cmd <= CMD_AUTO_REFRESH;
        elsif wait_counter = 11 then
          next_cmd <= CMD_AUTO_REFRESH;
        elsif wait_counter = 20 then
          next_state <= LOAD_MODE;
          next_cmd   <= CMD_LOAD_MODE;
        end if;

      -- load the mode register
      when LOAD_MODE =>
        if wait_counter = 1 then
          next_state <= IDLE;
        end if;

      -- wait for a read/write request
      when IDLE =>
        if refresh_counter >= TICKS_PER_REFRESH-1 then
          next_state <= REFRESH;
          next_cmd   <= CMD_AUTO_REFRESH;
        elsif rden = '1' or wren = '1' then
          next_state <= ACTIVE;
          next_cmd   <= CMD_ACTIVE;
        end if;

      -- execute an auto refresh command
      when REFRESH =>
        if wait_counter = 8 then
          next_state <= IDLE;
        end if;

      -- begin the read/write command
      when ACTIVE =>
        if wait_counter = 1 then
          if wren_reg = '1' then
            next_state <= WRITE;
            next_cmd   <= CMD_WRITE;
          else
            next_state <= READ;
            next_cmd   <= CMD_READ;
          end if;
        end if;

      -- execute a read command
      when READ =>
        next_state <= READ_WAIT;

      -- wait for the read to complete
      when READ_WAIT =>
        if ready_reg = '1' then
          next_state <= IDLE;
        end if;

      -- execute a write command
      when WRITE =>
        next_state <= IDLE;
    end case;
  end process;

  -- latch the next state
  latch_state : process (clk, reset)
  begin
    if reset = '1' then
      state <= INIT;
      cmd   <= CMD_NOP;
    elsif rising_edge(clk) then
      state <= next_state;
      cmd   <= next_cmd;
    end if;
  end process;

  -- Update the wait counter.
  --
  -- The wait counter is used to hold the state for a number of clock ticks.
  update_wait_counter : process (clk, reset)
  begin
    if reset = '1' then
      wait_counter <= 0;
    elsif rising_edge(clk) then
      if state /= next_state then -- state is changing
        wait_counter <= 0;
      else
        wait_counter <= wait_counter + 1;
      end if;
    end if;
  end process;

  -- Update the refresh counter.
  --
  -- The refresh counter is cleared once an auto refresh command is executed.
  update_refresh_counter : process (clk, reset)
  begin
    if reset = '1' then
      refresh_counter <= 0;
    elsif rising_edge(clk) then
      if state = REFRESH then
        refresh_counter <= 0;
      else
        refresh_counter <= refresh_counter + 1;
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
      if ready_reg = '1' then
        dout_reg <= sdram_dq;
      end if;
    end if;
  end process;

  -- set SDRAM clock signals
  sdram_clk <= clk;

  -- deassert the clock enable at the beginning of the initialisation sequence
  sdram_cke <= '0' when state = INIT and wait_counter = 0 else '1';

  -- the SDRAM data is ready after the CAS latency has elapsed
  ready_reg <= '1' when state = READ_WAIT and wait_counter >= CAS_LATENCY-1 else '0';

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
      "0010000000000" when INIT,
      MODE            when LOAD_MODE,
      row             when ACTIVE,
      "0010" & col    when READ,  -- auto precharge
      "0010" & col    when WRITE, -- auto precharge
      (others => '0') when others;

  -- set SDRAM data bus if we're writing, otherwise put it into a high impedance state
  sdram_dqm <= "00";
  sdram_dq <= din_reg when state = WRITE else (others => 'Z');

	-- set SDRAM control signals
  (sdram_cs_n, sdram_ras_n, sdram_cas_n, sdram_we_n) <= cmd;

  -- set output signals
  busy  <= '1' when state /= IDLE else '0';
  ready <= ready_reg;
  dout  <= dout_reg;
end architecture arch;
