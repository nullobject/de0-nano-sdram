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

use work.types.all;

-- This module implements a memory controller for a 16Mx16 SDRAM memory module.
--
-- The ready signal is asserted when the memory controller is ready to execute
-- a read or write operation.
entity sdram is
  port (
    -- clock
    clk : in std_logic;

    -- reset
    reset : in std_logic;

    -- controller interface
    addr  : in std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
    din   : in std_logic_vector(SDRAM_INPUT_DATA_WIDTH-1 downto 0);
    dout  : out std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
    rden  : in std_logic;
    wren  : in std_logic;
    ready : out std_logic;

    -- SDRAM interface
    sdram_clk   : out std_logic;
    sdram_cke   : out std_logic;
    sdram_cs_n  : out std_logic;
    sdram_ras_n : out std_logic;
    sdram_cas_n : out std_logic;
    sdram_we_n  : out std_logic;
    sdram_ba    : out std_logic_vector(SDRAM_BANK_WIDTH-1 downto 0);
    sdram_a     : out std_logic_vector(SDRAM_ADDR_WIDTH-1 downto 0);
    sdram_dqm   : out std_logic_vector(SDRAM_DATA_MASK_WIDTH-1 downto 0);
    sdram_dq    : inout std_logic_vector(SDRAM_DATA_WIDTH-1 downto 0)
  );
end sdram;

architecture arch of sdram is
  subtype command_t is std_logic_vector(2 downto 0);

  -- commands
  constant CMD_LOAD_MODE    : command_t := "000";
  constant CMD_AUTO_REFRESH : command_t := "001";
  constant CMD_PRECHARGE    : command_t := "010";
  constant CMD_ACTIVE       : command_t := "011";
  constant CMD_WRITE        : command_t := "100";
  constant CMD_READ         : command_t := "101";
  constant CMD_STOP         : command_t := "110";
  constant CMD_NOP          : command_t := "111";

  -- the number of words in a burst
  constant BURST_LENGTH : natural := 2;

  -- the ordering of the accesses within a burst
  constant BURST_TYPE : std_logic := '0'; -- 0=sequential, 1=interleaved

  -- the delay in clock cycles, between the start of a read command and the
  -- availability of the output data
  constant CAS_LATENCY : natural := 2; -- 2=below 133MHz, 3=above 133MHz

  -- the write burst mode toggles bursting during a write command
  constant WRITE_BURST_MODE : std_logic := '1'; -- 0=burst, 1=single

  -- the value written to the mode register to configure the memory
  constant MODE : std_logic_vector(SDRAM_ADDR_WIDTH-1 downto 0) := (
    "000" &
    WRITE_BURST_MODE &
    "00" &
    std_logic_vector(to_unsigned(CAS_LATENCY, 3)) &
    BURST_TYPE &
    std_logic_vector(to_unsigned(ilog2(BURST_LENGTH), 3))
  );

  -- The minimum number of clock ticks between each auto refresh command. The
  -- datasheet specifies that the auto refresh command needs to be executed
  -- 8192 times every 64ms. This value has been calculated for a 100MHz clock
  -- frequency.
  constant TICKS_PER_REFRESH : natural := 781;

  type state_t is (INIT, LOAD_MODE, IDLE, ACTIVE, READ, READ_WAIT, WRITE, REFRESH);

  -- state signals
  signal state, next_state : state_t;

  -- command signals
  signal cmd, next_cmd : command_t := CMD_NOP;

  -- counters
  signal wait_counter    : natural range 0 to 31;
  signal refresh_counter : natural range 0 to 1023;

  -- registers
  signal addr_reg : std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal din_reg  : std_logic_vector(SDRAM_INPUT_DATA_WIDTH-1 downto 0);
  signal dout_reg : std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
  signal wren_reg : std_logic;
  signal word_reg : std_logic_vector(SDRAM_DATA_WIDTH-1 downto 0);

  -- aliases to decode the address register
  alias col  : std_logic_vector(SDRAM_COL_WIDTH-1 downto 0) is addr_reg(SDRAM_COL_WIDTH-1 downto 0);
  alias row  : std_logic_vector(SDRAM_ROW_WIDTH-1 downto 0) is addr_reg(SDRAM_COL_WIDTH+SDRAM_ROW_WIDTH-1 downto SDRAM_COL_WIDTH);
  alias bank : std_logic_vector(SDRAM_BANK_WIDTH-1 downto 0) is addr_reg(SDRAM_COL_WIDTH+SDRAM_ROW_WIDTH+SDRAM_BANK_WIDTH-1 downto SDRAM_COL_WIDTH+SDRAM_ROW_WIDTH);
begin
  -- state machine
  fsm : process (state, cmd, wait_counter, refresh_counter, rden, wren, wren_reg)
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

      -- execute an auto refresh
      when REFRESH =>
        if wait_counter = 6 then
          next_state <= IDLE;
        end if;

      -- activate the row
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
        if wait_counter = CAS_LATENCY then
          next_state <= IDLE;
        end if;

      -- execute a write command
      when WRITE =>
        next_state <= IDLE;
    end case;
  end process;

  -- latch the next state
  latch_next_state : process (clk, reset)
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
      if state /= next_state then -- state changing
        wait_counter <= 0;
      else
        wait_counter <= wait_counter + 1;
      end if;
    end if;
  end process;

  -- Update the refresh counter.
  --
  -- The refresh counter is used to periodically trigger an auto refresh
  -- command.
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

  -- Latch the output data.
  --
  -- We need to latch the words from the SDRAM into the output register, as
  -- data is bursted.
  latch_read_data : process (clk)
  begin
    if rising_edge(clk) then
      if state = READ_WAIT then
        if wait_counter = CAS_LATENCY-1 then -- first word
          word_reg <= sdram_dq;
        elsif wait_counter = CAS_LATENCY-0 then -- last word
          dout_reg <= sdram_dq & word_reg;
        end if;
      end if;
    end if;
  end process;

  -- assert SDRAM chip select
  sdram_cs_n <= '0';

  -- set SDRAM clock signals
  sdram_clk <= clk;

  -- deassert the clock enable at the beginning of the initialisation sequence
  sdram_cke <= '0' when state = INIT and wait_counter = 0 else '1';

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

  -- set SDRAM data mask
  sdram_dqm <= (others => '0');

  -- set SDRAM input data if we're writing, otherwise put it into a high impedance state
  sdram_dq <= din_reg when state = WRITE else (others => 'Z');

	-- set SDRAM control signals
  (sdram_ras_n, sdram_cas_n, sdram_we_n) <= cmd;

  -- the memory controller is ready if we're in the IDLE state
  ready <= '1' when state = IDLE else '0';

  -- set output data
  dout  <= dout_reg;
end architecture arch;
