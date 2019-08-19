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

entity top is
  port (
    -- 50MHz clock
    clk : in std_logic;

    -- buttons
    key : in std_logic_vector(0 downto 0);

    -- LEDs
    led : out std_logic_vector(7 downto 0);

    -- SDRAM interface
    sdram_a     : out std_logic_vector(SDRAM_ADDR_WIDTH-1 downto 0);
    sdram_dq    : inout std_logic_vector(SDRAM_DATA_WIDTH-1 downto 0);
    sdram_dqm   : out std_logic_vector(SDRAM_DATA_MASK_WIDTH-1 downto 0);
    sdram_ba    : out std_logic_vector(SDRAM_BANK_WIDTH-1 downto 0);
    sdram_cas_n : out std_logic;
    sdram_cke   : out std_logic;
    sdram_cs_n  : out std_logic;
    sdram_ras_n : out std_logic;
    sdram_we_n  : out std_logic;
    sdram_clk   : out std_logic
  );
end top;

architecture arch of top is
  type state_t is (READ, READ_WAIT, WRITE, WRITE_WAIT);

  -- clock signals
  -- signal clk_5      : std_logic;
  -- signal pll_locked : std_logic;

  signal reset : std_logic;

  -- state signals
  signal state, next_state : state_t;

  -- counters
  signal wait_counter : natural range 0 to 31;
  signal data_counter : natural range 0 to 255;

  -- IO signals
  signal ioctl_addr : std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal ioctl_data : byte_t;
  signal ioctl_wren : std_logic;

  -- ROM signals
  signal sprite_addr : std_logic_vector(SPRITE_ROM_ADDR_WIDTH-1 downto 0);
  signal sprite_data : byte_t;
  signal char_addr   : std_logic_vector(CHAR_ROM_ADDR_WIDTH-1 downto 0);
  signal char_data   : byte_t;
  signal fg_addr     : std_logic_vector(FG_ROM_ADDR_WIDTH-1 downto 0);
  signal fg_data     : byte_t;
  signal bg_addr     : std_logic_vector(BG_ROM_ADDR_WIDTH-1 downto 0);
  signal bg_data     : byte_t;
begin
  -- pll : entity work.pll
  -- port map (
  --   inclk0 => clk,
  --   c0     => clk_5,
  --   locked => pll_locked
  -- );

  -- ROM controller
  rom_controller : entity work.rom_controller
  port map (
    reset => reset,
    clk   => clk,

    -- write interface
    ioctl_addr => ioctl_addr,
    ioctl_data => ioctl_data,
    ioctl_wren => ioctl_wren,

    -- read interface
    sprite_addr => sprite_addr,
    sprite_data => sprite_data,
    char_addr   => char_addr,
    char_data   => char_data,
    fg_addr     => fg_addr,
    fg_data     => fg_data,
    bg_addr     => bg_addr,
    bg_data     => bg_data,

    -- SDRAM interface
    sdram_clk   => sdram_clk,
    sdram_cke   => sdram_cke,
    sdram_cs_n  => sdram_cs_n,
    sdram_ras_n => sdram_ras_n,
    sdram_cas_n => sdram_cas_n,
    sdram_we_n  => sdram_we_n,
    sdram_ba    => sdram_ba,
    sdram_a     => sdram_a,
    sdram_dqm   => sdram_dqm,
    sdram_dq    => sdram_dq
  );

  -- state machine
  fsm : process (state, wait_counter, data_counter)
  begin
    next_state <= state;

    case state is
      when READ =>
        next_state <= READ_WAIT;

      when READ_WAIT =>
        if wait_counter = 6 then
          if data_counter = 63 then
            next_state <= WRITE;
          else
            next_state <= READ;
          end if;
        end if;

      when WRITE =>
        next_state <= WRITE_WAIT;

      when WRITE_WAIT =>
        if wait_counter = 3 then
          if data_counter = 255 then
            next_state <= READ;
          else
            next_state <= WRITE;
          end if;
        end if;
    end case;
  end process;

  -- latch the next state
  latch_next_state : process (clk, reset)
  begin
    if reset = '1' then
      state <= READ;
    elsif rising_edge(clk) then
      state <= next_state;
    end if;
  end process;

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

  update_data_counter : process (clk, reset)
  begin
    if reset = '1' then
      data_counter <= 0;
    elsif rising_edge(clk) then
      if state = READ_WAIT and next_state /= READ_WAIT then -- leaving the READ_WAIT state
        if data_counter = 63 then
          data_counter <= 0;
        else
          data_counter <= data_counter + 1;
        end if;
      elsif state = WRITE_WAIT and next_state /= WRITE_WAIT then -- leaving the WRITE_WAIT state
        data_counter <= data_counter + 1;
      end if;
    end if;
  end process;

  -- reset <= not pll_locked;
  reset <= not key(0);

  -- set IOCTL signals
  ioctl_addr <= std_logic_vector(to_unsigned(data_counter, ioctl_addr'length));
  ioctl_data <= std_logic_vector(to_unsigned(data_counter, ioctl_data'length));
  ioctl_wren <= '1' when state = WRITE else '0';

  -- set ROM signals
  sprite_addr <= std_logic_vector(to_unsigned(data_counter, sprite_addr'length));
  char_addr   <= std_logic_vector(to_unsigned(data_counter, char_addr'length));
  fg_addr     <= std_logic_vector(to_unsigned(data_counter, fg_addr'length));
  bg_addr     <= std_logic_vector(to_unsigned(data_counter, bg_addr'length));

  -- set output data
  led <= sprite_data;
end architecture arch;
