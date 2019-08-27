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
    sdram_ba    : out std_logic_vector(SDRAM_BANK_WIDTH-1 downto 0);
    sdram_dq    : inout std_logic_vector(SDRAM_DATA_WIDTH-1 downto 0);
    sdram_clk   : out std_logic;
    sdram_cke   : out std_logic;
    sdram_cs_n  : out std_logic;
    sdram_ras_n : out std_logic;
    sdram_cas_n : out std_logic;
    sdram_we_n  : out std_logic;
    sdram_dqml  : out std_logic;
    sdram_dqmh  : out std_logic
  );
end top;

architecture arch of top is
  type state_t is (INIT, WRITE, READ);

  -- clock signals
  signal sys_clk : std_logic;
  signal rom_clk : std_logic;
  signal cen_6   : std_logic;

  signal reset : std_logic;

  -- state signals
  signal state, next_state : state_t;

  -- counters
  signal data_counter : natural range 0 to 255;

  -- IOCTL signals
  signal ioctl_addr : std_logic_vector(21 downto 0);
  signal ioctl_data : std_logic_vector(15 downto 0);
  signal ioctl_we   : std_logic;

  -- ROM signals
  signal sprite_rom_addr : std_logic_vector(SPRITE_ROM_ADDR_WIDTH-1 downto 0);
  signal sprite_rom_data : std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
  signal char_rom_addr   : std_logic_vector(CHAR_ROM_ADDR_WIDTH-1 downto 0);
  signal char_rom_data   : std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
  signal fg_rom_addr     : std_logic_vector(FG_ROM_ADDR_WIDTH-1 downto 0);
  signal fg_rom_data     : std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
  signal bg_rom_addr     : std_logic_vector(BG_ROM_ADDR_WIDTH-1 downto 0);
  signal bg_rom_data     : std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);

  -- SDRAM signals
  signal sdram_addr  : std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal sdram_din   : std_logic_vector(SDRAM_INPUT_DATA_WIDTH-1 downto 0);
  signal sdram_dout  : std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
  signal sdram_valid : std_logic;
  signal sdram_ready : std_logic;
  signal sdram_rden  : std_logic;
  signal sdram_wren  : std_logic;

  -- debug
  attribute keep : boolean;
  attribute keep of rom_clk : signal is true;
  attribute keep of sys_clk : signal is true;
  attribute keep of cen_6   : signal is true;
begin
  pll : entity work.pll
  port map (
    inclk0 => clk,
    c0     => sdram_clk, -- 48MHz
    c1     => rom_clk,   -- 48MHz
    c2     => sys_clk,   -- 12Mhz
    locked => open
  );

  -- generate a 6MHz clock enable signal
  clock_divider_6 : entity work.clock_divider
  generic map (DIVISOR => 2)
  port map (clk => sys_clk, cen => cen_6);

  -- SDRAM controller
  sdram : entity work.sdram
  generic map (CLK_FREQ => 48.0)
  port map (
    clk => rom_clk,

    reset => reset,

    -- IO interface
    addr  => sdram_addr,
    din   => sdram_din,
    dout  => sdram_dout,
    ready => sdram_ready,
    valid => sdram_valid,
    rden  => sdram_rden,
    wren  => sdram_wren,

    -- SDRAM interface
    sdram_a     => sdram_a,
    sdram_ba    => sdram_ba,
    sdram_dq    => sdram_dq,
    sdram_cke   => sdram_cke,
    sdram_cs_n  => sdram_cs_n,
    sdram_ras_n => sdram_ras_n,
    sdram_cas_n => sdram_cas_n,
    sdram_we_n  => sdram_we_n,
    sdram_dqml  => sdram_dqml,
    sdram_dqmh  => sdram_dqmh
  );

  -- ROM controller
  rom_controller : entity work.rom_controller
  port map (
    clk => rom_clk,

    reset => reset,

    -- read interface
    sprite_rom_addr => sprite_rom_addr,
    sprite_rom_data => sprite_rom_data,
    char_rom_addr   => char_rom_addr,
    char_rom_data   => char_rom_data,
    fg_rom_addr     => fg_rom_addr,
    fg_rom_data     => fg_rom_data,
    bg_rom_addr     => bg_rom_addr,
    bg_rom_data     => bg_rom_data,

    -- write interface
    ioctl_addr => ioctl_addr,
    ioctl_data => ioctl_data,
    ioctl_we   => ioctl_we,

    -- SDRAM interface
    sdram_addr  => sdram_addr,
    sdram_din   => sdram_din,
    sdram_dout  => sdram_dout,
    sdram_valid => sdram_valid,
    sdram_ready => sdram_ready
  );

  -- state machine
  fsm : process (state, data_counter)
  begin
    next_state <= state;

    case state is
      when INIT =>
        if data_counter = 255 then
          next_state <= WRITE;
        end if;

      when WRITE =>
        if data_counter = 255 then
          next_state <= READ;
        end if;

      when READ =>
    end case;
  end process;

  -- latch the next state
  latch_next_state : process (sys_clk, reset)
  begin
    if reset = '1' then
      state <= INIT;
    elsif rising_edge(sys_clk) then
      if cen_6 = '1' then
        state <= next_state;
      end if;
    end if;
  end process;

  update_data_counter : process (sys_clk, reset)
  begin
    if reset = '1' then
      data_counter <= 0;
    elsif rising_edge(sys_clk) then
      if cen_6 = '1' then
        data_counter <= data_counter + 1;
      end if;
    end if;
  end process;

  reset <= not key(0);

  ioctl_addr <= std_logic_vector(to_unsigned(data_counter, ioctl_addr'length)) when ioctl_we = '1' else (others => '0');
  ioctl_data <= std_logic_vector(to_unsigned(data_counter, ioctl_data'length));
  ioctl_we   <= '1' when state = WRITE else '0';

  sdram_rden <= not ioctl_we;
  sdram_wren <= ioctl_we;

  -- set ROM signals
  sprite_rom_addr <= std_logic_vector(to_unsigned(data_counter/4, sprite_rom_addr'length));
  char_rom_addr   <= std_logic_vector(to_unsigned(data_counter/4, char_rom_addr'length));
  fg_rom_addr     <= std_logic_vector(to_unsigned(data_counter/4, fg_rom_addr'length));
  bg_rom_addr     <= std_logic_vector(to_unsigned(data_counter/4, bg_rom_addr'length));

  -- set output data
  led <= sprite_rom_data(7 downto 0);
end architecture arch;
