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
    sdram_a     : out unsigned(SDRAM_ADDR_WIDTH-1 downto 0);
    sdram_ba    : out unsigned(SDRAM_BANK_WIDTH-1 downto 0);
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
  constant TILE_ROM_SIZE : natural := 4096;

  type state_t is (INIT, PRELOAD, LOAD, IDLE);

  -- clock signals
  signal sys_clk : std_logic;
  signal cen_2   : std_logic;

  signal reset : std_logic;

  -- state signals
  signal state, next_state : state_t;

  -- counters
  signal data_counter : natural range 0 to TILE_ROM_SIZE-1;

  -- IOCTL signals
  signal ioctl_addr     : unsigned(IOCTL_ADDR_WIDTH-1 downto 0);
  signal ioctl_data     : byte_t;
  signal ioctl_wr       : std_logic;
  signal ioctl_download : std_logic;

  -- SDRAM signals
  signal sdram_addr  : unsigned(SDRAM_CTRL_ADDR_WIDTH-1 downto 0);
  signal sdram_din   : std_logic_vector(SDRAM_CTRL_DATA_WIDTH-1 downto 0);
  signal sdram_dout  : std_logic_vector(SDRAM_CTRL_DATA_WIDTH-1 downto 0);
  signal sdram_we    : std_logic;
  signal sdram_req   : std_logic;
  signal sdram_ack   : std_logic;
  signal sdram_valid : std_logic;
  signal sdram_ready : std_logic;

  -- ROM signals
  signal main_rom_addr   : unsigned(MAIN_ROM_ADDR_WIDTH-1 downto 0);
  signal main_rom_data   : std_logic_vector(MAIN_ROM_DATA_WIDTH-1 downto 0);
  signal sprite_rom_addr : unsigned(SPRITE_ROM_ADDR_WIDTH-1 downto 0);
  signal sprite_rom_data : std_logic_vector(SPRITE_ROM_DATA_WIDTH-1 downto 0);
  signal char_rom_addr   : unsigned(CHAR_ROM_ADDR_WIDTH-1 downto 0);
  signal char_rom_data   : std_logic_vector(CHAR_ROM_DATA_WIDTH-1 downto 0);
  signal fg_rom_addr     : unsigned(FG_ROM_ADDR_WIDTH-1 downto 0);
  signal fg_rom_data     : std_logic_vector(FG_ROM_DATA_WIDTH-1 downto 0);
  signal bg_rom_addr     : unsigned(BG_ROM_ADDR_WIDTH-1 downto 0);
  signal bg_rom_data     : std_logic_vector(BG_ROM_DATA_WIDTH-1 downto 0);

  -- debug
  attribute keep : boolean;
  attribute keep of sys_clk : signal is true;
  attribute keep of cen_2   : signal is true;
begin
  pll : entity work.pll
  port map (
    inclk0 => clk,
    c0     => sys_clk,   -- 48MHz
    c1     => sdram_clk, -- 48MHz
    locked => open
  );

  -- generate a 2MHz clock enable signal
  clock_divider_2 : entity work.clock_divider
  generic map (DIVISOR => 24)
  port map (clk => sys_clk, cen => cen_2);

  -- SDRAM controller
  sdram : entity work.sdram
  generic map (CLK_FREQ => 48.0)
  port map (
    reset => reset,
    clk   => sys_clk,

    -- IO interface
    addr  => sdram_addr,
    din   => sdram_din,
    dout  => sdram_dout,
    we    => sdram_we,
    req   => sdram_req,
    ack   => sdram_ack,
    ready => sdram_ready,
    valid => sdram_valid,

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
  generic map (
    MAIN_ROM_OFFSET   => 16#00#,
    SPRITE_ROM_OFFSET => 16#01#,
    CHAR_ROM_OFFSET   => 16#02#,
    FG_ROM_OFFSET     => 16#03#,
    BG_ROM_OFFSET     => 16#04#
  )
  port map (
    reset => reset,
    clk   => sys_clk,

    -- read interface
    main_rom_addr   => main_rom_addr,
    main_rom_data   => main_rom_data,
    sprite_rom_addr => sprite_rom_addr,
    sprite_rom_data => sprite_rom_data,
    char_rom_addr   => char_rom_addr,
    char_rom_data   => char_rom_data,
    fg_rom_addr     => fg_rom_addr,
    fg_rom_data     => fg_rom_data,
    bg_rom_addr     => bg_rom_addr,
    bg_rom_data     => bg_rom_data,

    -- write interface
    ioctl_addr     => ioctl_addr,
    ioctl_data     => ioctl_data,
    ioctl_wr       => ioctl_wr,
    ioctl_download => ioctl_download,

    -- SDRAM interface
    sdram_addr  => sdram_addr,
    sdram_din   => sdram_din,
    sdram_dout  => sdram_dout,
    sdram_we    => sdram_we,
    sdram_req   => sdram_req,
    sdram_ack   => sdram_ack,
    sdram_valid => sdram_valid,
    sdram_ready => sdram_ready
  );

  -- state machine
  fsm : process (state, data_counter)
  begin
    next_state <= state;

    case state is
      when INIT =>
        if data_counter = TILE_ROM_SIZE-1 then
          next_state <= PRELOAD;
        end if;

      -- preload the byte from the tile ROM
      when PRELOAD =>
        next_state <= LOAD;

      when LOAD =>
        if data_counter = TILE_ROM_SIZE-1 then
          next_state <= IDLE;
        end if;

      when IDLE =>
        -- do nothing
    end case;
  end process;

  -- latch the next state
  latch_next_state : process (sys_clk, reset)
  begin
    if reset = '1' then
      state <= INIT;
    elsif rising_edge(sys_clk) then
      if cen_2 = '1' then
        state <= next_state;
      end if;
    end if;
  end process;

  update_data_counter : process (sys_clk, reset)
  begin
    if reset = '1' then
      data_counter <= 0;
    elsif rising_edge(sys_clk) then
      if cen_2 = '1' then
        if state /= next_state then -- state changing
          data_counter <= 0;
        else
          data_counter <= data_counter + 1;
        end if;
      end if;
    end if;
  end process;

  write_rom_data : process (sys_clk)
  begin
    if rising_edge(sys_clk) then
      ioctl_wr <= '0';

      if cen_2 = '1' and state = LOAD then
        ioctl_addr <= to_unsigned(data_counter, ioctl_addr'length);
        ioctl_data <= std_logic_vector(to_unsigned(data_counter, ioctl_data'length));
        ioctl_wr   <= '1';
      end if;
    end if;
  end process;

  reset <= not key(0);

  ioctl_download <= '1' when state = LOAD else '0';

  -- set ROM signals
  main_rom_addr   <= to_unsigned(data_counter/2, main_rom_addr'length);
  sprite_rom_addr <= to_unsigned(data_counter/2, sprite_rom_addr'length);
  char_rom_addr   <= to_unsigned(data_counter/2, char_rom_addr'length);
  fg_rom_addr     <= to_unsigned(data_counter/2, fg_rom_addr'length);
  bg_rom_addr     <= to_unsigned(data_counter/2, bg_rom_addr'length);

  -- set output data
  led <= main_rom_data;
end architecture arch;
