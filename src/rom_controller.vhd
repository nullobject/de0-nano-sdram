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

-- The ROM controller provides an interface to the GPU for accessing tile data.
--
-- The original arcade hardware has multiple ROM chips that store the tile data
-- for the different graphics layers. Unfortunately, the Cyclone V chip doesn't
-- have enough memory blocks for us to implement these ROMs. Instead, we need
-- to store the tile data in the SDRAM.
--
-- The tile ROMs are accessed concurrently by the GPU, so the job of the ROM
-- controller is to manage reading the tile data from the SDRAM in a fair, and
-- timely manner.
entity rom_controller is
  port (
    -- clock
    clk : in std_logic;

    -- reset
    reset : in std_logic;

    -- ROM interface
    sprite_rom_addr : in std_logic_vector(SPRITE_ROM_ADDR_WIDTH-1 downto 0);
    sprite_rom_data : out byte_t;
    char_rom_addr   : in std_logic_vector(CHAR_ROM_ADDR_WIDTH-1 downto 0);
    char_rom_data   : out byte_t;
    fg_rom_addr     : in std_logic_vector(FG_ROM_ADDR_WIDTH-1 downto 0);
    fg_rom_data     : out byte_t;
    bg_rom_addr     : in std_logic_vector(BG_ROM_ADDR_WIDTH-1 downto 0);
    bg_rom_data     : out byte_t;

    -- SDRAM interface
    sdram_addr  : out std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
    sdram_data  : in std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
    sdram_rden  : out std_logic;
    sdram_ready : in std_logic
  );
end rom_controller;

architecture arch of rom_controller is
  -- enums
  type state_t is (READ, READ_WAIT);
  type rom_t is (SPRITE_ROM, CHAR_ROM, FG_ROM, BG_ROM);

  -- state signals
  signal state, next_state : state_t;

  -- ROM signals
  signal current_rom : rom_t;

  -- control signals
  signal read_done : std_logic;

  -- segment signals
  signal sprite_rom_cs         : std_logic;
  signal sprite_rom_sdram_addr : std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal sprite_rom_sdram_rden : std_logic;
  signal char_rom_cs           : std_logic;
  signal char_rom_sdram_addr   : std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal char_rom_sdram_rden   : std_logic;
  signal fg_rom_cs             : std_logic;
  signal fg_rom_sdram_addr     : std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal fg_rom_sdram_rden     : std_logic;
  signal bg_rom_cs             : std_logic;
  signal bg_rom_sdram_addr     : std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal bg_rom_sdram_rden     : std_logic;
begin
  sprite_rom_segment : entity work.segment
  generic map (
    ROM_ADDR_WIDTH => SPRITE_ROM_ADDR_WIDTH,
    SEGMENT_OFFSET => 16#00#
  )
  port map (
    clk => clk,

    cs => sprite_rom_cs,

    -- ROM interface
    rom_addr => sprite_rom_addr,
    rom_data => sprite_rom_data,

    -- SDRAM interface
    sdram_addr  => sprite_rom_sdram_addr,
    sdram_data  => sdram_data,
    sdram_rden  => sprite_rom_sdram_rden,
    sdram_ready => read_done
  );

  char_rom_segment : entity work.segment
  generic map (
    ROM_ADDR_WIDTH => CHAR_ROM_ADDR_WIDTH,
    SEGMENT_OFFSET => 16#40#
  )
  port map (
    clk => clk,

    cs => char_rom_cs,

    -- ROM interface
    rom_addr => char_rom_addr,
    rom_data => char_rom_data,

    -- SDRAM interface
    sdram_addr  => char_rom_sdram_addr,
    sdram_data  => sdram_data,
    sdram_rden  => char_rom_sdram_rden,
    sdram_ready => read_done
  );

  fg_rom_segment : entity work.segment
  generic map (
    ROM_ADDR_WIDTH => FG_ROM_ADDR_WIDTH,
    SEGMENT_OFFSET => 16#80#
  )
  port map (
    clk => clk,

    cs => fg_rom_cs,

    -- ROM interface
    rom_addr => fg_rom_addr,
    rom_data => fg_rom_data,

    -- SDRAM interface
    sdram_addr  => fg_rom_sdram_addr,
    sdram_data  => sdram_data,
    sdram_rden  => fg_rom_sdram_rden,
    sdram_ready => read_done
  );

  bg_rom_segment : entity work.segment
  generic map (
    ROM_ADDR_WIDTH => BG_ROM_ADDR_WIDTH,
    SEGMENT_OFFSET => 16#C0#
  )
  port map (
    clk => clk,

    cs => bg_rom_cs,

    -- ROM interface
    rom_addr => bg_rom_addr,
    rom_data => bg_rom_data,

    -- SDRAM interface
    sdram_addr  => bg_rom_sdram_addr,
    sdram_data  => sdram_data,
    sdram_rden  => bg_rom_sdram_rden,
    sdram_ready => read_done
  );

  -- state machine
  fsm : process (state, sdram_ready)
  begin
    next_state <= state;

    case state is
      -- execute a read operation
      when READ =>
        if sdram_ready = '1' then
          next_state <= READ_WAIT;
        end if;

      -- wait for the read to complete
      when READ_WAIT =>
        if sdram_ready = '1' then
          next_state <= READ;
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

  -- update the current ROM
  update_current_rom : process (clk, reset)
  begin
    if reset = '1' then
      current_rom <= SPRITE_ROM;
    elsif rising_edge(clk) then
      if read_done = '1' then
        current_rom <= rom_t'succ(current_rom);
      end if;
    end if;
  end process;

  -- the read is done when the SDRAM ready signal is asserted
  read_done <= '1' when state = READ_WAIT and sdram_ready = '1' else '0';

  -- set the segment chip select signals
  sprite_rom_cs <= '1' when current_rom = SPRITE_ROM else '0';
  char_rom_cs   <= '1' when current_rom = CHAR_ROM   else '0';
  fg_rom_cs     <= '1' when current_rom = FG_ROM     else '0';
  bg_rom_cs     <= '1' when current_rom = BG_ROM     else '0';

  -- set SDRAM address
  sdram_addr <= sprite_rom_sdram_addr or
                char_rom_sdram_addr or
                fg_rom_sdram_addr or
                bg_rom_sdram_addr;

  -- set SDRAM read enable
  sdram_rden <= sprite_rom_sdram_rden or
                char_rom_sdram_rden or
                fg_rom_sdram_rden or
                bg_rom_sdram_rden;
end architecture arch;