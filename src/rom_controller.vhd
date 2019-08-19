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
    -- reset
    reset : in std_logic;

    -- clock
    clk : in std_logic;

    -- write interface
    ioctl_addr : in std_logic_vector(24 downto 0);
    ioctl_data : in byte_t;
    ioctl_wren : in std_logic;

    -- read interface
    sprite_addr : in std_logic_vector(SPRITE_ROM_ADDR_WIDTH-1 downto 0);
    sprite_data : out byte_t;
    char_addr   : in std_logic_vector(CHAR_ROM_ADDR_WIDTH-1 downto 0);
    char_data   : out byte_t;
    fg_addr     : in std_logic_vector(FG_ROM_ADDR_WIDTH-1 downto 0);
    fg_data     : out byte_t;
    bg_addr     : in std_logic_vector(BG_ROM_ADDR_WIDTH-1 downto 0);
    bg_data     : out byte_t;

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
end rom_controller;

architecture arch of rom_controller is
  -- enums
  type state_t is (READ, READ_WAIT, WRITE, WRITE_WAIT);
  type rom_t is (SPRITE_ROM, CHAR_ROM, FG_ROM, BG_ROM);

  -- state signals
  signal state, next_state : state_t;

  -- ROM signals
  signal current_rom : rom_t;
  signal rom_addr    : std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);

  -- control signals
  signal read_done : std_logic;

  -- registers
  signal ioctl_addr_reg  : std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal ioctl_data_reg  : byte_t;
  signal ioctl_wren_reg  : std_logic;
  signal sprite_data_reg : byte_t;
  signal char_data_reg   : byte_t;
  signal fg_data_reg     : byte_t;
  signal bg_data_reg     : byte_t;

  -- SDRAM signals
  signal ready : std_logic;
  signal addr  : std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal din   : std_logic_vector(SDRAM_INPUT_DATA_WIDTH-1 downto 0) := (others => '0');
  signal dout  : std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
  signal rden  : std_logic;
  signal wren  : std_logic;
begin
  -- SDRAM controller
  sdram : entity work.sdram
  port map (
    reset => reset,
    clk   => clk,

    -- control signals
    ready => ready,

    -- IO interface
    addr => addr,
    din  => din,
    dout => dout,
    rden => rden,
    wren => wren,

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
  fsm : process (state, ready, ioctl_wren_reg)
  begin
    next_state <= state;

    case state is
      -- execute a read operation
      when READ =>
        if ioctl_wren_reg = '0' then
          if ready = '1' then
            next_state <= READ_WAIT;
          end if;
        else
          next_state <= WRITE;
        end if;

      -- wait for the read to complete
      when READ_WAIT =>
        if ready = '1' then
          next_state <= READ;
        end if;

      -- execute a write operation
      when WRITE =>
        if ioctl_wren_reg = '1' then
          if ready = '1' then
            next_state <= WRITE_WAIT;
          end if;
        else
          next_state <= READ;
        end if;

      -- wait for the write to complete
      when WRITE_WAIT =>
        if ready = '1' then
          next_state <= WRITE;
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

  -- latch the ROM data from the SDRAM
  latch_rom_data : process (clk)
  begin
    if reset = '1' then
      current_rom <= SPRITE_ROM;
    elsif rising_edge(clk) then
      if read_done = '1' then
        case current_rom is
          when SPRITE_ROM => sprite_data_reg <= dout(7 downto 0);
          when CHAR_ROM   => char_data_reg   <= dout(7 downto 0);
          when FG_ROM     => fg_data_reg     <= dout(7 downto 0);
          when BG_ROM     => bg_data_reg     <= dout(7 downto 0);
        end case;

        -- switch to the next ROM
        current_rom <= rom_t'succ(current_rom);
      end if;
    end if;
  end process;

  -- Latch the IOCTL signals.
  --
  -- These signals come from another (slower) clock domain. We need to latch
  -- them to synchronise them with the clock domain for the ROM controller.
  latch_ioctl : process (clk)
  begin
    if rising_edge(clk) then
      ioctl_addr_reg <= ioctl_addr;
      ioctl_data_reg <= ioctl_data;
      ioctl_wren_reg <= ioctl_wren;
    end if;
  end process;

  -- set ROM address
  --
  -- TODO: offset address for different ROMs
  with current_rom select
    rom_addr <=
      "0000000000000000000" & sprite_addr when SPRITE_ROM,
      "0000000000000000001" & char_addr   when CHAR_ROM,
      "0000000000000000010" & fg_addr     when FG_ROM,
      "0000000000000000011" & bg_addr     when BG_ROM;

  -- set SDRAM address
  addr <= ioctl_addr_reg when state = WRITE else rom_addr;

  -- set SDRAM input data
  din(7 downto 0) <= ioctl_data_reg;

  -- set read enable
  rden <= '1' when state = READ and ioctl_wren_reg = '0' else '0';

  -- set write enable
  wren <= '1' when state = WRITE and ioctl_wren_reg = '1' else '0';

  -- the read is done when the ready signal is asserted
  read_done <= '1' when state = READ_WAIT and ready = '1' else '0';

  -- set output data
  sprite_data <= sprite_data_reg;
  char_data   <= char_data_reg;
  fg_data     <= fg_data_reg;
  bg_data     <= bg_data_reg;
end architecture arch;
