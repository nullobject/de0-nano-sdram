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

-- A segment provides a read-only 8-bit interface to access a contiguous block
-- of memory located in SDRAM.
--
-- When vaild address is placed on the ROM address bus, the segment checks to
-- see if the page containing the address has previously been cached. If so,
-- the value is immediately placed on the ROM data bus.
--
-- Otherwise, the page containing the address is requested from the SDRAM.
entity segment is
  generic (
    ROM_ADDR_WIDTH : natural;
    SEGMENT_OFFSET : natural := 0;
    CACHE_SIZE     : natural := 4 -- bytes
  );
  port (
    -- clock
    clk : in std_logic;

    -- chip select
    cs : in std_logic;

    -- ROM interface
    rom_addr : in std_logic_vector(ROM_ADDR_WIDTH-1 downto 0);
    rom_data : out byte_t;

    -- SDRAM interface
    sdram_addr  : out std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
    sdram_data  : in std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
    sdram_rden  : out std_logic;
    sdram_ready : in std_logic
  );
end segment;

architecture arch of segment is
  constant LOG_CACHE_SIZE : natural := ilog2(CACHE_SIZE);

  -- the cache is just an array of bytes
  type cache_t is array (0 to CACHE_SIZE-1) of byte_t;

  -- the base address of the cache
  signal base_address : std_logic_vector(ROM_ADDR_WIDTH-1 downto 0);

  -- the cached data
  signal cache : cache_t;

  -- control signals
  signal hit : std_logic;

  -- registers
  signal sdram_rden_reg : std_logic;
begin
  update_cache_data : process (clk)
  begin
    if rising_edge(clk) then
      if cs = '1' and hit = '0' and sdram_ready = '1' then
        -- TODO: iterate to fill cache
        cache(0) <= sdram_data( 7 downto 0);
        cache(1) <= sdram_data(15 downto 8);
        cache(2) <= sdram_data(23 downto 16);
        cache(3) <= sdram_data(31 downto 24);

        -- TODO: calculate this
        base_address <= rom_addr(ROM_ADDR_WIDTH-1 downto LOG_CACHE_SIZE) & "00";
      end if;
    end if;
  end process;

  -- assert the hit signal if the ROM address is in the cache
  -- hit <= '1' when rom_addr(ROM_ADDR_WIDTH-1 downto LOG_CACHE_SIZE) = base_address(ROM_ADDR_WIDTH-1 downto LOG_CACHE_SIZE) else '0';
  hit <= '0';

  -- set ROM data
  --
  -- TODO: extract the correct byte from the cache
  rom_data <= cache(0);

  -- set SDRAM address
  sdram_addr <= std_logic_vector(resize(unsigned(rom_addr), sdram_addr'length)+SEGMENT_OFFSET) when cs = '1' else (others => '0');

  -- assert the SDRAM read enable if we have a cache miss
  sdram_rden <= cs and not hit;
end architecture arch;
