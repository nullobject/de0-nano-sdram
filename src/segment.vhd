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

-- A segment provides an 8-bit read-only interface to a contiguous block of
-- memory located in SDRAM.
--
-- When vaild address is placed on the ROM address bus, the segment checks to
-- see if the page containing the address has previously been cached. If so,
-- the value is immediately placed on the ROM data bus.
--
-- Otherwise, the page containing the address is requested from the SDRAM.
entity segment is
  generic (
    -- the cache size in bytes (must be a power of two)
    CACHE_SIZE : natural := 4;

    -- the width of the ROM address bus
    ROM_ADDR_WIDTH : natural;

    -- the offset of the segment in the SDRAM
    SEGMENT_OFFSET : natural := 0
  );
  port (
    -- clock
    clk : in std_logic;

    -- clock enable
    cen : in std_logic := '1';

    -- chip select
    cs : in std_logic;

    -- ROM interface
    rom_addr : in std_logic_vector(ROM_ADDR_WIDTH-1 downto 0);
    rom_data : out byte_t;

    -- SDRAM interface
    sdram_addr  : out std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
    sdram_data  : in std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
    sdram_rden  : out std_logic; -- requesting data
    sdram_ready : in std_logic -- data is ready
  );
end segment;

architecture arch of segment is
  constant LOG_CACHE_SIZE : natural := ilog2(CACHE_SIZE);

  -- the cache is just an array of bytes
  type cache_t is array (0 to CACHE_SIZE-1) of byte_t;

  -- the address of the first byte in the cache
  signal cache_addr : std_logic_vector(ROM_ADDR_WIDTH-1 downto 0);

  -- the cached data
  signal cache : cache_t;

  -- control signals
  signal hit : std_logic;

  -- debug
  attribute keep : boolean;
  attribute keep of hit : signal is true;
begin
  -- latch the SDRAM data
  latch_sdram_data : process (clk, cen)
  begin
    if rising_edge(clk) and cen = '1' then
      if hit = '1' then
        -- set ROM data from the cache
        rom_data <= cache(to_integer(unsigned(rom_addr)-unsigned(cache_addr)));
      elsif cs = '1' and hit = '0' then
        -- set ROM data
        rom_data <= sdram_data(7 downto 0);

        -- set the cache address
        cache_addr <= rom_addr;

        -- cache the SDRAM data
        for i in 0 to CACHE_SIZE-1 loop
          cache(i) <= sdram_data((i+1)*8-1 downto i*8);
        end loop;
      end if;
    end if;
  end process;

  -- assert the hit signal if the ROM address has been cached
  hit <= '1' when rom_addr >= cache_addr and rom_addr <= std_logic_vector(unsigned(cache_addr)+CACHE_SIZE-1) else '0';

  -- set SDRAM address
  sdram_addr <= std_logic_vector(resize(unsigned(rom_addr), sdram_addr'length)+SEGMENT_OFFSET) when cs = '1' else (others => '0');

  -- assert the SDRAM read enable if we have a cache miss
  sdram_rden <= not hit;
end architecture arch;
