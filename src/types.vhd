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
use ieee.math_real.all;

package types is
  -- SDRAM interface
  constant SDRAM_BANK_WIDTH      : natural := 2;
  constant SDRAM_ADDR_WIDTH      : natural := 13;
  constant SDRAM_DATA_WIDTH      : natural := 16;
  constant SDRAM_DATA_MASK_WIDTH : natural := 2;
  constant SDRAM_COL_WIDTH       : natural := 9;
  constant SDRAM_ROW_WIDTH       : natural := 13;

  -- IO interface
  constant SDRAM_INPUT_ADDR_WIDTH  : natural := 24;
  constant SDRAM_INPUT_DATA_WIDTH  : natural := 16;
  constant SDRAM_OUTPUT_DATA_WIDTH : natural := 32;

  -- calculates the log2 of the given number
  function ilog2(n : natural) return natural;
end package types;

package body types is
  function ilog2(n : natural) return natural is
  begin
    return natural(log2(real(n)));
  end ilog2;
end package body types;
