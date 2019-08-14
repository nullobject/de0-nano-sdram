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

entity top is
  port (
    -- 50MHz clock
    clk : in std_logic;

    -- LEDs
    led : out std_logic_vector(7 downto 0);

    -- SDRAM
    sdram_a     : out std_logic_vector(12 downto 0);
    sdram_dq    : inout std_logic_vector(15 downto 0);
    sdram_dqm   : out std_logic_vector(1 downto 0);
    sdram_ba    : out std_logic_vector(1 downto 0);
    sdram_cas_n : out std_logic;
    sdram_cke   : out std_logic;
    sdram_cs_n  : out std_logic;
    sdram_ras_n : out std_logic;
    sdram_we_n  : out std_logic;
    sdram_clk   : out std_logic
  );
end top;

architecture arch of top is
  type state_t is (WRITE, READ);

  -- state signals
  signal state, next_state : state_t;

  signal reset : std_logic;

  -- clock signals
  signal clk_5      : std_logic;
  signal pll_locked : std_logic;

  -- counter
  signal n : unsigned(7 downto 0);

  -- memory signals
  signal busy : std_logic;
  signal done : std_logic;
  signal addr : std_logic_vector(24 downto 0);
  signal din  : std_logic_vector(15 downto 0);
  signal dout : std_logic_vector(15 downto 0);
  signal rden : std_logic;
  signal wren : std_logic;
begin
  pll : entity work.pll
  port map (
    inclk0 => clk,
    c0     => clk_5,
    locked => pll_locked
  );

  -- SDRAM controller
  sdram : entity work.sdram
  port map (
    reset => reset,
    clk   => clk,

    -- control signals
    busy => busy,
    done => done,

    -- IO port
    addr => addr,
    din  => din,
    dout => dout,
    rden => rden,
    wren => wren,

    -- SDRAM signals
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

  latch_state : process (clk, reset)
  begin
    if reset = '1' then
      state <= WRITE;
    elsif rising_edge(clk) then
      state <= next_state;
    end if;
  end process;

  fsm : process (state, n)
  begin
    next_state <= state;

    case state is
      when WRITE =>
        if n = 255 then
          next_state <= READ;
        end if;

      when READ =>
        if n = 255 then
          next_state <= WRITE;
        end if;
    end case;
  end process;

  write_data : process (clk)
  begin
    if rising_edge(clk) then
      if state = WRITE and busy = '0' then
        wren <= '1';
        din(7 downto 0) <= std_logic_vector(n);
      elsif state = WRITE and done = '1' then
        n <= n + 1;
      elsif state = READ and busy = '0' then
        rden <= '1';
      elsif state = READ and done = '1' then
        led <= dout(7 downto 0);
        n <= n + 1;
      end if;
    end if;
  end process;

  -- reset while the PLL isn't locked
  reset <= not pll_locked;

  sdram_clk <= clk;

  addr <= std_logic_vector(resize(n, addr'length));
end architecture arch;
