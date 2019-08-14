create_clock -name clk -period 20 [get_ports clk]

derive_pll_clocks
derive_clock_uncertainty

# constrain output ports
set_false_path -from * -to [get_ports {led*}]
