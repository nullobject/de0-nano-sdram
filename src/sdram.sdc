derive_clock_uncertainty

create_clock -name clk -period 20 [get_ports clk]

# constrain output ports
set_false_path -from * -to [get_ports {led*}]
