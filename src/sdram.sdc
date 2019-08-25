create_clock -name clk -period 20 [get_ports clk]

derive_pll_clocks
create_generated_clock -name sdram_clk -source [get_pins {pll|altpll_component|auto_generated|pll1|clk[0]}] [get_ports {sdram_clk}]
derive_clock_uncertainty

# this is tAC in the data sheet
set_input_delay -max -clock sdram_clk 5.4ns [get_ports sdram_dq[*]]

# this is tOH in the data sheet
set_input_delay -min -clock sdram_clk 2.7ns [get_ports sdram_dq[*]]

# this is tIS in the data sheet (setup time)
set_output_delay -max -clock sdram_clk 1.5ns [get_ports {sdram_cke sdram_cs_n sdram_cas_n sdram_ras_n sdram_we_n sdram_ba* sdram_a* sdram_d*}]

# this is tIH in the data sheet (hold time)
set_output_delay -min -clock sdram_clk 0.8ns [get_ports {sdram_cke sdram_cs_n sdram_cas_n sdram_ras_n sdram_we_n sdram_ba* sdram_a* sdram_d*}]

# constrain input ports
set_false_path -from * -to [get_ports {key*}]

# constrain output ports
set_false_path -from * -to [get_ports {led*}]
