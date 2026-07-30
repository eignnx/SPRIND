## This file is a general .xdc for the Basys3 rev B board
## To use it in a project:
## - uncomment the lines corresponding to used pins
## - rename the used ports (in each line, after get_ports) according to the top level signal names in the project

proc bind_pin { pin name {extra_opts {}}} {
    set properties [dict create \
        PACKAGE_PIN $pin \
        IOSTANDARD $opts(-IOSTANDARD) \
    ]

    if {[llength $extra_opts] > 0} {
        if {[llength $extra_opts] % 2 != 0} {
            error "Extra opts to `bind_pin` must be key-value pairs"
        }

        set properties [dict merge $properties $extra_opts]
    }

    set_property -dict $properties [get_ports $name]
}

## Clock signal
## (Pin W5 is the 100MHz oscillator)
bind_pin W5 sys_clk
create_clock -add -name sys_clk -period 10.00 -waveform {0 5} [get_ports sys_clk]
#?
#create_generated_clock -name gated_clk_o -source [get_clocks sys_clk] [get_nets gated_clk_o[0]]
#?

# Switches
bind_pin V17 {io_sw[0]}
bind_pin V16 {io_sw[1]}
bind_pin W16 {io_sw[2]}
bind_pin W17 {io_sw[3]}
bind_pin W15 {io_sw[4]}
bind_pin V15 {io_sw[5]}
bind_pin W14 {io_sw[6]}
bind_pin W13 {io_sw[7]}
bind_pin V2  {io_sw[8]}
bind_pin T3  {io_sw[9]}
bind_pin T2  {io_sw[10]}
bind_pin R3  {io_sw[11]}
bind_pin W2  {io_sw[12]}
bind_pin U1  {io_sw[13]}
bind_pin T1  {io_sw[14]}
bind_pin R2  {io_sw[15]}


## LEDs
bind_pin U16 {io_led[0]}
bind_pin E19 {io_led[1]}
bind_pin U19 {io_led[2]}
bind_pin V19 {io_led[3]}
bind_pin W18 {io_led[4]}
bind_pin U15 {io_led[5]}
bind_pin U14 {io_led[6]}
bind_pin V14 {io_led[7]}
bind_pin V13 {io_led[8]}
bind_pin V3  {io_led[9]}
bind_pin W3  {io_led[10]}
bind_pin U3  {io_led[11]}
bind_pin P3  {io_led[12]}
bind_pin N3  {io_led[13]}
bind_pin P1  {io_led[14]}
bind_pin L1  {io_led[15]}


## 7-Segment Display
bind_pin W7 {io_7seg_seg[0]}
bind_pin W6 {io_7seg_seg[1]}
bind_pin U8 {io_7seg_seg[2]}
bind_pin V8 {io_7seg_seg[3]}
bind_pin U5 {io_7seg_seg[4]}
bind_pin V5 {io_7seg_seg[5]}
bind_pin U7 {io_7seg_seg[6]}

#bind_pin V7 io_7seg_dp

bind_pin U2 {io_7seg_an[0]}
bind_pin U4 {io_7seg_an[1]}
bind_pin V4 {io_7seg_an[2]}
bind_pin W4 {io_7seg_an[3]}


## Buttons
bind_pin U18 io_btn_center
#bind_pin T18 io_btn_up
#bind_pin W19 io_btn_left
#bind_pin T17 io_btn_right
bind_pin U17 io_btn_down


## Pmod Header JA
#bind_pin J1 {JA[0]};#Sch name = JA1
#bind_pin L2 {JA[1]};#Sch name = JA2
#bind_pin J2 {JA[2]};#Sch name = JA3
#bind_pin G2 {JA[3]};#Sch name = JA4
#bind_pin H1 {JA[4]};#Sch name = JA7
#bind_pin K2 {JA[5]};#Sch name = JA8
#bind_pin H2 {JA[6]};#Sch name = JA9
#bind_pin G3 {JA[7]};#Sch name = JA10

## Pmod Header JB
#bind_pin A14 {JB[0]};#Sch name = JB1
#bind_pin A16 {JB[1]};#Sch name = JB2
#bind_pin B15 {JB[2]};#Sch name = JB3
#bind_pin B16 {JB[3]};#Sch name = JB4
#bind_pin A15 {JB[4]};#Sch name = JB7
#bind_pin A17 {JB[5]};#Sch name = JB8
#bind_pin C15 {JB[6]};#Sch name = JB9
#bind_pin C16 {JB[7]};#Sch name = JB10

## Pmod Header JC
#bind_pin K17 {JC[0]};#Sch name = JC1
#bind_pin M18 {JC[1]};#Sch name = JC2
#bind_pin N17 {JC[2]};#Sch name = JC3
#bind_pin P18 {JC[3]};#Sch name = JC4
#bind_pin L17 {JC[4]};#Sch name = JC7
#bind_pin M19 {JC[5]};#Sch name = JC8
#bind_pin P17 {JC[6]};#Sch name = JC9
#bind_pin R18 {JC[7]};#Sch name = JC10

## Pmod Header JXADC
#bind_pin J3 {JXADC[0]};#Sch name = XA1_P
#bind_pin L3 {JXADC[1]};#Sch name = XA2_P
#bind_pin M2 {JXADC[2]};#Sch name = XA3_P
#bind_pin N2 {JXADC[3]};#Sch name = XA4_P
#bind_pin K3 {JXADC[4]};#Sch name = XA1_N
#bind_pin M3 {JXADC[5]};#Sch name = XA2_N
#bind_pin M1 {JXADC[6]};#Sch name = XA3_N
#bind_pin N1 {JXADC[7]};#Sch name = XA4_N


## VGA Connector
#bind_pin G19 {io_vga_red[0]}
#bind_pin H19 {io_vga_red[1]}
#bind_pin J19 {io_vga_red[2]}
#bind_pin N19 {io_vga_red[3]}
#bind_pin N18 {io_vga_blue[0]}
#bind_pin L18 {io_vga_blue[1]}
#bind_pin K18 {io_vga_blue[2]}
#bind_pin J18 {io_vga_blue[3]}
#bind_pin J17 {io_vga_green[0]}
#bind_pin H17 {io_vga_green[1]}
#bind_pin G17 {io_vga_green[2]}
#bind_pin D17 {io_vga_green[3]}
#bind_pin P19 io_vga_hsync]
#bind_pin R19 io_vga_vsync]

## USB-RS232 Interface
#bind_pin B18 io_rs232_rx
#bind_pin A18 io_rs232_tx


## USB HID (PS/2)
#bind_pin C17 io_ps2_clk  { PULLUP true }
#bind_pin B17 io_ps2_data { PULLUP true }


## Quad SPI Flash
## Note that CCLK_0 cannot be placed in 7 series devices. You can access it using the
## STARTUPE2 primitive.
#bind_pin D18 {io_qspi_db[0]}
#bind_pin D19 {io_qspi_db[1]}
#bind_pin G18 {io_qspi_db[2]}
#bind_pin F18 {io_qspi_db[3]}
#bind_pin K19 io_qspi_CSn


## Configuration options, can be used for all designs
set_property CONFIG_VOLTAGE 3.3 [current_design]
set_property CFGBVS VCCO [current_design]

## SPI configuration mode options for QSPI boot, can be used for all designs
set_property BITSTREAM.GENERAL.COMPRESS TRUE [current_design]
set_property BITSTREAM.CONFIG.CONFIGRATE 33 [current_design]
set_property CONFIG_MODE SPIx4 [current_design]
