`timescale 1ns / 1ps

typedef enum {
    INSTR_LI,
    INSTR_ADD
} Instr;

module top(
    input         sys_clk,
    input         io_btn_center,
    input         io_btn_down,
    input  [15:0] io_sw,
    output [15:0] io_led,
    output [6:0]  io_7seg_seg,
    output [3:0]  io_7seg_an
);
    logic rst;
    Reset reset_inst (
        .wakeup_clk(sys_clk),
        .reset_sources(io_btn_down),
        .master_reset(rst)
    );

    logic step_db;
    Debounce db_step_inst  ( sys_clk, rst, io_btn_center, step_db );

    logic clk;
    ClockModule clock_mod_inst (
        .sys_clk(sys_clk),
        .step_db(step_db),
        .single_stepping_db(io_sw[15]),
        .clk(clk)
    );

    logic [15:0] pc;
    ProgramCounter program_counter_inst (
        .rst(rst),
        .clk(clk),
        .out_en(1),
        .displacement(0),
        .replacement(0),
        .mode_inc(1),
        .mode_replace(0),
        .mode_displace(0),
        .address(pc)
    );

    assign io_led = pc;

    QuadHexDisplay hexdisplay_inst (
        .sys_clk(sys_clk),
        .i_data(pc),
        .io_7seg_seg(io_7seg_seg),
        .io_7seg_an(io_7seg_an)
    );
endmodule

