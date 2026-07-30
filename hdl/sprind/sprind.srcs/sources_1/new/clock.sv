module ClockModule (
    input logic sys_clk,
    input logic step_db,
    input logic single_stepping_db,
    output logic clk
);
    assign clk = (single_stepping_db) ? step_db : sys_clk;

    //MMCE2 mmce_inst (
    //    .CLKOUT0(mmcm_output)
    //);

    //BUFG clk_buf_p_i0 (
    //    .I(mmcm_output),
    //    .O(cpu_clk)
    //);

    //BUFGCE bufgce_i0 (
    //    .I(mmcm_output),
    //    .CE(enable),
    //    .O(gated_cpu_clk)
    //);
endmodule

