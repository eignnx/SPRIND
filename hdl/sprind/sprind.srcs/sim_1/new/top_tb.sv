`timescale 1ns / 100ps

module top_tb();
    logic sys_clk;
    parameter PERIOD = 10;
    initial begin
        sys_clk = 1'b0;
        #(PERIOD/2);
        forever
            #(PERIOD/2) sys_clk = ~sys_clk;
    end

    logic         io_btn_center = 0;
    logic         io_btn_down = 0;
    logic  [15:0] io_sw = '0;

    logic [15:0] io_led;
    logic [6:0]  io_7seg_seg;
    logic [3:0]  io_7seg_an;

    top dut (
        .sys_clk(sys_clk),
        .io_btn_center(io_btn_center),
        .io_btn_down(io_btn_down),
        .io_sw(io_sw),

        .io_led(io_led),
        .io_7seg_seg(io_7seg_seg),
        .io_7seg_an(io_7seg_an)
    );

    initial begin
        #(65_536 * PERIOD);
        assert (io_led == 16'h0000) else $error("io_led not off at t=2");
        #20;
        $finish;
    end
endmodule
