/**
 * Handles displaying a 16-bit hexadecimal number on the Basys 3's four
 * 7-segment displays.
 */
module QuadHexDisplay (
    input  logic        sys_clk,
    input  logic [15:0] i_data,
    output logic  [6:0] io_7seg_seg,
    output logic  [3:0] io_7seg_an
);
    logic [1:0] active_digit_idx = 2'b0;
    logic [3:0] active_digit = 4'bZZZZ;

    // Each digit needs to be illuminated for 4ms.
    // 4ms * (1clk/10ns) = 4e-3s / 10e-9s = 400_000 clk cycles
    localparam DELAY_CYCLES = 400000;
    localparam DELAY_BITS = $clog2(DELAY_CYCLES);
    logic [DELAY_BITS-1:0] delay_timer = '0;

    always_ff @(posedge sys_clk) begin
        delay_timer <= delay_timer + 1;
        if (delay_timer == DELAY_CYCLES)
            active_digit_idx <= active_digit_idx + 1;
    end

    always_comb begin
        case (active_digit_idx)
            2'd0: io_7seg_an = 4'b0111;
            2'd1: io_7seg_an = 4'b1011;
            2'd2: io_7seg_an = 4'b1101;
            2'd3: io_7seg_an = 4'b1110;
        endcase

        case (active_digit_idx)
            2'd0: active_digit = i_data[15:12];
            2'd1: active_digit = i_data[11:8];
            2'd2: active_digit = i_data[7:4];
            2'd3: active_digit = i_data[3:0];
        endcase
    end

    SevenSegDecoder sevseg_inst (
        .i_hex_digit(active_digit),
        .o_segments(io_7seg_seg)
    );
endmodule
