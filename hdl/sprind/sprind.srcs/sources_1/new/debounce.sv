/**
 * A shift-register-based signal debouncer.
 * Modified from: https://www.chipverify.com/verilog/verilog-debounce-circuit
 */
module Debounce #(
    parameter CLK_FREQ = 1_000_000,     // Clock frequency in Hz
    parameter SAMPLE_RATE_MS = 1        // Sample rate in milliseconds
)(
    input wire clk,           // System clock
    input wire rst,           // Active high reset
    input wire sig_in,        // Raw sig input
    output reg sig_out        // Debounced output
);

    // Calculate sampling period
    localparam SAMPLE_PERIOD = (CLK_FREQ / 1000) * SAMPLE_RATE_MS;
    localparam SAMPLE_WIDTH = $clog2(SAMPLE_PERIOD + 1);

    reg [SAMPLE_WIDTH-1:0] sample_counter;
    reg sample_tick;
    reg [7:0] shift_reg;  // 8-bit shift register
    reg sig_sync_0, sig_sync_1;

    // Synchronizer
    always @(posedge clk) begin
        if (rst) begin
            sig_sync_0 <= 1'b0;
            sig_sync_1 <= 1'b0;
        end else begin
            sig_sync_0 <= sig_in;
            sig_sync_1 <= sig_sync_0;
        end
    end

    // Sample rate generator
    always @(posedge clk) begin
        if (rst) begin
            sample_counter <= 0;
            sample_tick <= 0;
        end else begin
            if (sample_counter >= SAMPLE_PERIOD - 1) begin
                sample_counter <= 0;
                sample_tick <= 1;
            end else begin
                sample_counter <= sample_counter + 1;
                sample_tick <= 0;
            end
        end
    end

    // Shift register debouncer
    always @(posedge clk) begin
        if (rst) begin
            shift_reg <= 8'h00;
            sig_out <= 1'b0;
        end else if (sample_tick) begin
            shift_reg <= {shift_reg[6:0], sig_sync_1};

            // Output is high if all bits are 1, low if all bits are 0
            if (shift_reg == 8'hFF)
                sig_out <= 1'b1;
            else if (shift_reg == 8'h00)
                sig_out <= 1'b0;
        end
    end

endmodule
