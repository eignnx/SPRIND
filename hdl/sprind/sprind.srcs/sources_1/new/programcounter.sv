module ProgramCounter (
    input logic rst,
    input logic clk,
    input logic out_en,
    input logic [15:0] displacement,
    input logic [15:0] replacement,
    input logic mode_inc,
    input logic mode_replace,
    input logic mode_displace,
    output logic [15:0] address
);
    logic [15:0] state;
    assign address = (out_en) ? state : 16'bZZZZ;

    always_ff @(posedge clk) begin
        if (rst) begin
            state = 15'b0;
        end else if (mode_inc) begin
            state = state + 1;
        end else if (mode_displace) begin
            state = state + displacement;
        end else if (mode_replace) begin
            state = replacement;
        end
    end
endmodule

