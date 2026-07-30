typedef enum logic [3:0] {
    OP_ADD,
    OP_SUB,
    OP_ADD_CY,
    OP_SUB_BW,
    OP_AND,
    OP_OR,
    OP_XOR
} AluOp;

module Alu(
    input wire [15:0] x, // Left operand
    input wire [15:0] y, // Right operand
    input wire cy_in,

    input AluOp alu_op,

    input wire out_en,
    
    output logic [15:0] out,
    output logic cy_out
);
    always_comb begin
        if (out_en) begin
            cy_out = '0;
            out = '0;
            case (alu_op)
                OP_ADD: begin
                    {cy_out, out} = {1'b0, x} + {1'b0, y};
                end
                OP_SUB: begin
                    {cy_out, out} = {1'b0, x} + (~{1'b0, y} + 17'h00001);
                end
                OP_ADD_CY: begin
                    {cy_out, out} = {1'b0, x} + {1'b0, y} + cy_in;
                end
                OP_SUB_BW: begin
                    {cy_out, out} = {1'b0, x} + (~{1'b0, y} + cy_in);
                end
                OP_AND: out = x & y;
                OP_OR:  out = x | y;
                OP_XOR: out = x ^ y;
                default: begin
                    out = 16'h5555;
                    cy_out = 1'dX;
                end
            endcase
        end else begin
            out = 16'dZ;
            cy_out = '0;
        end
    end
endmodule;
