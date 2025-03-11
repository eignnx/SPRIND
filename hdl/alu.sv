module SPRIND_ALU (
    input clk;
    input [4:0] i_alu_opcode;
    input [15:0] i_input0;
    input [15:0] i_input1;

    output [15:0] o_result;
    output o_carry;
    output o_overflow;
    output o_zero;
    output o_sign;
);

    parameter OPCODE_ADD = 4'b0000;

    always @(posedge clk) begin
        case (i_alu_opcode)
            OPCODE_ADD: 
                begin
                    o_output <= i_input0 + i_input1;
                    o_carry <= 
                end
            default: 
        endcase
    end
    
endmodule