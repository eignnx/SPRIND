typedef enum logic [2:0] {
    REG_SP,
    REG_X,
    REG_Y,
    REG_Z,
    REG_W,
    REG_V,
    REG_A,
    REG_B
} Gpr;

module RegisterFile(
    input wire clk,

    input Gpr reg1_rd_addr,
    input Gpr reg2_rd_addr,

    input wire reg1_rd_en,
    input wire reg2_rd_en,

    output wire [15:0] reg1_data_out,
    output wire [15:0] reg2_data_out,

    input wire wr_en,
    input Gpr reg_wr_addr,
    input wire [15:0] data_in
);
    // There are 8 16-bit registers.
    reg [15:0] regs [7:0];

    assign reg1_data_out = reg1_rd_en ? regs[reg1_rd_addr] : 16'dZ;
    assign reg2_data_out = reg2_rd_en ? regs[reg2_rd_addr] : 16'dZ;

    always_ff @(posedge clk) begin
        if (wr_en)
            regs[reg_wr_addr] <= data_in;
    end;
endmodule;
