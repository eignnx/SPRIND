module Reset (
    input  logic wakeup_clk,
    input  logic reset_sources,
    output logic master_reset
);
    logic [15:0] reset_count = '0;
    logic clear_counter = 1;

    always_ff @(posedge wakeup_clk) begin
        clear_counter <= reset_sources;
        master_reset <= (reset_count != 16'hFFFF);

        if (clear_counter)
            reset_count <= 0;
        else if (reset_count != 16'hFFFF)
            reset_count <= reset_count + 1;
    end
endmodule

