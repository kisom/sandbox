module top (
	input	CLK,
	output	D1,
	output	D2,
	output	D3,
	output	D4,
	output	D5
);

	reg [20:0]	clock_counter;
	reg [3:0]	active = 0;
	reg [9:0]	pattern = 10'b1010000000;
	wire [4:0]	leds;
	assign leds = {D5, D4, D3, D2, D1};
	
	always @(posedge CLK) begin
		clock_counter <= clock_counter + 1;
		if (clock_counter == 21'b100100100111110000000) begin
			if (pattern[active] == 1)
				leds = 5'b11111;
			else
				leds = 5'b00000;
			active <= active + 1;
			if (active == 4'b1011) active <= 0;
		end
	end

endmodule
