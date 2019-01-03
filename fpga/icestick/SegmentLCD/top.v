module SegmentLCDTest (
	input	CLK,
	output	D1,
	output	D2,
	output	D3,
	output	D4,
	output	D5,
	output	BR4,
	output	BR5,
	output	BR6,
	output	BR7,
	output	BR8,
	output	BR9,
	output	BR10
);

	reg [21:0]	clock_counter = 22'b00000000000000000000000;
	reg [3:0]	number = 4'b0000;

	SegmentLCD	display (
		.number(number),
		.out({BR10, BR9, BR8, BR7, BR6, BR5, BR4})
	);

	always @(posedge CLK) begin
		clock_counter <= clock_counter + 1;
		if (clock_counter == 22'b1001001001111100000000) begin
			number <= number + 1;
			clock_counter <= 0;
		end
	end
endmodule
