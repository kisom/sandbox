module SegmentLCDTest (
	input	CLK,
	output	PIN_9,
	output	PIN_10,
	output	PIN_11,
	output	PIN_12,
	output	PIN_13,
	output	PIN_14,
	output	PIN_15,
	output	USBPU
);

	assign USBPU = 0;

	reg [21:0]	clock_counter = 21'b0000000000000000000000;
	reg [3:0]	number = 4'b0000;

	SegmentLCD	display (
		.number(number),
		.out({PIN_15, PIN_14, PIN_13, PIN_12, PIN_11, PIN_10, PIN_9})
	);

	always @(posedge CLK) begin
		clock_counter <= clock_counter + 1;
		if (clock_counter == 22'b1100001101010000000000) begin
			number <= number + 1;
			clock_counter <= 0;
		end
	end
endmodule
