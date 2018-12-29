`include "SegmentLCD.v"

/////////////////////////////////////////////////////////////////////////
/// TinyAdder.v
/// Author: K. Isom
/// Created: 2018-12-27
/////////////////////////////////////////////////////////////////////////
/// TinyAdder is a 4-bit adder that displays calculations on a pair of
/// 7-segment LCDs. The EXEC button doubles as a plus and equals button:
/// the first time it is pressed, entry begins with the second number. The
/// second time it is pressed, the sum of the two numbers is shown on the
/// display.
module TinyAdder (
	input	PIN_1,	// A[0]
	input	PIN_2,	// A[1]
	input	PIN_3,	// A[2]
	input	PIN_4,	// A[3]
	input	PIN_5,	// EXEC
	input	PIN_6,	// CLR

	// There are two 7-segment displays: 'low' shows the 4 LSBs, and 'hi'
	// shows the 4 MSBs. That is, given 0x12, 'low' would display '2' and
	// 'hi' would show '1'.
	output	PIN_9,	// LOW/A
	output	PIN_10,	// LOW/B
	output	PIN_11,	// LOW/C
	output	PIN_12,	// LOW/D
	output	PIN_13,	// LOW/E
	output	PIN_14,	// LOW/F
	output	PIN_15,	// LOW/G
	output	PIN_16, // HI/A
	output	PIN_17, // HI/B
	output	PIN_18, // HI/C
	output	PIN_19, // HI/D
	output	PIN_20, // HI/E
	output	PIN_21, // HI/F
	output	PIN_22, // HI/G

	output	LED,   // User/boot LED next to power LED
	output	USBPU  // USB pull-up resistor
);

	// Drive USB pull-up resistor to '0' to disable USB. This prevents us
	// from falling back into the bootloader, I think.
	assign USBPU = 0;
	
	wire [7:0]	sum = 8'b00000000;
	wire [3:0]	switches;

	assign switches = {PIN_4, PIN_3, PIN_2, PIN_1};

	// state determines what happens when the button is pressed.
	reg [1:0]	state = 2'b00;

	segment7 low (
		.number(sum[3:0]),
		.out({PIN_15, PIN_14, PIN_13, PIN_12, PIN_11, PIN_10, PIN_9})
	);

	segment7 hi (
		.number(sum[7:4]),
		.out({PIN_22, PIN_21, PIN_20, PIN_19, PIN_18, PIN_17, PIN_16})
	);

	// When EXEC is pressed, the adder's state is advanced. There are
	// three states:
	// 	+ state 00: entry of the first number.
	// 	+ state 01: entry of the second number.
	// 	+ state 10: the sum of the two numbers is displayed. Pressing
	// 	  exec again resets the display.
	always @(negedge PIN_5)
	begin
		case (state)
			2'b00: begin
				sum = switches;
				state <= 2'b01;
			end
			2'b01: begin
				sum = sum + switches;
				state = 2'b10;
				LED <= 1;
			end
			2'b10: begin
				sum = 8'b00000000;
				state = 2'b00;
				LED <= 0;
			end
		endcase
	end

	// When CLEAR is pressed, the adder should go back to its initial
	// state.
	always @(negedge PIN_6)
	begin
		state <= 2'b00;
		sum = 8'b0000000;
		LED <= 0;
	end
endmodule
