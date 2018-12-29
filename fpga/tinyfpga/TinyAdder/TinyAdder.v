module segment7 (
	input	[3:0]	number,
	output	[6:0]	out = 7'b0000000
);

	always @(*)
	begin
		case (number)
			4'b0000: out <= 7'b0111111; // 0
			4'b0001: out <= 7'b0000110; // 1
			4'b0010: out <= 7'b1011011; // 2
			4'b0011: out <= 7'b1001111; // 3
			4'b0100: out <= 7'b1100110; // 4
			4'b0101: out <= 7'b1101101; // 5
			4'b0110: out <= 7'b1111101; // 6
			4'b0111: out <= 7'b0000111; // 7
			4'b1000: out <= 7'b1111111; // 8
			4'b1001: out <= 7'b1100111; // 9
			4'b1010: out <= 7'b1110111; // A
			4'b1011: out <= 7'b1111100; // B
			4'b1100: out <= 7'b0111001; // C
			4'b1101: out <= 7'b1011110; // D
			4'b1110: out <= 7'b1111001; // E
			4'b1111: out <= 7'b1110001; // F	
		endcase
	end

endmodule

// look in pins.pcf for all the pin names on the TinyFPGA BX board
module TinyAdder (
	input	CLK,    // 16MHz clock
	input	PIN_1,	// A[0]
	input	PIN_2,	// A[1]
	input	PIN_3,	// A[2]
	input	PIN_4,	// A[3]
	input	PIN_5,	// EXEC
	input	PIN_6,	// CLR

	// 7-segment LCD displays
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

	// drive USB pull-up resistor to '0' to disable USB
	assign USBPU = 0;
	
	wire [7:0]	sum = 8'b00000000;
	wire [3:0]	switches;

	assign switches = {PIN_4, PIN_3, PIN_2, PIN_1};
	// state determines what happens when the button is pressed.
	reg [1:0]	state = 2'b00;

	assign LED = state[0];

	segment7 low (
		.number(sum[3:0]),
		.out({PIN_15, PIN_14, PIN_13, PIN_12, PIN_11, PIN_10, PIN_9})
	);

	segment7 hi (
		.number(sum[7:4]),
		.out({PIN_22, PIN_21, PIN_20, PIN_19, PIN_18, PIN_17, PIN_16})
	);

	always @(PIN_5)
	begin
		case (state)
			2'b00: begin
				sum = switches;
				state <= 2'b01;
			end
			2'b01: begin
				sum = sum + switches;
				state = 2'b10;
			end
		endcase
	end

	always @(PIN_6)
	begin
		state <= 2'b00;
		sum = 7'b0000000;
	end

endmodule
