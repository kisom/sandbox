/// btled: button-toggled LED.
/// This is a port of the TinyFPGA btled to the iCEStick.

module top (
	input	BR10,

	output	D1,
	output	D2,
	output	D3,
	output	D4,
	output	D5
);


	reg	state = 0;
	assign	D1 = state;
	assign	D2 = state;
	assign	D3 = state;
	assign	D4 = state;
	assign	D5 = state;

	always @(negedge BR10) state <= !state;

endmodule
