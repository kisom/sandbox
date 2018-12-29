/// btled
/// Button-toggled LED
///
/// This toggles the LED using a pushbutton on pin 6. The pushbutton
/// has one side connected to Vcc, and the other side to both pin 6 and
/// a 10K Ω resistor (because, for some reason, that's what I have on
/// hand).
module top (
	input	PIN_6,
	input	CLK,
	output	LED
);

	reg	state = 0;

	assign LED = state;

	// I found the negedge was better for debouncing.
	always @(negedge PIN_6)
		state <= !state;


endmodule
