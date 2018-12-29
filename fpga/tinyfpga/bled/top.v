/// bled
/// Button-press LED
///
/// This connects a pushbutton on pin 6 to the LED. The pushbutton has one
/// side connected to Vcc, and the other side to both pin 6 and a 10K
/// Ω resistor (because, for some reason, that's what I have on hand).
module top (
	input	PIN_6,
	output	LED
);

	assign LED = PIN_6;

endmodule
