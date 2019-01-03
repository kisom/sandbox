module NumberDisplay (
	input	PIN_1,
	input	PIN_2,
	input	PIN_3,
	input	PIN_4,
	output	PIN_9,
	output	PIN_10,
	output	PIN_11,
	output	PIN_12,
	output	PIN_13,
	output	PIN_14,
	output	PIN_15,
	output	USBPU
);

	assign	USBPU = 0;

	SegmentLCD	display (
		.number({PIN_4, PIN_3, PIN_2, PIN_1}),
		.out({PIN_15, PIN_14, PIN_13, PIN_12, PIN_11, PIN_10, PIN_9})
	);

endmodule
