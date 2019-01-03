module SwitchLED (
	 input	PIN_1,
	 output LED,
	 output USBPU
);

	assign	USBPU = 0;
	assign	LED = PIN_1;

endmodule
