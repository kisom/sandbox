// look in pins.pcf for all the pin names on the TinyFPGA BX board
module top (
    input CLK,    // 16MHz clock
    output LED,   // User/boot LED next to power LED
    output USBPU  // USB pull-up resistor
);
    // drive USB pull-up resistor to '0' to disable USB
    assign USBPU = 0;

    ////////
    // make a simple blink circuit
    ////////

    // keep track of time and location in blink_pattern
    reg [20:0]	clock_counter = 21'b000000000000000000000;
    reg [3:0]	blink_counter = 4'b0000;

    // pattern that will be flashed over the LED over time
    wire [9:0] blink_pattern = 10'b1010000000;

    // increment the blink_counter every clock
    always @(posedge CLK) begin
        clock_counter <= clock_counter + 1;
	if (clock_counter == 21'b110000110101000000000) begin
		blink_counter <= blink_counter + 1;
		clock_counter <= 0;
	end

	if (blink_counter == 4'b1011) blink_counter <= 0;
    end
    
    // light up the LED according to the pattern
    assign LED = blink_pattern[blink_counter];
endmodule
