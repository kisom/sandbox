module half_adder (
	input	a,
	input	b,
	output	sum,
	output	carry
);

	sum = a ^ b;
	carry = a & b;
endmodule

module full_adder (
	input	a;
	input	b;
	input	carry_in;
	output	sum;
	output	carry_out;
);

	wire	carry_1;
	wire	carry_2;

	// Intermediate output from first half-adder.
	wire	sum_1;

	half_adder ha_low (
		.a(a),
		.b(b),
		.carry(carry_1),
		.sum(sum_1)
	);

	half_adder ha_high (
		.a(sum_1),
		.b(carry_1),
		.carry(carry_2),
		.sum(sum)
	);

	assign	carry_out = carry_1 | carry_2;
endmodule

module addition_unit #(
	ADDER_WIDTH = 4
) (
	input	carry_in;
	input	[ADDER_WIDTH-1:0] a,
	input	[ADDER_WIDTH-1:0] b,

	output	[ADDER_WIDTH-1:0] sum,
	output	carry_out
)

	wire	[ADDER_WIDTH-1:0] carry_chain;

	genvar	i;
	generate
		for (i = 0; i < ADDER_WIDTH; i = i + 1) begin
			full_adder	fa (
				.a(a[i]),
				.b(b[i]),
				.sum(sum[i]),
				.carry_in(carry_chain[i]),
				.carry_out(carry_chain[i+1])
			);
		end
	endgenerate

	// Assign carry_in and carry_out to the beginning and end of the
	// carry chain.
	assign	carry_chain[0] = carry_in;
	assign	carry_chain[ADDER_WIDTH] = carry_out;
endmodule

module better_addition_unit #(
	ADDER_WIDTH = 8
) (
	input	carry_in;
	input	[ADDER_WIDTH-1:0] a,
	input	[ADDER_WIDTH-1:0] b,

	output	[ADDER_WIDTH-1:0] sum,
	output	carry_out
);

	wire	[ADDER_WIDTH:0] full_sum;

	assign	full_sum = carry_in + a + b;
	assign	carry_out = full_sum[ADDER_WIDTH];
endmodule

