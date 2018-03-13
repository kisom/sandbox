.syntax unified
.cpu cortex-m3
.thumb

.section .text
.global	vectors
vectors:
.align	2
.word	0
.word	_start

.thumb_func
.global	_start
_start:
	/* enable port C clock */
	ldr	r0,	=0x40021000
	ldr	r1,	[r0, #0x18]
	mov	r2,	#5
	orr	r1,	r2
	str	r1,	[r0, #0x18]

	/*
	 * enable output mode at a max of 2 MHz.
	 * note that the default is a general purpose output push-pull,
	 * which is what it should be.
	 */
	ldr	r0,	=0x40011000
	ldr	r1,	[r0, #0x04]
	mov	r2,	#0x02
	lsl	r2,	#0x14
	orr	r1,	r2
	str	r1,	[r0, #0x04]

	/* write LED pin (PC13) */
	ldr	r0,	=0x40011000
	ldr	r1,	[r0, #0x10]
	mov	r2,	#0x01
	lsl	r2,	#0x0d
	lsl	r2,	#0x10
	orr	r1,	r2
	str	r1,	[r0, #0x10]

hang:	bl .
