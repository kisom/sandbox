/*
 * Startup code for the STM32F103-based blue pill board.
 *
 * The stack pointer is initialised to the top of the stack (which
 * grows downward), so I pointed it to the end of the SRAM (where the
 * stack should be).
 */

.cpu cortex-m3
.thumb

.section	.text
.global vectors
vectors:
.align	2
.long	0x20005000		/* stack pointer points to top of SRAM */
.long	reset_handler		/* reset handler */
.long	hang			/* NMI handler */
.long	hang			/* hard_fault_handler */
.long	hang			/* memory management handler */
.long	hang			/* bus fault handler */
.long	hang			/* usage fault handler */
.skip	0x20			/* reserved */
.long	hang			/* svcall handler */
.long	hang			/* debug handler */
.skip	4			/* reserved */
.long	hang			/* pendsv handler */
.long	hang			/* systick handler */
.skip   0x100			/* remaining / IRQ vectors */


.thumb_func
hang:   b .

.thumb_func
reset_handler:
	bl	main
