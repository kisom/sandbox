/*
 * Startup code for the STM32F103-based blue pill board.
 *
 * TODO: revisit stack pointer
 * TODO: is the IRQv buffer actually needed right now?
 */

.globl	vectors
.align	2
.long	0x100			/* best guess at stack pointer */
.long	reset_handler		/* reset handler */
.long	0			/* NMI handler */
.long	0			/* hard_fault_handler */
.long	0			/* memory management handler */
.long	0			/* bus fault handler */
.long	0			/* usage fault handler */
.skip	0x20			/* reserved */
.long	0			/* svcall handler */
.long	0			/* debug handler */
.skip	4			/* reserved */
.long	0			/* pendsv handler */
.long	0			/* systick handler */
.skip   0x110			/* remaining / IRQ vectors */

.globl	reset_handler
reset_handler:
	bl	main
