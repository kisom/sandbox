MEMORY
{
    flash (rx) : ORIGIN = 0x08000000, LENGTH = 64K
    sram (rwx) : ORIGIN = 0x20000000, LENGTH = 20K
}

EXTERN(vectors);
ENTRY(reset_handler);

SECTIONS
{
	.text : {
		*(.vectors*)
 		*(.text*)
		. = ALIGN(4);
	} > flash
	.bss  : { *(.bss*) } > sram
}
