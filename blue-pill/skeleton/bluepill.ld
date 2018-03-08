MEMORY
{
    flash (rx) : ORIGIN = 0x00000000, LENGTH = 128K
    sram (rwx) : ORIGIN = 0x20000000, LENGTH = 20K
}

SECTIONS
{
   .text : { *(.text*) } > flash
   .bss  : { *(.bss*) } > sram
}
