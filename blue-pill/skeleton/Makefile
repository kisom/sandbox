# configurables
OBJS :=		startup.o
TARGET :=	
OBJS +=		$(TARGET).o

# targets
ELF :=		$(TARGET).elf
BIN :=		$(TARGET).bin

# toolchain setup
ARMTC :=	arm-none-eabi
CC :=		$(ARMTC)-gcc
LD :=		$(ARMTC)-gcc
AS :=		$(ARMTC)-as
ARMSIZE :=	$(ARMTC)-size
OBJCOPY :=	$(ARMTC)-objcopy
PAGER ?=	less

# compiler options
CPUFLAGS :=	-mcpu=cortex-m3 -mthumb -std=c99
CFLAGS :=	-Wall -Wextra -Os -MD $(CPUFLAGS) -g
LDFLAGS :=	$(CPUFLAGS) -nostartfiles -Wl,-T,bluepill.ld
LDLIBS :=	-lc -lnosys

# programmer options
STARTMEM :=	0x8000000

# targets

.PHONY: all
all: $(BIN)

$(ELF): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)
	$(ARMSIZE) $@

.PHONY: strip
strip: $(ELF)
	$(ARMTC)-strip $(ELF)
	$(ARMSIZE) $(ELF)

$(BIN): $(ELF)
	$(OBJCOPY) -O binary $< $@

.PHONY: flash
flash: $(BIN)
	st-flash write $(BIN) $(STARTMEM)

.PHONY: erase
erase:
	st-flash erase

.PHONY: reset
reset:
	st-flash reset

.PHONY: install
install: erase flash reset

.PHONY: clean
clean:
	rm -f *.o *.bin *.elf *.d *.map

.PHONY: disass
disass: $(ELF)
	$(ARMTC)-objdump -D $(ELF) | $(PAGER)

.PHONY: dump
dump: $(ELF)
	$(ARMTC)-objcopy -D $(ELF) > $(TARGET).dump
