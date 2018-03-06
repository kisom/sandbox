# configurables
OBJS :=		
TARGET :=	

# targets
ELF :=		$(TARGET).elf
BIN :=		$(TARGET).bin

# toolchain setup
ARMTC :=	arm-none-eabi
ARMCC :=	$(ARMTC)-gcc
CC :=		$(ARMCC)
LD :=		$(ARMCC)
ARMSIZE :=	$(ARMTC)-size
OBJCOPY :=	$(ARMTC)-objcopy

# compiler options
CPUFLAGS :=	-mcpu=cortex-m3 -mthumb
CFLAGS :=	-Wall -Wextra -Os -MD $(CPUFLAGS)
LDFLAGS :=	$(CPUFLAGS) -nostartfiles -Wl,-T,bluepill.ld
LDLIBS :=	-lc -lnosys

# programmer options
STARTMEM :=	0x8000000

# targets

.PHONY: all
all: $(BIN)

$(ELF): $(OBJS)
	$(ARMCC) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)
	$(ARMSIZE) -A $@

$(BIN): $(ELF)
	$(OBJCOPY) -O binary $< $@

.PHONY: flash
flash: $(BIN)
	st-flash write $(BIN) $(STARTMEM)

.PHONY: erase
erase:
	st-flash erase

.PHONY: install
install: erase flash

.PHONY: clean
clean:
	rm -f *.o *.bin *.elf *.d *.map

