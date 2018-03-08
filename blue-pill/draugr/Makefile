# configurables
OBJS :=		startup.o
TARGET :=	blink
OBJS += $(TARGET).o

# targets
ELF :=		$(TARGET).elf
BIN :=		$(TARGET).bin

# toolchain setup
ARMTC :=	arm-none-eabi
AS :=		$(ARMTC)-as
CC :=		$(ARMTC)-gcc
CXX :=		$(ARMTC)-g++
LD :=		$(ARMTC)-ld
ARMSIZE :=	$(ARMTC)-size
OBJCOPY :=	$(ARMTC)-objcopy
PAGER ?=	less
OPENOCD ?=	/usr/share/openocd

# compiler options
CPUFLAGS :=	-mcpu=cortex-m3 -mthumb
CFLAGS :=	-Wall -Wextra -Os -MD $(CPUFLAGS)
CXXFLAGS :=	$(CFLAGS) -std=c++14 -fno-rtti -fno-exceptions -ffunction-sections -fdata-sections -fno-builtin
LDFLAGS :=	$(CPUFLAGS) -nostartfiles -Wl,-T,stm32f103.ld
LDLIBS :=	-lc -lnosys

# programmer options
STARTMEM :=	0x8000000

### build targets ###
.PHONY: all
all: $(BIN)

$(ELF): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)
	$(ARMSIZE) $@

$(BIN): $(ELF)
	$(OBJCOPY) -O binary $< $@

.PHONY: clean
clean:
	rm -f *.o *.bin *.elf *.d *.map

### programming targets ###
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

### miscellaneous targets ###
.PHONY: disass
disass: $(ELF)
	$(ARMTC)-objdump -D $(ELF) | $(PAGER)

.PHONY: dump
dump: $(ELF)
	$(ARMTC)-objdump -D $(ELF) > $(TARGET).dump

.PHONY: ocd
ocd:
	openocd -f $(OPENOCD)/scripts/interface/stlink-v2.cfg -f $(OPENOCD)/scripts/target/stm32f1x.cfg
