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

# compiler options
CPUFLAGS :=	-mcpu=cortex-m3 -mthumb
CFLAGS :=	-Wall -Wextra -Os -MD $(CPUFLAGS)
CXXFLAGS :=	$(CFLAGS) -std=c++14 -fno-rtti -fno-exceptions -ffunction-sections -fdata-sections -fno-builtin
LDFLAGS :=	$(CPUFLAGS) -nostartfiles -Wl,-T,stm32f103.ld
LDLIBS :=	-lc -lnosys

# programmer options
STARTMEM :=	0x8000000

# targets

.PHONY: all
all: $(BIN)

$(ELF): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)
	$(ARMSIZE) $@

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

