# configurables
OBJS :=		
TARGET :=	blink
OBJS += $(TARGET).o

# targets
ELF :=		$(TARGET).elf
BIN :=		$(TARGET).bin

# toolchain setup
ARMTC :=	arm-none-eabi
ARMCC :=	$(ARMTC)-gcc
ARMCXX :=	$(ARMTC)-g++
CC :=		$(ARMCC)
CXX :=		$(ARMCXX)
LD :=		$(ARMCXX)
ARMSIZE :=	$(ARMTC)-size
OBJCOPY :=	$(ARMTC)-objcopy

# compiler options
CPUFLAGS :=	-mcpu=cortex-m3 -mthumb
CFLAGS :=	-Wall -Wextra -Os -MD $(CPUFLAGS)
CXXFLAGS :=	-Wall -Wextra -Os -MD $(CPUFLAGS) -std=c++14
LDFLAGS :=	$(CPUFLAGS) -nostartfiles -Wl,-T,stm32f103.ld
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

