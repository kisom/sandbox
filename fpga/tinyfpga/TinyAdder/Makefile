MODULES :=	SegmentLCD.v TinyAdder.v
TARGET :=	hardware.bin

.PHONY: all
all: $(TARGET)

$(TARGET): $(MODULES)
	apio build

.PHONY: upload clean build
build clean upload:
	apio $@
