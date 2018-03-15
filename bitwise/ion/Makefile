TARGET :=	ion
OBJS :=		$(TARGET).o
CFLAGS :=	-g -std=c99

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

.PHONY: clean
clean:
	rm -f $(OBJS) $(TARGET)