TARGET :=	ion
OBJS :=		$(TARGET).o
CFLAGS ?=	-g -std=c99 -Wall -Werror

.PHONY: all run
all: run
	
run: $(TARGET)
	@echo "running $(TARGET)"
	./$(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

.PHONY: clean
clean:
	rm -f $(OBJS) $(TARGET)

.PHONY: release
release: clean
	CFLAGS="$(CFLAGS) -DRELEASE" make $(TARGET)