PLATFORM ?=	default
CSTD :=		c99
CFLAGS ?=	-std=$(CSTD) -Wall -Werror -O0 -g -DPLATFORM_$(PLATFORM)
LDFLAGS :=	-static
OBJS :=		stack.o		\
		eval.o		\
		word.o		\
		kf.o
TARGET :=	kf-$(PLATFORM)

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS)

clean-objs:
	rm -f $(OBJS)

clean: clean-objs
	rm -f kf-pc kf-default

install: $(TARGET)
	cp $(TARGET) ~/bin
	chmod 0755 ~/bin/$(TARGET)

cross:
	make PLATFORM=default clean-objs all
	make PLATFORM=pc clean-objs all
