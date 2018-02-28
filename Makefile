CXXSTD :=	c++14
CXXFLAGS :=	-std=$(CXXSTD) -Wall -Werror -Os -static
LDFLAGS :=	-static
OBJS :=		linux/io.o	\
		io.o		\
		system.o	\
		parser.o	\
		word.o		\
		dict.o		\
		kforth.o
TARGET :=	kforth

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS)
	strip $@

clean:
	rm -f $(OBJS) $(TARGET)

install: $(TARGET)
	cp $(TARGET) ~/bin
	chmod 0755 ~/bin/$(TARGET)
