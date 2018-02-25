CXXSTD :=	c++14
CXXFLAGS :=	-std=$(CXXSTD) -Wall -Werror -g -O0
OBJS :=		linux/io.o	\
		io.o		\
		parser.o	\
		word.o		\
		dict.o		\
		kforth.o
TARGET :=	kforth

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CXX) $(CFLAGS) -o $@ $(OBJS)

clean:
	rm -f $(OBJS) $(TARGET)

install: $(TARGET)
	cp $(TARGET) ~/bin
	chmod 0755 ~/bin/$(TARGET)
