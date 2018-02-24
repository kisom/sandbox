CXXSTD :=	c++14
CXXFLAGS :=	-std=$(CXXSTD) -Wall -Werror -g -O0
OBJS :=		linux/io.o	\
		io.o		\
		parser.o	\
		kforth.o
TARGET :=	kforth

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CXX) $(CFLAGS) -o $@ $(OBJS)

clean:
	rm -f $(OBJS) $(TARGET)
