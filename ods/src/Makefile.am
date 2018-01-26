AM_CPPFLAGS  =  -Wall -Wextra -pedantic -Wshadow -Wpointer-arith -Wcast-align
AM_CPPFLAGS +=  -Wwrite-strings -Wmissing-declarations -Wno-long-long -Werror
AM_CPPFLAGS +=  -Wunused-variable -std=c++17 -D_XOPEN_SOURCE -O0 -g -I.
AM_CPPFLAGS +=  -fno-elide-constructors -Weffc++ -fPIC

bin_PROGRAMS :=		ch01ex01 ch01ex03 ch01ex04 ch01ex05 ch01ex06 \
			list_bench uset_bench sset_bench
ch01ex01_SOURCES :=	ch01ex01.cc
ch01ex03_SOURCES :=	ch01ex03.cc
ch01ex04_SOURCES :=	ch01ex04.cc
ch01ex05_SOURCES :=	ch01ex05.cc
ch01ex06_SOURCES :=	ch01ex06.cc
list_bench_SOURCES :=	list_bench.cc
uset_bench_SOURCES :=	uset_bench.cc
sset_bench_SOURCES :=	sset_bench.cc

BENCH_OPS ?=	1000000

benchmarks: list_bench uset_bench sset_bench
	@echo "LIST BENCHMARKS"
	@./list_bench $(BENCH_OPS) > /dev/null
	@echo
	@echo "USET BENCHMARKS"
	@./uset_bench $(BENCH_OPS) > /dev/null
	@echo
	@echo "SSET BENCHMARKS"
	@./sset_bench $(BENCH_OPS) > /dev/null