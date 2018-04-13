AM_CPPFLAGS  =	-Wall -Wextra -pedantic -Wshadow -Wpointer-arith -Wcast-align
AM_CPPFLAGS +=	-Wwrite-strings -Wmissing-declarations -Wno-long-long -Werror
AM_CPPFLAGS +=	-Wunused-variable -std=c++14 -D_XOPEN_SOURCE -O2 -I.
AM_CPPFLAGS +=	-fno-elide-constructors	 -Weffc++

pkgconfigdir =			$(libdir)/pkgconfig
pkgconfig_DATA =		libalgotune-1.pc

lib_LTLIBRARIES =		libalgotune.la
nobase_include_HEADERS =	algotune.h
libalgotune_la_SOURCES =	algotune.cc
