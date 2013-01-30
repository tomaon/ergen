#
 CXX=/opt/gnu/gcc/4.6.3/bin/g++
#CXX=/usr/bin/g++

 CCFLAGS  =
 CCFLAGS += -std=c++98 # c++03
 CCFLAGS += -D__unix
 CCFLAGS += -D__STDC_FORMAT_MACROS -D__STDC_CONSTANT_MACROS
 CCFLAGS += -g
#CCFLAGS += -O2
 CCFLAGS += -Wall
#CCFLAGS += -Wextra
#CCFLAGS += -Werror
 CCFLAGS += -Wno-write-strings
 CCFLAGS += -Wno-reorder
 CCFLAGS += -fPIC
 CCFLAGS += -fno-common

#LDFLAGS=
#AR=
#ARFLAGS=

include ../prj/Makefile

generate:
	(cd ..; bin/EGenLoader -c 1000 -t 1000 -w 1)
