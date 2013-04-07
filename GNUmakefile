# The primary makefile for this project. A 'Makefile' (if any) is
# generated depending on configuration.

PROG := exportemplapp
SRCDIRS := src datasrc-dummy engine-hw console-ui
DEPFLAGS := $(patsubst %, -I% ,$(SRCDIRS))
CXXFLAGS := -Wall -std=c++0x
LDFLAGS := 
SRCFILES := $(wildcard $(patsubst %, %/*.cpp, $(SRCDIRS)))
OBJFILES := $(patsubst %.cpp, %.o, $(SRCFILES))
GENFILES := 

default : build

.depend : GNUmakefile $(GENFILES)
	fastdep $(DEPFLAGS) --remakedeptarget=$@ $(SRCFILES) > $@

-include .depend

build : $(PROG)

info:
	@echo $(PROG)
	@echo $(OBJFILES)
	@echo $(SRCFILES)

$(PROG) : $(OBJFILES)
	g++ -o $@ $(OBJFILES) $(LDFLAGS)

%.o : %.cpp
	g++ -c $(CXXFLAGS) $(DEPFLAGS) -o $@ $<

clean :
	-rm $(PROG) $(OBJFILES)
