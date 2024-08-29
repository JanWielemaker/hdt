# Build HDT library for SWI-Prolog

# TODO: hdt-cpp/Makefile has a lot of undefined variables
# MAKEFLAGS=--warn-undefined-variables

HDTHOME=hdt-cpp
LIBHDT=$(HDTHOME)/libhdt
LIBCDS=$(HDTHOME)/libcds
HDTLIB=$(LIBHDT)/.libs
CDSLIB=$(LIBCDS)/.libs
SOBJ=	$(SWIPL_MODULE_DIR)/hdt4pl.$(SWIPL_MODULE_EXT)
OBJ=	c/hdt4pl.o
NPROC:=$(shell expr $$(nproc) + 1)
MAKE_J=-j$(NPROC)
COFLAGS=-O2 -Wall
CXXFLAGS=$(SWIPL_CFLAGS) -I$(LIBHDT)/include -std=c++17 $(COFLAGS)
LIBS=	$(HDTLIB)/libhdt.a $(CDSLIB)/libcds.a
# WARNING: A previous version of this Makefile set LD=g++
#          ... this confuses hdt-cpp's use of libtool.
#          The correct way of using the linker is with $(CC):
#          see the "implicit variables" in the GNU make documentation
#          Note that pack_version(2) defines `SWIPL_CC` and
#          "swipl pack install ." creates ./buildenv.sh, which
#          defines the SWIPL_* environment variables

# The following variables should be set by Make, but in case they're
# not, get the values that swipl sets (also in buildenv.sh)
CC?=$(SWIPL_CC)
CXX?=$(SWIPL_CXX)

# The following should be set by buildenv.sh:
SWIPL?=swipl

# A dummy file, which is created if $(HDTHOME) succeeds
HDT_CPP_SENTINEL=.hdt-cpp-sentinel

all:	$(SOBJ)

$(SOBJ): $(OBJ) $(HDT_CPP_SENTINEL)
	mkdir -p $(SWIPL_MODULE_DIR)
	$(CXX) $(SWIPL_MODULE_LDFLAGS) -o $@ $(OBJ) $(LIBS) $(SWIPL_MODULE_LIB) -lserd-0

c/hdt4pl.o: c/hdt4pl.cpp $(HDT_CPP_SENTINEL)
	$(CXX) $(CXXFLAGS) -c -o $@ c/hdt4pl.cpp

$(HDT_CPP_SENTINEL): $(HDTHOME)/Makefile
	set -x -e && $(MAKE) -C $(HDTHOME) $(MAKE_J)
	touch $(HDT_CPP_SENTINEL)

$(HDTHOME)/Makefile:
	./configure

FORCE:

check::
	$(MAKE) -C $(HDTHOME) $(MAKE_J) check

check::
	$(SWIPL) -g test_hdt -t halt test/test_hdt.pl

install::

clean:
	$(RM) -f $(OBJ) $(OBJ2) $(HDT_CPP_SENTINEL)
	[ ! -f $(HDTHOME)/Makefile ] || (cd $(HDTHOME) && git reset --hard)
	[ ! -f $(HDTHOME)/Makefile ] || $(MAKE) -C $(HDTHOME) clean

distclean: clean
	$(RM) -f $(SOBJ)
	[ ! -f $(HDTHOME)/Makefile ] || $(MAKE) -C $(HDTHOME) distclean
	cd $(HDTHOME) && git clean -d -f -x

# For development - need to have done pack_install(.)
#   which also creates buildenv.sh, so you can do:
#   make -C path/to/hdt dev-build
# You may need to comment out the 'git reset' and 'git submodule update'
# lines in configure and the "clean" rule of this Makefile

dev-build:
	. ./buildenv.sh && $(MAKE) all check
	$(SWIPL) -g run_tests -t halt test/test_hdt.pl
