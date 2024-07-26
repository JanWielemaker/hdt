# Build HDT library for SWI-Prolog

HDTHOME=hdt-cpp
LIBHDT=$(HDTHOME)/libhdt
LIBCDS=$(HDTHOME)/libcds
HDTLIB=$(LIBHDT)/.libs
CDSLIB=$(LIBCDS)/.libs
SOBJ=	$(SWIPL_MODULE_DIR)/hdt4pl.$(SWIPL_MODULE_EXT)
NPROC:=$(shell expr $$(nproc) + 1)
MAKE_J=-j$(NPROC)
COFLAGS=-O2
CXXFLAGS=$(SWIPL_CFLAGS) -I$(LIBHDT)/include -std=c++17 $(COFLAGS)
# This doesn't work because the *.so files get picked first:
#     LIBS=	-L$(HDTLIB) -L$(CDSLIB) -lhdt -lcds
# Instead, we copy the *.a files into the same directory as $(OBJ)
# - see the rules for $(OBJ2).
OBJ=	c/hdt4pl.o
LIBS=	$(HDTLIB)/libhdt.a $(CDSLIB)/libcds.a
# WARNING: A previous version of this Makefile set LD=g++
#          ... this confuses hdt-cpp's use of libtool.
#          The correct way of using the linker is with $(CC):
#          see the "implicit variables" in the GNU make documentation
#          Note that pack_version(2) defines `SWIPL_CC` and
#          "swipl pack install ." creates ./buildenv.sh, which
#          defines the SWIPL_* environment variables

CC?=$(SWIPL_CC)
CXX?=$(SWIPL_CXX)

all:	$(SOBJ)

$(SOBJ): $(OBJ) .hdt-cpp-sentinel
	mkdir -p $(SWIPL_MODULE_DIR)
	$(CXX) $(SWIPL_MODULE_LDFLAGS) -o $@ $(OBJ) $(LIBS) $(SWIPL_MODULE_LIB) -lserd-0

c/hdt4pl.o: c/hdt4pl.cpp .hdt-cpp-sentinel
	$(CXX) $(CXXFLAGS) -c -o $@ c/hdt4pl.cpp

.hdt-cpp-sentinel: $(HDTHOME)/Makefile
	set -x -e && $(MAKE) -C $(HDTHOME) $(MAKE_J)
	touch .hdt-cpp-sentinel

$(HDTHOME)/Makefile:
	./configure

FORCE:

check::
	$(MAKE) -C $(HDTHOME) $(MAKE_J) check

check::
	$(SWIPL) -g test_hdt -t halt test/test_hdt.pl

install::

clean:
	rm -f $(OBJ) $(OBJ2)
	[ ! -f $(HDTHOME)/Makefile ] || (cd $(HDTHOME) && git reset --hard)
	[ ! -f $(HDTHOME)/Makefile ] || $(MAKE) -C $(HDTHOME) clean
	rm -f .hdt-cpp-sentinel

distclean: clean
	rm -f $(SOBJ)
	[ ! -f $(HDTHOME)/Makefile ] || $(MAKE) -C $(HDTHOME) distclean
	cd $(HDTHOME) && git clean -d -f -x

# For development - need to have done pack_install(.)
#   which also creates buildenv.sh, so you can do:
#   make -C path/to/hdt dev-build
# You may need to comment out the 'git reset' and 'git submodule update'
# lines in configure and the "clean" rule of this Makefile

dev-build:
	. ./buildenv.sh && $(MAKE)
	swipl -g run_tests -t halt test/test_hdt.pl
