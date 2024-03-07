# Build HDT library for SWI-Prolog

HDTHOME=hdt-cpp
LIBHDT=$(HDTHOME)/libhdt
LIBCDS=$(HDTHOME)/libcds
HDTLIB=$(LIBHDT)/.libs
CDSLIB=$(LIBCDS)/.libs
SOBJ=	$(SWIPL_MODULE_DIR)/hdt4pl.$(SWIPL_MODULE_EXT)
NPROC:=$(shell expr $$(nproc) + 1)
MAKE_J=-j$(NPROC)
CFLAGS=$(SWIPL_CFLAGS) -I$(LIBHDT)/include -g
# This doesn't work because the *.so files get picked first:
#     LIBS=	-L$(HDTLIB) -L$(CDSLIB) -lhdt -lcds
# Instead, we copy the *.a files into the same directory as $(OBJ)
# - see the rules for $(OBJ2).
LIBS= -Lc -lhdt -lcds
OBJ=	c/hdt4pl.o
OBJ2=	c/libcds.a c/libhdt.a
# WARNING: A previous version of this Makefile set LD=g++
#          ... this confuses hdt-cpp's use of libtool.
#          The correct way of using the linker is with $(CC):
#          see the "implicit variables" in the GNU make documentation
#          Note that pack_version(2) defines `SWIPL_CC` and
#          "swipl pack install ." creates ./buildenv.sh, which
#          defines the SWIPL_* environment variables

CC=$(SWIPL_CC)

all:	$(SOBJ)

c/libhdt.a: $(HDTLIB)/libhdt.a
	ln -f $< $@

c/libcds.a: $(CDSLIB)/libcds.a
	ln -f $< $@

$(SOBJ): $(OBJ) $(OBJ2)
	mkdir -p $(SWIPL_MODULE_DIR)
	$(CC) $(ARCH) $(SWIPL_MODULE_LDFLAGS) -o $@ $< $(LIBS) $(SWIPL_MODULE_LIB) -lserd-0

c/hdt4pl.o: c/hdt4pl.cpp $(HDTLIB)/libhdt.a $(CDSLIB)/libcds.a
	$(CC) $(ARCH) $(CFLAGS) -c -o $@ c/hdt4pl.cpp

$(HDTLIB)/libhdt.a $(HDTLIB)/libcds.a: FORCE
	$(MAKE) -C $(HDTHOME) $(MAKE_J)

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

distclean: clean
	rm -f $(SOBJ)
	[ ! -f $(HDTHOME)/Makefile ] || $(MAKE) -C $(HDTHOME) distclean
	cd $(HDTHOME) && git clean -d -f -x
