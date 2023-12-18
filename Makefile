# Build HDT library for SWI-Prolog

HDTHOME=hdt-cpp
LIBHDT=$(HDTHOME)/libhdt
LIBCDS=$(HDTHOME)/libcds
HDTLIB=$(LIBHDT)/.libs
CDSLIB=$(LIBCDS)/.libs
SOBJ=	$(PACKSODIR)/hdt4pl.$(SOEXT)
NPROC:=$(shell expr $$(nproc) + 1)
MAKE_J=-j$(NPROC)
CFLAGS+=-I$(LIBHDT)/include -g
# This doesn't work because the *.so files get picked first:
#     LIBS=	-L$(HDTLIB) -L$(CDSLIB) -lhdt -lcds
# Instead, we copy the *.a files intothe same directory as $(OBJ)
# - see the rules for $(OBJ2).
LIBS= -Lc -lhdt -lcds
OBJ=	c/hdt4pl.o
OBJ2=	c/libcds.a c/libhdt.a
LD=g++

all:	$(SOBJ)

c/libhdt.a: $(HDTLIB)/libhdt.a
	ln -f $< $@

c/libcds.a: $(CDSLIB)/libcds.a
	ln -f $< $@

$(SOBJ): $(OBJ) $(OBJ2)
	mkdir -p $(PACKSODIR)
	$(LD) $(ARCH) $(LDSOFLAGS) -o $@ $< $(LIBS) $(SWISOLIB) -lserd-0

c/hdt4pl.o: c/hdt4pl.cpp $(HDTLIB)/libhdt.a $(CDSLIB)/libcds.a
	$(CC) $(ARCH) $(CFLAGS) -c -o $@ c/hdt4pl.cpp

$(HDTLIB)/libhdt.a $(HDTLIB)/libcds.a: FORCE
	$(MAKE) -C $(HDTHOME) $(MAKE_J)

FORCE:

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
