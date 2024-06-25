# Header Dictionary Triples (HDT) for SWI-Prolog

This  repository  is  work  in  progress   to  provide  access  to  [HDT
files](http://www.rdfhdt.org/)  from  SWI-Prolog  based    on  the  [C++
library](https://github.com/rdfhdt/hdt-cpp.git) for these files.

HDT files form a natural addition to SWI-Prolog's memory based RDF store
to access large amounts of static background knowledge without enlarging
the memory footprint.

This repository is organised as a SWI-Prolog _pack_.  To install it,
perform the steps below.  Installation and usage is tested on Ubuntu
and Fedora.  This should work on most Unix-like machines.
Installation on Windows requires more creativity though.

## Installation

### Installing dependencies

In addition to the usual development tools  such `make` and a C compiler
we need GNU automake and related tools and the RDF base libraries `serd`
and `raptor2`. Below are the dependencies  for `apt` based Linux systems
and `rpm` based systems.

For Debian/Ubuntu based systems

    apt-get install libtool automake autoconf libserd-dev libraptor2-dev

For Fedora

    dnf install aclocal automake libtool serd-devel raptor2-devel

3. After the prerequisites are installed, the HDT library can be
   installed from within Prolog using the following command:

```bash
?- pack_install(hdt).
```

## Usage

If the installation went well, you can load the HDT library with the following command:


```bash
?- [library(hdt)].
```

## Status

The [HDT](https://www.rdfhdt.org/) format is   attractive  for accessing
large amounts of _background_ RDF data.  HDT   is  the basis for several
companies that provide RDF technology at scale. Unfortunately the public
version of the software is poorly maintained.

This     pack     is     based     on       our     [fork     of     the
hdt-cpp](https://github.com/JanWielemaker/hdt-cpp). The fork   is mostly
the work of Peter Ludemann, fixing several   issues  with modern C++ and
libraries.

## Notes

The `hdt-cpp` directory has a number of utilities in
`hdt-cpp/libhdt/tools`.  For details, see `hdt-cpp/README.md`:
* hdt2rdf
* hdtInfo
* hdtSearch
* modifyHeader
* rdf2hdt
* replaceHeader
* searchHeader
