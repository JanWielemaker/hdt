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

1. Install a recent version of Serd. One way of doing this
on Ubuntu is: `sudo apt install libserd-0-0 serdi`

The currently installed version can be found by the command `serdi
-v`. The minimum version of Serd is 0.28.0 (see `hdt-cpp/README.md` or
the `before_install` rule in `hdt-cpp/.travis.yml`).

If that isn't available, it can be installed and built by:
```bash
curl -s http://download.drobilla.net/serd-0.30.0.tar.bz2 | tar -xj && \
  cd serd-0.30.0 && \
  python2 ./waf configure && \
  python2 ./waf && \
  sudo ./waf install;
```

You may wish to specify `--prefix=/usr/local` or `--prefix=$HOME/.local`
to `waf configure`.

You can uninstall by `python2 ./waf uninstall`

2. Install Raptor2.

   On Fedora: `sudo dnf install raptor2-devel`

   On Ubuntu: `sudo apt-get install libraptor2-dev`

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

Usable, but still experimental.

## Notes

See also [RDF basics wiki](https://swi-prolog.discourse.group/t/rdf-basics/4105).

The `hdt-cpp` directory has a number of utilities in
`hdt-cpp/libhdt/tools`.  For details, see `hdt-cpp/README.md`:
* hdt2rdf
* hdtInfo
* hdtSearch
* modifyHeader
* rdf2hdt
* replaceHeader
* searchHeader

