The files were generated by:

```
./hdt-cpp/libhdt/tools/hdtInfo hdt-cpp/libhdt/data/literals.hdt >literals.hdtInfo-output
echo '? ? ?' | ./libhdt/tools/hdtSearch lib-cpp/libhdt/data/literals.hdt 2>/dev/null >literals.hdtSearch-output
```

To do this, you need to run "make" separately (the "make" that runs from `pack_install(.)` does something different because it has other options to `./configure`):
```sh
cd hdt-cpp && ./autogen.sh && ./configure && make -j8
```

TODO: use hdt2rdf, rdf2hdt