% -*- mode: Prolog; coding: utf-8 -*-

/* These tests use the data in data/hdt-cpp-libhdt-data-literals.hdt
   See the comments in data/README.md about where this comes from.

   TODO: Test hdt_create_from_file/3. One possible way:
         (cd data && ../../hdt-cpp/libhdt/tools/rdf2hdt -f turtle breg-dcat-example.ttl breg-dcat-example.hdt)
         hdt_create_from_file('/tmp/breg-dcat-example.hdt', 'data/breg-dcat-example.ttl', [format(ttl),base_uri('file://breg-dcat-example.ttl')]).
         hdt_open(H, '/tmp/breg-dcat-example.hdt'), foreach(hdt_search(H, S, P, O), (writeq(S-P-O),nl)), hdt_close(H).
         ... and compare with data/breg-dcat-example.hdt
         echo '? ? ?'|./hdt-cpp/libhdt/tools/hdtSearch /tmp/breg-dcat-example.hdt
*/

:- module(test_hdt,
         [ test_hdt/0
         ]).

:- use_module(library(plunit)).
:- use_module(library(hdt)).

test_hdt :-
    run_tests([hdt]).

:- begin_tests(hdt).

test(hdt_search_id) :-
    hdt_open_literals(H),
    bagof(S-P-O, hdt_search_id(H, S, P, O), SPOs),
    assertion(SPOs == [1-1-1,
                       1-1-2,
                       1-1-3,
                       1-1-4,
                       1-1-5,
                       1-1-6,
                       1-1-7,
                       1-1-8,
                       1-1-9]),
    hdt_close(H).

/*
  TBD: hdtSearch gives a slightly different result that hdt_search/4.
       This seems to be because TripleString::setObject()
       removes suffix "^^http://www.w3.org/2001/XMLSchema#string"
  Note that '^^' and '@' are binary operators (exported from library(hdt)).
  echo '? ? ?'|./hdt-cpp/libhdt/tools/hdtSearch ./test/data/hdt-cpp-libhdt-data-literals.hdt
s p "a"
s p "a"@en
s p "a"^^<bcd>
s p "abc"
s p "abc"@en
s p "abc"^^<bcd>
s p "bc"
s p "bc"@en
s p "bc"^^<bcd>
9 results in 51 us
*/

test(hdt_search) :-
    hdt_open_literals(H),
    bagof(S-P-O, hdt_search(H, S, P, O), SPOs),
    assertion(SPOs ==
             [s-p-("a"^^'http://www.w3.org/2001/XMLSchema#string'),
              s-p-"a"@en,
              s-p-("a"^^bcd),
              s-p-("abc"^^'http://www.w3.org/2001/XMLSchema#string'),
              s-p-"abc"@en,
              s-p-("abc"^^bcd),
              s-p-("bc"^^'http://www.w3.org/2001/XMLSchema#string'),
              s-p-"bc"@en,
              s-p-("bc"^^bcd)
             ]),
    hdt_close(H).

hdt_open_literals(H) :-
    predicate_property(test_hdt, file(TestHdtFile)),
    directory_file_path(TestHdtDir, _, TestHdtFile),
    string_concat(TestHdtDir, "/data/hdt-cpp-libhdt-data-literals.hdt", LiteralsHdtPath),
    hdt_open(H, LiteralsHdtPath).

:- end_tests(hdt).

