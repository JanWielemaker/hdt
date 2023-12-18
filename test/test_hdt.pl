% -*- mode: Prolog; coding: utf-8 -*-

/* These tests use the data in ../hdt-cpp/libhdt/data/literals.hdt
   See the comments in data/README.md if a test fails - the failure
   might be because the data has changed.
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
    bagof(S:P:O, hdt_search_id(H, S, P, O), SPOs),
    assertion(SPOs == [1:1:1,
                       1:1:2,
                       1:1:3,
                       1:1:4,
                       1:1:5,
                       1:1:6,
                       1:1:7,
                       1:1:8,
                       1:1:9]),
    hdt_close(H).

hdt_open_literals(H) :-
    predicate_property(test_hdt, file(TestHdtFile)),
    directory_file_path(TestHdtDir, _, TestHdtFile),
    atom_concat(TestHdtDir, '/../hdt-cpp/libhdt/data/literals.hdt', LiteralsHdtPath),
    hdt_open(H, LiteralsHdtPath).

:- end_tests(hdt).

