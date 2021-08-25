% simplest example to load Thea and an ontology
:- assert(library_directory('/home/liao/thea.git')).
:- use_module(library(owl2_io)).
:- use_module(library(owl2_model)).
:- use_module(library(owl2_util)).
:- load_axioms('pizza.owl').
