:- use_module(library(csv)).
:- consult('utils/artists_parser.pl').
:- consult('utils/constants.pl').
:- consult('utils/data_processing.pl').
:- consult('utils/id3_trainer.pl').

% Main predicate to generate sequences
generate_sequences :-
    writeln('Generating question sequences...'),
    train_id3_and_generate_sequences('test.csv', 'song_sequences.csv'),
    writeln('Sequences generated successfully in song_sequences.csv').

% Run when loaded
:- generate_sequences. 