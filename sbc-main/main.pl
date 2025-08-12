:- consult('utils/artists_parser.pl').
:- consult('utils/constants.pl').
:- consult('utils/data_processing.pl').
:- consult('utils/id3.pl').
:- consult('song_loader.pl').
:- consult('game.pl').
:- consult('questions.pl').
:- consult('quiz.pl').
:- consult('recommender.pl').

:- initialization(run).

% run/0
% Load the dataset and enter the menu loop.
run :-
    load_songs('test.csv'),
    menu_loop.

% menu_loop/0
% Display the main menu, read the user's choice, and dispatch.
menu_loop :-
    nl,
    writeln('Welcome to the Music Game!'),
    writeln('Please choose an option:'),
    writeln(' 1. Guess the song'),
    writeln(' 2. Quiz'),
    writeln(' 3. Recommend songs'),
    writeln(' 4. Exit'),
    write('Your choice (1-4): '), flush_output,
    read(Choice),
    read_line_to_string(user_input, _),  % consume any trailing input
    handle_choice(Choice).

% handle_choice(+Choice)
% Execute the selected mode or exit/reprompt on invalid input.
handle_choice(1) :-
    start_guess_mode,
    menu_loop.
handle_choice(2) :-
    start_quiz_mode,
    menu_loop.
handle_choice(3) :-
    start_recommend_mode,
    menu_loop.
handle_choice(4) :-
    writeln('Goodbye!'),
    !.
handle_choice(_) :-
    writeln('Invalid choice, please try again.'),
    menu_loop.

% start_guess_mode/0
% Entry point for "Guess the song" mode.
start_guess_mode :-
    writeln('Press Enter to start...'),
    read_line_to_string(user_input, _),
    find_song.

% start_quiz_mode/0
% Entry point for "Quiz" mode.
start_quiz_mode :-
    start_quiz.
