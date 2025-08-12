:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(csv)).
:- use_module(library(readutil)).

% Quiz state
:- dynamic current_song/1.
:- dynamic current_answers/1.
:- dynamic score/1.
:- dynamic song_sequence/2.  % New: stores sequence for each song
:- dynamic remaining_songs/1.

% Initialize quiz with a random song
start_quiz :-
    % Clean up any existing state
    retractall(current_song(_)),
    retractall(current_answers(_)),
    retractall(score(_)),
    retractall(song_sequence(_, _)),
    retractall(remaining_songs(_)),
    
    % Load sequences from CSV
    load_sequences('song_sequences.csv'),
    
    % Initialize new state
    assertz(score(0)),
    findall(SongID, song_sequence(SongID, _), AllSongs),
    (AllSongs = [] ->
        writeln('Error: No songs found in sequences. Please generate sequences first.'),
        fail
    ; writeln('Welcome to the Song Quiz!'),
      writeln('Choose the correct answers.'),
      writeln('Let\'s begin!'),
      nl,
      show_quiz_options(AllSongs)).

% Show quiz options menu
show_quiz_options(AllSongs) :-
    writeln('Please choose an option:'),
    writeln('1. Get a random song'),
    writeln('2. Enter a song name'),
    write('Your choice (1 or 2): '), flush_output,
    read(Choice),
    read_line_to_codes(user_input, _),
    (Choice = 1 ->
        random_member(SongID, AllSongs),
        assertz(current_song(SongID)),
        assertz(current_answers([])),
        assertz(remaining_songs(AllSongs)),
        show_current_song(random),
        ask_questions
    ; Choice = 2 ->
        write('Enter song name: '), flush_output,
        read_line_to_string(user_input, SongName),
        find_song_by_name(SongName, AllSongs, SongID),
        assertz(current_song(SongID)),
        assertz(current_answers([])),
        assertz(remaining_songs(AllSongs)),
        show_current_song(named),
        ask_questions
    ; writeln('Invalid choice. Please try again.'),
      show_quiz_options(AllSongs)).

% Find song by name (case-insensitive partial match)
find_song_by_name(SongName, AllSongs, SongID) :-
    % Convert input to lowercase for case-insensitive matching
    downcase_atom(SongName, LowerName),
    % Find all matching songs
    findall(ID-Name, 
        (member(ID, AllSongs),
         song(ID, Name, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
         downcase_atom(Name, LowerSongName),
         sub_atom(LowerSongName, _, _, _, LowerName)),
        Matches),
    % Handle matches
    (Matches = [] ->
        writeln('No songs found matching that name. Please try again.'),
        write('Enter song name: '), flush_output,
        read_line_to_string(user_input, NewSongName),
        find_song_by_name(NewSongName, AllSongs, SongID)
    ; Matches = [FoundID-_] ->
        SongID = FoundID
    ; % Multiple matches
        writeln('Multiple songs found. Please choose one:'),
        show_matching_songs(Matches, 1),
        write('Enter the number of your choice: '), flush_output,
        read(Choice),
        nth1(Choice, Matches, SongID-_)
    ).

% Show list of matching songs with numbers
show_matching_songs([], _).
show_matching_songs([ID-Name|Rest], N) :-
    format('~d. ~w~n', [N, Name]),
    N1 is N + 1,
    show_matching_songs(Rest, N1).

% Load sequences from CSV
load_sequences(File) :-
    writeln('Loading sequences from CSV...'),
    csv_read_file(File,  [_Header | Rows], [functor(song_sequence_raw), arity(3), separator(0'|)]),
    
    forall(member(Row, Rows),
        (Row = song_sequence_raw(SongID, _, SequenceStr),
         parse_sequence(SequenceStr, Sequence),
         assertz(song_sequence(SongID, Sequence)))).

% Parse sequence string into list
parse_sequence(SequenceStr, Attributes) :-
    % Remove surrounding brackets
    atom_string(SequenceStrAtom, SequenceStr),
    sub_atom(SequenceStrAtom, 1, _, 1, Inner),
    % Split by comma
    split_string(Inner, ",", " ", Strings),
    maplist(atom_string, Attributes, Strings).



show_current_song(Mode) :-
    current_song(SongID),
    (song(SongID, Name, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
        findall(Artist, artist_song(SongID, _, Artist), Artists),
        nl, write('----------------------------------------'), nl,
        write('Current song: '), write(Name), nl,
        write('Artists: '), write(Artists), nl,
        write('----------------------------------------'), nl,
        nl,
        write('Do you want to try a different song? (yes/no): '), flush_output,
        read(UserChoice),
        (UserChoice = yes ->
            (Mode = random -> reselect_random_song
            ; Mode = named   -> reselect_named_song)
        ; true)
    ; writeln('Error: Song not found in database.'), fail).


reselect_random_song :-
    current_song(CurrentID),
    remaining_songs(AllSongs),
    exclude(=(CurrentID), AllSongs, OtherSongs),
    (OtherSongs = [] ->
        writeln('No other songs available. Keeping current song.')
    ; random_member(NewID, OtherSongs),
      retract(current_song(_)),
      assertz(current_song(NewID)),
      show_current_song(random)
    ).

reselect_named_song :-
    remaining_songs(AllSongs),
    write('Enter new song name: '), flush_output,
    read_line_to_codes(user_input, _),
    read_line_to_string(user_input, NewName),
    find_song_by_name(NewName, AllSongs, NewID),
    retract(current_song(_)),
    assertz(current_song(NewID)),
    show_current_song(named).



% Ask questions about the song using pre-generated sequence
ask_questions :-
    current_song(SongID),
    song_sequence(SongID, Sequence),
    current_answers(Answers),
    length(Answers, AnsCount),
    length(Sequence, SeqCount),
    (AnsCount >= SeqCount ->
        show_results(SongID)
    ; nth0(AnsCount, Sequence, NextAttribute),
      question(NextAttribute, QuestionText, QuestionOptions),
      ask_question(question(NextAttribute, QuestionText, QuestionOptions))).

% Ask a single question and process answer
ask_question(question(Attribute, Text, Options)) :-
    current_song(SongID),
    nl, write('----------------------------------------'), nl,
    write(Text), nl,
    write('Options: '), write(Options), nl,
    write('Your answer: '), flush_output,
    read(Answer),
    
    % Check if answer is correct
    (song(SongID, _, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit, Instrumental,
          Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo),
     attribute_value(Attribute, CorrectValue, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit,
                    Instrumental, Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo) ->
        % Update score and store answer
        (Answer = CorrectValue ->
            score(CurrentScore),
            NewScore is CurrentScore + 1,
            retract(score(CurrentScore)),
            assertz(score(NewScore)),
            write('Correct!'), nl
        ; write('Incorrect! The correct answer was: '), write(CorrectValue), nl),
        
        % Store the answer
        current_answers(CurrentAnswers),
        retract(current_answers(CurrentAnswers)),
        assertz(current_answers([answer(Attribute, Answer) | CurrentAnswers])),
        
        % Update remaining songs based on the answer
        remaining_songs(CurrentSongs),
        findall(S, (member(S, CurrentSongs),
                   song(S, _, V, Y, A, D, Du, E, Ex, I, K, P, L, Lo, M, Sp, T),
                   attribute_value(Attribute, CorrectValue, V, Y, A, D, Du, E, Ex, I, K, P, L, Lo, M, Sp, T)),
                NewSongs),
        retract(remaining_songs(_)),
        assertz(remaining_songs(NewSongs)),
        
        % Show number of remaining songs
        length(NewSongs, RemainingCount),
        write('Number of songs remaining: '), write(RemainingCount), nl,
        write('----------------------------------------'), nl,
        
        % Continue with next question
        ask_questions
    ; write('Error: Could not process answer.'),
      fail).

% Show final results
show_results(SongID) :-
    nl, write('----------------------------------------'), nl,
    write('Quiz completed!'), nl, nl,
    % Show the song name and artists
    (song(SongID, Name, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
        findall(Artist, artist_song(SongID, _, Artist), Artists),
        write('Song: '), write(Name), nl,
        write('Artists: '), write(Artists), nl
    ; write('Song information not found for ID: '), write(SongID), nl),
    nl,
    % Show score and number of questions
    score(FinalScore),
    current_answers(AnsList),
    length(AnsList, TotalQuestions),
    write('Final score: '), write(FinalScore), write('/'), write(TotalQuestions), nl, nl,
    write('Questions and answers:'), nl,
    (AnsList = [] ->
        writeln('No questions were answered.')
    ; reverse(AnsList, ReversedAnswers),  % Show questions in order they were asked
      show_answers(ReversedAnswers, SongID, 1)),  % Start numbering from 1
    write('----------------------------------------'), nl.

% Show user's answers compared to correct answers, with numbering and formatting
show_answers([], _, _).
show_answers([answer(Attribute, UserAnswer) | Rest], SongID, N) :-
    % Get the question text
    (question(Attribute, Text, _) ->
        % Get the correct answer
        (song(SongID, _, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit, Instrumental,
              Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo) ->
            (attribute_value(Attribute, CorrectAnswer, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit,
                        Instrumental, Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo) ->
                % Display question and answers with numbering and formatting
                write(N), write('. '), write(Text), nl,
                write('   Your answer: '), write(UserAnswer), write(' (correct: '), write(CorrectAnswer), write(')'), nl, nl,
                N1 is N + 1,
                show_answers(Rest, SongID, N1)
            ; write('Error: Could not find correct answer for attribute: '), write(Attribute), nl,
              N1 is N + 1,
              show_answers(Rest, SongID, N1))
        ; write('Error: Could not find song for ID: '), write(SongID), nl,
          N1 is N + 1,
          show_answers(Rest, SongID, N1))
    ; write('Error: Could not find question text for attribute: '), write(Attribute), nl,
      N1 is N + 1,
      show_answers(Rest, SongID, N1)). 