:- use_module(library(lists)).

get_answers([], _, FoundSongs) :-
    nl, write('----------------------------------------'), nl,
    write('Finished asking questions.'), nl,
    (FoundSongs = [] ->
        write('No matching songs found.') 
    ; length(FoundSongs, N),
      (N = 1 ->
          display_song(FoundSongs)
      ; write('We couldn\'t narrow it down to one song.'), nl,
        write('We found '), write(N), write(' possible matches.'), nl,
        write('Here is one of them:'), nl,
        display_song(FoundSongs)
      )
    ).

get_answers([question(Attribute, Text, Options) | Rest], AnswersSoFar, FoundSongs) :-
    nl, write('----------------------------------------'), nl,
    write(Text), nl,
    write('Options: '), write(Options), nl,
    write('Additional option: skip'), nl,
    write('Your answer: '), flush_output,
    read(Val),
    (Val = skip ->
        length(FoundSongs, N),
        write('Question skipped. Current number of matching songs: '), write(N), nl,
        get_answers(Rest, AnswersSoFar, FoundSongs)

    ; member(Val, Options) ->
        findall(SongID, match([answer(Attribute, Val) | AnswersSoFar], SongID), NewMatches),
        length(NewMatches, N),
        write('Found '), write(N), write(' matching songs.'), nl,
        (N = 0 ->
            write('sorry but I dont know a song like that. Please choose a different option.'), nl,
            % extrage opțiunile posibile rămase din FoundSongs
            findall(ValidVal,
                ( member(SongID, FoundSongs),
                  song(SongID, _, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit, Instrumental,
                       Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo),
                  attribute_value(Attribute, ValidVal, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit,
                                  Instrumental, Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo)
                ),
                PossibleVals),
            sort(PossibleVals, RemainingOptions),
            write('I know songs only with the next values: '), write(RemainingOptions), nl,
            get_answers([question(Attribute, Text, RemainingOptions) | Rest], AnswersSoFar, FoundSongs)

        ; N = 1 ->
            write('Perfect! I know exactly what song you are thinking of.'), nl,
            display_song(NewMatches)
        ; write('Multiple matches found. Let\'s continue with more questions.'), nl,
          get_answers(Rest, [answer(Attribute, Val) | AnswersSoFar], NewMatches)
        )

    ; write('Invalid option! Please choose from the list.'), nl,
      get_answers([question(Attribute, Text, Options) | Rest], AnswersSoFar, FoundSongs)
    ).


% Filter songs
match([], _).
match([answer(A, V) | R], SongID) :-
    song(SongID, _, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit, Instrumental,
         Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo),
    attribute_value(A, V, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit, Instrumental,
                   Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo),
    match(R, SongID).

attribute_value(valence, V, V1, _, _, _, _, _, _, _, _, _, _, _, _, _, _) :- V = V1.
attribute_value(year, V, _, V1, _, _, _, _, _, _, _, _, _, _, _, _, _) :- V = V1.
attribute_value(acousticness, V, _, _, V1, _, _, _, _, _, _, _, _, _, _, _, _) :- V = V1.
attribute_value(danceability, V, _, _, _, V1, _, _, _, _, _, _, _, _, _, _, _) :- V = V1.
attribute_value(duration, V, _, _, _, _, V1, _, _, _, _, _, _, _, _, _, _) :- V = V1.
attribute_value(energy, V, _, _, _, _, _, V1, _, _, _, _, _, _, _, _, _) :- V = V1.
attribute_value(explicit, V, _, _, _, _, _, _, V1, _, _, _, _, _, _, _, _) :- V = V1.
attribute_value(instrumentalness, V, _, _, _, _, _, _, _, V1, _, _, _, _, _, _, _) :- V = V1.
attribute_value(key, V, _, _, _, _, _, _, _, _, V1, _, _, _, _, _, _) :- V = V1.
attribute_value(popularity, V, _, _, _, _, _, _, _, _, _, V1, _, _, _, _, _) :- V = V1.
attribute_value(liveness, V, _, _, _, _, _, _, _, _, _, _, V1, _, _, _, _) :- V = V1.
attribute_value(loudness, V, _, _, _, _, _, _, _, _, _, _, _, V1, _, _, _) :- V = V1.
attribute_value(mode, V, _, _, _, _, _, _, _, _, _, _, _, _, V1, _, _) :- V = V1.
attribute_value(speechiness, V, _, _, _, _, _, _, _, _, _, _, _, _, _, V1, _) :- V = V1.
attribute_value(tempo, V, _, _, _, _, _, _, _, _, _, _, _, _, _, _, V1) :- V = V1.

% Display song information
display_song([SongID|_]) :-
    song(SongID, Name, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit, Instrumental,
         Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo),
    findall(Artist, artist_song(SongID, _, Artist), Artists),
    nl, write('----------------------------------------'), nl,
    write('Found a matching song!'), nl,
    write('Name: '), write(Name), nl,
    write('Artists: '), write(Artists), nl,
    write('Decade: '), write(Year), nl,
    write('Mood (Valence): '), write(Valence), nl,
    write('Energy: '), write(Energy), nl,
    write('Danceability: '), write(Dance), nl,
    write('Acousticness: '), write(Acoustic), nl,
    write('Duration: '), write(Duration), nl,
    write('Explicit content: '), write(Explicit), nl,
    write('Instrumental: '), write(Instrumental), nl,
    write('Musical Key: '), write(Key), nl,
    write('Popularity: '), write(Popularity), nl,
    write('Liveness: '), write(Liveness), nl,
    write('Loudness: '), write(Loudness), nl,
    write('Mode (0=minor, 1=major): '), write(Mode), nl,
    write('Speechiness: '), write(Speechiness), nl,
    write('Tempo: '), write(Tempo), nl,
    write('----------------------------------------'), nl.

% Find the song
find_song :-
    write('I will ask you some questions to guess the song you are thinking of.'), nl,
    write('Please answer each question by choosing one of the options.'), nl,
    write('You can type "skip" to skip a question if you\'re not sure.'), nl, nl,
    findall(question(Attribute, Text, Options), question(Attribute, Text, Options), Questions),
    findall(SongID, song(SongID, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), AllSongs),

    get_answers(Questions, [], AllSongs).

