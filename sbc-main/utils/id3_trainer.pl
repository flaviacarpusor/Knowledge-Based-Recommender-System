:- use_module(library(csv)).
:- use_module(library(lists)).

% Predicate to train ID3 and generate attribute sequences
train_id3_and_generate_sequences(InputFile, OutputFile) :-
    % Load and process the data
    load_songs(InputFile),
    
    % Get all songs
    findall(SongID, song(SongID, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), AllSongs),
    
    % Generate sequences for each song
    findall(sequence(SongID, Name, Attributes),
        (member(SongID, AllSongs),
         song(SongID, Name, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
         generate_attribute_sequence(SongID, Attributes)),
        Sequences),
    
    % Write to CSV
    write_sequences_to_csv(Sequences, OutputFile).

% Generate attribute sequence for a single song using ID3
generate_attribute_sequence(SongID, Attributes) :-
    % Get all possible attributes
    findall(Attribute, question(Attribute, _, _), AllAttributes),
    
    % Get song attributes
    song(SongID, _, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit, Instrumental,
         Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo),
    
    % Start with empty sequence and all songs
    findall(S, song(S, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), AllSongs),
    generate_sequence_dfs(SongID, AllSongs, AllAttributes, [], Attributes).

% Generate sequence using DFS with information gain
generate_sequence_dfs(_, [SingleSong], _, Acc, Acc) :-
    !.  % Stop when we've reduced to one song
generate_sequence_dfs(SongID, CurrentSongs, AvailableAttributes, Acc, FinalAttributes) :-
    AvailableAttributes \= [],
    % Find attribute with maximum information gain
    find_best_attribute(CurrentSongs, AvailableAttributes, BestAttribute),
    
    % Get the value for this attribute for the target song
    song(SongID, _, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit, Instrumental,
         Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo),
    attribute_value(BestAttribute, Value, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit,
                   Instrumental, Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo),
    
    % Filter songs that match this value
    findall(S, (member(S, CurrentSongs),
                song(S, _, V, Y, A, D, Du, E, Ex, I, K, P, L, Lo, M, Sp, T),
                attribute_value(BestAttribute, Value, V, Y, A, D, Du, E, Ex, I, K, P, L, Lo, M, Sp, T)),
            FilteredSongs),
    
    % Add attribute to sequence
    append(Acc, [BestAttribute], NewAcc),
    
    % Remove used attribute
    delete(AvailableAttributes, BestAttribute, RemainingAttributes),
    
    % Continue DFS
    generate_sequence_dfs(SongID, FilteredSongs, RemainingAttributes, NewAcc, FinalAttributes).

% Find attribute with maximum information gain
find_best_attribute(Songs, Attributes, BestAttribute) :-
    findall(gain(Attribute, Gain),
        (member(Attribute, Attributes),
         information_gain(Songs, question(Attribute, _, _), Gain)),
        Gains),
    sort(2, @>=, Gains, [gain(BestAttribute, _)|_]).

% Write sequences to CSV
write_sequences_to_csv(Sequences, OutputFile) :-
    open(OutputFile, write, Stream),
    % Write header
    writeln(Stream, 'song_id|song_name|attribute_sequence'),
    
    % Write each sequence
    forall(member(sequence(SongID, Name, Attributes), Sequences),
        (format(Stream, '~w|~w|', [SongID, Name]),
         write_attribute_sequence(Stream, Attributes),
         nl(Stream))),
    
    close(Stream).
% Write attribute sequence like [a,b,c] into the CSV
write_attribute_sequence(Stream, Attributes) :-
    write(Stream, '['),
    write_attributes_list(Stream, Attributes),
    write(Stream, ']').

write_attributes_list(_, []).
write_attributes_list(Stream, [A]) :-
    write(Stream, A).
write_attributes_list(Stream, [A|Rest]) :-
    write(Stream, A),
    write(Stream, ','),
    write_attributes_list(Stream, Rest).
