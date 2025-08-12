% utils/id3.pl - ID3 Algorithm Implementation

:- module(id3, [
    information_gain/3,
    entropy/2,
    attribute_value/17
]).

:- use_module(library(lists)).

% Extract attribute value from song data
attribute_value(valence, V, V, _, _, _, _, _, _, _, _, _, _, _, _, _, _).
attribute_value(year, V, _, V, _, _, _, _, _, _, _, _, _, _, _, _, _).
attribute_value(acousticness, V, _, _, V, _, _, _, _, _, _, _, _, _, _, _, _).
attribute_value(danceability, V, _, _, _, V, _, _, _, _, _, _, _, _, _, _, _).
attribute_value(duration, V, _, _, _, _, V, _, _, _, _, _, _, _, _, _, _).
attribute_value(energy, V, _, _, _, _, _, V, _, _, _, _, _, _, _, _, _).
attribute_value(explicit, V, _, _, _, _, _, _, V, _, _, _, _, _, _, _, _).
attribute_value(instrumentalness, V, _, _, _, _, _, _, _, V, _, _, _, _, _, _, _).
attribute_value(key, V, _, _, _, _, _, _, _, _, V, _, _, _, _, _, _).
attribute_value(popularity, V, _, _, _, _, _, _, _, _, _, V, _, _, _, _, _).
attribute_value(liveness, V, _, _, _, _, _, _, _, _, _, _, V, _, _, _, _).
attribute_value(loudness, V, _, _, _, _, _, _, _, _, _, _, _, V, _, _, _).
attribute_value(mode, V, _, _, _, _, _, _, _, _, _, _, _, _, V, _, _).
attribute_value(speechiness, V, _, _, _, _, _, _, _, _, _, _, _, _, _, V, _).
attribute_value(tempo, V, _, _, _, _, _, _, _, _, _, _, _, _, _, _, V).

% Information gain calculation
information_gain(Songs, Question, Gain) :-
    % Calculate entropy of the current set
    entropy(Songs, CurrentEntropy),
    % Get all possible values for this attribute
    findall(Value, (
        member(SongID, Songs),
        song(SongID, _, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit, Instrumental,
             Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo),
        attribute_value(Question, Value, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit,
                       Instrumental, Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo)
    ), Values),
    sort(Values, UniqueValues),
    % Calculate weighted entropy for each value
    findall(WeightedEntropy, (
        member(Value, UniqueValues),
        findall(SongID, (
            member(SongID, Songs),
            song(SongID, _, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit, Instrumental,
                 Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo),
            attribute_value(Question, Value, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit,
                           Instrumental, Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo)
        ), Subset),
        length(Subset, SubsetSize),
        length(Songs, TotalSize),
        Weight is SubsetSize / TotalSize,
        entropy(Subset, SubsetEntropy),
        WeightedEntropy is Weight * SubsetEntropy
    ), WeightedEntropies),
    sum_list(WeightedEntropies, ExpectedEntropy),
    % Calculate information gain
    Gain is CurrentEntropy - ExpectedEntropy.

% Calculate entropy of a set of songs
entropy(Songs, Entropy) :-
    length(Songs, Total),
    findall(Count, (
        member(SongID, Songs),
        song(SongID, _, Valence, Year, Acoustic, Dance, Duration, Energy, Explicit, Instrumental,
             Key, Popularity, Liveness, Loudness, Mode, Speechiness, Tempo),
        findall(_, (
            member(OtherID, Songs),
            song(OtherID, _, V, Y, A, D, Du, E, Ex, I, K, P, L, Lo, M, Sp, T),
            Valence = V, Year = Y, Acoustic = A, Dance = D, Duration = Du,
            Energy = E, Explicit = Ex, Instrumental = I, Key = K,
            Popularity = P, Liveness = L, Loudness = Lo, Mode = M,
            Speechiness = Sp, Tempo = T
        ), Matches),
        length(Matches, Count)
    ), Counts),
    sort(Counts, UniqueCounts),
    findall(LogTerm, (
        member(Count, UniqueCounts),
        Probability is Count / Total,
        LogTerm is -Probability * log(Probability)
    ), LogTerms),
    sum_list(LogTerms, Entropy). 