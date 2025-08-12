
% Normalize duration between 0 and 1
normalize_duration(Dur, Normalized) :-
    min_duration(Min),
    max_duration(Max),
    Normalized is (Dur - Min) / (Max - Min).

% Normalize tempo between 0 and 1
normalize_tempo(Tempo, Normalized) :-
    min_tempo(Min),
    max_tempo(Max),
    Normalized is (Tempo - Min) / (Max - Min).

% Normalize popularity between 0 and 1
normalize_popularity(Pop, Normalized) :-
    min_popularity(Min),
    max_popularity(Max),
    Normalized is (Pop - Min) / (Max - Min).


% Cluster into 10 labeled intervals (c1..c10)
clusterize(Value, Cluster) :-
    ClusterSize is 0.1,
    Index is floor(Value / ClusterSize),
    ClusterNum is Index + 1,
    format(atom(Cluster), 'c~d', [ClusterNum]).


boolify(0, no).
boolify(1, yes).

decade(Year, Decade) :-
    integer(Year),
    Dec is (Year // 10) * 10,
    format(atom(Decade), 'the~ds', [Dec]).
