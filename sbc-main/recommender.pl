:- use_module(library(lists)).
:- use_module(song_loader).         
:- use_module(utils/data_processing).

% Attribute names in the order they appear in the profile vector
attribute_names([
  year, valence, acousticness, danceability, duration, energy,
  explicit, instrumentalness, key, popularity, liveness,
  loudness, mode, speechiness, tempo
]).

% Entry point for recommendation mode
start_recommend_mode :-
    writeln('Enter at least 3 song IDs or name substrings, separated by commas:'),
    read_line_to_string(user_input, Line),
    split_string(Line, ",", " \t", Inputs),
    maplist(resolve_identifier, Inputs, ValidIDs0),
    list_to_set(ValidIDs0, ValidIDs),
    length(ValidIDs, N),
    (  N < 3
    -> writeln('Error: Please provide at least 3 valid songs.'), nl,
       start_recommend_mode
    ;  recommend(ValidIDs, 5)
    ).

% Recommend the top K most similar songs
recommend(ValidIDs, K) :-
    build_profiles(ValidIDs, TargetProfiles),
    findall(
      candidate(ID, AvgScore, TotalMatches, AllDiffs),
      (
        song(ID,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
        \+ member(ID, ValidIDs),
        extract_profile(ID, CandProfile),
        aggregate_similarity(CandProfile, TargetProfiles, AvgScore, TotalMatches),
        differences_all(TargetProfiles, CandProfile, AllDiffs)
      ),
      Candidates
    ),
    sort(2, @>=, Candidates, Sorted),
    take(K, Sorted, TopK),
    display_recommendations(TopK).

% resolve_identifier(+UserString, -SongID)
resolve_identifier(String, ID) :-
    ( atom_string(ID, String),
      song(ID,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) ->
        true
    ; downcase_atom(String, Lower),
      song(ID,Name,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
      downcase_atom(Name, LName),
      sub_atom(LName, _, _, _, Lower)
    ),
    !.

% Build profiles for each target
build_profiles([], []).
build_profiles([ID|Rest], [Profile|PrRest]) :-
    extract_profile(ID, Profile),
    build_profiles(Rest, PrRest).

% Extract 15 discrete attributes into a vector
extract_profile(ID, [Year,Val,Ac,Dc,Du,En,Ex,Instr,K,P,L,Ld,M,Sp,Te]) :-
    song(ID,_,Val,Year,Ac,Dc,Du,En,Ex,Instr,K,P,L,Ld,M,Sp,Te).

% Compute average and total matches across all targets
aggregate_similarity(Profile, Targets, AvgScore, TotalMatches) :-
    findall(
      M,
      ( member(T, Targets),
        sim(Profile, T, M)
      ),
      Matches
    ),
    sum_list(Matches, Sum),
    length(Targets, N),
    AvgScore is Sum / N,
    TotalMatches = Sum.

% Count matching attributes
sim(P1, P2, Matches) :-
    pairwise(P1, P2, 0, Matches).

pairwise([], [], Acc, Acc).
pairwise([X|Xs], [X|Ys], Acc, Matches) :-
    !, Acc1 is Acc + 1,
    pairwise(Xs, Ys, Acc1, Matches).
pairwise([_|Xs], [_|Ys], Acc, Matches) :-
    pairwise(Xs, Ys, Acc, Matches).

% Compute differences against each target
differences_all(Targets, Cand, AllDiffs) :-
    attribute_names(Names),
    findall(
      target(I, Diffs),
      (
        nth1(I, Targets, Ref),
        findall(
          Name-RefVal-CandVal,
          ( nth1(J, Names, Name),
            nth1(J, Ref,    RefVal),
            nth1(J, Cand,   CandVal),
            RefVal \= CandVal
          ),
          Diffs
        )
      ),
      AllDiffs
    ).

% Display recommendations in numbered, readable form
display_recommendations([]) :- !.
display_recommendations(List) :-
    writeln(''),
    display_recommendations(List, 1).

display_recommendations([], _).
display_recommendations([candidate(ID, Avg, Total, AllDiffs)|Rest], N) :-
    format('Recommendation ~w:~n', [N]),
    song(ID, Title, _,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
    findall(A, artist_song(ID,_,A), Artists),
    format('  Title    : ~w~n', [Title]),
    format('  Artists  : ~w~n', [Artists]),
    format('  Matches  : ~w/45 total (avg: ~2f per target)~n', [Total, Avg]),
    writeln('  Differences per target:'),
    forall(
      member(target(I, Diffs), AllDiffs),
      (
        format('    vs Target ~w:~n', [I]),
        forall(
          member(Attr-RefVal-CandVal, Diffs),
          format('      - ~w: ~w vs ~w~n', [Attr, RefVal, CandVal])
        )
      )
    ),
    writeln(''),
    N1 is N + 1,
    display_recommendations(Rest, N1).

% Take the first K elements of a list
take(0, _, []).
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :-
    N > 0,
    N1 is N - 1,
    take(N1, Xs, Ys).
