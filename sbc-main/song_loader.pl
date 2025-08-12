:- use_module(library(csv)).
:- dynamic song/17.
:- dynamic artist_song/3.
:- consult('utils/artists_parser.pl').
:- consult('utils/constants.pl').
:- consult('utils/data_processing.pl').

% Load file and ignore header
load_songs(File) :-
    writeln('Loading data...'),
    retractall(song(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)),
    retractall(artist_song(_,_,_)),
    csv_read_file(File, [_Header | Rows], [functor(song_raw), arity(19), separator(0',)]),
    maplist(process_row, Rows),
    writeln('Data loaded successfully!').

% Process row
process_row(song_raw(Val, Year, Ac, ArtistsRaw, Dance, Dur, En, Ex, ID,
                     Inst, Key, Live, Loud, Mode, Name,
                     Pop, _ReleaseDate, Speech, Tempo)) :-

    % Round and discretize
    ValR is round(Val * 100) / 100, clusterize(ValR, ValC),
    AcR is round(Ac * 100) / 100, clusterize(AcR, AcC),
    DanceR is round(Dance * 100) / 100, clusterize(DanceR, DanceC),
    normalize_duration(Dur, DurNorm), clusterize(DurNorm, DurC),
    EnR is round(En * 100) / 100, clusterize(EnR, EnC),
    LiveR is round(Live * 100) / 100, clusterize(LiveR, LiveC),
    normalize_popularity(Pop, PopNorm), clusterize(PopNorm, PopC),
    SpeechR is round(Speech * 100) / 100, clusterize(SpeechR, SpeechC),
    normalize_tempo(Tempo, TempoNorm), clusterize(TempoNorm, TempoC),
    LoudR is round((Loud + 60) / 60 * 100) / 100, clusterize(LoudR, LoudC),

    % Binary conversions
    boolify(Ex, Explicit),
    (Inst > 0.5 -> Instr = yes ; Instr = no),

    % Decade
    decade(Year, YearBin),

    % Save discretized song
    assertz(song(ID, Name, ValC, YearBin, AcC, DanceC, DurC, EnC, Explicit, Instr, 
                 Key, PopC, LiveC, LoudC, Mode, SpeechC, TempoC)),

    % Process artist list
    parse_artists(ArtistsRaw, ArtistList),
    forall(member(A, ArtistList), assertz(artist_song(ID, Name, A))).
