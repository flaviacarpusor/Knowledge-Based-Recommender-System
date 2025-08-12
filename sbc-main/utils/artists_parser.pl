
% Remove quotes and convert to atom list
parse_artists(String, Artists) :-
    atom(String), !,
    atom_string(String, Str),
    remove_brackets(Str, CleanedStr),
    split_string(CleanedStr, ",", " ", ArtistStrList),
    maplist(clean_artist, ArtistStrList, Artists).

remove_brackets(Str, Cleaned) :-
    sub_atom(Str, 1, _, 1, Mid),  % remove '[' and ']'
    string_codes(Mid, Codes),
    exclude(=(39), Codes, NoQuotes), % remove quotes
    string_codes(Cleaned, NoQuotes).

clean_artist(Str, Cleaned) :-
    string_codes(Str, Codes),
    exclude(=(32), Codes, NoSpaces),
    string_codes(CleanedStr, NoSpaces),
    atom_string(Cleaned, CleanedStr).