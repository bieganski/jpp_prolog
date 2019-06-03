:- use_module(library(lists)) .

zsumujListy([], []) .
zsumujListy([node(_, L1, L2)|Ns], R) :- append(L1, L2, R1), zsumujListy(Ns, R2), append(R1, R2, R) .

istnieje([node(V, _, _)|_], V) .
istnieje([_|Ns], V) :- istnieje(Ns, V) .

istnieja(_, []) .
istnieja(G, [R|Rs]) :- istnieje(G, R), istnieja(G, Rs) .

uzywaIstniejacych(G) :- zsumujListy(G, R), istnieja(G, R) .

unikalne([], X) :- unikalne(X) .
unikalne([node(V, _, _)|Ns], X) :- unikalne(Ns, [V|X]) .
unikalne([]) .
unikalne([V|Vs]) :- \+ member(V, Vs), unikalne(Vs) .

% istniejaOdwrotne([v, e], [e, v])
istniejaOdwrotne([]) .
istniejaOdwrotne([[V, E]|Reszta]) :- 
    member([E, V], Reszta), 
    delete(Reszta, [E, V], Dobre),
    istniejaOdwrotne(Dobre) .

skierowaneF([], []) .
skierowaneF([node(V, _, Fs)|Ns], R) :- zip(V, Fs, R1), skierowaneF(Ns, R2), append(R1, R2, R) .

poprawneF(G) :- skierowaneF(G, X), sort(X, Y), istniejaOdwrotne(Y) .

maksymalnie6Kazdego([X|XS]) :- maksymalnie6Kazdego(XS, X, 1) .
maksymalnie6Kazdego([], _, _) .
maksymalnie6Kazdego([X|XS], X, I) :- J is (I + 1), maksymalnie6Kazdego(XS, X, J) .
maksymalnie6Kazdego([Y|YS], _, _) :- maksymalnie6Kazdego(YS, Y, 1) .

maksymalnie3F(G) :- skierowaneF(G, X), sort(X, Y), flatten(Y, Z), msort(Z, A), write(A), maksymalnie6Kazdego(A) .


jestEFGrafem([]) .
jestEFGrafem(G) :- 
    unikalne(G, []),
    uzywaIstniejacych(G), 
    poprawneF(G) .

% zip(1, [1,2,3], X)
% X = [[1, 1], [1, 2], [1, 3]]
zip(_, [], []) .
zip(El, [X|XS], [[El, X]|R]) :- zip(El, XS, R) .

% wszystkieKombinacje([1,2,3], [4,5], X)
% X = [[[1, 4], [1, 5]], [[2, 4], [2, 5]], [[3, 4], [3, 5]]]
wszystkieKombinacje([], _, []) .
wszystkieKombinacje([El|L1], L2, Wyn) :- zip(El, L2, Z), wszystkieKombinacje(L1, L2, R), append(Z, R, Wyn).

% wchodzi(+G, +Do, -Lista TERMÓW mających E-krawędź ->Do)
wchodzace([], _, []) .
wchodzace([N|Ns], NDo, [NTerm|R]) :- 
    N = node(NTerm, Es, _), 
    NDo = node(Do, _, _), 
    wchodzace(Ns, NDo, R), 
    member(Do, Es) .
wchodzace([_|Ns], NDo, R) :- wchodzace(Ns, NDo, R) .


% nieWchodzi(G, do)
nieWchodzi([], node(_, _, _)) .
nieWchodzi([node(_, Es, _)|Ns], NDo) :- NDo = node(Do, _, _), nieWchodzi(Ns, NDo), \+ member(Do, Es) .

% nieWychodzi(z)
nieWychodzi(node(_, [], _)) .

% poczatkowo [N|Ns] jest G, stopniowo je zmniejszamy, ale potrzebujemy
% pamiętać też kopię G
potencjalneVS(G, X) :- potencjalneVS(G, G, X) .
potencjalneVS(_, [], []) .
potencjalneVS(G, [N|Ns], [N|R]) :- nieWchodzi(G, N), potencjalneVS(G, Ns, R) .
potencjalneVS(G, [_|Ns], R) :- potencjalneVS(G, Ns, R) .

potencjalneVE([], []) .
potencjalneVE([N|Ns], [N|R]) :- nieWychodzi(N), potencjalneVE(Ns, R) .
potencjalneVE([_|Ns], R) :- potencjalneVE(Ns, R) .

znajdzNode([N|_], Term, N) :- N = node(Term, _, _) .
znajdzNode([_|Ns], Term, N) :- znajdzNode(Ns, Term, N) .


osiagalny(G, X, Y) :- osiagalny(G, X, Y, []) .
osiagalny(_, S, S, _) . 
osiagalny(G, node(St, [Neigh|Neighs], Fs), E, Odw) :- 
    znajdzNode(G, Neigh, NeighNode),
    (   member(Neigh, Odw) ->  (   osiagalny(G, node(St, Neighs, Fs), E, Odw)   ) 
                                ; (   osiagalny(G, node(St, Neighs, Fs), E, Odw) 
                                        ; osiagalny(G, NeighNode, E, [Neigh|Odw])) ) .

% paryBezPowtorzen(V, V, X), length(X, Binom(|V|, 2))
% same_length(arg1, arg2)
paryBezPowtorzen([], [], []) .
paryBezPowtorzen([X|XS], [_|YS], R) :- zip(X, YS, R1), paryBezPowtorzen(XS, YS, R2), append(R1, R2, R) .

sprawdzPary(_, []) .
sprawdzPary(G, [P|Pary]) :-
    [V1, V2] = P,
    (   osiagalny(G, V1, V2) ; osiagalny(G, V2, V1) ),
    sprawdzPary(G, Pary) .

ulozony(G) :- paryBezPowtorzen(G, G, R), sprawdzPary(G, R) .
   

jestDobrzeUlozony(G) :- 
    length(G, X), X >= 2,
    maksymalnie3F(G),
    potencjalneVS(G, [_|[]]),
    potencjalneVE(G, [_|[]]),
    ulozony(G) .


paryEF1(_, _, E, node(_, Es, Fs), R) :- 
    % write('lol'), write(Es), write(Fs), nl,
    delete(Fs, E, NotEndFs), 
    wszystkieKombinacje(Es, NotEndFs, R) .
    
sprawdzIstnienie1(G, V1, W1) :-
    znajdzNode(G, V1,  node(_, _, V1Fs)),
    znajdzNode(G, W1,  node(_, W1Es, _)),
    PotencjalneU1 = W1Es,
    PotencjalneU2 = V1Fs,
    % write(PotencjalneU1), nl,
    % write(PotencjalneU2), nl,
    intersection(PotencjalneU1, PotencjalneU2, X),
    % write(X), nl,
    X \= [] .

paryEF2(G, S, _, V, R) :- 
    V = node(_, _, Fs),
    delete(Fs, S, NoStartFs),
    wchodzace(G, V, DoV),
    wszystkieKombinacje(DoV, NoStartFs, R) .

sprawdzIstnienie2(G, V1, W1) :-
    znajdzNode(G, V1,  node(_, _, V1Fs)),
    PotencjalneU1 = V1Fs,
    wchodzace(G, W1, PotencjalneU2),
    intersection(PotencjalneU1, PotencjalneU2, X),
    X \= [] .

znajdzDobraPare1(_, _, []) .
znajdzDobraPare1(G, Node, [[E, F]|Pary]) :- sprawdzIstnienie1(G, E, F), znajdzDobraPare1(G, Node, Pary) .

znajdzDobraPare2(_, _, []) .
znajdzDobraPare2(G, Node, [[E, F]|Pary]) :- sprawdzIstnienie2(G, E, F), znajdzDobraPare2(G, Node, Pary) .


jestDobrzePermutujacy(G) :- 
    jestDobrzeUlozony(G), 
    potencjalneVS(G, [S]),
    potencjalneVE(G, [E]),
    jestDobrzePermutujacy(G, G, S, E) .
jestDobrzePermutujacy(_, [], _, _) .
jestDobrzePermutujacy(G, [N|Ns], S, E) :-
    jestDobrzePermutujacy(G, Ns, S, E),
    paryEF1(G, S, E, N, Pary1),
    paryEF2(G, S, E, N, Pary2),
    znajdzDobraPare1(G, N, Pary1),
    znajdzDobraPare2(G, N, Pary2) .

jestSucc(_, [], _) .
jestSucc(G, [Poprz|Ps], [Nast|Ns]) :- 
    jestSucc(G, Ps, Ns),
    znajdzNode(G, Poprz, node(_, Es, _)),
    member(Nast, Es) .


