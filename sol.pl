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
    uzywaIstniejacych(G), 
    unikalne(G, []),
    poprawneF(G) .

% zip(1, [1,2,3], X)
% X = [[1, 1], [1, 2], [1, 3]]
zip(_, [], []) .
zip(El, [X|XS], [[El, X]|R]) :- zip(El, XS, R) .

% wszystkieKombinacje([1,2,3], [4,5], X)
% X = [[[1, 4], [1, 5]], [[2, 4], [2, 5]], [[3, 4], [3, 5]]]
wszystkieKombinacje([], _, []) .
wszystkieKombinacje([El|L1], L2, [Z|R]) :- zip(El, L2, Z), wszystkieKombinacje(L1, L2, R) .

% wchodzi(+G, +Do, -Lista wierzchołków mających E-krawędź ->Do)
wchodzi([], _, []) .
wchodzi([NZ|Ns], NDo, [NZ|R]) :- 
    NZ = node(_, Es, _), 
    NDo = node(Do, _, _), 
    wchodzi(Ns, NDo, R), 
    member(Do, Es) .
wchodzi([_|Ns], NDo, R) :- wchodzi(Ns, NDo, R) .


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
    

osiagalny(G, X, Y) :- osiagalny(G, X, Y, []) .
osiagalny(_, S, S, _) . 
osiagalny(G, node(St, [Neigh|Neighs], Fs), E, Odw) :- 
    znajdzNode(G, Neigh, NeighNode),
    (   member(Neigh, Odw) ->  (   osiagalny(G, node(St, Neighs, Fs), E, Odw)   ) 
                                ; (   osiagalny(G, node(St, Neighs, Fs), E, Odw) 
                                        ; osiagalny(G, NeighNode, E, [Neigh|Odw])) ) .


jestDobrzeUlozony(G) :- 
    length(G, X), X >= 2,
    maksymalnie3F(G),
    potencjalneVS(G, [_|[]]),
    potencjalneVE(G, [_|[]]),
    ulozony(G) .

  
