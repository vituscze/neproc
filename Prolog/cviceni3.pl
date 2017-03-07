% 3. cvičení, 2017-03-07

% Zmínit: Proč fail/false nefunguje pro ošetření dělení nulou?

% Pro zopakování:
% Seznam je buď prázdný ([]), nebo je tvořen hlavou H a zbytkem seznamu T
% ([H|T]).
%
% Syntaktické zkratky:
% [A|[B|[C|[]]]] = [A,B|[C|[]]] = [A,B,C|[]] = [A,B,C]

elem(X, [X|_]).
elem(X, [_|T]) :- elem(X, T).
% Standardní knihovna: member
%
% Často se používá jako member(-, +)
% member(X, [a,b,c]).
% X = a;
% X = b;
% X = c;
% false.

addFront(X, XS, [X|XS]).
% addFront(a, [b,c], R).
% R = [a,b,c].
%
% addFront(X, XS, [a,b,c]).
% X = a,
% XS = [b,c].

addBack(X, [], [X]).
addBack(X, [Y|YS], [Y|R]) :- addBack(X, YS, R).
% addBack([a,b], c, R).
% R = [a,b,c].
%
% addBack(XS, X, [a,b,c]).
% XS = [a,b],
% X = c.

% delete(X, S, R) smaže jeden výskyt X v seznamu S.
%
% delete(a, [a,b,c,a,d,e], R).
% R = [b,c,a,d,e];
% R = [a,b,c,d,e];
% false.
%
% delete(d, [a,b,c], R).
% false.
%
% Lze použít i obráceným směrem:
% delete(a, R, [b,c,d]).
% R = [a,b,c,d];
% R = [b,a,c,d];
% R = [b,c,a,d];
% R = [b,c,d,a];
% false.
%
% Standardní knihovna: select
delete(X, [X|R], R).
delete(X, [Y|YS], [Y|R]) :-
  delete(X, YS, R).

% deleteAll(X, S, R) smaže všechny výskyty X ze seznamu S.
% deleteAll(a, [a,b,c,a,d], R).
% R = [b,c,d];
% R = [b,c,a,d]; % Problém!
% R = [a,b,c,d];
% R = [a,b,c,a,d];
% false.
deleteAll(X, [X|XS], R) :-
  deleteAll(X, XS, R).
deleteAll(X, [Y|YS], [Y|R]) :-
  deleteAll(X, YS, R).
deleteAll(_, [], []).

% Spojování seznamů.
%
% Standardní knihovna: append
app([], YS, YS).
app([X|XS], YS, [X|R]) :-
  app(XS, YS, R).

% Aritmetika v Prologu
%
% 1 + 2 = 3.
% false. % ???
%
% Termy 1 + 2 a 3 nejsou shodné. 1 + 2 nejdříve musíme vyhodnotit, než
% se pokusíme o unifikaci.

% operátor is/2
% 3 is 1 + 2.
% true.
%
% X is 1 + 2.
% X = 3.
%
% 1 + 2 is X.
% ERROR: Arguments are not sufficiently instantiated
%
% V X is Y, Y musí být aritmetický výraz, nemůže obsahovat volné proměnné.
% Y = 1 + 2, X is Y. % OK

len([], 0).
len([_|T], R) :-
  len(T, N),
  R is N + 1.

% Pro porovnávání máme:
% X =:= Y
% X =\= Y
% X < Y
% X =< Y
% X >= Y
% X > Y
%
% X a Y musí být aritmetické výrazy bez volných proměnných.

% Permutace pomocí select/3.
perm([], []).
perm([X|XS], R) :-
  perm(XS, PXS),
  select(X, R, PXS).

split([X|XS], X, XS).
split([_|XS], X, R) :-
  split(XS, X, R).

% split(A, B, C) vs append(_, [B|C], A)

% Kombinace pomocí split/3.
comb(0, _, []).
comb(N, XS, [X|R]) :-
  N > 0,
  N2 is N - 1,
  split(XS, X, Rem),
  comb(N2, Rem, R).

% Otáčení seznamu.
% Problém: kvadratická časová složitost. Přidání na konec seznamu je
% O(n) operace, dohromady
% n + (n - 1) + (n - 1) + ... + 2 + 1 = n(n + 1)/2 = O(n^2)
revBad([], []).
revBad([X|XS], R2) :-
  revBad(XS, R),
  append(R, [X], R2).

% Řešení: použijeme pomocnou proměnnou, ve které postupně konstruujeme výsledek.
% Tato proměnná je tzv. akumulátor.
rev(XS, R) :- rev_(XS, [], R).

rev_([], A, A).
rev_([X|XS], A, R) :-
  rev_(XS, [X|A], R).

% TCO - Tail Call Optimization
% Pokud je poslední podcíl rekurzivní výskyt definovaného predikátu, předchozí
% podcíle jsou deterministické a neexistuje další nevyzkoušená větev výpočtu,
% můžeme rekurzi implementovat efektivně (nemusí se vytvářet stack frame).

lenTCO(X, R) :- lenTCO_(X, 0, R).

lenTCO_([], A, A).
lenTCO_([_|T], A, R) :- A2 is A + 1, lenTCO_(T, A2, R).

middle(XS, R) :- middle_(XS, XS, R).

middle_([X|_], [], X).
middle_([X|_], [_], X).
middle_([_|R1], [_,_|R2], X) :-
  middle_(R1, R2, X).