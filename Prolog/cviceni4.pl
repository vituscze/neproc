% 4. cvičení, 2017-03-14

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

% Přístup k prostřednímu prvku seznamu bez použití aritmetiky.
middle(XS, R) :- middle_(XS, XS, R).

middle_([X|_], [], X).
middle_([X|_], [_], X).
middle_([_|R1], [_,_|R2], X) :-
  middle_(R1, R2, X).

% Binární stromy
% Prolog nemá zabudované nástroje podobně jako pro seznamy, ale stromy
% se dají stavět podobně: strom je buď prádzný (nil) nebo je to uzel s
% dvěma podstromy (t(L, X, R)).
%
%    2
%   / \    =  t(t(nil, 1, nil), 2, t(nil, 3, nil))
%  1   3

% Testovací strom.
getTree(t(t(t(nil, 1, nil), 3, t(nil, 4, nil)), 5, t(nil, 7, t(nil, 8, nil)))).

% Průchod stromem
%
% Pořadí klauzulí určuje, jestli se jedná o preorder, inorder nebo postorder
% průchod. V tomhle případě je to inorder.
elemTree(X, t(L, _, _)) :-
  elemTree(X, L).
elemTree(X, t(_, X, _)).
elemTree(X, t(_, _, R)) :-
  elemTree(X, R).

% Hloubka stromu.
depth(nil, 0).
depth(t(L, _, R), H) :-
  depth(L, HL),
  depth(R, HR),
  H is 1 + max(HL, HR).

% Velikost stromu.
%
% Taky by se dalo implementovat pomocí akumulátoru.
size(nil, 0).
size(t(L, _, R), S) :-
  size(L, SL),
  size(R, SR),
  S is 1 + SL + SR.

% Převod na seznam.
toList(T, L) :- toList_(T, [], L).

toList_(nil, A, A).
toList_(t(L, X, R), A, A4) :-
  toList_(R, A, A2),
  A3 = [X|A2],
  toList_(L, A3, A4).

% Setřídený seznam, bez opakování.
isSorted([]).
isSorted([_]).
isSorted([X,Y|R]) :-
  X < Y,
  isSorted([Y|R]).

% Binární vyhledávací stromy.
isBST(T) :-
  toList(T, L),
  isSorted(L).

% elemTree prohledává celý strom, pro BVS stačí prohledat pouze jednu
% větev.

bstInsert(X, nil, t(nil, X, nil)).
bstInsert(X, t(L, X, R), t(L, X, R)).
bstInsert(X, t(L, Y, R), t(New, Y, R)) :-
  X < Y,
  bstInsert(X, L, New).
bstInsert(X, t(L, Y, R), t(L, Y, New)) :-
  X > Y,
  bstInsert(X, R, New).

% Řez
% Občas by se nám hodilo kontroloval backtracking, např. aby Prolog zbytečně
% neprohledával výpočty, o kterých víme, že nevedou k výsledku.

max(A, B, B) :- A < B.
max(A, B, A) :- A >= B.

% Dotaz max(1,2,R) použije první klauzuli, ale při odmítnutí (backtracking)
% zkusí i druhou klauzuli, která zjevně nemůže uspět.
%
% Řez je speciální cíl !, který vždy uspěje. Pokud se ale přes něj pokusíme
% backtrackovat, okamžitě způsobí selhání splňovaného cíle.
%
% Jinými slovy, řez prořezává vyhledávací strom: všechny neprozkoumané větve
% začínající právně splňovaným cílem a končící ! jsou odstraněny.

max2(A, B, B) :- A < B, !.
max2(A, B, A) :- A >= B.

% Pozor:

max3(A, B, B) :- A < B, !.
max3(A, _, A).

% Co se stane pokud položíme dotaz max3(1,2,1)?

p(1).
p(X) :- r(X).
p(6).

r(2).
r(X) :- s(X), !.
r(5).

s(3).
s(4).

% Jak opravit deleteAll?
deleteAll(X, [X|XS], R) :- % Pokud se podaří X unifikovat s hlavou seznamu
  !,                       % odřízneme ostatní dvě klauzule.
  deleteAll(X, XS, R).
deleteAll(X, [Y|YS], [Y|R]) :-
  deleteAll(X, YS, R).
deleteAll(_, [], []).

% Všimněte si, že řez v predikátu max2/3 nemění výsledky, pouze zefektivňuje
% program. Zatímco v predikátu deleteAll/3 je řez nepostradatelný, jinak
% tento predikát dává nesprávné výsledky.
%
% Řezy prvního typu jsou tzv. zelené řezy. Pokud takovýho řez odstraníme,
% program bude pořád produkovat stejné výsledky.
%
% Řezy druhého typu jsou tzv. červené řezy. Ty naopak mění význam programu.

% Negace pomocí řezu.
% not(X) :- X, !, fail.
% not(X).
%
% Lze taky použít jako operátor \+
%
% POZOR: Nejedná se negaci, tak jak ji znáte z logiky. Pokud not(X) uspěje,
% Prologu se nepodařilo dokázat X. To je něco jiného, než dokázat negaci X.

clovek(adam).
clovek(lenka).

muz(adam).
zena(lenka).

% ?- \+ muz(lenka).
% true.
%
% Navíc negace nefunguje příliš dobře pokud se v dotazu nacházejí volné
% proměnné.
%
% ?- \+ muz(X).
% false.
%
% Otázka: proč dostaneme false?
%
% ?- clovek(X), \+ muz(X).
% X = lenka.

% Řez je v Prologu něco jako goto. Pokud to jde, snažte se mu vyvarovat pomocí
% konstruktů jako je not, \+, \=, \==, once, -> atp.
%
% once najde pouze první řešení
% once(P) :- P, !.

max4(X, Y, R) :-
  ( X < Y -> R = Y
  ; R = X
  ).
