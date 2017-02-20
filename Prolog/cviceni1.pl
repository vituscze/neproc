% 1. cvičení, 2017-02-21

% Program v Prologu je databáze faktů a odvozovacích pravidel popsaných
% pomocí Hornovských klauzulí. Ty v Prologu vypadají takto:
%   H :- B1, B2, ... Bn.
% Tohle odpovídá implikaci B1 & B2 & ... & Bn -> H. B je tělo klauzule,
% H je její hlava. Tělo může být prázdné, pak klauzule jen udává nějaký 
% fakt. 

muz(jirka).
muz(pavel).
muz(adam).

zena(marie).
zena(adela).
zena(jitka).

% Klauzule nemusí být pouze unární, můžeme např. definovat relaci X je
% rodičem Y:

% rodic(X, Y)
% X je rodičem Y.
rodic(jirka, marie).
rodic(jirka, adam).

rodic(jitka, marie).

rodic(pavel, jirka).

% Databázi načteme pomocí File/Consult... Obvyklá instalance také asociuje
% příponu .pl s SWI Prologem, takže je možné soubor přímo otevřít v SWI Prologu.
%
% Po načtení se můžeme zeptat, jestli je nějaký fakt v databázi:
%
% ?- muz(jirka).
% true.
%
% ?- muz(jitka).
% false.
%
% V dotazu se také mohou vyskytovat proměnné, ty poznáme podle toho, že
% začínají podtržítkem nebo velkým písmenem. Prolog se pak pokusí najít
% hodnotu proměnné, tak aby výsledný dotaz platil.
%
% ?- muz(X).
% X = jirka
%
% Nalezenou hodnotu můžeme odmítnout a Prolog se pak pokusí najít další řešení.
% To se dělá pomocí středníku ;
%
% ?- muz(X).
% X = jirka;
% X = pavel;
% X = adam;
% false.
%
% false nám říká, že další řešení už neexistují. 

% Nová fakta můžeme odvodit pomocí pravidel. Pokud víme, že X je žena a Y
% je rodičem X, pak X je dcerou Y. Tohle můžeme v Prologu vyjádřit pomocí
% klauzule s neprádzným tělem:

% dcera(X, Y)
% X je dcerou Y.
dcera(X, Y) :- zena(X), rodic(Y, X).

% syn(X, Y)
% X je synem Y.
syn(X, Y) :- muz(X), rodic(Y, X).

% V těle klauzule se mohou použít i proměnné, které se v hlavě nevyskytují.

% sestra(X, Y)
% X je sestrou Y.
sestra(X, Y) :-
  zena(X),
  rodic(Z, X),
  rodic(Z, Y),
  X \= Y.

% Pozor: pokud vynecháme poslední řádku, tak bude X svou vlastním sestrou
% (např. sestra(marie, marie)). X \= Y říká, že X a Y jsou různé (nelze je
% unifikovat, bude příště). 

% deda(X, Y)
% X je dědou Y.
deda(X, Y) :-
  muz(X),
  rodic(X, Z),
  rodic(Z, Y).
