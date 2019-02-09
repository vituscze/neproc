% 6. cvičení, 2017-03-28

:- op(550, xfx, ekv).
:- op(500, xfy, imp).
:- op(450, xfy, or).
:- op(400, xfy, and).
:- op(350,  fx, non).

correct(X) :- ground(X), correct_(X).

correct_(X) :- atom(X).
correct_(F ekv G) :- correct_(F), correct_(G).
correct_(F imp G) :- correct_(F), correct_(G).
correct_(F or  G) :- correct_(F), correct_(G).
correct_(F and G) :- correct_(F), correct_(G).
correct_(  non G) :-              correct_(G).

sat(F) :-
  correct(F),
  vars(F, Vars),
  genModel(Vars, Model),
  eval(F, Model, true).

mergeU(XS, [], XS) :- !.
mergeU([], YS, YS) :- !.
mergeU([X|XS], [Y|YS], R) :-
  ( X @< Y -> mergeU(XS, [Y|YS], S), R = [X|S]
  ; X == Y -> mergeU(XS, YS, S), R = [X|S]
  ; mergeU([X|XS], YS, S), R = [Y|S]
  ).


vars(X, [X]) :- atom(X).
vars(non F, R) :- vars(F, R).
vars(F, RR) :-
  F =.. [H, L, R],
  member(H, [ekv, imp, or, and]),
  vars(L, R1),
  vars(R, R2),
  mergeU(R1, R2, RR).

% Operátor =..
%
% Skládání/rozebírání termů. Lze použít oběma směry.
%
% ?- X =.. [f,5,g(x)].
% X = f(5,g(x)).
%
% ?- 1+2*3 =.. X.
% X = [+,1,2*3].
%
% Pozor!
% F =.. [_,x,y].
% ERROR: ...
%
% Další specifičtější operace:
% functor(Term, Func, Arity)
% arg(N,Term,Arg)

% Doplňte definice následujících predikátů.
genModel(Vars, Model).
eval(F, Model, Value).

% Jak dostat všechny výsledky do seznamu?
%
% bagof, setof, findall
%
% bagof(Rep, Goal, List).
% Rep popisuje, jak chceme výsledky uložit.
% Goal je cíl, který se snažíme splnit.
% List je seznam výsledků.

pairs(X, Y, P) :- bagof(p(A, B), (member(A, X), member(B, Y)), P).

% Co když X = []? Cíl pak nelze splnit a dotaz selže.
%
% setof(Rep, Goal, List).
% Jako bagof, jen setříděné a bez duplicit.
%
% findall(Rep, Goal, List).
% Vrací prázdný seznam v případě, kdy Goal nelze splnit.

% Jiné použití unifikace - rozdílové seznamy a neúplné datové struktury.
%
% [1,2,3,4|X]
% X je samozřejmě zbytek seznamu. Když pak za X dosadíme další seznam, provedeme
% spojení seznamů. Problém: musíme projít celý seznam, než se dostaneme
% k proměnné X, tj. žádná výhoda oproti append/3.
%
% Co kdybychom X zpřístupnili zvenku?
%
% [1,2,3,4|X]-X

finalize(X-Y, Z, X) :- Y = Z.

% ?- finalize([1,2,3,4|X]-X, [5,6,7], R).
% X = ...,
% R = [1,2,3,4,5,6,7].

fromList(L, DL-X) :- fromList(L, X, DL).

fromList([], X, X).
fromList([Y|YS], X, [Y|DL]) :- fromList(YS, X, DL).

toList(L-[], L).

% Spojování seznamů v konstantním čase!
appDL(X-Y, Y-Z, X-Z).

% Ne všechno je ale super.
f(XS-X, YS-Y, R) :-
  X = [4,5,6],
  Y = [a,b,c],
  append(XS, YS, R).

g(XS, YS, R) :-
  append(XS, [4,5,6], XS1),
  append(YS, [a,b,c], YS1),
  append(XS1, YS1, R).

% ?- X = [1|Y]-Y, f(X, X, R).
% false.
%
% ?- X = [1], g(X, X, R).
% R = ...
%
% Jinými slovy: rozdílový seznam lze použít nejvýše jednou.

line(0, []).
line(N, [_|R]) :-
  N > 0,
  N2 is N - 1,
  line(N2, R).

matrix(_, 0, []).
matrix(N, M, [R|RS]) :-
  M > 0,
  M2 is M - 1,
  line(N, R),
  matrix(N, M2, RS).

m2l([], []).
m2l([X|XS], R) :-
  m2l(XS, R1),
  append(X, R1, R).

% Naplníme prvky matice pomocí zploštěného seznamu.
% ?- matrix(2,3,R), m2l(R,[a,b,c,d,e,f]).
% R = [[a,b],[c,d],[e,f]].
