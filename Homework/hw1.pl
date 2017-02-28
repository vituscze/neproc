% 1. domácí úloha
%
% a) Implementujte celočíselné dělení se zbytkem na unárně reprezentovaných
% číslech.
% div(+Delenec, +Delitel, ?Podil, ?Zbytek)
%
% div(s(s(s(0))), s(s(0)), D, R).
% D = s(0),
% R = s(0).
%
% div(s(s(s(s(0)))), s(s(0)), D, R).
% D = s(s(0)),
% R = 0.
%
% b) Implementujte predikát, který spočte n-té Fibonacciho číslo lépe než
% v exponenciálním čase (ideálně pouze lineárně mnoho sčítání).
%
% fib(+N, ?Vysledek)
%
% Nápověda: Zkuste nejdřív implementovat obecnější predikát, kde si můžete
% zvolit počáteční čísla.
%
% F_0 = 4
% F_1 = 5
% F_2 = 4 + 5 = 9
% F_3 = 5 + 9 = 14
%
% generalizedFib(3, 4, 5, R).
% R = 14.
%
%
% c) (BONUSOVÁ ÚLOHA) Implementuje predikát pro sčítání dvou binárních čísel.
%
% Můžete použít např. následující reprezentaci:
%
% 13[dec] = 1101[bin] = b(1, b(0, b(1, b(1, e))))

toNat(N, R) :-
  integer(N),
  toNat_(N, R).

toNat_(N, R) :- N > 0 ->
  (N2 is N - 1, toNat_(N2, R2), R = s(R2));
  R = 0.

fromNat(0, 0).
fromNat(s(N), R) :-
  fromNat(N, R2),
  R is R2 + 1.

nat(0).
nat(s(X)) :- nat(X).

add(0, Y, Y) :- nat(Y).
add(s(X), Y, s(Z)) :-
  add(X, Y, Z).

less(0, s(Y)) :- nat(Y).
less(s(X), s(Y)) :- less(X, Y).
