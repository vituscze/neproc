% 3. domácí úloha
%
% Vyberte si jednu úlohu a vyřešte ji. Pokud odevzdáte obě, druhá se počítá
% jako bonus.
%
% a) Misionáři a lidojedi (prohledávání stavového prostoru)
%
% Na jednom břehu řeky stojí tři misionáři a tři kanibalové, k dispozici mají
% loďku pro dvě osoby. Přepravte je na druhý břeh tak aby:
%   1. v loďce vždy byla alespoň jedna osoba (tj. není možné poslat prázdnou
%      loďku na druhý břeh)
%   2. lidojedi nesnědli misionáře (což se stane, pokud je na jednom břehu
%      alespoň jeden misionář a ostře více lidojedů než misionářů)
%
% Navrhěte řešení zobecněného problému pro M misionářů, L lidojedů a R řek.
% Každá řeka má jednu loďku. Loďku nelze mezi řekami přenášet. Pro jednoduchost
% můžete předpokládat, že současně lze použít nejvýše jedna loďka.
%
% solve(+M, +L, +R, ?Path).
%
% M, L, R jsou vstupní čísla. Path je nalezená cesta (seznam stavů od
% počátečního ke koncovému).
%
% Dejte si pozor na cykly ve stavovém grafu.
%
% b) Regulární výrazy
%
% Implementuje následující predikát:
%
% find(+Pattern, +Text, ?PosStart, ?PosEnd)
%
% Pattern je regulární výraz, viz níže
% Text je seznam atomů, např. [h,e,l,l,o]
% PosStart a PosEnd určují, kde se nachází výskyt nalezeného vzoru.
%
% Regulární výraz je definován induktivně:
%   1. X je regulární výraz, pokud atom(X)
%   2. X je regulární výraz, pokud X je seznam atomů.
%   3. pokud jsou X, Y reg. výrazy, pak je také X .: Y
%   4. pokud jsou X, Y reg. výrazy, pak je také X .+ Y
%   5. pokud je X reg. výraz, pak je také X .*
%
% Sémantika:
%   1. x matchuje pouze znak x
%   2. [x,y,z] matchuje znaky x, y nebo z
%   3. X .: Y matchuje X a potom Y
%   4. X .+ Y matchuje X nebo Y
%   5. X .* matchuje X libovolně mnohokrát
%
% Př.
% find(([a,b] .: c) .*, [a,c,b,c], S, E).
% S = E, E = 0 ; % prádzný řetězec na pozici 0
% S = 0, E = 2 ; % řetězec ac na pozici 0
% S = 0, E = 4 ; % řetězec acbc na pozici 0
% S = E, E = 1 ; % prázdný řetězec na pozici 1
% S = E, E = 2 ; % prázdný řetězec na pozici 2
% S = 2, E = 4 ; % řetězec bc na pozici 2
% S = E, E = 3 ; % prázdný řetězec na pozici 3
% S = E, E = 4 ; % prázdný řetězec na pozici 4
% false.
%
% Dejte si pozor, abyste správně ošetřili vnořené výskyty .*:
% find(a .* .*, [a], S, E).
% S = E, E = 0 ;
% S = 0, E = 1 ;
% S = E, E = 1 ;
% false.

:- op(150, yf,  .*).
:- op(200, xfy, .:).
:- op(250, xfy, .+).
