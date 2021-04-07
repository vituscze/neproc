import Data.Complex

-- 12. cvičení 2017-05-09
--
-- Minule jsme se podívali na typovou třídu Functor.
--
-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b
--
-- Dva typické příklady typových konstruktorů, které jsou součástí třídy
-- Functor, jsou: Maybe a []
--
-- instance Functor Maybe where
--     fmap _ Nothing  = Nothing
--     fmap f (Just a) = Just (f a)
--
-- instance Functor [] where
--     fmap = map
--
-- Dalším krokem je typová třída Monad, což je mnohem silnější verze třídy
-- Functor. Nejdřív ale začneme konkrétním příkladem.
--
-- > :t lookup
-- lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
--
-- Pokud chceme najít hodnotu pro daný klíč ve dvou (nebo více) asociativních
-- seznamech najedou, můžeme použít např.

lookup2, lookup2' :: (Eq a) => a -> [(a, b)] -> [(a, c)] -> Maybe (b, c)
lookup2 a ab ac = case lookup a ab of
    Nothing -> Nothing
    Just b  -> case lookup a ac of
        Nothing -> Nothing
        Just c  -> Just (b, c)

-- Pro tři a více seznamů už by tahle funkce začala být nepřehledná, přitom
-- ale neděláme nic zajímavého. Naštěstí můžeme použít case i takhle:

lookup2' a ab ac = case (lookup a ab, lookup a ac) of
    (Just b, Just c) -> Just (b, c)
    _                -> Nothing

-- Často se náme ale může stát, že klíč, který budeme hledat v druhém (třetím,
-- atp.) seznamu závisí na nalezené hodnotě z prvního seznamu.

lookupChain :: (Eq a, Eq b) => a -> [(a, b)] -> [(b, c)] -> Maybe (b, c)
lookupChain a ab bc = case lookup a ab of
    Nothing -> Nothing
    Just b  -> case lookup b bc of
        Nothing -> Nothing
        Just c  -> Just (b, c)

-- Tady už nám předchozí trik nepomůže a musíme použít "case kaskádu".
--
-- Musí to jít lépe! Všimněte si, co se v téhle funkci děje: Nejprve se podíváme
-- na první lookup. Pokud neuspěl, tak končíme. Pokud uspěl, tak vezeme
-- nalezenou hodnotu a použijeme ji v další části kódu.
--
-- Tohle jsme schopni s použitím funkcí vyššího řádu jednoduše reprezentovat.

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing  _ = Nothing  -- První akce neuspěla, konec.
andThen (Just a) f = f a      -- První akce uspěla, vezmeme hodnotu a předáme
                              -- ji zbytku (zde v podobě funkce).

lookupChain' :: (Eq a, Eq b) => a -> [(a, b)] -> [(b, c)] -> Maybe c
lookupChain' a ab bc =
    lookup a ab `andThen` \b ->
    lookup b bc

-- Jsme na dobré cestě, ale funkce lookupChain' nám vrátí pouze druhý nalezený
-- prvek, zatímco originální implementace vracela oba nalezené prvky. To lze
-- ale snadno opravit, buď za použití funkce fmap, nebo dalším použitím andThen.

lookupChain'' :: (Eq a, Eq b) => a -> [(a, b)] -> [(b, c)] -> Maybe (b, c)
lookupChain'' a ab bc =
    lookup a ab `andThen` \b  ->
    lookup b bc `andThen` \c ->
    Just (b, c)

-- resp. (pozor, hodně apostrofů)

lookupChain''' :: (Eq a, Eq b) => a -> [(a, b)] -> [(b, c)] -> Maybe (b, c)
lookupChain''' a ab bc =
    lookup a ab `andThen` \b ->
    fmap (\c -> (b, c)) (lookup b bc)

-- Další důležitý poznatek je ten, že Just je v určitém smyslu neutrální
-- operace.
--
-- Just x `andThen` f     ==  f x
-- v      `andThen` Just  ==  v

done :: a -> Maybe a
done = Just

lookupChain3 :: (Eq a, Eq b, Eq c)
             => a                                 -- První klíč
             -> [(a, b)] -> [(b, c)] -> [(c, d)]  -- Asociativní seznamy
             -> Maybe (b, c, d)
lookupChain3 a ab bc cd =
    lookup a ab `andThen` \b ->
    lookup b bc `andThen` \c ->
    lookup c cd `andThen` \d ->
    done (b, c, d)

-- Na Maybe a se můžeme dívat jako na výpočet, který buď vyprodukuje hodnotu
-- typu a, nebo skončí neúspěchem. andThen nám potom umožňuje skládat tyto
-- "výpočty s neúspěchem".
--
-- Můžeme najít podobnou operaci pro jiné typy?

sqrt' :: Complex Double -> [Complex Double]
sqrt' z = [mkPolar r' theta', mkPolar (-r') theta']
  where
    (r, theta) = polar z

    theta' = theta / 2
    r'     = sqrt r

-- Pokud bychom chtěli spočítat 4. odmocninu, tak sqrt' můžeme aplikovat
-- dvakrát. To se dá udělat např. takhle:

root4 :: Complex Double -> [Complex Double]
root4 x = concat (map sqrt' (sqrt' x))

-- Nejdříve namapujeme, tak pomocí funkce concat zploštíme dvojitý seznam.

mystery x f = concat $ map f x

-- > :t mystery
-- mystery :: [a] -> (a -> [b]) -> [b]
--
-- Tohle je povědomé! Porovnejte typ této funkce s typem funkce andThen.

andThenL :: [a] -> (a -> [b]) -> [b]
andThenL = mystery

root8, root8' :: Complex Double -> [Complex Double]
root8 x =
    sqrt' x `andThenL` \y ->
    sqrt' y `andThenL` \z ->
    sqrt' z

-- Nebo jednoduše

root8' x = sqrt' x `andThenL` sqrt' `andThenL` sqrt'

-- Na seznam se obvykle díváme pouze jako na strukturu obsahující data. Ale
-- podobně jako na Maybe a můžeme nahlížet jako na výpočet, který může skončit
-- neúspěchem, tak [a] je výpočet, který může nabývat více hodnot. Trochu
-- jako nedeterminismus v Prologu.
--
-- Zbývá nám najít ekvivalent pro operaci done. Musí být neutrální vzhledem
-- k andThenL, což nám dává jen jednu možnost.

doneL :: a -> [a]
doneL a = [a]

times :: [a] -> [b] -> [(a, b)]
times as bs =
    as `andThenL` \a ->
    bs `andThenL` \b ->
    doneL (a, b)

-- Máme dva konkrétní příklady, teď tento koncept "spojování výpočtů" můžeme
-- generalizovat.
--
-- class Applicative m => Monad m where
--     return :: a -> m a                  -- done, doneL
--     (>>=)  :: m a -> (a -> m b) -> m b  -- andThen, andThenL
--
-- Pozn. return má možná trochu nešťastné jméno. Narozdíl od return
-- v ostatních jazycích nekončí výpočet. Místo return můžeme používat
-- funkci pure (z typové třídy Applicative), která dělá to samé.

weird :: Maybe Int
weird =
    return 1 >>= \_ ->
    Nothing

-- > weird == Nothing
-- True

times' :: [a] -> [b] -> [(a, b)]
times' as bs =
    as >>= \a ->
    bs >>= \b ->
    return (a, b)

lookupChain3' :: (Eq a, Eq b, Eq c)
              => a
              -> [(a, b)] -> [(b, c)] -> [(c, d)]
              -> Maybe (b, c, d)
lookupChain3' a ab bc cd =
    lookup a ab >>= \b ->
    lookup b bc >>= \c ->
    lookup c cd >>= \d ->
    return (b, c, d)

-- Použití >>= je v Haskellu tak časté, že pro něj existuje syntaktická
-- zkratka, tzv. do notace.
--
-- Na zarovnání záleží, jedině tak pozná Haskell, jestli začínáme novou řádku,
-- pokračujeme předchozí nebo končíme do blok.

times'' :: [a] -> [b] -> [(a, b)]
times'' as bs = do
    a <- as
    b <- bs
    return (a, b)

failIf :: Bool -> Maybe ()
failIf True = Nothing
failIf _    = Just ()

weird' :: (Eq a) => (b -> Bool) -> a -> [(a, b)] -> Maybe b
weird' cond a ab = do
    b <- lookup a ab
    failIf (cond b)   -- Pokud nás hodnota z failIf nezajímá, můžeme
                      -- šipku vynechat.
    return b

-- Kromě toho můžeme ještě používat zkrácenou formu let.
--
-- do x <- y
--    let z = x * x
--    ...
--
-- return na konci do-bloku není nutný, tak jak jsme viděli např. u funkce
-- root8.

root8'' :: Complex Double -> [Complex Double]
root8'' x = do
    y <- sqrt' x
    z <- sqrt' y
    sqrt' z

-- do notace lze přeložit zpět do obyčejného výrazu:
--
-- do a <- ma    ===  ma >>= \a ->
--    mb              mb
--
-- do ma         ===  ma >>= \_ ->
--    mb              mb
--
-- do let x = y  ===  let x = y
--    ma              in  ma
--
