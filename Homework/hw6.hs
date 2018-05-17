import Data.Char

-- 6. úloha
--
-- DEADLINE: 2017-05-23
--
-- 1) Definujte následující funkce:

mapMonad :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapMonad = undefined

-- 'mapMonad f l' aplikuje funkci 'f' na každý prvek seznamu 'l', provede
-- výslednou akci (typu 'm b') a všechny výsledky (typu 'b') uloží do seznamu.
--
-- > let assoc = [(1, "hi"), (2, "hello")]
--
-- > mapMonad (\k -> lookup k assoc) [1,2]
-- Just ["hi","hello"]
--
-- > mapMonad (\k -> lookup k assoc) [1,2,3]
-- Nothing
--
-- > mapMonad (\x -> [-x, x]) [1..3]
-- [[-1,-2,-3],[-1,-2,3],[-1,2,-3],[-1,2,3],[1,-2,-3],[1,-2,3],[1,2,-3],[1,2,3]]

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM = undefined

-- 'liftM f ma' provede akci 'ma' a na výsledek aplikuje funkci 'f'. Je to
-- vlastně 'fmap', jenom implementovaný pomocí monadických operací (>>=,
-- return), resp. do notace.
--
-- > liftM (^2) Nothing
-- Nothing
--
-- > liftM (^2) (Just 4)
-- Just 16
--
-- > liftM (^2) [1..3]
-- [1,4,9]

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap = undefined

-- 'ap mf ma' nejprve provede akci 'mf' (a získá funkci 'f' typu 'a -> b'),
-- poté provede akci 'ma' (a získá hodnotu 'a' typu 'a') a nakonec vrátí
-- výsledek aplikace 'f a'.
--
-- > ap (Just (+1)) (Just 2)
-- Just 3
--
-- > liftM (,) "abc" `ap` [1,2]
-- [('a',1),('a',2),('b',1),('b',2),('c',1),('c',2)]

join :: (Monad m) => m (m a) -> m a
join = undefined

-- 'join mm' nejprve provede akci 'mm' (a získá další akci 'm' typu 'm a'),
-- kterou pak následně provede.
--
-- > join [[1,2,3],[4],[5,6]]
-- [1,2,3,4,5,6]
--
-- > join Nothing
-- Nothing
--
-- > join $ Just Nothing
-- Nothing
--
-- > join $ Just (Just 2)
-- Just 2
--
--
-- 2) Jednoduchý backtrackovací parser lze reprezentovat jako funkci, která
-- na vstup dostane text, který cheme parsovat, a vrátí zpět buď 'Nothing'
-- (pokud parser neuspěl) nebo 'Just (a,s)', kde 'a' je naparsovaná hodnota
-- a 's' je zbytek vstupu (tj. to, co parser nezkonzumoval).
--
-- Konkrétně budeme používat následující typ:

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Definujte:

satisfy :: (Char -> Bool) -> Parser Char
satisfy = undefined

-- 'satisfy p' je základní parser. Funguje takto: podívá se na vstupní text,
-- pokud je text prázdný, tak selže (tj. vrátí 'Nothing'), jinak se podívá na
-- první znak. Pokud pro tento znak platí podmínka daná funkcí 'p', tak uspěje,
-- vrátí zpět rozpoznaný znak a zbytek textu. V opačném případě selže.
--
-- > runParser (satisfy (== 'x')) "ab"
-- Nothing
--
-- > runParser (satisfy (== 'x')) ""
-- Nothing
--
-- > runParser (satisfy (== 'x')) "xabc"
-- Just ('x',"abc")

failure :: Parser a
failure = undefined

-- 'failure' je parser, který vždy selže.
--
-- > runParser failure "abc"
-- Nothing

orElse :: Parser a -> Parser a -> Parser a
orElse = undefined

-- 'orElse p1 p2' (doporučuji používat jako 'p1 `orElse` p2') je parser, který
-- nejprve zkusí provést 'p1'. Pokud 'p1' uspěje s hodnotou 'a', vrátí zpátky
-- 'a' a skončí. Pokud 'p1' neuspěje, zkusí parser 'p2'.
--
-- > runParser (satisfy (== 'a') `orElse` satisfy (== 'b')) "abc"
-- Just ('a',"bc")
--
-- > runParser (satisfy (== 'a') `orElse` satisfy (== 'b')) "bc"
-- Just ('b', "c")
--
-- > runParser (satisfy (== 'a') `orElse` satisfy (== 'b')) "c"
-- Nothing
--
--
-- Máme základní parser a výběr mezi dvěma (a více) parsery. Zbývá implementovat
-- spojování parserů. To zařídí instance typové třídy 'Monad'. Deklarace je
-- připravná, stačí jen doplnit definice 'return' a '(>>=)'.
--
-- Pro definici '(>>=)' doporučuji:
--
--   Parser m >>= f = ...

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Monad Parser where
    return = undefined
    (>>=)  = undefined

-- 'return a' je parser, který vždy uspěje s hodnotou 'a' a vstupní text
-- ponechá beze změny.
--
-- > runParser (return True) "abc"
-- Just (True,"abc")
--
-- 'm >>= f' je parser, který nejprve provede 'm' (a získá hodnotu 'a'
-- typu 'a'), potom vezme funkci 'f', aplikuje ji na 'a' a dostane parser typu
-- 'Parser b', který pak provede.

parseTwo :: Parser String
parseTwo = do
    a <- satisfy (not . isSpace)
    b <- satisfy (not . isSpace)
    return [a,b]

-- > runParser parseTwo "a c"
-- Nothing
--
-- > runParser parseTwo "abc"
-- Just ("ab","c")
--
-- BONUS) (za 10 bodů) Implementuje následující:

string :: String -> Parser ()
string = undefined

-- 'string s' je parser, který parsuje přesně řetězec 's'. Samotný parser
-- v tomto případě nic nevrací, protože by to bylo zbytečné (dostali bychom
-- stejný řetězec jako na vstupu).
--
-- > runParser (string "hello") "hello there"
-- Just (()," there")
--
-- > runParser (string "hello") "hell"
-- Nothing

skipMany :: Parser a -> Parser ()
skipMany = undefined

skipSome :: Parser a -> Parser ()
skipSome = undefined

-- Funkce 'skipMany' a 'skipSome' dostanou jako vstup parser 'p' a aplikují jej,
-- dokud 'p' neselže. Výsledky parseru 'p' se jednoduše zahodí.
--
-- 'skipMany' přeskočí 0 nebo více výskytů. 'skipSome' přeskočí 1 nebo více
-- výskytů.
--
-- Hint: Bude se vám hodit funkce 'orElse'.
--
-- Hint: 'skipSome' a 'skipMany' se dají jednoduše definovat mutuální rekurzí,
-- tj. 'skipSome' je definovaný pomocí 'skipMany', 'skipMany' pomocí 'skipSome'.
--
-- > runParser (skipSome $ string "ab") "aabab"
-- Nothing
--
-- > runParser (skipSome $ string "ab") "ababababbb"
-- Just ((),"bb")
--
-- > runParser (skipMany $ string "ab") "aaaaaaa"
-- Just ((),"aaaaaaa")

whitespace :: Parser ()
whitespace = undefined

-- Na závěr nějaký pořádný parser. Pokud chcete parsovat nějaký zdrojový kód,
-- velice často potřebujete přeskakovat bílé znaky (mezery, tabulátory, konce
-- řádků). Normálně by tohle šlo vyřešit snadno pomocí:
--
--   skipSpaces = skipMany $ satisfy isSpace
--
-- Funkce isSpace je z modulu Data.Char.
--
-- Ale ve většině zdrojových kódu se také objevují komentáře, které se
-- také musejí přeskočit.
--
-- Definujte tedy parser 'whitespace', který přeskočí všechny bílé znaky
-- (včetně komentářů). Komentáře máme jednodřádkové, začínající řetězcem
-- "//", nebo víceřádkové, začínající "/*" a končící "*/".
--
-- Použijte přitom dříve definované funkce, bude se vám hodit 'string',
-- 'satisfy', 'skipSome' a 'skipMany'.
--
-- > runParser whitespace " x += 2;"
-- Just ((),"x += 2;")
--
-- > runParser whitespace "  // This adds 2 to x\n  x += 2;"
-- Just ((),"x += 2;")
--
-- > runParser whitespace "/*\n  This doesn't work:\n  x *= 2;\n*/\n  x += 2;"
-- Just ((),"x += 2;")
