import qualified Control.Monad as M
import           Data.Char

-- 6. úloha
--
-- 1) Jednoduchý backtrackovací parser lze reprezentovat jako funkci, která
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
-- Just ('b',"c")
--
-- > runParser (satisfy (== 'a') `orElse` satisfy (== 'b')) "c"
-- Nothing
--
--
-- Máme základní parser a výběr mezi dvěma (a více) parsery. Zbývá implementovat
-- spojování parserů.

parserPure :: a -> Parser a
parserPure = undefined

-- 'parserPure a' je parser, který vždy uspěje a vrátí hodnotu 'a'. Vstupní
-- text ponechá beze změny.
--
-- runParser (parserPure 1) "abc" == Just (1,"abc")

parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind = undefined

-- 'parserBind m f' nejprve spustí parser m. Pokud tenhle parser uspěje s hodnotou
-- 'a', tak pustí parser 'f a' na zbylém textu.

instance Functor Parser where
    fmap = M.liftM

instance Applicative Parser where
    pure  = parserPure
    (<*>) = M.ap

instance Monad Parser where
    (>>=) = parserBind

-- Správnost implementace 'satisfy', 'parserPure' a 'parserBind' pomůže
-- ověřit následující definice. Pokud se v GHCi zeptáte na hodnotu 'correct',
-- měli byste dostat zpět 'True'.
correct :: Bool
correct = and
    [ p ""    == Nothing
    , p "a"   == Nothing
    , p " "   == Nothing
    , p " a"  == Nothing
    , p "  "  == Nothing
    , p "aa"  == Nothing
    , p "a "  == Just ("a ", "")
    , p "a b" == Just ("a ", "b")
    ]
  where
    p = runParser $ do
        a <- satisfy (not . isSpace)
        b <- satisfy isSpace
        pure [a,b]

-- 2) V této části nepoužívejte konstruktor 'Parser'. Místo toho parsery
-- sestavte pomocí dříve definovaných funkcí.

string :: String -> Parser String
string = undefined

-- 'string s' je parser, který parsuje přesně řetězec 's'. Parser vrací
-- naparsovaný řetězec.
--
-- > runParser (string "hello") "hello there"
-- Just ("hello"," there")
--
-- > runParser (string "hello") "hell"
-- Nothing

many :: Parser a -> Parser [a]
many = undefined

some :: Parser a -> Parser [a]
some = undefined

-- Funkce 'many' a 'some' dostanou jako vstup parser 'p' a aplikují jej,
-- dokud 'p' neselže. Výsledky všech použití parseru 'p' se nashromáždí
-- do seznamu.
--
-- 'many' zkusí najít 0 a více výskutů. 'some' zkusí najít 1 a více výskytů.
--
-- Hint: Bude se vám hodit funkce 'orElse'.
--
-- Hint: 'some' a 'many' se dají jednoduše definovat mutuální rekurzí,
-- tj. 'some' je definovaný pomocí 'many', 'many' pomocí 'some'.
--
-- > runParser (some $ string "ab") "aabab"
-- Nothing
--
-- > runParser (some $ string "ab") "ababababbb"
-- Just (["ab","ab","ab","ab"],"bb")
--
-- > runParser (many $ string "ab") "aaaaaaa"
-- Just ([],"aaaaaaa")

-- BONUS) Implementuje následující parser. Stejně jako v předchozí části
-- nepoužívejte 'Parser' explicitně.

whitespace :: Parser ()
whitespace = undefined

-- Pokud chcete parsovat nějaký zdrojový kód, velice často potřebujete přeskakovat
-- bílé znaky (mezery, tabulátory, konce řádků). Normálně by tohle šlo vyřešit
-- snadno pomocí:
--
--   spaces = many $ satisfy isSpace
--
-- Funkce 'isSpace' je z modulu Data.Char.
--
-- Ale ve většině zdrojových kódu se také objevují komentáře, které se
-- také musejí přeskočit.
--
-- Definujte tedy parser 'whitespace', který přeskočí všechny bílé znaky
-- (včetně komentářů). Komentáře máme jednodřádkové, začínající řetězcem
-- "//", nebo víceřádkové, začínající "/*" a končící "*/".
--
-- Použijte přitom dříve definované funkce, bude se vám hodit 'string',
-- 'satisfy', 'some' a 'many'.
--
-- > runParser whitespace " x += 2;"
-- Just ((),"x += 2;")
--
-- > runParser whitespace "  // This adds 2 to x\n  x += 2;"
-- Just ((),"x += 2;")
--
-- > runParser whitespace "/*\n  This doesn't work:\n  x *= 2;\n*/\n  x += 2;"
-- Just ((),"x += 2;")
