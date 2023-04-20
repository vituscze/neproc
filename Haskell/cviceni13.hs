module Main where

import System.IO

-- 13. cvičení 2017-05-16
--
-- Minule jsme se podívali na dvě konkrétní monády - Maybe a [].
--
-- instance Monad Maybe where
--     return = Just
--
--     Nothing >>= _ = Nothing
--     Just a  >>= f = f a
--
-- instance Monad [] where
--     return a = [a]
--
--     a >>= f = concat (map f a)
--
--
-- > :i Monad
-- class Applicative m => Monad (m :: * -> *) where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a
--
-- V předpokladech se objevuje typová třída Applicative. Applicative je
-- silnější než Functor, ale slabší než Monad. V novějších verzích GHC je
-- tedy nutné definovat instanci Applicative, ale pokud máme k dispozici
-- instanci Monad, lze tohle udělat snadno.
--
--   instance Applicative Maybe where
--       pure  = return
--       (<*>) = ap
--
--   ap mf ma = do f <- mf; a <- ma; return (f a)
--
-- Vhodnější je implementovat pure přímo v instanci typové třídy Applicative;
-- return už potom dostaneme automaticky.
--
-- Dalším (velice důležitým) příkladem monády je typový konstruktor IO, který
-- reprezentuje výpočty se vstupem a výstupem (obecněji klasické "imperativní"
-- programy). Definice IO je dána konkrétní implementací, takže pro nás je to
-- black box.
--
-- > :t getLine
-- getLine :: IO String
--
-- getLine je "vstupně-výstupní" výpočet, který produkuje hodnotu typu String a
-- to tak, že načte jednu řádku ze standardního vstupu.
--
-- > getLine
-- hi
-- "hi"
--
-- > :t putStrLn
-- putStrLn :: String -> IO ()
--
-- putStrLn je funkce, která vezme String a vrací IO akci, která daný řetězec
-- vypíše na standardní výstup. Výsledná hodnota má typ (), což je datový typ
-- obsahující právě jednu hodnotu. Můžete se na to dívat jako na void
-- z C-čkových jazyků.
--
-- > putStrLn "hello"
-- hello

greet :: IO ()
greet = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn $ "Hello " ++ name ++ "!"

-- Je užitečné si uvědomit, že IO String neobsahuje String. Je to pouze popis
-- akce, která (snad) vyprodukuje String na konci.
--
-- Můžeme to přirovnat k rozdílu mezi dortem a receptem na dort. Recept na dort
-- samozřejmě žádný dort neobsahuje, dort dostaneme teprve až recept úspěšně
-- "provedeme".
--
-- Speciálně tedy nehledejte funkci typu:
--
--   extract :: IO a -> a
--
-- Pokud chceme vytvořit binárku (tj. spustitelný soubor), musíme definovat
-- speciální hodnotu main.

main :: IO ()
main = greet

-- Pak můžeme jednoduše pustit GHC na zdrojový kód.
--
-- (v příkazové řádce)
-- > ghc cviceni13.hs
-- [1 of 1] Compiling Main             ( cviceni13.hs, cviceni13.o )
-- Linking cviceni13.exe ...
-- > cviceni13.exe
-- What is your name?
-- world
-- Hello world!

getTwoLines :: IO String
getTwoLines = do
    l1 <- getLine
    l2 <- getLine
    pure $ l1 ++ "\n" ++ l2

-- Dobrý nápad je strukturovat program tak, aby byl v IO jen kód, který tam
-- musí být.
--
-- Např. je mnohem lepší mít:
--
--   process :: Input -> Output
--
--   readAndProcess :: IO Output
--   readAndProcess = do
--       d <- getData
--       let d' = process d
--       pure d'
--
-- místo jedné akce, která dělá vše najednou.
--
--   process :: IO Output
--   process = do
--       d <- getData
--       ...           -- Spousta kódu, který řeší zpracování d
--       pure d'
--
-- Než se přesuneme k práci se soubory, podívejme se na pár dalších funkcí,
-- které lze použít s monádami.

when :: (Monad m) => Bool -> m () -> m ()
when c m = if c then m else pure ()

unless :: (Monad m) => Bool -> m () -> m ()
unless = when . not

echo :: IO ()
echo = do
    l <- getLine
    putStrLn l
    unless (null l) echo

forever :: (Monad m) => m a -> m b
forever m = do
    _ <- m
    forever m

-- Pro provádění více akcí najednou můžeme použít:
--
-- sequence  :: (Monad m) => [m a] -> m [a]
-- sequence_ :: (Monad m) => [m a] -> m ()

getThreeLines :: IO [String]
getThreeLines = sequence [getLine, getLine, getLine]

-- sequence_ je varianta, která zahazuje výsledky. To se může hodit, pokud
-- nás výsledky akcí nezajímají, jako např. u putStrLn.
--
--   print :: (Show a) => a -> IO ()
--   print = putStrLn . show
--
--
-- Práce se soubory.
--
-- Většina funkcí se nachází v modulu System.IO (viz první řádka tohoto
-- souboru).
--
-- Pro otevírání a zavírání souboru máme následující funkce:
--
--   openFile :: FilePath -> IOMode -> IO Handle
--   hClose   :: Handle -> IO ()
--
-- Pro zápis a čtení pak:
--
--   hPutStrLn :: Handle -> String -> IO ()
--   hGetLine  :: Handle -> IO String
--
-- > :i IOMode
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

writeTmp :: IO ()
writeTmp = do
    h <- openFile "tmp.txt" WriteMode
    hPutStrLn h "stuff"
    hClose h

-- Pokud nechceme explicitně řešit otevírání a zavírání, můžeme použít:
--
--   withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

writeTmp' :: IO ()
writeTmp' = withFile "tmp.txt" WriteMode $ \h -> hPutStrLn h "stuff"

-- Zkuste si naprogramovat vlastní verzi withFile.

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' = undefined

-- Pro IO existuje obrovské množství funkcí, tady jsme viděli jen špičku
-- ledovce.
--
--
-- Podívejme se na další monádu.
--
-- Jak víme, Haskell je čistě funkcionální jazyk, což speciálně znamená, že
-- nelze měnit hodnotu proměnných. Pokud jsme potřebovali mít nějakou informaci,
-- která se postupně mění, tak jsme to prozatím řešili tak, že jsme přidali
-- další argument a návratovou hodnotu.

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show)

label :: Int -> Tree a -> (Tree Int, Int)
label c  Leaf         = (Leaf, c)
label c1 (Node l _ r) = (Node l' x' r', c4)
  where
    (l', c2) = label c1 l
    (x', c3) = (c2, c2 + 1)
    (r', c4) = label c3 r

-- Tenhle proces můžeme generalizovat.

newtype State s a = State { runState :: s -> (a, s) }

postIncrement :: State Int Int
postIncrement = State $ \s -> (s, s + 1)

-- Na argument s se můžeme dívat jako na hodnotu proměnné před změnou, na
-- s + 1 na výstupu pak jako na hodnotu proměnné po změně.
--
-- Budou se nám ještě hodit dvě operace, které s tímto stavem pracují.

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- Teď jen potřebujeme skládaní těchto operací.

instance Functor (State s) where
    fmap f (State m) = State $ \s -> let (a, s') = m s in (f a, s')

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)

    State mf <*> State ma = State $ \s ->
        let (f, s')  = mf s
            (a, s'') = ma s'
        in  (f a, s'')

instance Monad (State s) where
    -- return není zapotřebí, už jsme implementovali pure
    State m >>= f = State $ \s ->
        let (a, s') = m s
        in  runState (f a) s'

-- Předchozí funkci label můžeme přepsat takto:

label' :: Tree a -> State Int (Tree Int)
label' Leaf         = pure Leaf
label' (Node l _ r) = do
    l' <- label' l
    x' <- postIncrement
    r' <- label' r
    pure (Node l' x' r')

-- Ještě poznámka na závěr. Možná vás napadla otázka, jestli je možné implementovat funkci
-- extract :: (Monad m) => m a -> a.
--
-- Předpokládejme, že funkci extract máme k dispozici, pak můžeme definovat:
--
--   import System.Random
--
--   f :: Int -> Int
--   f x = x + extract (randomRIO (0, 100))
--
-- kde randomRIO je funkce typu (Int, Int) -> IO Int, která vygeneruje náhodné číslo v daném
-- rozmezí. Z definice je zřejmé, že se funkce f nemůže chovat jako matematická funkce, tj.
-- pokud f zavoláme dvakrát se stejným argumentem, nemusíme dostat stejné výsledky. V obecnosti
-- tedy funkci extract implementovat nelze.
--
-- To ovšem neznamená, že podobné funkce neexistují pro specifické monády. Např. pro Maybe máme:
--
--   extractMaybe :: a -> Maybe a -> a
--   extractMaybe d Nothing  = d
--   extractMaybe _ (Just x) = x
--
-- a pro State máme:
--
--   extractState :: s -> State s a -> a
--   extractState s (State f) = fst (f s)
--
