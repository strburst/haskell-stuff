-- Intermediate Haskell/More on datatypes
-- https://en.wikibooks.org/wiki/Haskell/More_on_datatypes

type Name = String

data Person = Person {
    name     :: Name,
    father   :: Maybe Person,
    mother   :: Maybe Person
} deriving (Show)

-- 1st generation
james = Person "James" Nothing Nothing
anna = Person "Anna" Nothing Nothing
michael = Person "Michael" Nothing Nothing
amy = Person "Amy" Nothing Nothing
peter = Person "Peter" Nothing Nothing
jill = Person "Jill" Nothing Nothing
paul = Person "Paul" Nothing Nothing
jane = Person "Jane" Nothing Nothing

-- 2nd generation
isaac = Person "Isaac" (Just james) (Just anna)
rachel = Person "Rachel" (Just michael) (Just amy)
manuel = Person "Manuel" (Just michael) (Just amy)
hannah = Person "Hannah" (Just paul) (Just jane)

-- 3rd generation
jacob = Person "Jacob" (Just isaac) (Just rachel)
jamie = Person "Jamie" (Just manuel) (Just hannah)

paternalGrandfather1 :: Person -> Maybe Person
paternalGrandfather1 c = case father c of
    Nothing  -> Nothing
    Just dad -> father dad

paternalGrandfather2 :: Person -> Maybe Person
paternalGrandfather2 c = father c >>= (\dad -> father dad)

paternalGrandfather3 :: Person -> Maybe Person
paternalGrandfather3 c = do
    dad <- father c
    gf  <- father dad
    return gf

-- bothGrandfathers1 :: Person -> (Maybe Person, Maybe Person)
-- bothGrandfathers1 c =
--     father c >>=
--         (\dad -> father dad >>=
--             (\gf1 -> mother c >>=
--                 (\mom -> father mom >>=
--                     (\gf2 -> return (gf1, gf2)))))

-- bothGrandfathers2 :: Person -> (Maybe Person, Maybe Person)
-- bothGrandfathers2 c = do
--     dad <- father c
--     gf1 <- father dad
--     mom <- mother c
--     gf2 <- father mom
--     return (gf1, gf2)

-- data Rising a where
--     Rising :: Num a => a -> Rising a

-- instance Monad (Rising a) where
--    (>>=) (Rising a) f = f (a + 1)
--    return item = Rising item

data TestMonad a = TestMonad a

instance Functor TestMonad where
    fmap (TestMonad a) f = TestMonad (f a)

instance Monad TestMonad where
    (TestMonad a) >>= f = f a
    return a = TestMonad a
