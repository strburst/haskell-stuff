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
