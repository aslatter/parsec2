-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Perm
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
-- 
-- Maintainer  :  Antoine Latter <aslatter@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable (uses existentially quantified data constructors)
--
-- This module implements permutation parsers. The algorithm used
-- is fairly complex since we push the type system to its limits :-)
-- The algorithm is described in:
--
-- /Parsing Permutation Phrases,/
-- by Arthur Baars, Andres Loh and Doaitse Swierstra.
-- Published as a functional pearl at the Haskell Workshop 2001.
-- 
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Perm
                  ( PermParser  -- abstract

                  , permute
                  , (<||>), (<$$>)
                  , (<|?>), (<$?>)
                  ) where

import Text.ParserCombinators.Parsec

{---------------------------------------------------------------

---------------------------------------------------------------}
infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>


{---------------------------------------------------------------
  test -- parse a permutation of 
  * an optional string of 'a's
  * a required 'b'
  * an optional 'c'
---------------------------------------------------------------}
test input
  = parse (do{ x <- ptest; eof; return x }) "" input

ptest :: Parser (String,Char,Char)
ptest  
  = permute $
    (,,) <$?> ("",many1 (char 'a'))
         <||> char 'b' 
         <|?> ('_',char 'c')


{---------------------------------------------------------------
  Building a permutation parser
---------------------------------------------------------------}

-- | The expression @perm \<||> p@ adds parser @p@ to the permutation
-- parser @perm@. The parser @p@ is not allowed to accept empty input -
-- use the optional combinator ('<|?>') instead. Returns a
-- new permutation parser that includes @p@.
(<||>) :: PermParser tok st (a -> b) -> GenParser tok st a -> PermParser tok st b
(<||>) perm p     = add perm p                  

-- | The expression @f \<$$> p@ creates a fresh permutation parser
-- consisting of parser @p@. The the final result of the permutation
-- parser is the function @f@ applied to the return value of @p@. The
-- parser @p@ is not allowed to accept empty input - use the optional
-- combinator ('<$?>') instead.
--
-- If the function @f@ takes more than one parameter, the type variable
-- @b@ is instantiated to a functional type which combines nicely with
-- the adds parser @p@ to the ('<||>') combinator. This
-- results in stylized code where a permutation parser starts with a
-- combining function @f@ followed by the parsers. The function @f@
-- gets its parameters in the order in which the parsers are specified,
-- but actual input can be in any order.
(<$$>) :: (a -> b) -> GenParser tok st a -> PermParser tok st b
(<$$>) f p        = newperm f <||> p

-- | The expression @perm \<||> (x,p)@ adds parser @p@ to the
-- permutation parser @perm@. The parser @p@ is optional - if it can
-- not be applied, the default value @x@ will be used instead. Returns
-- a new permutation parser that includes the optional parser @p@.
(<|?>) :: PermParser tok st (a -> b) -> (a, GenParser tok st a) -> PermParser tok st b
(<|?>) perm (x,p) = addopt perm x p

-- | The expression @f \<$?> (x,p)@ creates a fresh permutation parser
-- consisting of parser @p@. The the final result of the permutation
-- parser is the function @f@ applied to the return value of @p@. The
-- parser @p@ is optional - if it can not be applied, the default value
-- @x@ will be used instead.
(<$?>) :: (a -> b) -> (a, GenParser tok st a) -> PermParser tok st b
(<$?>) f (x,p)    = newperm f <|?> (x,p)



{---------------------------------------------------------------
  The permutation tree
---------------------------------------------------------------}

-- | The type @PermParser tok st a@ denotes a permutation parser that,
-- when converted by the 'permute' function, parses
-- @tok@ tokens with user state @st@ and returns a value of
-- type @a@ on success.
--
-- Normally, a permutation parser is first build with special operators
-- like ('<||>') and than transformed into a normal parser
-- using 'permute'.
data PermParser tok st a = Perm (Maybe a) [Branch tok st a]
data Branch tok st a     = forall b. Branch (PermParser tok st (b -> a)) (GenParser tok st b)


-- | The parser @permute perm@ parses a permutation of parser described
-- by @perm@. For example, suppose we want to parse a permutation of:
-- an optional string of @a@'s, the character @b@ and an optional @c@.
-- This can be described by:
--
-- >  test  = permute (tuple <$?> ("",many1 (char 'a'))
-- >                         <||> char 'b'
-- >                         <|?> ('_',char 'c'))
-- >        where
-- >          tuple a b c  = (a,b,c)
permute :: PermParser tok st a -> GenParser tok st a
permute (Perm def xs)
  = choice (map branch xs ++ empty)
  where
    empty
      = case def of
          Nothing -> []
          Just x  -> [return x]

    branch (Branch perm p)
      = do{ x <- p
          ; f <- permute perm
          ; return (f x)
          }

-- build permutation trees
newperm :: (a -> b) -> PermParser tok st (a -> b)
newperm f
  = Perm (Just f) []

add :: PermParser tok st (a -> b) -> GenParser tok st a -> PermParser tok st b
add perm@(Perm mf fs) p
  = Perm Nothing (first:map insert fs)
  where
    first   = Branch perm p
    insert (Branch perm' p')
            = Branch (add (mapPerms flip perm') p) p'

addopt :: PermParser tok st (a -> b) -> a -> GenParser tok st a -> PermParser tok st b
addopt perm@(Perm mf fs) x p
  = Perm (fmap ($ x) mf) (first:map insert fs)
  where
    first   = Branch perm p
    insert (Branch perm' p')
            = Branch (addopt (mapPerms flip perm') x p) p'


mapPerms :: (a -> b) -> PermParser tok st a -> PermParser tok st b
mapPerms f (Perm x xs)
  = Perm (fmap f x) (map (mapBranch f) xs)
  where
    mapBranch f (Branch perm p)
      = Branch (mapPerms (f.) perm) p
