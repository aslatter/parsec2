-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Language
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
-- 
-- Maintainer  :  Antoine Latter <aslatter@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable (uses non-portable module Text.ParserCombinators.Parsec.Token)
--
-- A helper module that defines some language definitions that can be used
-- to instantiate a token parser (see "Text.ParserCombinators.Parsec.Token").
-- 
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Language
                     ( haskellDef, haskell
                     , mondrianDef, mondrian
                   
                     , emptyDef
                     , haskellStyle
                     , javaStyle   
                     , LanguageDef (..)                
                     ) where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token 

           
-----------------------------------------------------------
-- Styles: haskellStyle, javaStyle
-----------------------------------------------------------               

-- | This is a minimal token definition for Haskell style languages. It
-- defines the style of comments, valid identifiers and case
-- sensitivity. It does not define any reserved words or operators.
haskellStyle :: LanguageDef st
haskellStyle= emptyDef                      
                { commentStart   = "{-"
                , commentEnd     = "-}"
                , commentLine    = "--"
                , nestedComments = True
                , identStart     = letter
                , identLetter    = alphaNum <|> oneOf "_'"
                , opStart        = opLetter haskellStyle
                , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"              
                , reservedOpNames= []
                , reservedNames  = []
                , caseSensitive  = True                                   
                }         
                           
-- | This is a minimal token definition for Java style languages. It
-- defines the style of comments, valid identifiers and case
-- sensitivity. It does not define any reserved words or operators.
javaStyle  :: LanguageDef st
javaStyle   = emptyDef
                { commentStart   = "/*"
                , commentEnd     = "*/"
                , commentLine    = "//"
                , nestedComments = True
                , identStart     = letter
                , identLetter    = alphaNum <|> oneOf "_'"
                , reservedNames  = []
                , reservedOpNames= []
                , caseSensitive  = False
                }

-----------------------------------------------------------
-- minimal language definition
-----------------------------------------------------------                
emptyDef   :: LanguageDef st
emptyDef    = LanguageDef 
               { commentStart   = ""
               , commentEnd     = ""
               , commentLine    = ""
               , nestedComments = True
               , identStart     = letter <|> char '_'
               , identLetter    = alphaNum <|> oneOf "_'"
               , opStart        = opLetter emptyDef
               , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , reservedOpNames= []
               , reservedNames  = []
               , caseSensitive  = True
               }
                


-----------------------------------------------------------
-- Haskell
-----------------------------------------------------------               

-- | A lexer for the haskell language.
haskell :: TokenParser st
haskell      = makeTokenParser haskellDef

-- | The language definition for the Haskell language.
haskellDef  :: LanguageDef st
haskellDef   = haskell98Def
                { identLetter    = identLetter haskell98Def <|> char '#'
                , reservedNames  = reservedNames haskell98Def ++ 
                                   ["foreign","import","export","primitive"
                                   ,"_ccall_","_casm_"
                                   ,"forall"
                                   ]
                }

-- | The language definition for the language Haskell98.
haskell98Def :: LanguageDef st
haskell98Def = haskellStyle
                { reservedOpNames= ["::","..","=","\\","|","<-","->","@","~","=>"]
                , reservedNames  = ["let","in","case","of","if","then","else",
                                    "data","type",
                                    "class","default","deriving","do","import",
                                    "infix","infixl","infixr","instance","module",
                                    "newtype","where",
                                    "primitive"
                                    -- "as","qualified","hiding"
                                   ]
                }         
                
                
-----------------------------------------------------------
-- Mondrian
-----------------------------------------------------------               

-- | A lexer for the mondrian language.
mondrian :: TokenParser st
mondrian    = makeTokenParser mondrianDef

-- | The language definition for the language Mondrian.
mondrianDef :: LanguageDef st
mondrianDef = javaStyle
                { reservedNames = [ "case", "class", "default", "extends"
                                  , "import", "in", "let", "new", "of", "package"
                                  ]
                , caseSensitive  = True
                }


