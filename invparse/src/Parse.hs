module Parse where
import           AST
import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.Token

--
-- Parsing
--

-- | Parse an instruction in the invariant language
-- The invariant language has very few
parseInstruction = do
  error "SOON"

--
-- Lexing
--

-- | Lexer for invariant specification
lexer = makeTokenParser invariantStyle

-- | { optional argument }
optional = braces lexer

-- | [ a list of possible things ]
list = brackets lexer

-- | <user supplied name>
variable = angles lexer

-- | Instruction name (e.g., ADD)
name = identifier lexer

-- | Is the stack pointer allowed here?
stackPointer = operator lexer

-- | arguments and function

invariantStyle :: LanguageDef st
invariantStyle = emptyDef
                 { commentStart    = "/*"
                 , commentEnd      = "*/"
                 , commentLine     = "//"
                 , nestedComments  = False
                 , identStart      = letter
                 , identLetter     = alphaNum
                 , opStart         = opLetter invariantStyle
                 , opLetter        = oneOf "'"
                 , reservedOpNames = []
                 , reservedNames   = []
                 , caseSensitive   = True
                 }
