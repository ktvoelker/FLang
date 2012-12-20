
module Pretty where

class Pretty a where
  type TokenKind a
  tokens :: a -> [Token (TokenKind a)]
  space  :: TokenKind a -> TokenKind a -> Bool

data Token a = Word String a | LineBreak Int | Depth Int
  deriving (Eq, Ord, Show)

data SToken = Word String | Space | LineBreak Int | Depth Int
  deriving (Eq, Ord, Show)

prepare :: (Pretty a, TokenKind a ~ b) => [Token b] -> [SToken]
prepare = undefined

instance Show Token where
  show w@Word{} = chars w
  show _ = ""
  showList xs = undefined

{--
 - Stuff we need:
 - 1. ReaderT to keep track of the current nominal indentation level and syntactic
 -    depth, as well as what the syntactic depth was just after the last mandatory
 -    line break. Also the current desired line length and spaces per indent.
 - 2. Do we need StateT?
 - 3. List monad for backtracking across various line-breaking options.
 - 4. Where does the output go when finalized? We can just return it.
 -
 - Find all the tokens up to the next hard line break. If they don't fit on one line,
 - apply various line-breaking strategies to split them into multiple lines. Each
 - strategy should be given the tokens that need splitting (with the precomputed
 - intervening whitespace), as well as the actual available line length, taking into
 - account the ambient indentation. Each strategy produces a result:
 -
 -   data SplitResult = Success [Line] Score | Failure
 -
 - There may be some strategies that can be applied at a variety of different
 - syntactic depths, which is fine. Just pick the result with the highest score.
 -
 - There could also be some scoring threshold above which we just stop trying
 - alternatives. This especially makes sense if we can sort the strategies into a
 - preferred order.
 -}

