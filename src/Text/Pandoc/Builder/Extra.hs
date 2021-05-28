module Text.Pandoc.Builder.Extra
  ( header1
  , sub
  , module Text.Pandoc.Builder
  , IsString (..)
  )
where

import Data.String
import Text.Pandoc.Builder
import Text.Pandoc.Walk

header1 :: Inlines -> Blocks
header1 = header 1

-- Not super efficient but plays nicely with everything
sub :: Blocks -> Blocks
sub =
  walk $ \case
    (Header n a ii) -> Header (n + 1) a ii
    x -> x
