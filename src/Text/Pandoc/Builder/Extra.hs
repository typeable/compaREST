module Text.Pandoc.Builder.Extra
  ( header0
  , sub
  , module Text.Pandoc.Builder
  , IsString (..)
  )
where

import Data.String
import Text.Pandoc.Builder
import Text.Pandoc.Walk

header0 :: Inlines -> Blocks
header0 = header 0

-- Not super efficient but plays nicely with everything
sub :: Blocks -> Blocks
sub =
  walk $ \case
    (Header n a ii) -> Header (n + 1) a ii
    x -> x
