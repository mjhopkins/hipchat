module Control.Lens.AsText where

import           Control.Lens
import           Data.Text    (Text)

class AsText a where
  parse :: Prism' Text a

instance AsText Text where
  parse = id

-- instance AsText a => FromHttpApiData a where
  -- parseUrlPiece = matching parse

-- instance AsText a => ToHttpApiData a where
  -- toUrlPiece = review parse

-- instance AsText a => IsString a where
  -- fromString = (^?! parse) . T.pack
