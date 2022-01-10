module Util where

import Relude

-- utility functions for easy lookup
todo :: Text -> a
todo msg = error ("TODO: " <> msg)
