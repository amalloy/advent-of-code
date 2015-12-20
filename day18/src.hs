import qualified Data.Map as M
import Data.Maybe (mapMaybe)

data Life coord light = Life {neighbors :: (coord -> [coord]),
                              next :: light -> [light] -> light,
                              grid :: M.Map coord light
                             }

advance :: Ord coord => Life coord light -> Life coord light
advance game = game {grid = M.fromList . map tick . M.toList $ m}
  where m = grid game
        tick (c,light) = (c, let coords = neighbors game c
                                 others = mapMaybe (`M.lookup` m) coords
                             in next game light others)

type Coord a = (a,a)
type Light = Bool
