module TSPData where

import qualified Data.Map as Map

data Loc = Loc {
    locx :: Float,
    locy :: Float
} deriving (Show)

type GrpahDist = Float
type NodeLbl = Int
type Graph = Map.Map NodeLbl Loc
type Gene = [NodeLbl]
data TravState = TravState [NodeLbl] (Map.Map NodeLbl GrpahDist)

listCities :: Graph -> [NodeLbl]
listCities = Map.keys

graphDist :: Graph -> NodeLbl -> NodeLbl -> Float
graphDist g n1 n2 =
    sqrt $
        ((locx n2_loc - locx n1_loc) ** 2.0)
        + ((locy n2_loc - locy n1_loc) ** 2.0)
    where
        n1_loc = g Map.! n1
        n2_loc = g Map.! n2

loadCitiesDataAsGraph :: [String] -> TSPData.Graph
loadCitiesDataAsGraph lines = 
    Map.fromList $ parseline <$> lines
    where
        parseline :: String -> (TSPData.NodeLbl, TSPData.Loc)
        parseline line = toTuple $ words line

        toTuple :: [String] -> (TSPData.NodeLbl, TSPData.Loc)
        toTuple [n,x,y] = ( (read n), TSPData.Loc (read x) (read y) )
        toTuple _ = ( -1, TSPData.Loc 0 0 )
