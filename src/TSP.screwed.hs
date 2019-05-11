module TSP where

import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Identity
import Control.Monad.Morph
import System.Random
import qualified Data.Map as Map
import Data.List
--import Data.List.Shuffle
import qualified Data.Set as Set
import System.IO.Unsafe
import Shuffle
import Data.Time.Clock.POSIX (getPOSIXTime)
--import Data.Array.ST
--import Control.Monad
--import Control.Monad.ST
--import Data.STRef

data Loc = Loc {
    locx :: Float,
    locy :: Float
} deriving (Show)

type GrpahDist = Float
type NodeLbl = Int
type Graph = Map.Map NodeLbl Loc
type Gene = [NodeLbl]
data TravState = TravState [NodeLbl] (Map.Map NodeLbl GrpahDist)

--cities :: Graph
--cities = Map.fromList [
--     (0, Loc 0.3 8.123)
--    ,(1, Loc 12.3 4298.13)
--    ,(2, Loc 93.1301 1112.1239)
--    ,(3, Loc 92.381 928.83)
--    ,(4, Loc 3.1301 1112.1239)
--    ,(5, Loc 93.1301 9238.029)
--    ,(6, Loc 7393.1301 290.928)
--    ,(7, Loc 738 299)
--    ,(8, Loc 301 8)
--    ,(9, Loc 791 98)]

cities :: Graph
cities = Map.fromList [
     (1 , Loc 11003.611100 42102.50000)
    ,(2 , Loc 11108.611100 42373.88890)
    ,(3 , Loc 11133.333300 42885.83330)
    ,(4 , Loc 11155.833300 42712.50000)
    ,(5 , Loc 11183.333300 42933.33330)
    ,(6 , Loc 11297.500000 42853.33330)
    ,(7 , Loc 11310.277800 42929.44440)
    ,(8 , Loc 11416.666700 42983.33330)
    ,(9 , Loc 11423.888900 43000.27780)
    ,(10, Loc 11438.333300 42057.22220)
    ,(11, Loc 11461.111100 43252.77780)
    ,(12, Loc 11485.555600 43187.22220)
    ,(13, Loc 11503.055600 42855.27780)
    ,(14, Loc 11511.388900 42106.38890)
    ,(15, Loc 11522.222200 42841.94440)
    ,(16, Loc 11569.444400 43136.66670)
    ,(17, Loc 11583.333300 43150.00000)
    ,(18, Loc 11595.000000 43148.05560)
    ,(19, Loc 11600.000000 43150.00000)
    ,(20, Loc 11690.555600 42686.66670)
    ,(21, Loc 11715.833300 41836.11110)
    ,(22, Loc 11751.111100 42814.44440)
    ,(23, Loc 11770.277800 42651.94440)
    ,(24, Loc 11785.277800 42884.44440)
    ,(25, Loc 11822.777800 42673.61110)
    ,(26, Loc 11846.944400 42660.55560)
    ,(27, Loc 11963.055600 43290.55560)
    ,(28, Loc 11973.055600 43026.11110)
    ,(29, Loc 12058.333300 42195.55560)
    ,(30, Loc 12149.444400 42477.50000)
    ,(31, Loc 12286.944400 43355.55560)
    ,(32, Loc 12300.000000 42433.33330)
    ,(33, Loc 12355.833300 43156.38890)
    ,(34, Loc 12363.333300 43189.16670)
    ,(35, Loc 12372.777800 42711.38890)
    ,(36, Loc 12386.666700 43334.72220)
    ,(37, Loc 12421.666700 42895.55560)
    ,(38, Loc 12645.000000 42973.33330)]


listCities :: Graph -> [NodeLbl]
listCities = Map.keys

--createGenePool :: (RandomGen g) => Int -> [NodeLbl] -> Rand g [[NodeLbl]]
--createGenePool n = replicateM n . shuffle

createGenePool :: (MonadRandom m) => Int -> [NodeLbl] -> m [[NodeLbl]]
createGenePool n = replicateM n . shuffle

labelGenePool :: Graph -> [Gene] -> [ (Gene,Float) ]
labelGenePool graph genes =
    fmap (\gene -> (gene, geneDist graph gene)) genes

geneDist :: Graph -> [NodeLbl] -> Float
geneDist _ [] = 0
geneDist g xs = geneDist' g $ xs ++ [head xs] -- include route back home.
    where
    geneDist' _ [] = 0
    geneDist' _ (x:[]) = 0
    geneDist' g (x:y:[]) = graphDist g x y
    geneDist' g (x:y:rs) = (graphDist g x y) + (geneDist' g (y:rs))

graphDist :: Graph -> NodeLbl -> NodeLbl -> Float
graphDist g n1 n2 =
    sqrt $ (locx n1_loc * locy n1_loc) + (locx n2_loc * locy n2_loc)
    where
        n1_loc = g Map.! n1
        n2_loc = g Map.! n2

selection :: Int -> [Gene] -> [Gene]
selection _ [] = []
selection n xs = 
    let competitors = take n xs
        xsRemainder = drop n xs
        distLabeledGenes = labelGenePool cities competitors
        winner = fst $ min' distLabeledGenes in 
    winner : (selection n xsRemainder)
    where
        min' :: (Ord b) => [(a,b)] -> (a,b)
        min' = foldr1 (\x y -> if (snd x) <= (snd y) then x else y)

-- order 1 crossover 
-- http://www.rubicite.com/Tutorials/GeneticAlgorithms/CrossoverOperators/Order1CrossoverOperator.aspx
--order1Crossover :: (RandomGen g) => Gene -> Gene -> Rand g Gene
--order1Crossover p1 p2 = do
--    -- 1. choose a 'swath' from p1. (random length and starting position)
--    rlen <- getRandomR (1, length p1) 
--    rpos <- getRandomR (0, length p1 - rlen)
--    let swath = take rlen . drop rpos $ p1  -- sublist
--    let swathSet = Set.fromList swath
--    let ch = filter (\n -> Set.notMember n swathSet) p2
--    return $ (take rpos ch) ++ swath ++ (drop rpos ch)

order1Crossover :: (MonadRandom m) => Gene -> Gene -> m Gene
order1Crossover p1 p2 = do
    -- 1. choose a 'swath' from p1. (random length and starting position)
    rlen <- getRandomR (1, length p1) 
    rpos <- getRandomR (0, length p1 - rlen)
    let swath = take rlen . drop rpos $ p1  -- sublist
    let swathSet = Set.fromList swath
    let ch = filter (\n -> Set.notMember n swathSet) p2
    return $ (take rpos ch) ++ swath ++ (drop rpos ch)

--test :: (RandomGen g) => Rand g Int
--test = do
--    r1 <- getRandomR (1, 10)
--    return r1

-- 2. For all elements of p2: Is it a member of the swath?
--    If so, skip. Otherwise, append.
-- 3. Inject the swath into p2 at the same position as p1.

--crossover :: (RandomGen g) => [Gene] -> Rand g [Gene]
--crossover gs = do
--    crossed <- forM (pairup gs) $ uncurry order1Crossover
--    return crossed 
--    where
--        pairup :: [a] -> [(a,a)]
--        pairup [] = []
--        pairup (x:y:zs) = (x,y) : (pairup zs)
--        pairup (x:zs) = (x,x) : (pairup zs)

crossover :: (MonadRandom m) => [Gene] -> m [Gene]
crossover gs = do
    crossed <- forM (pairup gs) $ uncurry order1Crossover
    return crossed 
    where
        pairup :: [a] -> [(a,a)]
        pairup [] = []
        pairup (x:y:zs) = (x,y) : (pairup zs)
        pairup (x:zs) = (x,x) : (pairup zs)

-- interesting read:
-- https://arxiv.org/pdf/1808.08329.pdf

bestInGenePool :: [Gene] -> Gene
bestInGenePool = foldr1 (\x y ->
    if (geneDist cities x) <= (geneDist cities y) then x else y)

generation :: (RandomGen g) => [Gene] -> StateT [String] (Rand g) [Gene]
generation genes = do
    log <- get
    offsprings <- lift $ crossover (selection 5 genes)
    let d = (length genes) - (length offsprings)
    let newPool = offsprings ++ (take d genes)
    put $ log ++ [ (show $ geneDist cities $ bestInGenePool newPool) ]
    --put $ log ++ [ show $ head genes ] ++ [ show $ head newPool ]
    return newPool

generation' :: (MonadRandom m) => [Gene] -> m [Gene]
generation' genes = do
    offsprings <- crossover (selection 5 genes)
    let d = (length genes) - (length offsprings)
    let newPool = offsprings ++ (take d genes)
    return newPool

generationN :: (RandomGen g) => Int -> [Gene] -> StateT [String] (Rand g) [[Gene]]
generationN 0 _ = return []
generationN n g0 = do
    g1 <- generation g0
    g2 <- generationN (n-1) g1
    return (g1 : g2)

generationN' :: (RandomGen g) => Int -> [Gene] -> RandT g IO [[Gene]]
generationN' 0 _ = return []
generationN' n g0 = do
    g1 <- generation' g0
    liftIO $ putStrLn "hi"
    g2 <- generationN' (n-1) g1
    return (g1 : g2)

-- https://stackoverflow.com/questions/17325485/combining-statet-io-with-state
--hoistRand :: (Monad m) => Rand s a -> RandT s m a
--hoistRand = RandT . (return .)

--http://hackage.haskell.org/package/mmorph-1.0.0/docs/Control-Monad-Morph.html#g:4
--https://stackoverflow.com/questions/9054731/avoiding-lift-with-monad-transformers

--start :: (RandomGen g) => Rand g ([Gene], [String])
start :: (RandomGen g) => RandT g IO [Gene]
start = do
    liftIO $ putStrLn "asdfasdf"
    genes <- createGenePool 1000 (listCities cities)
    --genes' <- last <$> replicateM 1000 (generation genes)
    --result <- runStateT (generation genes) ""
    genes' <- generationN' 200 genes
    --let genes' = last $ fst $ result
    --let log = snd $ result
    --return (genes', log)
    return $ head genes' -- temporary

run :: IO ()
run = do
    putStrLn "Starting."
    seed <- (round . (* 1000)) <$> getPOSIXTime 
    let result = evalRandT start (mkStdGen seed)
    --let genePool = fst result
    --putStrLn $ show (snd result)
    --putStrLn $ "best route: " ++ (show $ bestInGenePool genePool)
    --putStrLn $ show (length result)
    return ()

