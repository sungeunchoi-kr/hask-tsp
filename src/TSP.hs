module TSP where

import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Ord
import System.IO.Unsafe
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)
import Shuffle

import TSPData

-- interesting read:
-- https://arxiv.org/pdf/1808.08329.pdf

geneDist :: Graph -> [NodeLbl] -> Float
geneDist _ [] = 0
geneDist g xs = geneDist' g $ xs ++ [head xs] -- include route back home.
    where
    geneDist' _ [] = 0
    geneDist' _ (x:[]) = 0
    geneDist' g (x:y:[]) = graphDist g x y
    geneDist' g (x:y:rs) = (graphDist g x y) + (geneDist' g (y:rs))

bestInGenePool :: Graph -> [Gene] -> Gene
bestInGenePool cities = minimumBy (comparing $ geneDist cities)

worstInGenePool :: Graph -> [Gene] -> Gene
worstInGenePool cities = maximumBy (comparing $ geneDist cities)

genePoolDistDelta cities p =
    (geneDist cities $ worstInGenePool cities p)
    - (geneDist cities $ bestInGenePool cities p)

selection :: Graph -> Int -> [Gene] -> [Gene]
selection _ _ [] = []
selection cities n xs = 
    let competitors = take n xs
        xsRemainder = drop n xs
        winner = bestInGenePool cities competitors in 
    winner : (selection cities n xsRemainder)

createGenePool :: (MonadRandom m) => Int -> [NodeLbl] -> m [[NodeLbl]]
createGenePool n = replicateM n . shuffle

-- order 1 crossover 
-- http://www.rubicite.com/Tutorials/GeneticAlgorithms/CrossoverOperators/Order1CrossoverOperator.aspx
order1Crossover :: (MonadRandom m) => Gene -> Gene -> m Gene
order1Crossover p1 p2 = do
    -- 1. choose a 'swath' from p1. (random length and starting position)
    -- 2. For all elements of p2: Is it a member of the swath?
    --    If so, skip. Otherwise, append.
    -- 3. Inject the swath into p2 at the same position as p1.
    rlen <- getRandomR (1, length p1) 
    rpos <- getRandomR (0, length p1 - rlen)
    let swath = take rlen . drop rpos $ p1  -- sublist
    let swathSet = Set.fromList swath
    let ch = filter (\n -> Set.notMember n swathSet) p2
    return $ (take rpos ch) ++ swath ++ (drop rpos ch)

crossover :: (MonadRandom m) => [Gene] -> m [Gene]
crossover gs = do
    crossed <- forM (pairup gs) $ uncurry order1Crossover
    return crossed 
    where
        pairup :: [a] -> [(a,a)]
        pairup [] = []
        pairup (x:y:zs) = (x,y) : (pairup zs)
        pairup (x:zs) = (x,x) : (pairup zs)

mutate :: (MonadRandom m) => Float -> Gene -> m Gene
mutate p g = do
    p_i <- getRandom
    if p_i > p
        then return g
        else operation g
    where
        operation :: (MonadRandom m) => Gene -> m Gene
        operation g = do
            i1 <- getRandomR (0, length g - 1)
            i2 <- getRandomR (0, length g - 1)
            return $ swapElementsAt i1 i2 g

        swapElementsAt :: Int -> Int -> [a] -> [a]
        swapElementsAt a b list = list1 ++ [list !! b] ++ list2 ++ [list !! a] ++ list3
            where   list1 = take a list;
                    list2 = drop (succ a) (take b list);
                    list3 = drop (succ b) list

mutateN :: (MonadRandom m) => Float -> [Gene] -> m [Gene]
mutateN p gs = forM gs (mutate p)

generation :: (MonadRandom m) => Graph -> [Gene] -> m [Gene]
generation cities genes = do
    let genes' = selection cities 3 genes
    offsprings <- crossover genes'
    offsprings_mu <- mutateN 0.1 offsprings
    let d = (length genes) - (length offsprings_mu)
    return $ offsprings_mu ++ (take d genes)

generationN :: (RandomGen g) => Graph -> Int -> [Gene] -> RandT g IO [Gene]
generationN cities 0 g0 = generation cities g0
generationN cities n g0 = do
    g1 <- generation cities g0
    liftIO $ putStrLn $
        (show $ geneDist cities $ bestInGenePool cities g1) ++ ";"
        ++ (show $ geneDist cities $ worstInGenePool cities g1) ++ ";"
        ++ (show $ bestInGenePool cities g1)
    g2 <- generationN cities (n-1) g1
    return g2

start :: (RandomGen g) => Graph -> RandT g IO [Gene]
start cities = do
    genes <- createGenePool 1000 (listCities cities)
    result <- generationN cities 1000 genes
    return result

run :: Graph -> IO ()
run g = do
    --putStrLn "Starting."
    seed <- (round . (* 1000)) <$> getPOSIXTime 
    genePool <- evalRandT (start g) (mkStdGen seed)
    --putStrLn $ "best route: " ++ (show $ bestInGenePool genePool)
    return ()

