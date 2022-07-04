module BenchmarkPrograms where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Criterion.Main
import Criterion (benchmark)
import           Statistics.Distribution        ( logProbability )
import qualified Statistics.Distribution.Binomial  as SB
import           Numeric.Log
import Control.Monad
import Data.Bifunctor
import Data.Maybe
import Data.Vector (Vector, fromList, toList)
import Data.List

{- Lin Regression -}

data LinRegrParams = LinRegrParams {
  m :: Double, c :: Double, σ :: Double
} deriving Show

fromLinRegrParams :: LinRegrParams -> (Double, Double, Double)
fromLinRegrParams (LinRegrParams m c σ) = (m, c, σ)

linRegrPrior :: MonadSample m => Maybe Double -> Maybe Double -> Maybe Double -> m LinRegrParams
linRegrPrior m0 c0 σ0 = do
  m <- normal 0 3
  c <- normal 0 5
  σ <- uniform 1 3
  let m' = fromMaybe m m0
      c' = fromMaybe c c0
      σ' = fromMaybe σ σ0
  return (LinRegrParams m' c' σ')

simulateLinRegr :: MonadSample m => Maybe Double -> Maybe Double -> Maybe Double -> [Double] -> m [Double]
simulateLinRegr m0 c0 σ0 xs  = do
  LinRegrParams m c σ <- linRegrPrior m0 c0 σ0
  foldM (\ys x -> do
            y <- normal (m * x + c) σ
            return (y:ys)) [] xs

inferLinRegr :: MonadInfer m => Maybe Double -> Maybe Double -> Maybe Double -> [(Double, Double)] -> m LinRegrParams
inferLinRegr m0 c0 σ0 xys  = do
  LinRegrParams m c σ <- linRegrPrior m0 c0 σ0
  mapM_ (\(x, y_obs) -> score (normalPdf (m * x + c) σ y_obs)) xys
  return (LinRegrParams m c σ)


linRegrData :: Int -> [(Double, Double)]
linRegrData n_datapoints = zip [0 .. (fromIntegral n_datapoints)] (map (*3) [0 .. (fromIntegral n_datapoints)])

-- Execute log regression
simLinRegr :: Int -> Int -> IO [[Double]]
simLinRegr n_samples n_datapoints = do
  sampleIO $ replicateM n_samples (simulateLinRegr Nothing Nothing Nothing [0 .. (fromIntegral n_datapoints)])

lwLinRegr :: Int -> Int -> IO ()
lwLinRegr n_samples n_datapoints = do
  map snd <$> (sampleIO $ prior $ replicateM n_samples (runWeighted $ inferLinRegr Nothing Nothing Nothing (linRegrData n_datapoints)))
  return ()

mhLinRegr :: Int -> Int -> IO ()
mhLinRegr n_samples n_datapoints = do
  sampleIO $ prior $ mh n_samples (inferLinRegr Nothing Nothing Nothing (linRegrData n_datapoints))
  return ()

{- Hidden Markov Model -}

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

binomial :: MonadSample m => Int -> Double -> m Int
binomial n p = discrete $ SB.binomial n p

binomialPdf :: Int -> Double -> Int -> Log Double
binomialPdf n p y = Exp $ logProbability (SB.binomial n p) y

data Params = Params {
    transition_p  :: {-# UNPACK #-} !Double,
    observation_p :: {-# UNPACK #-} !Double
} deriving Show

fromParams :: Params -> (Double, Double)
fromParams (Params trans_p obs_p) = (trans_p, obs_p)

initialParams :: Params
initialParams = Params 0.5 0.5

-- HMM SIM Model
simulateHmm1step :: MonadSample m => Params -> Int -> m (Int, Int)
simulateHmm1step (Params transition_p observation_p) x = do
  dX <- bernoulli transition_p
  let x' = x + boolToInt dX
  y  <- binomial x observation_p
  return (x', y)

simulateHmmNsteps :: MonadSample m => Params -> Int -> Int -> m [Int]
simulateHmmNsteps params x n = f x [] n
  where f x ys 0  = return ys
        f x ys n' = do
          (x', y) <- simulateHmm1step params x
          f x' (ys ++ [y]) (n' - 1)

-- HMM Infer Model
obsModel :: MonadInfer m => Double -> Int -> Int -> m Int
obsModel observation_p x y_obs = do
  score (binomialPdf x observation_p y_obs)
  return y_obs

transModel :: MonadSample m => Double -> Int -> m Int
transModel transition_p x = do
  dX <- bernoulli transition_p
  return (x + boolToInt dX)

inferHmm1step :: MonadInfer m => Params -> Int -> Int -> m Int
inferHmm1step (Params transition_p observation_p) x y_obs = do
  x' <- transModel transition_p x
  y' <- obsModel observation_p x' y_obs
  return x'

inferHmmNsteps :: MonadInfer m => Params -> Int -> [Int] -> m Int
inferHmmNsteps params x [] = return x
inferHmmNsteps params x (y:ys) = do
  x' <- inferHmm1step params x y
  inferHmmNsteps params x' ys

hmmPrior :: MonadSample m => m Params
hmmPrior = do
  trans_p <- uniform 0 1
  obs_p   <- uniform 0 1
  return (Params trans_p obs_p)

inferHMM :: MonadInfer m => Int -> [Int] -> m Params
inferHMM x_0 ys = do
  params  <- hmmPrior
  inferHmmNsteps params x_0 ys
  return params

--- Execute HMM
simHMM :: Int -> Int -> IO [[Int]]
simHMM n_samples n_steps = do
   yss <- sampleIO $ replicateM n_samples $ simulateHmmNsteps initialParams 0 n_steps
   return yss

lwHMM :: Int -> Int -> IO ()
lwHMM n_samples n_steps = do
  ys <- simHMM 1 n_steps
  sampleIO $ replicateM n_samples $ runWeighted (inferHMM 0 (head ys))
  return ()

mhHMM :: Int -> Int -> IO ()
mhHMM n_samples n_steps = do
  ys <- simHMM 1 n_steps
  sampleIO $ prior $ mh n_samples (inferHMM 0 (head ys))
  return ()

{- Latent Dirichlet Allocation -}
data LDAParams = LDAParams {
    θ :: [Double],   -- probabilities of each topic in a document
    φ :: [[Double]]  -- probabilities of each word in a topic in a document
   } deriving Show


-- Topic Model SIM
documentDistSim :: MonadSample m => LDAParams -> [String] -> Int -> Int -> m [String]
documentDistSim (LDAParams doc_topic_ps topic_word_ps) vocab n_topics n_words = do
  let sampleWord = do
        topic_idx  <- categorical (fromList doc_topic_ps)
        let word_ps = topic_word_ps !! topic_idx
        word_idx   <- categorical (fromList word_ps)
        let word    = vocab !! word_idx
        return word
  replicateM n_words sampleWord

-- Topic Model Infer
wordDist :: MonadInfer m => [String] -> [Double] -> String -> m String
wordDist vocab ps w = do
  let w_p = ps !! fromJust (elemIndex w vocab)
  score (Exp w_p)
  return w

topicWordPrior :: MonadSample m => [String] -> m [Double]
topicWordPrior vocab
  = toList <$> dirichlet (fromList $ replicate (length vocab) 1)

docTopicPrior ::  MonadSample m  => Int -> m [Double]
docTopicPrior n_topics
  = toList <$> dirichlet (fromList $ replicate n_topics 1)

documentDistInfer :: MonadInfer m => [String] -> Int -> [String] -> m LDAParams
documentDistInfer vocab n_topics words = do
  -- Distribution over topics for a given document
  doc_topic_ps  <- docTopicPrior n_topics
  -- Generate distribution over words for each topic
  topic_word_ps <- replicateM n_topics $ topicWordPrior vocab
  let scoreWords [] = return words
      scoreWords (w:ws) = do
        z <- categorical (fromList doc_topic_ps)
        let word_ps = topic_word_ps !! z
        wordDist vocab word_ps w
        scoreWords ws
  scoreWords words
  return (LDAParams doc_topic_ps topic_word_ps)

-- Execute topic model
vocabulary :: [String]
vocabulary = ["DNA", "evolution", "parsing", "phonology"]

document :: [String]
document = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA", "DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

initialLDAParams :: LDAParams
initialLDAParams = LDAParams [0.5, 0.5] [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],[1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]]

simLDA :: Int -> Int -> IO [[String]]
simLDA n_samples n_words = do
  sampleIOfixed $ replicateM n_samples (documentDistSim initialLDAParams vocabulary 2 n_words)

lwLDA :: Int -> Int -> IO ()
lwLDA n_samples n_words = do
  sampleIOfixed $ prior $ replicateM n_samples (runWeighted $ documentDistInfer vocabulary 2 (take n_words document))
  return ()

mhLDA :: Int -> Int -> IO ()
mhLDA n_samples n_words = do
  sampleIOfixed $ prior $ mh n_samples (documentDistInfer vocabulary 2 (take n_words document))
  return ()