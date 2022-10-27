{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

{- | A [Latent Dirichlet Allocation (LDA)](https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation) model
     (or topic model) for learning the distribution over words and topics in a text document.
-}

module LDA where

import Model ( Model, dirichlet, discrete', categorical )
import Sampler ( Sampler )
import Control.Monad ( replicateM )
import Data.Kind (Constraint)
import Env ( Observables, Observable(get), Assign((:=)), nil, (<:>) )
import Inference.SIM as SIM ( simulate )
import Inference.MH as MH ( mh )

{- | An LDA environment.

     Assuming 1 document with 2 topics and a vocabulary of 4 words,
     the parameters of the model environment would have the following shape:

      θ would be [[prob_topic_1, prob_topic_2]                         -- probabilities of topics in document 1
                 ]

      φ would be [[prob_word_1, prob_word_2, prob_word_3, prob_word_4] -- probabilities of words in topic 1
                  [prob_word_1, prob_word_2, prob_word_3, prob_word_4] -- probabilities of words in topic 2
                 ]
-}
type TopicEnv =
  '[ "θ" ':= [Double],  -- ^ probabilities of each topic in a document
     "φ" ':= [Double],  -- ^ probabilities of each word in a topic
     "w" ':= String     -- ^ word
   ]

-- | Prior distribution for topics in a document
docTopicPrior :: Observable env "θ" [Double]
  -- | number of topics
  => Int
  -- | probability of each topic
  -> Model env ts [Double]
docTopicPrior n_topics = dirichlet (replicate n_topics 1) #θ

-- | Prior distribution for words in a topic
topicWordPrior :: Observable env "φ" [Double]
  -- | vocabulary
  => [String]
  -- | probability of each word
  -> Model env ts [Double]
topicWordPrior vocab
  = dirichlet (replicate (length vocab) 1) #φ

-- | A distribution generating words according to their probabilities
wordDist :: Observable env "w" String
  -- | vocabulary
  => [String]
  -- | probability of each word
  -> [Double]
  -- | generated word
  -> Model env ts String
wordDist vocab ps =
  categorical (zip vocab ps) #w

-- | Distribution over the topics in a document, over the distribution of words in a topic
topicModel :: (Observables env '["φ", "θ"] [Double],
               Observable env "w" String)
  -- | vocabulary
  => [String]
  -- | number of topics
  -> Int
  -- | number of words
  -> Int
  -- | generated words
  -> Model env ts [String]
topicModel vocab n_topics n_words = do
  -- Generate distribution over words for each topic
  topic_word_ps <- replicateM n_topics $ topicWordPrior vocab
  -- Generate distribution over topics for a given document
  doc_topic_ps  <- docTopicPrior n_topics
  replicateM n_words (do  z <- discrete' doc_topic_ps
                          let word_ps = topic_word_ps !! z
                          wordDist vocab word_ps)

-- | Topic distribution over many topics
topicModels :: (Observables env '["φ", "θ"] [Double],
               Observable env "w" String)
  -- | vocabulary
  => [String]
  -- | number of topics
  -> Int
  -- | number of words for each document
  -> [Int]
  -- | generated words for each document
  -> Model env ts [[String]]
topicModels vocab n_topics doc_words = do
  mapM (topicModel vocab n_topics) doc_words


-- | Example possible vocabulary
vocab :: [String]
vocab = ["DNA", "evolution", "parsing", "phonology"]

-- | Simulating from topic model
simLDA :: Sampler [String]
simLDA = do
  -- Specify model inputs
  let n_words  = 100
      n_topics = 2
  -- Specify model environment
      env_in = #θ := [[0.5, 0.5]] <:>
               #φ := [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],
                      [1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]] <:>
               #w := [] <:> nil
  -- Simulate from topic model
  (words, env_out) <- SIM.simulate (topicModel vocab n_topics n_words) env_in
  return words

-- | Example document of words
topic_data :: [String]
topic_data     = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

-- | MH inference from topic model
mhLDA :: Sampler ([[Double]], [[Double]])
mhLDA  = do
  -- Specify model inputs
  let n_words   = 100
      n_topics  = 2
  -- Specify model environment
      env_mh_in = #θ := [] <:>  #φ := [] <:> #w := topic_data <:> nil
  -- Run MH for 500 iterations
  env_mh_outs <- MH.mh 500 (topicModel vocab n_topics n_words) env_mh_in ["φ", "θ"]
  -- Draw the most recent sampled parameters from the MH trace
  let env_pred   = head env_mh_outs
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (θs, φs)
