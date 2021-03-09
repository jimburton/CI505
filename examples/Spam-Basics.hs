{-

The basic types and functions for our naive Bayesian spam filter.

You can see the training in action by using the `train' and `classify'
functions. For example,

λ> let empty = (0, 0, M.empty)
λ> classify empty "Hi, this email is spam"
(Unclear,0.5)
λ> let wm = train empty "Hi, this email is spam" Spam
λ> classify wm "Hi, buy some viagra"
(Unclear,0.5)
λ> let wm' = train wm "Hi, this email is spam" Spam
λ> let wm'' = train wm' "Hi, this is a spam" Spam
λ> classify wm'' "Hi, this email is spam"
(Spam,0.71533006)

-}
import qualified Data.Map as M

{-| A type used to classify messages -}
data MsgType = Ham | Unclear | Spam deriving (Show, Read, Eq)

{-| A WordFeature is a word along with the number of times we have
seen it occuring in ham and spam messages, respectively. -}
data WordFeature = WordFeature { word :: String   -- ^ The word
                               , hamCount :: Int  -- ^ The number of times it has been seen in Ham
                               , spamCount :: Int -- ^ The number of times it has been seen in Spam
                               }
                   deriving (Show, Eq)

{-| A WMap is the current state of the filter. The first two fields are the total
number of ham and spam messages seen by this filter, and the map allows us to look
up the WordFeature associated with a given word. -}
type WMap = ( Int -- ^ Total Ham messages seen
            , Int -- ^ Total Spam messages seen
            , M.Map String WordFeature) -- ^ 


{-| A message with a higher rating than maxHamScore is not ham. -}
maxHamScore :: Float
maxHamScore = 0.4

{-| A message with a lower rating than minSpamScore is not spam. -}
minSpamScore :: Float
minSpamScore = 0.6

{-| Train the filter on a string representing an email. 
-}
train :: WMap    -- ^ The filter.
      -> String  -- ^ The contents of an email.
      -> MsgType -- ^ Is it ham or spam?
      -> WMap    -- ^ The updated filter.
train w s t = foldl (incrementCount t) w (words s)

{-| Update the ham or spam counts in the WMap for this particular word. -}
incrementCount :: MsgType -- ^ Is it Ham or Spam?
               -> WMap    -- ^ The filter.
               -> String  -- ^ The word.
               -> WMap    -- ^ The updated filter.
incrementCount t (hc, sc, m) s = 
    let mfeat = M.lookup s m
        hcf   = if t==Ham then (+1) else id
        scf   = if t==Spam then (+1) else id
        feat = case mfeat of
                 Nothing -> getWordFeat s (hcf 0) (scf 0)
                 Just wf -> wf { hamCount = hcf (hamCount wf)
                               , spamCount = scf (spamCount wf) }
    in 
      (hcf hc, scf sc, M.insert s feat m)

{-| Classify the contents of a message as Ham|Spam|Unclear, based on
the contents of the WMap. -}
classify :: WMap -> String -> (MsgType, Float)
classify wm str = let feats = extractFeatures wm (words str)
                      s = score wm feats in
                  (classification s, s)

{-| Returns the type associated with a given score. -}
classification :: Float -> MsgType
classification s | s <= maxHamScore  = Ham
                 | s >= minSpamScore = Spam
                 | otherwise         = Unclear

{-| Turn a list of strings into a list of WordFeatures. -}
extractFeatures :: WMap -> [String] -> [WordFeature]
extractFeatures m = map (getWordFeature m) 

{-| Look up a word in the WMap, retrieving the WordFeature associated
with this word. If it isn't in the WMap yet, create a new WordFeature. -}
getWordFeature :: WMap -> String -> WordFeature 
getWordFeature (_, _, m) str =
  maybe (WordFeature str 0 0) id (M.lookup str m)

{-| Wrapper for the WordFeature type constructor. -}
getWordFeat :: String -> Int -> Int -> WordFeature
getWordFeat w ham spam = WordFeature { word = w
                                     , hamCount = ham
                                     , spamCount = spam} 

{-| The basic probability that a WordFeature contains a spam word. -}
spamProb :: WMap -> WordFeature -> Float
spamProb (sc, hc, m) feat = 
    let spamFreq = fromIntegral (spamCount feat) / fromIntegral (max 1 sc)
        hamFreq = fromIntegral (hamCount feat) / fromIntegral (max 1 hc)
    in
      spamFreq / (spamFreq + hamFreq) 

{-| The Bayesean probability that a WordFeature contains a spam word --
i.e., the probability based on the probabilities of the other words in the message
being spam. -}
bayesSpamProb (sc, hc, m) feat = 
    let assumedProb =  0.5
        weight = 1
        basicProb = spamProb (sc, hc, m) feat
        dataPoints = fromIntegral (spamCount feat) + fromIntegral (hamCount feat)
    in
      ((weight * assumedProb) + (dataPoints * basicProb)) / (weight + dataPoints)

{-| Produce a score for a list of WordFeatures representing an individual message. -}
score :: WMap -> [WordFeature] -> Float
score (sc, hc, m) feats = 
    let spamProbs = map (bayesSpamProb (sc, hc, m)) feats
        hamProbs = map (1.0-) spamProbs
        numProbs = length spamProbs + length hamProbs
        h = 1.0 - fisher spamProbs numProbs
        s = 1.0 - fisher hamProbs numProbs in
    ((1-h) + s) / 2.0

{-| Fisher's combined probability test. -}
fisher :: [Float] -> Int -> Float
fisher probs numProbs = inverseChiSquare 
                        (sum (map log probs) * negate 2.0) (2*numProbs)

{-| The inverse chi-squared function. -}
inverseChiSquare :: Float -> Int -> Float
inverseChiSquare value df = 
    if odd df then error "Degree must be even"
    else let m = value / 2.0
             e = exp 1
             sum = e ** negate m
             term = sum
             dfRange = take (df `div` 2) $ iterate (+1) 1
             (sum', term') = foldl (\(s,t) i -> let t' = t * (m/i) in
                                                (s+t', t')) (sum, term) dfRange
         in min sum' 1.0

