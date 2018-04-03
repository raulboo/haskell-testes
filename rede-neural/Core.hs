module NeuralNetwork.Core
( NeuralNetwork (..)
, ActivFunction (..)
, mkNeuralNetwork
, sigmoid
, feedForwardNN
, trainNN
, trainBatchNN
, predictNN
, loadNN
, saveNN
) where

import System.Random
import Data.List
import Data.Maybe
import NeuralNetwork.Matrix

data NeuralNetwork = NeuralNetwork { layerMap :: [Int]
                                   , weightsAndBiases :: [[[Float]]]
                                   , activation :: ActivFunction
                                   , learningRate :: Float
                                   }

data ActivFunction = ActivFunction { name :: String
                                   , function :: Float -> Float
                                   , derivFunc :: Float -> Float
                                   }

sigmoid = ActivFunction "Sigmoid" sig sig'
  where sig x = 1 / (1 + (exp $ negate x))
        sig' y = y * (1 - y)

-- sigmoid :: Float -> Float
-- sigmoid x = 1 / (1 + (exp $ negate x))
-- sigmoid' :: Float -> Float
-- sigmoid' y = y * (1 - y)

mkNeuralNetwork :: [Int] -> ActivFunction -> Float -> Int -> NeuralNetwork
mkNeuralNetwork (x:[]) activFunc lr _ = NeuralNetwork [x] [] activFunc lr
mkNeuralNetwork nodes activFunc lr rgn = NeuralNetwork nodes weightsAndBiases activFunc lr
 where weightsAndBiases = map transpose [
         let layerNodes = nodes !! x
             nextLayerNodes = nodes !! (x+1)
         in [[let index = x*totalNodes + y*totalLayers + z
              in randomSeq !! index
         | z <- [0..(nextLayerNodes-1)]]
         | y <- [0..layerNodes]]
         | x <- [0..(totalLayers - 2)]]
       totalLayers = length nodes
       totalNodes = sum nodes
       randomSeq = randoms $ mkStdGen rgn :: [Float]

feedForwardNN :: Real a => NeuralNetwork -> [a] -> Maybe Int -> Maybe [Float]
feedForwardNN neural inputs stepsAmount
 | (length inputs + 1) /= (length . head . head $ wb) = Nothing
 | normalizedSteps > (length wb) = Nothing
 | otherwise = Just $ output wb (map realToFrac inputs)
               (function $ activation neural) 0 normalizedSteps
 where normalizedSteps = fromMaybe (length wb) stepsAmount
       wb = weightsAndBiases neural
       output :: [[[Float]]] -> [Float] -> (Float -> Float) -> Int -> Int -> [Float]
       output wbs inputs activFunc layer steps
         | layer == steps = inputs
         | otherwise = let layerWB = wbs !! layer
                           t_inputsMatrix = transpose [inputs ++ [1]]
                           outputs = map activFunc $ concat $ matrixMultiply
                                     layerWB t_inputsMatrix
                       in output wbs outputs activFunc (layer + 1) steps

backPropagationNN :: (Real a, Real b) => NeuralNetwork -> [a] -> [b] -> Maybe NeuralNetwork
backPropagationNN neural input expected
 | result == [] = Nothing
 | length expected /= length finalRes = Nothing
 | otherwise = Just $ NeuralNetwork (layerMap neural) newWb (activation neural)
               (learningRate neural)
 where newWb = zipWith matrixSum wb wbdeltas
       matrixSum = matrixOperation (+)
       wbdeltas = [zipWith (++) (w_deltas !! x) (transpose [gradient !! x])
                  | x <- [0..(length w_deltas - 1)]]
       w_deltas = [matrixMultiply (transpose [gradient !! x]) [result !! x]
                  | x <- [0..(length wb - 1)]]
       gradient = [let currentRes = result !! x
                   in map (*lr) $ zipWith (*) (map (/(sum currentRes)) . map devActFunc $ currentRes)
                   $ take (length $ wb !! (x-1)) (errors !! x)
                  | x <- [1..(length wb)]]
       lr = learningRate neural
       devActFunc = derivFunc $ activation neural
       errors = [concat $ defError wb output_errors x
                | x <- [0..(length wb)]]
       defError :: Real a => [[[Float]]] -> [a] -> Int -> [[Float]]
       defError wb output_errors layer
         | layer == length wb = transpose [map realToFrac output_errors]
         | otherwise = let currLay = wb !! layer
                       in matrixMultiply (transpose currLay) $ take
                       (length currLay) (defError wb output_errors $ succ layer)
       output_errors = zipWith subtract finalRes (map realToFrac expected)
       finalRes = last result
       result = [fromMaybe [] $ feedForwardNN neural input $ Just x | x <- [0..(length wb)]]
       wb = weightsAndBiases neural

defaultNN :: NeuralNetwork
defaultNN = mkNeuralNetwork [1] sigmoid 1 0

trainNN :: (Real a, Real b) => NeuralNetwork -> Int -> [a] -> [b] -> NeuralNetwork
trainNN nn 0 input expected = fromMaybe defaultNN
                             $ backPropagationNN nn input expected
trainNN nn steps input expected = trainNN (fromMaybe defaultNN
                                 (backPropagationNN nn input expected))
                                 (steps - 1) input expected

shuffleList :: [a] -> StdGen -> [a]
shuffleList [] _ = []
shuffleList xs rgn = (xs !! index) : (shuffleList (removeIndex index xs) $ snd randomNum)
  where index = fst randomNum
        randomNum = randomR (0, length xs - 1) rgn

removeIndex :: Int -> [a] -> [a]
removeIndex i xs = (fst splitXs) ++ sndPortion
  where sndPortion = drop 1 $ snd splitXs
        splitXs = splitAt i xs

trainBatchNN :: (Real a, Real b) => NeuralNetwork -> Int -> [([a], [b])] -> Int -> NeuralNetwork
trainBatchNN neural stepPerBt batchPairs rgn = executeBatches neural $ shuffleList
                                               (concatMap (replicate stepPerBt)
                                               batchPairs) (mkStdGen rgn)
  where executeBatches :: (Real a, Real b) => NeuralNetwork -> [([a], [b])] -> NeuralNetwork
        executeBatches nn shuffledBatchPairs = foldl (\acc x -> trainNN acc 1 (fst x) (snd x))
                                               nn shuffledBatchPairs


predictNN :: Real a => NeuralNetwork -> [a] -> Maybe [Float]
predictNN nn input = feedForwardNN nn input Nothing

loadNN :: String -> IO NeuralNetwork
loadNN filename = do
  file <- readFile filename
  let listOfArgs = lines file
      laymap = read $ listOfArgs !! 0
      wbs = read $ listOfArgs !! 1
      activ = if (listOfArgs !! 2) == "Sigmoid" then sigmoid
              else ActivFunction "" id (\x -> 1.0)
      lr = read $ listOfArgs !! 3
      nn = NeuralNetwork laymap wbs activ lr
  return nn

saveNN :: NeuralNetwork -> String -> IO ()
saveNN nn filename = do
  let laymap = (show $ layerMap nn) ++ "\n"
      wbs = (show $ weightsAndBiases nn) ++ "\n"
      activ = (name $ activation nn) ++ "\n"
      lr = (show $ learningRate nn ) ++ "\n"
      parsednn = laymap ++ wbs ++ activ ++ lr
  writeFile filename parsednn
  return ()
