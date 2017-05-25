-- Fill in the DistanceConversions module first, and import it here
-- create a higher-order function for converting an area between two dimensions
-- this will take the function for converting a distance, and an area to convert
-- using the functions defined in the DistanceConversions module
-- Example areaConv inchesToCentimetres 9 = 58.0644
import qualified DistanceConversions as DC

areaConv :: (Float -> Float) -> Float -> Float
areaConv linearConversion = linearConversion . linearConversion

-- define a function for converting square inches into square centimetres
sqInToSqCm :: Float -> Float
sqInToSqCm = areaConv DC.inchesToCentimetres

-- define a function for converting square chains (22 yards) to square metres
sqChainsToSqM :: Float -> Float
sqChainsToSqM = areaConv chainsToM
  where
    chainsToM ch = 0.01 * chainsToCm ch
    chainsToCm = DC.chainsToYards . DC.yardsToFeet . DC.feetToInches . DC.inchesToCentimetres
