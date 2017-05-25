module DistanceConversions
( yardsToFeet
, feetToInches
, inchesToCentimetres
, chainsToYards
) where

-- Define yards to feet
yardsToFeet ::  Float -> Float
yardsToFeet y = 3 * y

-- Define feet to inches
feetToInches :: Float -> Float
feetToInches f = 12 * f

-- Define inches to centimetres
inchesToCentimetres :: Float -> Float
inchesToCentimetres i = 2.54 * i

-- Define chains to yards
chainsToYards :: Float -> Float
chainsToYards ch = 22 * ch
