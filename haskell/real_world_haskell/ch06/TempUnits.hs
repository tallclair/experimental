module TempUnits where

class TempUnit t where
  toKelvin :: t -> Kelvin   -- ^ Convert the temperature unit to degrees Kelvin
  fromKelvin :: Kelvin -> t -- ^ Convert degrees Kelvin to the temperature unit
  convert :: (TempUnit u) => t -> u -- ^ Convert to an arbitrary temperature unit
  convert = fromKelvin . toKelvin

newtype Kelvin = K Int deriving (Eq, Ord, Read, Show)
instance TempUnit Kelvin where
  toKelvin = id
  fromKelvin = id

newtype Celcius = C Int deriving (Eq, Ord, Read, Show)
instance TempUnit Celcius where
  toKelvin (C x) = K (x - 274)
  fromKelvin (K x) = C (x + 274)

newtype Fahrenheit = F Int deriving (Eq, Ord, Read, Show)
instance TempUnit Fahrenheit where
  toKelvin (F x) = K (((x + 460) * 5) `div` 9)
  fromKelvin (K x) = F (((x * 9) `div` 5) - 460)
