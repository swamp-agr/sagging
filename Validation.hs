{-# LANGUAGE FlexibleContexts #-}
module Validation where

import Data.List (sortBy, elemIndex, zip4, unzip4, nub)
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)
import Prelude hiding (putStr)


data AConf = Digit | Integer | Positive | NonNegative | Comparing | Intervaling deriving (Eq, Show, Ord)

data Value = String | Double deriving (Show, Eq, Read, Ord)
data State = Disabled | Active deriving (Show, Ord, Eq)

data InitialValue = InitialValue 
                    { stringValue :: String
                    , labelName :: String  
                    , initState :: State  
                    , validation :: [AConf] 
                    } deriving (Show, Eq, Ord) 

data InitialForce = InitialForce { coord' :: InitialValue
                                 , force' :: InitialValue
                                 } deriving (Show, Eq, Ord)

data InitialLine = InitialLine { coord'' :: InitialValue
                               , lineCounter :: InitialValue
                               , lineType :: InitialValue
                               , bushing :: InitialValue
                               , traverse :: InitialValue
                               } deriving (Show, Eq, Ord)

data InitialConditions = InitialConditions 
                         { iLine :: InitialValue
                         , saggingCoeff :: InitialValue
                         , extremWinterTemp :: InitialValue
                         , extremSummerTemp :: InitialValue
                         , iceDepth :: InitialValue
                         , mountingTemp :: InitialValue
                         , windPressure :: InitialValue
                         , fiberType' :: InitialValue
                         , fiberSpacing :: InitialValue
                         , lines' :: [InitialLine]
                         , forceCounter :: InitialValue
                         , forces :: [InitialForce]
                         , errors :: [Maybe String]
                         , forceError :: Maybe String
                         } deriving (Show, Eq, Ord)

type ForceError = Maybe String 


toD' :: String -> Maybe Value
toD' a 
  | isNumeric a == False = Nothing
  | otherwise = Just (read a :: Value)


isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False
 
isDouble :: String -> Bool
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False
 
isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s
                               
{-initialConditions a b c = 
  mapM (\a1 b1 c1 -> initialConditions' a1 b1 c1 f) a b c
  where f = b !! 0-}

{- initialConditions' a b c d : 
a - лейбл виджета
b - значение виджета
c - массив проверок
d - значение длины пролёта
-}
initialConditions' :: [Char] -> String -> [AConf] -> String -> Maybe String
initialConditions' a "" _ _ = Just ("Величина " ++ a ++ " не задана.")
initialConditions' a b [] _ | b /= "" = Nothing
initialConditions' _ _ _ e | isNumeric e == False = Just ("Величина Длина пролёта не задана.")
initialConditions' a b (Digit:ds) e 
  | isNumeric b == True  = initialConditions' a b ds e
  | isNumeric b == False = Just ("Величина " ++ a ++ " не является числом.")
initialConditions' a b (Integer:ds) e                              
  | isInteger b == True  = initialConditions' a b ds e
  | isInteger b == False = Just ("Величина " ++ a ++ " не является целым числом.")
initialConditions' a b (Positive:ds) e
  | toD b > 0 = initialConditions' a b ds e
  | toD b <= 0 = Just ("Величина " ++ a ++ " не является положительным числом.")
initialConditions' a b (NonNegative:ds) e
  | toD b >= 0 = initialConditions' a b ds e
  | toD b < 0 = Just ("Величина " ++ a ++ " не является неотрицательным числом.")
initialConditions' a b (Comparing:[]) e
  | toD b <= toD e && ((isNumeric e) == True) = Nothing
  | toD b >  toD e && ((isNumeric e) == True) = Just ("Величина " ++ a ++ " больше длины нити.")
initialConditions' "Коэффициент провисания" b (Intervaling:[]) e
  | toD b <= 60 && toD b >= 40 = Nothing
  | toD b >  60 || toD b <  40 = Just ("Величина Коэффициент провисания неправильно задана.")
initialConditions' "Тип провода 1..6" b (Intervaling:[]) e
  | toD b <= 6 && toD b >= 1 = Nothing
  | otherwise = Just ("Величина Тип провода 1..6 неправильно задана.")
initialConditions' "Число сил" b (Intervaling:[]) e
  | toD b <= 10 && toD b >= 0 = Nothing
  | otherwise = Just ("Величина Число сил неправильно задана.")

toD :: String -> Double
toD a = read a :: Double

                
-- a - массив сил
-- e - число активных сосредоточенных сил
forceConditions :: [InitialForce] -> [InitialLine] -> InitialValue -> Maybe String
forceConditions a b e 
  | length e' == 0 = Just ("Величина Число сил не задана.")
  | isNumeric e' == False = Just ("Величина Число сил не задана.")
  | g > 0 && g /= (read e' :: Int)  && (isNumeric e' == True) =  Just ("Нажмите кнопку \"Добавить\".")
  | g > 0 && g == (read e' :: Int) && (isNumeric e' == True) = Nothing
  | length h' > 0 = Nothing
  | length h == 0 && length h' == 0 = Just ("Для продолжения необходимо задать нагрузку.")
    where h = zip (f $ map coord' a) (f $ map force' a)
          h' = f $ map coord'' b
          e' = stringValue e
          g = length h
          f = filter ((/= Disabled) . initState)


initRestrictions = [ [Digit, Positive], [Integer, Positive, Intervaling]
                   , [Digit], [Digit], [Digit, Positive], [Digit, Positive]
                   , [Digit, Positive], [], [Digit, NonNegative]
                   , [Integer, NonNegative, Intervaling] 
                   ]                         

lineRestrictions = [ [Digit, Positive, Comparing]  
                   , [Integer, Positive]  
                   , [Integer, Positive, Intervaling] 
                   , [Digit, Positive] 
                   , [Digit, Positive] 
                   ]

forceRestrictions = [ [Digit, Positive, Comparing]
                    , [Integer, Positive] ]

filterActive (_,_,a,_)  
  | a == Disabled = False
  | otherwise     = True
