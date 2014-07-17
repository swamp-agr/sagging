module Evaluation where

import Data.List (sortBy, elemIndex, elemIndices, transpose, nub)
import Validation                    
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)
import Prelude hiding (putStr)
import Text.Printf 
import GHC.Float

data PType = IsForce | IsLine deriving (Eq, Show, Ord)

data ProcessedLines = ProcessedLines 
                      { pCoord'' :: Double
                      , pLineCounter :: Double
                      , pLineType :: Double
                      , pBushing :: Double
                      , pTraverse :: Double
                      , pLState :: State
                      } deriving (Show, Eq, Ord)

data ProcessedForces = ProcessedForces                       
                       { pCoord' :: Double
                       , pForce' :: Double
                       , pFState :: State
                       , pType :: PType
                       , pNumLines :: Int
                       } deriving (Show, Eq, Ord)

data InputParams = InputParams 
                       { pLine :: Double
                       , pSaggingCoeff :: Double
                       , pXWinter :: Double
                       , pXSummer :: Double
                       , pIceDepth :: Double
                       , pMounting :: Double
                       , pWindPressure :: Double
                       , pFiberType' :: String
                       , pFiberSpacing :: Double
                       , pForceCounter :: Double
                       , pForces :: [ProcessedForces]
                       , pErrors :: [Maybe String]
                       } deriving (Show, Eq, Ord)


processInitConditions :: InitialConditions -> InputParams
processInitConditions a 
  | (nub $ filter (/= (Just "")) $ filter (/= Nothing) $ (errors a ++ [(forceError a)])) == [] = 
        InputParams (process $ iLine a) (process $ saggingCoeff a) (process $ extremWinterTemp a)
        (process $ extremSummerTemp a) (process $ iceDepth a) (process $ mountingTemp a) 
        (process $ windPressure a) (stringValue $ fiberType' a) (process $ fiberSpacing a)
        (process $ forceCounter a) 
        (sortForces
          ((filter ((/= Disabled) . pFState) $ map processForce $ forces a) 
          ++ (map 
              (lineToForce (process $ iceDepth a) 
               (process $ fiberSpacing a)
              ) $ filter ((/= Disabled) . pLState) $ map processLine $ lines' a))
        )
        ((errors a) ++ [(forceError a)])
  | otherwise = InputParams 0 0 0 0 0 0 0 "" 0 0 [ProcessedForces 0 0 Disabled IsForce 0] (nub $ filter (/= (Just "")) (errors a) ++ [(forceError a)])
    
process = toD . stringValue

processLine :: InitialLine -> ProcessedLines
processLine a = ProcessedLines (process $ coord'' a) (process $ lineCounter a)
                (process $ lineType a) (process $ bushing a) (process $ traverse a) 
                (head $ nub [ initState $ coord'' a, initState $ lineCounter a, initState $ lineType a
                            , initState $ bushing a, initState $ traverse a ])
                
processForce :: InitialForce -> ProcessedForces  
processForce a = ProcessedForces (process $ coord' a) ((* 9.8) . process $ force' a) 
                 (head $ nub [ initState $ coord' a, initState $ force' a ]) IsForce 1
                                                                                      

{- вычисляет силу от данных линии
a - линия
b - толщина слоя льда
c - расстояние между нитями
-}
lineToForce :: Double -> Double -> ProcessedLines -> ProcessedForces
lineToForce b c a = 
  ProcessedForces (pCoord'' a) f Active IsLine (round $ pLineCounter a)
  where izol = (pBushing a) * 9.8
        trav = (pTraverse a) * 9.8
        qprov = wire (pLineType a) wireWeight
        d = wire (pLineType a) wireOuterDiam
        outerDiam = iceWireDiam d b
        fibD = basalAreaD d
        outD = basalAreaD outerDiam
        iceD = outD - fibD
        qIce = linearLoad iceSpecWeight iceD 
        f = ( (qprov + qIce) * c + izol ) * (pLineCounter a) + trav

type OverLoadError = Maybe String

data AllParams = AllParams 
                 { dots :: Double
                 , _line :: Double
                 , _saggingCoeff :: Double
                 , _mTemp :: Double
                 , _wTemp :: Double
                 , _qMounting :: Double
                 , _approvedTension :: Double
                 , _Youngus :: Double
                 , _alpha :: Double
                 , _avgThreadWeight :: Double
                 , _diam :: Double
                 , _avgIceWeight :: Double
                 , _iceDepth :: Double
                 , _windPressure :: Double
                 , _sectionX :: Double
                 , _approvedForce :: Double
                 , _xWTemp :: Double
                 , _xSTemp :: Double
                 , _vertReactA :: Double
                 , _vertReactB :: Double
                 , _horizReact :: Double
                 , _iceTemp :: Double
                 , _threadFullLength :: Double
                 , _saggEquiv :: Double
                 , _arrowEquiv :: Double
                 , _qEquiv :: Double
                 , _fullQ :: Double
                 , _qThread :: Double
                 , _fiberType :: String
                 , _force :: [ProcessedForces]
                 , _saggingLine :: ([Double],[Double])
                 } deriving (Eq)


instance Show AllParams where
  show a = concat $ map (++"\n") [ "1. Количество точек на параболе: " ++ (show $ dots a)
                                   , "2. Длина пролёта: " ++ (show $ _line a)
                                   , "4. Коэффициент провисания: " ++ (show $ _saggingCoeff a)
                                   , "5. Температура первого состояния (при монтаже): " ++ (show $ _mTemp a)
                                   , "6. Температура второго состояния: " ++ (show $ _wTemp a)
                                   , "7. Нагрузка q1 первого состония: " ++ (show $ _qMounting a)
                                   , "9. Допускаемое напряжение: " ++ (show $ _approvedTension a)
                                   , "10. Модуль Юнга: " ++ (show $ _Youngus a)
                                   , "11. Коэффициент температурного расширения: " ++ (show $ _alpha a)
                                   , "12. Удельный вес нити: " ++ (show $ _avgThreadWeight a)
                                   , "13. Диаметр нити: " ++ (show $ _diam a)
                                   , "14. Удельный вес льда: " ++ (show $ _avgIceWeight a)
                                   , "15. Толщина слоя льда: " ++ (show $ _iceDepth a)
                                   , "16. Сила давления ветра: " ++ (show $ _windPressure a)
                                   , "17. Координата сечения x, где Q = 0: " ++ (show $ _sectionX a)
                                   , "18. Допускаемая сила, Н: " ++ (show $ _approvedForce a)
                                   , "19. Экстремальная температура зимой: " ++ (show $ _xWTemp a)
                                   , "20. Экстремальная температура летой: " ++ (show $ _xSTemp a)
                                   , "21. Вертикальная реакция левой опоры Ra: " ++ (show $ _vertReactA a)
                                   , "22. Вертикальная реакция правой опоры Rb: " ++ (show $ _vertReactB a)
                                   , "23. Горизонтальная реакция (сила натяжения нити, Н): " ++ (show $ _horizReact a)
                                   , "24. Температура обледенения, градусы: " ++ (show $ _iceTemp a)
                                   , "27. Длина подвешенной нити с сосредоточенной нагрузкой: " ++ (show $ _threadFullLength a)
                                   , "28. Коэффициент провисания, вычисленный для эквивалентного состояния: " ++ (show $ _saggEquiv a)
                                   , "29. Стрела провисания в эквивалентном состоянии: " ++ (show $ _arrowEquiv a)
                                   , "30. Погонная нагрузка в эквивалентном состоянии: " ++ (show $ _qEquiv a)
                                   , "32. Полная погонная нагрузка (с ветром и льдом) в схеме с сосредоточенной нагрузкой: " ++ (show $ _fullQ a)
                                   , "33. Погонная нагрузка от веса проволоки: " ++ (show $ _qThread a)
                                   ]

getAllParams :: InputParams -> AllParams
getAllParams a =
  AllParams { dots = parDots
            , _line = (pLine a)
            , _saggingCoeff = (pSaggingCoeff a)
            , _mTemp = (pMounting a)
            , _wTemp = (-40.0)
            , _qMounting = q1
            , _approvedTension = (fib (pFiberType' a) fiberPress)
            , _Youngus = modulusYoung
            , _alpha = (fib (pFiberType' a) fiberAlpha)
            , _avgThreadWeight = (fib (pFiberType' a) fiberSpecWeight)
            , _diam = (fib (pFiberType' a) fiberDiam)
            , _avgIceWeight = iceSpecWeight
            , _iceDepth = (pIceDepth a)
            , _windPressure = (pWindPressure a)
            , _sectionX = tsx
            , _approvedForce = (fib (pFiberType' a) kFiberAvgForce)
            , _xWTemp = (pXWinter a)
            , _xSTemp = (pXSummer a)
            , _vertReactA = rA
            , _vertReactB = rB
            , _horizReact = tens
            , _iceTemp = iceTemp
            , _threadFullLength = al
            , _saggEquiv = saggEq
            , _arrowEquiv = arrowEq
            , _qEquiv = lloadEquiv
            , _fullQ = fl
            , _qThread = tL
            , _fiberType = (pFiberType' a)
            , _force = (pForces a)
            , _saggingLine = sl
            } 
  where q1 = totalLinearLoad' (pIceDepth a) (pFiberType' a) (pWindPressure a)
        sumForce = totalForce (pForces a)
        sumMomF = totalMomentum (pForces a)
        rA = vertRefReactL sumForce q1 (pLine a) rB
        rB = vertRefReactR sumMomF q1 (pLine a)
        tsx = totalSectionX' (pLine a) parDots (pForces a) rA q1
        tens = tension parDots (pLine a) (pSaggingCoeff a) q1 (pForces a)
        al = arcLength (pLine a) parDots sl 
        sl = saggingLine (pLine a) parDots q1 (pForces a) tens 
        saggEq = saggingEquiv' (pLine a) al
        arrowEq = saggingV (pLine a) saggEq
        lloadEquiv = linearLoadEquiv (pLine a) saggEq tens
        fl = fullLoad (pLine a) (pFiberType' a) (pIceDepth a) (pWindPressure a) q1
        tL = linearLoad (fib (pFiberType' a) fiberSpecWeight) 
             (basalAreaD $ fib (pFiberType' a) fiberDiam)




sortForces a = sortBy compLength a
 where compLength a b  
         | (/=) (pCoord' a) (pCoord' b) = compare (pCoord' a) (pCoord' b)
         | otherwise = compare (pForce' a) (pForce' b)
       
-- Провода
wireType = [1 .. 6] :: [Double] -- Тип провода
wireMark = ["АП-16мм²","АП-25мм²","АП-35мм²","АПВ-16мм²"
           ,"АПВ-25мм²","АП-26мм²"] -- Маркировка провода
wireWeight = [0.44, 0.68, 0.95, 0.69, 1.08, 1.4] -- Погонный вес, Н/м
wireOuterDiam = [5.1, 6.4, 7.5, 6.9, 8.6, 9.8] -- Наружный диаметр, мм

-- Нити
fiber = [ "Стальная, 4.0", "Оцинков., 4.3"
        , "Высокопроч., 5.0", "Стальная, 6.0"] -- вид проволоки
fiberType = [1..4]                             -- Тип нити
kFiberForce = [6.324, 14.52, 24.48, 12.46]     -- P_p, кН
kFiberAvgForce = [2110.0, 4840.0, 8160.0, 4150.0]      -- [P], Н
fiberDiam = [4.0, 4.3, 5.0, 6.0]               -- диаметр, мм
fiberPress = [168.0, 333.0, 416.0, 147.0]              -- avgSigma, МПа
fiberEPress = replicate 4 200000               -- E, МПа
fiberAlpha = replicate 4 1.2e-5                -- 1/град
fiberSpecWeight = replicate 4 78.0             -- Удельный вес, кН/м3
fiberDelta = [26.25, 3.72, 10, 16.02]          -- delta, %
fiberPsi = [53.04, 12.58, 37.92, 56.48]        -- psi, %
fiberSigma = [503.42, 1000.4, 1247, 440.8]     -- sigma, MPa

fib :: [Char] -> [a] -> a
fib a b = b !! (head c)
  where c = elemIndices a fiber

wire :: Double -> [a] -> a
wire a b = b !! (head c)
  where c = elemIndices a wireType

parDots = 101 :: Double -- число точек на параболе
levelDegree = 0 -- Разность уровней точек подвеса
iceTemp = (-5) :: Double -- температура монтажа, гр. Цельсия
iceSpecWeight = 9 :: Double -- удельный вес льда, кН/м3
modulusYoung = 200000 :: Double

-- Вычисление наружного диаметра провода со льдом, мм
-- a - диаметр нити, мм
-- b - толщина слоя льда, мм
iceWireDiam a b = a + 2 * b

-- вычисление площади поперечного сечения, мм2
-- a - диаметр
basalAreaD a = pi * a**2 / 4

-- вычисление площади сечения льда
-- a - диаметр нити
-- b - толщина льда
basalIceAreaD a b = (basalAreaD $ iceWireDiam a b) - (basalAreaD a)

-- вычисление погонной нагрузки 
-- a - удельный вес
-- b - площадь
linearLoad a b = a * b / 1000

-- вычисление полной погонной нагрузки
-- a - нагрузка от веса проволоки
-- b - нагрузка от веса льда
-- c - нагрузка от ветра
totalLinearLoad :: Double -> Double -> Double -> Double
totalLinearLoad a b c = sqrt $ (a + b)**2 + c**2

totalLinearLoad' iceDepth line windPressure = 
  let d = fib line fiberDiam -- диаметр 
      outerD = iceWireDiam d iceDepth
      sL = basalAreaD d             
      sSection = basalIceAreaD d iceDepth
      ql = linearLoad iceSpecWeight sSection
      qstal = linearLoad (fib line fiberSpecWeight) sL
      qvetra = linearLoad windPressure outerD
  in totalLinearLoad qstal ql qvetra

-- вычисление суммы сил
-- a - массив сил
totalForce :: [ProcessedForces] -> Double
totalForce a = foldl (+) 0 b 
  where b = map pForce' a

-- вычисление суммы моментов 
-- a - массив сил
totalMomentum :: [ProcessedForces] -> Double
totalMomentum a = foldl (+) 0 sc
                  where sc = scalar xs ys
                        xs = map pCoord' a
                        ys = map pForce' a
                        
-- вычисление вертикальной реакции правой опоры
-- a - сумма моментов                        
-- b - погонная нагрузка в состоянии 1                        
-- c - длина пролёта                        
vertRefReactR a b c = (a / c) + (c * b / 2)

-- вычисление вертикальной реакции левой опоры
-- a - сумма сил
-- b - погонная нагрузка в состоянии 1
-- c - длина пролёта 
-- d - вертикальная реакция правой опоры
vertRefReactL a b c d = a + (b * c) - d

-- вычисление длины шага
-- a - длина пролёта
-- b - число точек на параболе
stepV :: Double -> Double -> Double
stepV a b = (/) a $ b - 1

-- вычисление x сечения, где Q = 0
-- a - длина пролёта
-- b - число точек на параболе
-- c - массив сосредоточенных сил
-- d -- вертикальная реакция левой опоры
-- e -- погонная нагрузка в состоянии 1
totalSectionX' :: Double -> Double -> [ProcessedForces] -> Double -> Double -> Double
totalSectionX' a b c d e = 
  (x + step)
  where step = stepV a b
        xList = map (sectionX step) [1 .. b]
        xForceList = map (\l -> (l,c)) xList
        xFilteredList = map (\(a1,b1) -> (a1, takeWhile ((<= a1) . pCoord') b1)) xForceList
        xFolded = map (\(a1,b1) -> (a1, foldl (+) 0 (map pForce' b1))) xFilteredList
        xSectionForce = map (transverse d e) xFolded
        x = fst $ last $ takeWhile ((> 0) . snd) xSectionForce

compareForce ([],[]) a = (a,([],[]))
compareForce ((b:[]),(c:[])) a = (a,([b],[c]))
compareForce ((b:bs),(c:cs)) a 
  | (a >= b)  = (a,([b],[c])) +++ compareForce (bs,cs) a
  | otherwise = (a,([b],[0])) +++ compareForce (bs,cs) a 
                
(+++) (a, (b1, c1)) (b, (b2, c2)) = (a, (b1 ++ b2, c1 ++ c2))

leftSumForce (a,(_,b)) = (a,d)
  where d = foldl (+) 0 b

transverse e f (a,d) = (a, e - f * a - d)



-- вычисление координаты сечения
-- b - длина шага        
sectionX :: Num a => a -> a -> a
sectionX a b = a * (b - 1)

-- вычисление провисания (провеса)
-- a - длина нити        
-- b - коэффициент провисания        
saggingV a b = a / b 

-- вычисление момента от правой опорной реакции
-- a - вертикальная реакция правой опоры
-- b - Координата X сечения
momentumVertRefReactL a b = a * b

-- вычисление момента от погонной нагрузки
-- a - погонная нагрузка для состояния 1
-- b - координата Х сечения
momentumLinearLoad :: Double -> Double -> Double
momentumLinearLoad a b = (a * b**2) / 2

-- вычисление суммы моментов от сосредоточенных сил 
-- a - массив сил
-- b - координата Х сечения
totalMomentum' :: [ProcessedForces] -> Double -> Double
totalMomentum' a b = 
  foldl (+) 0 sc
  where sc = scalar xs ys
        xs = map ((+b) . negate . pCoord')  a'
        ys = map pForce' a'
        a' = filter ((<= b) . pCoord') a

scalar [] [] = []
scalar (a:[]) (b:[]) = [a*b]
scalar (a:as) (b:bs) = [a*b] ++ scalar as bs

-- вычисление суммарного момента 
-- a - момент от правой опорной реакции                        
-- b - момент от погонной нагрузки
-- c - сумма моментов от сосредоточенных сил
totalMomentum'' a b c = a - b - c

-- вычисление силы натяжения нити
-- x1 - число точек на параболе
-- x2 - длина провеса
-- x4 - коэффициент провисания
-- q - погонная нагрузка
-- forceList - список пар координат и величин сил
--tension :: (Integral a) => a -> Double -> Double -> Double -> Double -> [Char] -> [(Double, Double)] -> Double
tension points lineLength saggingCoeff q forceList = 
  sumMom / proves
  where sumMom = totalMomentum'' momRa momQ sumMomForce
        proves = saggingV lineLength saggingCoeff
        momRa = momentumVertRefReactL rA xCoord
        momQ = momentumLinearLoad q xCoord
        sumMomForce = totalMomentum' forceList xCoord
        xCoord = totalSectionX' lineLength points forceList rA q
        rA = vertRefReactL sumForce q lineLength rB
        rB = vertRefReactR sumMomF q lineLength
        sumForce = totalForce forceList
        sumMomF = totalMomentum forceList

-- вычисление массива пар для линии провеса
-- a - длина провода        
-- b - число точек на параболе        
-- d - погонная нагрузка в состоянии 1        
-- e - массив координат и сил        
-- f - натяжение нити        
saggingLine :: Double -> Double -> Double -> [ProcessedForces] -> Double -> ([Double],[Double])
saggingLine a b d e f = 
  unzip $ zip x1 x2
  where x1 = map ((/ f) . negate) x3
        x2 = map (sectionX $ stepV a b) [1 .. b]
        x3 = map sum $ transpose [x4, x5, x6]
        x4 = map (*vrl) x2
        x5 = map (negate . (/ 2) . (*d) . (**2)) x2
        x6 = map (negate . totalMomentum' e) x2
        vrl = vertRefReactL (totalForce e) d a vrr
        vrr = vertRefReactR (totalMomentum e) d a


-- вычисление массы по весу
weightFromForce = (/ 9.8)

-- вычисление длины линии провеса
-- a - длина линии нити
-- b - число шагов
-- c - массив координат линии нити
saggingLength a b c = 
  foldl (+) 0 dl
  where dx = stepV a b
        x0 = head c
        dl = map ((*dx).(**(-1)).cos.(/ dx).(+ negate x0)) $ tail c
        
-- расчёт длины линии провеса
-- a - длина провода        
-- b - число точек на параболе        
-- c - x-координаты линии провеса        
-- d - y-координаты линии провеса        
arcLength :: Double -> Double -> ([Double],[Double]) -> Double
arcLength a b (c,d) =
  foldl (+) 0 dl
  where dx = stepV a b
        dy = map (\(a1,b1) -> a1 - b1) $ zip (tail c) (init c)
        dl = map ((/) dx . cos . atan . (/ dx)) dy


-- вычисление величины провеса параболы
-- (коэффициента, при котором длина нити
-- с сосред. нагрузками равна длине параболы)
-- в эквивалентном состоянии
-- a - длина линии провеса
-- b - длина нити
saggingEquiv' a b = 
  t
  where x = map ((+a) . (*8) . (*a) . (**(-1)) . (*3) . (**2)) y
        y = [10, 10.001 .. 100]
        z = zip x y
        t = snd $ last $ takeWhile ((>= b) . fst) z
         

-- вычисление величины провеса параболы
-- (коэффициента, при котором длина нити
-- с сосред. нагрузками равна длине параболы)       
-- a - длина нити        
-- b - число точек на параболе
-- fl - массив сил и координат
-- h - натяжение нити, tension0        
-- q1 - погонная нагрузка в состоянии 1
saggingEquivFull :: Double -> Double -> [ProcessedForces] -> 
                    Double -> Double -> Double
saggingEquivFull a b fl h q1 = 
  saggingEquiv' a al
  where al = arcLength a b sl 
        sl = saggingLine a b q1 fl h
        

-- вычисление эквивалентной погонной погрузки в 1 сост
-- l - длина нити        
-- k - провес параболы в эквив. состоянии
-- h - натяжение нити, горизонтальная сила       
linearLoadEquiv l k h = 8 * f * h / l**2
  where f = l / k

-- вычисление эквивалентной погонной нагрузки для 1 сост
-- l - длина нити
-- lType - тип нити
-- iceDepth - толщина слоя льда
-- windPressure - давление ветра
-- q1 - эквивалентная погонная нагрузка в состоянии 1
linearLoadEquiv2 l lType iceDepth windPressure q1 = 
  qstal + q1 - qq
  where qq = sqrt $ vertQ**2 + qW**2 -- общая нагрузка
        qW = windPressure * outerDiam / 1000 -- погонная нагрузка от ветра
        vertQ = qstal + qI -- вертикальная погонная нагрузка
        qstal = (fib lType fiberSpecWeight) * sL / 1000 -- погонная нагрузка от веса провода
        qI = iceSpecWeight * sI / 1000 -- погонная нагрузка от веса льда
        sL = pi * d**2 / 4 -- площадь провода
        d = fib lType fiberDiam -- диаметр провода
        sI = sC - sL -- площадь поверхности сечения льда
        outerDiam = d + 2 * iceDepth -- наружный диаметр
        sC = pi * outerDiam ** 2 / 4  -- общая площадь сечения провода со льдом

fullLoad l lType iceDepth windPressure q1 =
  qq
  where qq = sqrt $ vertQ**2 + qW**2 -- общая нагрузка
        qW = windPressure * outerDiam / 1000 -- погонная нагрузка от ветра
        vertQ = qstal + qI -- вертикальная погонная нагрузка
        qstal = (fib lType fiberSpecWeight) * sL / 1000 -- погонная нагрузка от веса провода
        qI = iceSpecWeight * sI / 1000 -- погонная нагрузка от веса льда
        sL = pi * d**2 / 4 -- площадь провода
        d = fib lType fiberDiam -- диаметр провода
        sI = sC - sL -- площадь поверхности сечения льда
        outerDiam = d + 2 * iceDepth -- наружный диаметр
        sC = pi * outerDiam ** 2 / 4  -- общая площадь сечения провода со льдом

-- вычисление напряжения в 1-м состоянии
-- a - усилие в нити
-- b - диаметр нити, мм
sigma1 a b = a / (basalAreaD b)

-- вычисление напряжения в 2-м экв. состоянии. МПа
-- a - усилие в нити
-- b - диаметр нити, мм
-- q1 - погонная нагрузка q от сил 1-го состояния
-- q2 - погонная нагрузка q от сил 2-го состояния
-- t1 - температура при обледенении, град
-- t2 - экстремальная температура зимняя, град
-- e - модуль Юнга, н/мм2
-- alfa - коэффициент температурного расширения
-- l длина пролёта, мм
sigma2 :: Double -> [Char] -> Double -> Double -> Double -> Double -> Double -> Double
sigma2 a ltype q1 q2 t1 t2 l =
  xs + 0.1
  where x = map sum $ transpose [x1, (map negate x2), (map negate x3)]
        x3 = repeat (gamma2**2 * l'**2 * e / 24)
        temp = map (* 0.1) [1..]
        x2 = map ((*x4).(**2)) temp
        x1 = map (**3) temp
        x4 = sig1 - (gamma1**2 * l'**2 * e / (24 * sig1**2)) 
             - alfa * e * (t2 - t1)
        sig1 = sigma1 a b
        gamma1 = q1 / (basalAreaD b) / 1000
        gamma2 = q2 / (basalAreaD b) / 1000
        l' = l * 1000
        xs = last $ fst $ unzip $ zip temp (takeWhile (<0) x)
        alfa = fib ltype fiberAlpha
        b = fib ltype fiberDiam
        e = modulusYoung

-- вычисление силы натяжения в 2-м экв. состоянии. МПа
-- a - напряжение в 1-м экв. состоянии
-- b - диаметр нити, мм
-- q1 - погонная нагрузка q от сил 1-го состояния
-- q2 - погонная нагрузка q от сил 2-го состояния
-- t1 - температура при обледенении, град
-- t2 - экстремальная температура зимняя, град
-- e - модуль Юнга, н/мм2
-- alfa - коэффициент температурного расширения
-- l длина пролёта, мм
force2Condition a ltype q1 q2 t1 t2 l = 
  sig2 * sL
  where sig2 = sigma2 a ltype q1 q2 t1 t2 l  
        sL = basalAreaD d
        d = fib ltype fiberDiam

     

-- вычисление стрелы провисания в экв состоянии 2
-- l - длина нити
-- e - модуль Юнга
-- alfa - коэффициент температурного расширения
-- d - диаметр
-- k - коэффициент провисания эквивалентного расстояния
-- t1 - температура обледенения
-- t2 - температура экстремальная зимняя
-- q1 - погонная нагрузка q от сил 1-го состояния
-- q2 - погонная нагрузка q от сил 1-го состояния
--f2 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
f2 l alfa d k t1 t2 q1 q2 =
  xs
  where x = map sum $ transpose [x1,  (map negate x2), (map negate x3)]
        x3 = repeat (3 * q2 * l**4 / (64 * e' * (basalAreaD d') ) )
        temp = map (* 0.0001) [1.0, 2.0..]
        x2 = map (*x4) temp
        x1 = map (**3) temp
        x4 = f1**2 + 0.375 * alfa * l**2 * (t2 - t1) 
             - 3 * q1 * l**4 / (64 * f1 * e' * (basalAreaD d'))
        xs = last $ fst $ unzip $ zip temp (takeWhile (< 0) x)
        f1 = saggingV l k -- стрела провисания для экв состояния 1
        e' = 1000000 * e
        d' = d / 1000
        e = modulusYoung
        
-- провес реальной схемы 
-- l - длина пролета
-- rst1 - провес реальной схемы в 1-м состоянии
-- d - диаметр нити, мм
-- k - коэффициент провеса эквивалентного состояния
-- t1 - температура при обледенении, градусы
-- t2 - температура экстремальная зимняя, градусы
-- q1 -- погонная нагузка q от сил 1-го состояния
-- q2 -- погонная нагрузка q от сил 2-го состояния
realSchemeTension2 :: Double -> Double -> [Char] -> Double -> Double -> Double -> Double -> Double -> Double
realSchemeTension2 l rst1 l1 k t1 t2 q1 q2 =
  rst1 * x43 / f1
  where x43 = f2 l alfa d k t1 t2 q1 q2
        f1 = l / k
        alfa = fib l1 fiberAlpha
        d = fib l1 fiberDiam
        
data OutParams = OutParams 
                 { _oh :: Double -- сила
                 , _of :: Double -- провес
                 , _ok :: Double -- коэффициент провисания
                 }  deriving (Show)
                 
data EquivParams = EquivP { _yk :: Double
                          , _yq1 :: Double
                          , _yq2 :: Double
                          , _yt1 :: Double
                          , _yt2 :: Double
                          }  deriving (Show)
                   
                   
getOut1 :: AllParams -> OutParams
getOut1 a = OutParams { _oh = tension (dots a) (_line a) (_saggingCoeff a) (_qMounting a) (_force a)
                      , _of = saggingV (_line a) (_saggingCoeff a)
                      , _ok = _saggingCoeff a
                      } 

getEquiv1 :: AllParams -> EquivParams
getEquiv1 a = EquivP { _yk  = _saggEquiv a
                       , _yq1 = _qEquiv a 
                       , _yq2 = linearLoadEquiv2 (_line a) (_fiberType a) (_iceDepth a) (_windPressure a) (_qEquiv a)
                       , _yt1 = _iceTemp a
                       , _yt2 = _xWTemp a
                       } 


getOut2 :: AllParams -> OutParams -> EquivParams -> OutParams
getOut2 a b c = OutParams { _oh = force2Condition (_oh b) (_fiberType a) (_yq1 c) (_yq2 c) (_yt1 c) (_yt2 c) (_line a)
                         , _of = f2' 
                         , _ok = saggingV (_line a) f2'
                         } 
  where f2' = realSchemeTension2 (_line a) (_of b) (_fiberType a) (_yk c)  (_yt1 c) (_yt2 c) (_yq1 c) (_yq2 c)

getEquiv2 :: AllParams -> EquivParams -> Double -> Double -> EquivParams
getEquiv2 a b c d = EquivP { _yk  = saggingV (_line a) f2'
                           , _yq1 = _yq1 b
                           , _yq2 = _yq2 b
                           , _yt1 = c
                           , _yt2 = d
                           } where f2' = f2 (_line a) (_alpha a) (_diam a) (_yk b) (_yt1 b) (_yt2 b) (_yq1 b) (_yq2 b)

data CResult = CResult { allParams :: AllParams
                       , equivParams :: [EquivParams]
                       , outParams :: [OutParams] 
                       } deriving (Show)

getOverload :: CResult -> OverLoadError
getOverload a 
  | (maximum $ take 2 $ map _oh $ outParams a) < 1.05 * (fib (_fiberType $ allParams a) kFiberAvgForce) = Nothing
  | (maximum $ take 2 $ map _oh $ outParams a) >= 1.5 * (fib (_fiberType $ allParams a) kFiberAvgForce) = 
    Just ("Переход на двухнитевую схему невозможен,\nт.к. расчётное значение силы натяжения нити " 
          ++ (show $ maximum $ take 2 $ map _oh $ outParams a) 
          ++ "\nпревышает в 1.5 раза допускаемое значение.")
  | otherwise = Just ("Переход на двухнитевую схему возможен,\n"
                      ++ "однако для определения значений силы тяжести и стрелы провеса"
                      ++ "\nпотребуется специальный расчёт")
