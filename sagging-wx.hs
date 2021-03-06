{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Applicative
import Graphics.UI.WX
import Graphics.UI.WXCore
import Validation
import Evaluation
import Control.Concurrent.Chan
import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)
import Data.List (transpose, intersperse)
import Text.Printf 
import Control.Concurrent

print' = putStr . fromString

data BValue = BValue1 { bVal1 :: String
                      , bMin1 :: Int
                      , bMax1 :: Int
                      }
            | BValue2 { bVal2 :: [String] }
              deriving Show

data BRow = BRow1 { bName1      :: String
                  , bDimension1 :: String
                  , bValue1     :: BValue
                  } 
          | BRow2 { bValue2 :: BValue
                  }
          | BRow3 { bValue3 :: [String] }
          | BRow4 { bName4      :: String
                  , bDimension4 :: String
                  , bValue4     :: [ BValue ]
                  }
          | BRow5 { bName5 :: String
                  , bDimension5 :: String
                  , bValue5 :: BValue
                  , bButtonText :: String  
                  }

data GRow = GRow1 { gName1 :: StaticText ()
                  , gDimension1 :: StaticText ()
                  , gValue1 :: SpinCtrl ()
                  } 
          | GRow2 { gValue2 :: RadioBox ()
                  }
          | GRow3 { gName3 :: [StaticText ()]
                  , gValue3 :: [CheckBox ()] }
          | GRow4 { gName4 :: StaticText ()
                  , gDimension4 :: StaticText ()
                  , gValue4 :: [ SpinCtrl () ]
                  } 
          | GRow5 { gName5 :: StaticText ()
                  , gDimension5 :: StaticText ()
                  , gValue5 :: SpinCtrl ()
                  , gButton5 :: Button ()
                  } deriving Show

val1 = BValue1 "" 0 1000
val2 = BValue1 "" 1 6

a1 = BRow1 "Длина пролёта" "м" val1
a2 = BRow1 "Коэффициент провисания" "о.в." (BValue1 "40" 40 60)
a3 = BRow1 "Экстр. температура зимой" "°С" (BValue1 "-30" (-50) (-10))
a4 = BRow1 "Экстр. температура летом" "°С" (BValue1 "40" 5 50)
a5 = BRow1 "Толщина слоя льда" "мм" (BValue1 "5" 0 100)
a6 = BRow1 "Температура монтажа" "°С" (BValue1 "5" (-50) 50)
a7 = BRow1 "Давление ветра" "Н/м" (BValue1 "206" 0 6000)
a8 = BRow2 (BValue2 fiber)
a9 = BRow1 "Расстояние между нитями" "м" val1
a10 = BRow3 $ map (("Линия "++) . show) [1 .. 4] 
a11 = BRow4 "Координата" "м" [ val1, val1, val1, val1 ]
a12 = BRow4 "Число проводов" "" [ val2, val2, val2, val2 ]
a13 = BRow4 "Тип провода 1..6" "" [ val2, val2, val2, val2 ]
a14 = BRow4 "Вес изолятора" "кг" [ val1, val1, val1, val1 ]
a15 = BRow4 "Вес траверса" "кг" [ val1, val1, val1, val1 ]
a16 = BRow5 "Число сил" "" (BValue1 "" 0 10) "Добавить"
a17 = BRow4 "Координата X[i]" "м" [ val1, val1, val1, val1, val1 ]
a18 = BRow4 "Сила P[i]" "кг" [ val1, val1, val1, val1, val1 ]
a19 = BRow4 "Координата X[i]" "м" [ val1, val1, val1, val1, val1 ]
a20 = BRow4 "Сила P[i]" "кг" [ val1, val1, val1, val1, val1 ]

rowToGUI ::  Window a -> BRow -> IO GRow
rowToGUI w (BRow1 a b c) = do
  _a <- staticText w [text := a]
  _b <- staticText w [text := b]  
  _c <- spinCtrl w (bMin1 c) (bMax1 c) [text := (bVal1 c)]
  return $ GRow1 _a _b _c
rowToGUI w (BRow2 a) = do
  _a <- radioBox w Vertical (bVal2 a) []
  return $ GRow2 _a
rowToGUI w (BRow3 a) = do
  _a <- mapM (\x -> checkBox w [text := x]) a
  _b <- mapM (\x -> staticText w [text := " "]) [1,2]
  return $ GRow3 _b _a
rowToGUI w (BRow4 a b c) = do
  _a <- staticText w [text := a]
  _b <- staticText w [text := b]
  _v <- mapM (\spin -> spinCtrl w (bMin1 spin) (bMax1 spin) [text := (bVal1 spin), enabled := False]) c
  return $ GRow4 _a _b _v
rowToGUI w (BRow5 a b c d) = do
  _a <- staticText w [text := a]
  _b <- staticText w [text := b]
  _v <- spinCtrl w (bMin1 c) (bMax1 c) [text := (bVal1 c)]
  _d <- button w [text := d]
  return $ GRow5 _a _b _v _d

rowToLayout :: GRow -> IO [Layout]
rowToLayout (GRow1 a b c) = do return $ [ widget a, widget b, minsize (sz 45 25) $ widget c ]
rowToLayout (GRow2 a) = do return $ [ widget a ]
rowToLayout (GRow3 a b) = do return $ (map widget a) ++ (map widget b)
rowToLayout (GRow4 a b c) = do 
  return 
    $ [ widget a, widget b ] 
    ++ map (minsize (sz 45 25) . widget) c
rowToLayout (GRow5 a b c d) = do return $ [ widget a, widget b, minsize (sz 45 25) $ widget c, widget d ]

enableByChange :: GRow -> [[SpinCtrl ()]] -> IO ()
enableByChange a b = do
  s <- get (gValue5 a) selection
  enableByIndex s b

enableByIndex :: Int -> [[SpinCtrl ()]] -> IO ()
enableByIndex a b 
  | a > 0 && a < 10 = do mapM_ (\y -> mapM_ (\z -> set (y !! z) [enabled := True]) [0 .. a]) $ concatF b
                         mapM_ (\y -> mapM_ (\z -> set (y !! z) [enabled := False]) [a .. 9]) $ concatF b
  | a == 10 = mapM_ (\x -> mapM_ (\y -> set y [enabled := True]) x) b
  | otherwise = mapM_ (\x -> mapM_ (\y -> set y [enabled := False]) x) b

concatF :: [[SpinCtrl ()]] -> [[SpinCtrl ()]]
concatF a = [(a !! 0) ++ (a !! 2), (a !! 1) ++ (a !! 3)]
concatRowF :: [GRow] -> [GRow]
concatRowF a = [ (GRow4 n1 d1 x), (GRow4 n2 d2 y) ]
  where list = concatF $ map gValue4 a
        n1 = gName4 (a !! 0)
        n2 = gName4 (a !! 1)
        d1 = gDimension4 (a !! 0)
        d2 = gDimension4 (a !! 1)
        x = head list
        y = last list

rowToInitBy (a,b,c) = rowToInit (a !! b) c
rowToInitLBy (a,b,c) = rowToInitL (a !! b) c

extractInitial wm init1 fiberType fiberDelta init2 forceCount init3
  = do t1 <- (mapM rowToInitBy $ zip3 (repeat $ init1 ++ [fiberType, fiberDelta
                                                         , forceCount]) [0..] 
                                              initRestrictions)
       t2 <- mapM rowToInitLBy $ zip3 (repeat init2) [0..] 
             lineRestrictions
             
       t3 <- mapM rowToInitLBy $ zip3 (repeat $ concatRowF init3) [0..] 
             forceRestrictions
       let initLines = map (\a -> InitialLine (a !! 0) (a !! 1) (a !! 2) (a !! 3) (a !! 4)) $ transpose t2
       let initForces = map (\a -> InitialForce (a !! 0) (a !! 1)) $ transpose t3
       let initConds = 
             InitialConditions (t1 !! 0) (t1 !! 1) (t1 !! 2) 
             (t1 !! 3) (t1 !! 4) (t1 !! 5) (t1 !! 6) (t1 !! 7) 
             (t1 !! 8) initLines (t1 !! 9) initForces 
             (map 
              (\a -> 
                initialConditions' (labelName a) (stringValue a) 
                (validation a) (stringValue (t1 !! 0)))
              $ filter ((/= Disabled) . initState) $
              t1 ++ map coord'' initLines ++ map lineCounter initLines
              ++ map lineType initLines ++ map bushing initLines
              ++ map traverse initLines ++ map coord' initForces
              ++ map force' initForces) 
             (forceConditions initForces initLines (t1 !! 9))
       let procParams = processInitConditions initConds
       let aParams = getAllParams procParams
           
       b1 <- destroyFrame wm
       print procParams
       getOutParams procParams b1

destroyFrame wm = do
  a <- takeMVar wm
  case a of
    Just b -> do close b
                 b1 <- frame []
                 putMVar wm (Just b1)
                 return b1
    Nothing -> do b1 <- frame []
                  putMVar wm (Just b1)
                  return b1

getOutParams :: InputParams -> Frame () -> IO ()
getOutParams a b
  | (filter (/= Nothing) $ pErrors a) /= [] = 
    do set b [text := "!!! ОШИБКА !!!", clientSize := sz 200 200]
       a' <- staticText b [text := concat $ intersperse "\n" $ (filter (/= "") $ map fromMaybe $ pErrors a)]
       b3 <- button b [ text := "Выход", on command := close b, position := Point 100 200]
       set b [ layout := margin 1 $ minsize (sz 200 200) $ column 1 [floatCenter $ minsize (sz 200 200) $ widget a', floatCenter $  widget b3 ] ]
       return ()
  | otherwise = 
    do print "1"
       let aParams = getAllParams a
           o1 = getOut1 aParams
           e1 = getEquiv1 aParams
           o2 = getOut2 aParams o1 e1
           e2 = getEquiv2 aParams e1 (_xWTemp aParams) (_xSTemp aParams)
           o3 = getOut2 aParams o2 e2
           e3 = getEquiv2 aParams e2 (_xSTemp aParams) (_mTemp aParams)
           o4 = getOut2 aParams o3 e3
           al = CResult aParams [ e1, e2, e3 ] [ o1, o2, o3, o4 ]
           oe = getOverload al
           print' :: (Show a) => a -> IO ()
           print' = putStr . fromString . (++ "\n\n") . show 
         in do print' aParams
               printResult al oe b
               {-print' o1
               print' e1
               print' o2
               print' e2
               print' o3
               print' e3
               print' o4-}
       return ()

rowToInit :: GRow -> [AConf] -> IO InitialValue
rowToInit (GRow1 a _ c) d = do
  _name <- get (a) text
  _value <- get (c) selection
  _state <- get (c) enabled
  return (InitialValue (show _value) _name (getState _state) d)
rowToInit (GRow2 a) b = do
  _select <- get a selection
  _list   <- get a items
  _state  <- get a enabled
  return (InitialValue (_list !! _select) "Тип нити" (getState _state)  b)
rowToInit (GRow5 a b c d) e = do
  _name <- get a text
  _value <- get c selection
  _state <- get c enabled
  return (InitialValue (show _value) _name (getState _state) e)

rowToInitL (GRow4 a _ c) d  = do
    _name <- get a text
    _value <- mapM (\x -> get x selection) c
    _state <- mapM (\x -> get x enabled) c
    return $ map (\(x,y) -> (InitialValue (show x) _name (getState y) d)) $ zip _value _state

getState :: Bool -> State
getState True  = Active
getState False = Disabled

main :: IO ()
main
  = start hello


hello 
  = do f     <- frame    [text := "Программа расчёта однонитевых тросовых подвесок сети наружного освещения", clientSize := sz 600 680]
--       p <- panel f [] 9
       title <- staticText f [text := "Программа расчёта однонитевых тросовых\n    подвесок сети наружного освещения"]
       windowMgr <- newEmptyMVar
       putMVar windowMgr (Nothing :: Maybe (Frame ()))
       r11 <- mapM (rowToGUI f) [ a1, a2, a3, a4, a5, a6, a7 ]
       l11 <- mapM rowToLayout r11
       r12 <- rowToGUI f a8
       r8 <- staticText f [text := "Тип нити"]
       l12 <- rowToLayout r12
       
       let rb = gValue2 r12

       r21 <- rowToGUI f a9
       l21 <- rowToLayout r21
       r22 <- mapM (rowToGUI f) [ a10, a11, a12, a13, a14, a15 ]
       l22 <- mapM rowToLayout r22
       
       r31 <- rowToGUI f a16
       l31 <- rowToLayout r31
       r32 <- mapM (rowToGUI f) [ a17, a18, a19, a20 ]
       l32 <- mapM rowToLayout r32

       eval <- button f [text := "Ок"]
       quit <- button f [text := "Выход", on command := close f]

       cb <- return $ gValue3 (head r22)
       _r21 <- return $ map gValue4 (tail r22)
       _r32 <- return $ map gValue4 r32
       
       mapM_ (\(a,b) -> set a [on command := mapM_ (\x -> set (x !! b) [enabled :~ not]) _r21]) $ zip cb [0..]
       set (gButton5 r31) [on command := enableByChange r31 _r32]

       set f [layout := minsize (sz 900 400) $ margin 10 $ column 5 [ floatCenter (widget title)
                                 , row 1 $ [boxed "Параметры" (floatLeft $ 
                                             row 2 [ grid 5 5 l11 
                                                   , grid 5 5 [[hspace 10]]
                                                   , grid 5 5 [ [widget r8], l12 ] 
                                                   ]
                                            )
                                 , boxed "Нагрузка на нить" (floatLeft $
                                             grid 5 5 [ [row 3 l21]
                                                      , [grid 5 5 l22]
                                                      ]
                                            )
                                 , boxed "Дополнительная нагрузка" (floatLeft $
                                             grid 5 5 [ [row 3 l31]
                                                      , [grid 5 5 l32]
                                                      ]
                                             )]
                                 , floatCenter $ row 1 [widget eval, widget quit]
                                 ] 
             ] 
        
       set eval [on command := extractInitial windowMgr r11 r12 r21 (tail r22) r31 r32]
       
fromMaybe :: (Maybe String) -> String
fromMaybe Nothing = ""
fromMaybe (Just a) = a

printResult res Nothing f = do 
  set f [text := "Результат", clientSize := sz 400 650]
  p <- panel f [on paint := drawItems res, position := pt 0 0, clientSize := sz 400 650]

  b1 <- button p [ text := "Печать"
                 , on command := fileSave f res
                 , position := pt 50 600]
  b2 <- button p [ text := "Выход"
                 , on command := close f
                 , position := pt 250 600
                 ]
  set f [layout := minsize (sz 400 650) $ hfill $ vfill $ widget p]

printResult _ err f = do
  set f [text := "!!! ОШИБКА !!!"]
  t <- staticText f [text := (fromMaybe err), clientSize := sz 300 200]
  b2 <- button f [ text := "Выход"
                 , on command := close f
                 , position := pt 100 200
                 ]
  set f [layout := minsize (sz 300 200) $ hfill $ vfill $ column 1 $ [floatCenter $ widget t, floatCenter $ widget b2]]


fileSave p res = 
  do t <- fileSaveDialog p True True "Сохранить" [("Ping File, *.png", [ 
                                                      "*.png"
                                                      , "*.tiff"
                                                      , "*.jpg"
                                                      ,  "*.bmp" ])] "" "example.png"
     mdc <- memoryDCCreate
     mdcbmp <- bitmapCreateEmpty (sz 400 600) (-1)
     memoryDCSelectObject mdc mdcbmp
     drawItems' res mdc
     
     case t of
       Just a -> do bitmapSaveFile mdcbmp (a) wxBITMAP_TYPE_PNG objectNull
                    return ()
       Nothing -> return ()
     print $ show t
     return ()

drawItems a dc _ = do
  drawItems' a dc 

drawItems' a dc = do
  polygon dc [ Point 0 0
             , Point 400 0
             , Point 400 650
             , Point 0 650
             ] 
    [ penCap := CapButt
    , penKind := PenSolid
    , color := rgb 255 255 255
    ]

  mapM_ (line' dc [ penCap := CapButt
                  , penKind := PenSolid
                  , penWidth := 3
                  , color := rgb 0 0 255
                  ]) [ (Point 40 50, Point 360 50) 
                     ]
  
  mapM_ (line' dc [ penCap := CapButt
                  , penKind := PenDash DashDot
                  , penWidth := 2
                  , color := rgb 0 0 0
                  ]) [ (Point 40 110, Point 360 110) 
                     , (Point 40 180, Point 360 180)
                     , (Point 40 250, Point 360 250)
                     , (Point 40 215, Point 360 215)
                     ]

  mapM_ (line' dc [penCap := CapButt, penKind := PenSolid, penWidth := 2, color := rgb 0 0 0]) 
    [ (Point 40 45, Point 40 115)
    , (Point 360 45, Point 360 115)
    , (Point 40 180, Point 40 250)
    , (Point 360 180, Point 360 250)
    ]
  mapM_ (text' dc ) [ ("При наиболее тяжёлых условиях", Point 90 270, [fontFamily := FontModern, fontShape := ShapeItalic])
                    , ("Линия провисания нити (троса)", Point 90 150, [fontFamily := FontModern, fontShape := ShapeItalic])
                    , ("Температура\t\t\t\t\t°С\t" ++ (show $ _iceTemp $ allParams a), Point 40 290, [fontFamily := FontModern])
                    , ("Толщина льда\t\t\t\tмм\t" ++ (show $ _iceDepth $ allParams a), Point 40 310, [fontFamily := FontModern])
                    , ("Давление ветра\t\t\t\tН/м\t" ++ (show $ _windPressure $ allParams a), Point 40 330, [fontFamily := FontModern])
                    , ("Сила натяжения нити\t\tН\t" ++ (roundToStr $ _horizReact $ allParams a), Point 40 350, [fontFamily := FontModern])
                    , ("Допускаемое значение силы\tН\t" ++ (show $ _approvedForce $ allParams a), Point 40 370, [fontFamily := FontModern])
                    , ("Условие прочности выполняется (допускается перегрузка до 5%)", Point 35 390, [fontFamily := FontModern, fontSize := 6, fontWeight := WeightBold])
                    , ("При экстр. низкой т-ре\t\t°С\t" ++ (show $ _xWTemp $ allParams a), Point 40 410, [fontFamily := FontModern, fontShape := ShapeItalic])
                    , ("Сила натяжения нити\t\tН\t" ++ (roundToStr $ _oh $ (outParams a) !! 0), Point 40 430, [fontFamily := FontModern])
                    , ("Допускаемое значение силы\tН\t" ++ (show $ _approvedForce $ allParams a), Point 40 450, [fontFamily := FontModern])
                    , ("Условие прочности выполняется (допускается перегрузка до 5%)", Point 35 470, [fontFamily := FontModern, fontSize := 6, fontWeight := WeightBold])
                    , ("Результаты расчёта жесткости (стрелы провисания)", Point 45 490, [fontFamily := FontModern, fontSize := 8, fontShape := ShapeItalic])
                    , ("\t\t\t\t1\t\t2\t\t3\t\t4", Point 20 510, [fontFamily := FontModern])
                    , ("Температура\t\t" ++ (show $ _iceTemp $ allParams a) 
                       ++ "\t\t" ++ (show $ _xWTemp $ allParams a) 
                       ++ "\t" ++ (show $ _xSTemp $ allParams a) 
                       ++ "\t\t" ++ (show $ _mTemp $ allParams a), Point 20 530, [fontFamily := FontModern])
                    , ("Коэфф. провеса\t" ++ (roundToStr $ _ok $ (outParams a) !! 0) 
                       ++ "\t" ++ (roundToStr $ _ok $ (outParams a) !! 1) 
                       ++ "\t" ++ (roundToStr $ _ok $ (outParams a) !! 2) 
                       ++ "\t" ++ (roundToStr $ _ok $ (outParams a) !! 3), Point 20 550, [fontFamily := FontModern])
                    , ("Провисание нити\t" ++ (roundToStr $ _of $ (outParams a) !! 0) 
                       ++ "\t\t" ++ (roundToStr $ _of $ (outParams a) !! 1) 
                       ++ "\t\t" ++ (roundToStr $ _of $ (outParams a) !! 2) 
                       ++ "\t\t" ++ (roundToStr $ _of $ (outParams a) !! 3), Point 20 570, [fontFamily := FontModern])
                    ]
  let ax = fst (_saggingLine $ allParams a)
      ay = snd (_saggingLine $ allParams a)
      lx = abs (head ax - max')
      max' = maximum $ map abs ax
      half' = max' / 2
      ly = abs (head ay - last ay)
      bx = map (floor . (+180.0) . (/ lx) . (* 70.0) . negate) ax
      by = map (floor . (+40.0) . (/ ly) . (* 320.0)) ay
      cz = zip by bx
      pts = map (\(x,y) -> Point x y) cz
    in do polyline dc pts [ penCap := CapButt
                       , penKind := PenSolid
                       , penWidth := 2
                       , color := rgb 0 0 0
                       ]
          mapM_ (text' dc) [ (roundToStr half', Point 8 205, [fontFamily := FontModern])
                           , (roundToStr max', Point 8 240, [fontFamily := FontModern])
                           ]
  text' dc ((++ " м") $ roundToStr $ _line $ allParams a, Point 175 90, [fontFamily := FontModern])
  -- отображение схематичных сил
  let aa = map (onLine . floor . (+40.0) . (/ l) . (* 320.0) . pCoord') (filter ((== IsForce) . pType) $ _force $ allParams a)
      l = (_line $ allParams a)
      onLine a = (Point a 30, Point a 50) 
    in do mapM_ (line' dc [penCap := CapButt, penKind := PenSolid, penWidth := 2, color := rgb 0 0 0]) aa
          mapM_ (arrowLine dc) aa
  -- отображение схематичных линий     
  let lforces = filter ((== IsLine) . pType) $ _force $ allParams a
      l = (_line $ allParams a)
      aa = map (onLine . floor . (+40.0) . (/ l) . (* 320.0) . pCoord') lforces
      crossLine a = map (\d -> (Point (floor $ c + 7 + 3 * (d - 1)) 40, Point (floor $ c - 7 + 3 * (d - 1)) 60)) [0 .. fromIntegral (b-1)]
        where   c = ((+40.0) . (/l) . (* 320.0) . pCoord') a
                b = pNumLines a
      ab = concat $ map (crossLine) lforces
      onLine a = (Point a 30, Point a 50)
      lXLine a  = Point a 60
      lXForce a = Point a 10
      la = map (lXLine . floor . (+25.0) . (/ l) . (* 320.0) . pCoord') $ _force $ allParams a
      tla = map ((++ " м") . roundToStr . pCoord') $ _force $ allParams a
      fa = map (lXForce . floor . (+25.0) . (/ l) . (* 320.0) . pCoord') $ _force $ allParams a
      tfa = map ((++ " кг") . roundToStr . weightFromForce . pForce') $ _force $ allParams a
    in do mapM_ (line' dc [penCap := CapButt, penKind := PenSolid, penWidth := 2, color := rgb 0 0 0]) aa
          mapM_ (arrowLine dc) aa
          mapM_ (line' dc [penCap := CapButt, penKind := PenSolid, penWidth := 1, color := rgb 0 0 0]) ab
          mapM_ (text' dc) $ zip3 tla la $ repeat [fontFamily := FontModern]
          mapM_ (text' dc) $ zip3 tfa fa $ repeat [fontFamily := FontModern]



  return ()
     
text' dc (t,p1,prop) = drawText dc t p1 prop
line' dc t (p1, p2) = line dc p1 p2 t

roundToStr :: (PrintfArg a, Floating a) => a -> String
roundToStr f = printf "%0.2f" f

arrowLine dc (a,b) = polygon dc [leftEnd, rightEnd, b] [color := rgb 0 0 0]
  where leftEnd = Point (floor $ (-) (fromIntegral $ pointX b) (deltaY / 2)) (pointY endpoint)
        rightEnd = Point (floor $ (+) (fromIntegral $ pointX b) (deltaY / 2)) (pointY endpoint)
        endpoint = Point (pointX b) (floor $ (fromIntegral $ pointY b) - deltaY)
        deltaY = 0.3 * (fromIntegral $ (pointY b) - (pointY a))