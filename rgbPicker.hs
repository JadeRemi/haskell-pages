{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

import qualified Data.Text as T

data Color = Color { red :: Int
                   , green :: Int
                   , blue :: Int
                   }

defaultColor :: Color
defaultColor = Color 0 0 0

renderColorBox :: MonadWidget t m => Dynamic t Color -> m ()
renderColorBox colorDyn = do
    let colorStyle = ffor colorDyn $ \color ->
            T.concat [ "background-color: rgb("
                     , T.pack $ show $ red color
                     , ","
                     , T.pack $ show $ green color
                     , ","
                     , T.pack $ show $ blue color
                     , ");"
                     ]
    elDynAttr "div" (Map.singleton "style" <$> colorStyle) $
        return ()

renderColorSlider :: MonadWidget t m => T.Text -> (Color -> Int) -> (Color -> Int -> Color) -> Dynamic t Color -> m ()
renderColorSlider label getter setter colorDyn = do
    el "label" $ text label
    let valueDyn = getter <$> colorDyn
    el "input" $ do
        valueDyn' <- inputElement $ def & inputElementConfig_initialValue .~ T.pack (show <$> valueDyn)
                                        & inputElementConfig_inputType .~ "range"
                                        & inputElementConfig_min .~ "0"
                                        & inputElementConfig_max .~ "255"
        let newValueDyn = T.read . T.unpack <$> valueDyn'
        performEvent_ $ setter <$> updated colorDyn <*> newValueDyn

main :: IO ()
main = mainWidgetWithHead $ do
    el "title" $ text "RGB Color Picker"
    colorDyn <- foldDyn ($) defaultColor $ liftA2 (\f x -> f x) <$> [setRed, setGreen, setBlue] <*> [0..255]
    renderColorBox colorDyn
    renderColorSlider "Red" red (\color value -> color { red = value }) colorDyn
    renderColorSlider "Green" green (\color value -> color { green = value }) colorDyn
    renderColorSlider "Blue" blue (\color value -> color { blue = value }) colorDyn