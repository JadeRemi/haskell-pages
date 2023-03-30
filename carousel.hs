{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

import qualified Data.Text as T

data Image = Image { imageSrc :: T.Text }

sampleImages :: [Image]
sampleImages = [ Image "https://via.placeholder.com/400x200/0000FF/FFFFFF?text=Image+1"
               , Image "https://via.placeholder.com/400x200/00FF00/FFFFFF?text=Image+2"
               , Image "https://via.placeholder.com/400x200/FF0000/FFFFFF?text=Image+3"
               ]

renderImage :: MonadWidget t m => Image -> m ()
renderImage image = elAttr "img" ("src" =: imageSrc image) $ return ()

renderCarousel :: MonadWidget t m => [Image] -> m ()
renderCarousel images =
    elClass "div" "carousel" $ do
        elClass "div" "carousel-container" $ do
            elClass "div" "carousel-track" $ do
                _ <- dyn $ renderImage <$> images
        elClass "div" "carousel-controls" $ do
            _ <- button "Previous"
            _ <- button "Next"
            return ()

renderDropZone :: MonadWidget t m => m (Event t Image)
renderDropZone = do
    elClass "div" "drop-zone" $ do
        text "Drag and drop images here"
    dragAndDropZone $ \_ files -> do
        let imageSrcs = [Image $ T.pack $ "data:" <> fileContentType f <> ";base64," <> T.unpack (fileContents f) | f <- files]
        return $ Image <$> imageSrcs

main :: IO ()
main = mainWidgetWithHead $ do
    el "title" $ text "Image Carousel"
    imagesVar <- foldDyn (++) sampleImages =<< renderDropZone
    renderCarousel =<< holdUniqDyn imagesVar