{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

import qualified Data.Text as T
import qualified Data.Map as Map

data SvgElement = SvgElement { svgType :: T.Text
                             , svgAttributes :: Map.Map T.Text T.Text
                             , svgChildren :: [SvgElement]
                             }

renderSvgElement :: MonadWidget t m => SvgElement -> m ()
renderSvgElement element =
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") (pure $ svgAttributes element) $ do
        mapM_ renderSvgElement (svgChildren element)

data EditorState = EditorState { editorSvg :: SvgElement }

defaultSvg :: SvgElement
defaultSvg = SvgElement "svg" (Map.fromList [("viewBox", "0 0 400 400")]) []

renderEditor :: MonadWidget t m => Dynamic t EditorState -> m ()
renderEditor editorState = do
    svgDyn <- holdUniqDyn $ editorSvg <$> editorState
    renderSvgElement =<< sample svgDyn

renderEditorControls :: MonadWidget t m => Dynamic t EditorState -> m ()
renderEditorControls editorState = do
    elClass "div" "editor-controls" $ do
        _ <- button "Add Rect" >>= performEvent_ . fmap (const $ modifySvg editorState addRect)
        _ <- button "Add Circle" >>= performEvent_ . fmap (const $ modifySvg editorState addCircle)
        _ <- button "Add Path" >>= performEvent_ . fmap (const $ modifySvg editorState addPath)
        return ()
    where
        modifySvg :: Dynamic t EditorState -> (SvgElement -> SvgElement) -> m ()
        modifySvg editorState f =
            modifyDynamic editorState $ \editor -> editor { editorSvg = f $ editorSvg editor }

addRect :: SvgElement -> SvgElement
addRect parent = parent { svgChildren = svgChildren parent ++ [SvgElement "rect" Map.empty []] }

addCircle :: SvgElement -> SvgElement
addCircle parent = parent { svgChildren = svgChildren parent ++ [SvgElement "circle" Map.empty []] }

addPath :: SvgElement -> SvgElement
addPath parent = parent { svgChildren = svgChildren parent ++ [SvgElement "path" Map.empty []] }

main :: IO ()
main = mainWidgetWithHead $ do
    el "title" $ text "SVG Editor"
    editorState <- foldDyn ($) (EditorState defaultSvg) $ leftmost [addRect, addCircle, addPath]
    renderEditor editorState
    renderEditorControls editorState