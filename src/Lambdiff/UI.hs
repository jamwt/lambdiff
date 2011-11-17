module Lambdiff.UI where

import Control.Monad (mapM_)
import System.Exit
import qualified Data.ByteString.Char8 as S
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events (Event(Key))

import Lambdiff.Types

renderUI :: [FileDiff] -> IO ()
renderUI diffs = do
    initGUI
    window <- windowNew
    outbox <- vBoxNew True 2
    bar    <- labelNew $ Just "λiff v0.1 -- use keys (j, k, J, K, l, h, P, N, Esc)"
--    labelSetText bar "lambdiff"
--    entrySetHasFrame bar False
--    set bar [entryEditable := False]
    box <- hBoxNew True 8
    boxSetHomogeneous outbox False
    boxPackStart outbox box PackGrow 0
    boxPackStart outbox bar PackNatural 0
    tv <- treeViewNew
    scrollbox <- scrolledWindowNew Nothing Nothing
    ibox <- hBoxNew True 5
    i2 <- textViewNew
    i3 <- textViewNew

    textViewSetCursorVisible i2 False
    textViewSetCursorVisible i3 False

    scrolledWindowAddWithViewport scrollbox ibox
    boxSetHomogeneous box False
    boxPackStart box tv PackNatural 0
    boxPackStart box scrollbox PackGrow 0

    boxPackStart ibox i2 PackGrow 0
    boxPackStart ibox i3 PackGrow 0
    boxSetHomogeneous ibox False

    textViewSetEditable i2 False
    textViewSetEditable i3 False

    table <- textTagTableNew
    tagMono <- textTagNew $ Just "mono"
    tagRemove <- textTagNew $ Just "remove"
    tagAdd <- textTagNew $ Just "add"
    tagChange <- textTagNew $ Just "change"
    tagDull <- textTagNew $ Just "dull"
    tagMissing <- textTagNew $ Just "missing"
    tagLineNo <- textTagNew $ Just "lineno"
    tagSep <- textTagNew $ Just "sep"

    set tagMono [textTagFamily    := "monospace",
                 textTagWeight    := 400
                 ]
    set tagRemove [ textTagParagraphBackground  := "#f66"
                 ]
    set tagAdd   [ textTagParagraphBackground  := "#6f6"
                 ]
    set tagChange [ textTagParagraphBackground  := "#ff6"
                 ]
    set tagSep [ textTagParagraphBackground  := "#000",
                 textTagSize := 2500
                 ]
    set tagDull [ textTagForeground  := "#999"
                 ]
    set tagMissing [ textTagParagraphBackground  := "#eee" ]

    set tagLineNo [ textTagBackground  := "#fff",
                    textTagForeground := "#999"
                    ]

    textTagTableAdd table tagMono
    textTagTableAdd table tagRemove
    textTagTableAdd table tagAdd
    textTagTableAdd table tagChange
    textTagTableAdd table tagDull
    textTagTableAdd table tagMissing
    textTagTableAdd table tagLineNo
    textTagTableAdd table tagSep

    buf1 <- textBufferNew $ Just table
    buf2 <- textBufferNew $ Just table

    list <- listStoreNew diffs
    col <- treeViewColumnNew
    textRender <- cellRendererTextNew
    treeViewColumnPackStart col textRender True
    cellLayoutSetAttributes col textRender list $ \row -> [
        cellText := (let FileDiff fn _ _ = row in S.unpack fn)
        ]
    treeViewSetModel tv list
    treeViewAppendColumn tv col
    treeViewSetHeadersVisible tv False

    textViewSetBuffer i2 buf1
    textViewSetBuffer i3 buf2
    onCursorChanged tv $ processChange diffs scrollbox tv buf1 buf2

    set window [containerBorderWidth := 10,
                containerChild := outbox ]
    onDestroy window mainQuit
    onKeyPress window $ handleKeyPress (length diffs) tv scrollbox
    widgetShowAll window

    (w, _) <- widgetGetSize tv
    let size = min 250 w
    widgetSetSizeRequest tv size (-1)

    mainGUI

pageDown scroll = do
    pageMove scroll (+)

pageUp scroll = do
    pageMove scroll (-)

pageMove scroll op = do
    adj <- scrolledWindowGetVAdjustment scroll
    here <- adjustmentGetValue adj
    pageDown <- adjustmentGetPageIncrement adj
    pageSize <- adjustmentGetPageSize adj
    upperLimit <- adjustmentGetUpper adj
    let halfPage = pageDown / 2.0
    let proposed = here `op` halfPage
    let bounded = max 0 $ min (upperLimit - pageSize) proposed
    adjustmentSetValue adj bounded

nextFile = moveFile (+1)
prevFile = moveFile (subtract 1)

downFiles = moveFile (+10)
upFiles = moveFile (subtract 10)

moveFile op numItems tv = do
    (ix, _) <- treeViewGetCursor tv
    case ix of
        [] -> return ()
        (x:[]) -> do
                    let adj = max 0 $ min (numItems - 1) $ op x
                    treeViewSetCursor tv [adj] Nothing
        _      -> error "unexpected index!"

shrink = slide (flip subtract)
grow = slide (+)

slide op tv = do
    (w, _) <- widgetGetSize tv
    let size = max 0 $ w `op` 30
    widgetSetSizeRequest tv size (-1)

handleKeyPress numItems tv scroll (Key rel _ _ mods _ _ _ val name char) = do
    case name of
        "J" -> pageDown scroll
        "Page_Down" -> pageDown scroll
        "space" -> pageDown scroll

        "K" -> pageUp scroll
        "Page_Up" -> pageUp scroll

        "j" -> nextFile numItems tv
        "Down" -> nextFile numItems tv

        "k" -> prevFile numItems tv
        "Up" -> prevFile numItems tv

        "h" -> shrink tv
        "Left" -> shrink tv
        "l" -> grow tv
        "Right" -> grow tv

        "p" -> upFiles numItems tv
        "P" -> upFiles numItems tv
        "n" -> downFiles numItems tv
        "N" -> downFiles numItems tv

        "Escape" -> exitWith ExitSuccess
        o   -> (return ())
    return True
--            scrolledWindowSetVAdjustment scroll adj

processChange :: [FileDiff] -> ScrolledWindow -> TreeView -> TextBuffer -> TextBuffer -> IO ()
processChange diffs scroll tv buf1 buf2 = do
    (ix, _) <- treeViewGetCursor tv
    case ix of
        [] -> return ()
        (x:[]) -> do
                    let diff = diffs !! x
                    writeTextForDiff diff scroll buf1 buf2
        _      -> error "unexpected index!"

clearLastLine buf = do
    lines <- textBufferGetLineCount buf
    start <- textBufferGetIterAtLineOffset buf (lines - 2) 0
    end   <- textBufferGetEndIter buf
    textBufferDelete buf start end

writeTextForDiff :: FileDiff -> ScrolledWindow -> TextBuffer -> TextBuffer -> IO ()
writeTextForDiff (FileDiff fn dir secs) scroll buf1 buf2 = do
    textBufferSetText buf1 ""
    textBufferSetText buf2 ""
    adj <- scrolledWindowGetVAdjustment scroll
    adjustmentSetValue adj 0

    mapM_ (\(DiffSection lines)->(mapM_ writeLine $ lines) >> writeSep) secs

    clearLastLine buf1
    start1   <- textBufferGetStartIter buf1
    end1     <- textBufferGetEndIter buf1
    textBufferApplyTagByName buf1 "mono" start1 end1

    clearLastLine buf2
    start2   <- textBufferGetStartIter buf2
    end2     <- textBufferGetEndIter buf2
    textBufferApplyTagByName buf2 "mono" start2 end2

  where
    lineNum n = concat [replicate (5 - (length s)) ' ',  s, " "]
      where
        s = show n

    writeSep = do
        cur   <- textBufferGetEndIter buf1
        mark1 <- textBufferCreateMark buf1 Nothing cur True
        cur   <- textBufferGetEndIter buf2
        mark2 <- textBufferCreateMark buf2 Nothing cur True
        textBufferInsertByteStringAtCursor buf1 "\n"
        textBufferInsertByteStringAtCursor buf2 "\n"
        start1 <- textBufferGetIterAtMark buf1 mark1
        start2 <- textBufferGetIterAtMark buf2 mark2
        {-start1 <- textBufferGetStartIter buf1-}
        {-start2 <- textBufferGetStartIter buf2-}
        end1 <- textBufferGetEndIter buf1
        end2 <- textBufferGetEndIter buf2
        textBufferApplyTagByName buf1 "sep" start1 end1
        textBufferApplyTagByName buf2 "sep" start2 end2

    writeLine (DiffLine dir ml1 ml2) = do
        cur   <- textBufferGetEndIter buf1
        mark1 <- textBufferCreateMark buf1 Nothing cur True
        cur   <- textBufferGetEndIter buf2
        mark2 <- textBufferCreateMark buf2 Nothing cur True
        markMid1 <- case ml1 of
            Just (no, bs) -> do
                textBufferInsertAtCursor buf1 $ lineNum no
                cur  <- textBufferGetEndIter buf1
                mark <- textBufferCreateMark buf1 Nothing cur True
                textBufferInsertByteStringAtCursor buf1 bs
                return $ Just mark
            Nothing -> textBufferInsertByteStringAtCursor buf1 "\n" >> (return Nothing)
        markMid2 <- case ml2 of
            Just (no, bs) -> do
                textBufferInsertAtCursor buf2 $ lineNum no
                cur  <- textBufferGetEndIter buf2
                mark <- textBufferCreateMark buf2 Nothing cur True
                textBufferInsertByteStringAtCursor buf2 bs
                return $ Just mark
            Nothing -> textBufferInsertByteStringAtCursor buf2 "\n" >> (return Nothing)
        start1 <- textBufferGetIterAtMark buf1 mark1
        start2 <- textBufferGetIterAtMark buf2 mark2
        {-start1 <- textBufferGetStartIter buf1-}
        {-start2 <- textBufferGetStartIter buf2-}
        end1 <- textBufferGetEndIter buf1
        end2 <- textBufferGetEndIter buf2

        case dir of
            DirectionAdded -> do
                textBufferApplyTagByName buf1 "missing" start1 end1
                textBufferApplyTagByName buf2 "add" start2 end2
            DirectionRemoved -> do
                textBufferApplyTagByName buf1 "remove" start1 end1
                textBufferApplyTagByName buf2 "missing" start2 end2
            DirectionChanged -> do
                textBufferApplyTagByName buf1 "change" start1 end1
                textBufferApplyTagByName buf2 "change" start2 end2
            DirectionNone -> do
                textBufferApplyTagByName buf1 "dull" start1 end1
                textBufferApplyTagByName buf2 "dull" start2 end2

        case markMid1 of
            Just mark -> do
                i <- textBufferGetIterAtMark buf1 mark
                textBufferApplyTagByName buf1 "lineno" start1 i
            Nothing -> return ()
        case markMid2 of
            Just mark -> do
                i <- textBufferGetIterAtMark buf2 mark
                textBufferApplyTagByName buf2 "lineno" start2 i
            Nothing -> return ()
