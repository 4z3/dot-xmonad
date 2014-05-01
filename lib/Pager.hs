module Pager
    ( pager
    , defaultPagerConfig
    , PagerConfig(..)
    , PagerMatch(..)
    ) where

import Control.Monad ( forM, forM_, when, zipWithM )
import Data.Char
import Data.List
import Data.Ord
import Data.Map ( fromList )
import Data.Maybe ( isJust, fromJust )
import XMonad
import XMonad.Actions.GridSelect ( defaultColorizer )
import XMonad.StackSet hiding ( filter )
import XMonad.Util.Font
import XMonad.Util.Image ( drawIcon )
import XMonad.Util.XUtils

import Debunk
import Submap


data PagerMatch = PagerMatchInfix | PagerMatchPrefix

data PagerConfig = PagerConfig
    { pc_font           :: String
    , pc_cellwidth      :: Dimension
    , pc_cellheight     :: Dimension
    --, pc_cellpadding    :: Dimension
    , pc_borderwidth    :: Dimension
    , pc_bordercolor    :: String
    , pc_matchcolor     :: String
    , pc_matchmethod    :: PagerMatch
    , pc_uncolor        :: (String, String) -- ^ color of current (unavailable) cell (background, foreground)
    , pc_wrap           :: Bool
    }



defaultPagerConfig = PagerConfig "xft:Sans-8" 100 30 2 "white" "magenta" PagerMatchInfix ("#232323", "#424242") True


data PagerState = PagerState
    { ps_windows    :: [Window]
    , ps_search     :: String
    , ps_font       :: XMonadFont
    , ps_focus      :: (Position, Position)
    , ps_strings    :: [String]
    }


reachableCoords :: PagerState -> [(Position, Position)]
reachableCoords PagerState{ps_strings=xs} = take (length xs) wave


match :: PagerMatch -> String -> [String] -> Maybe String
match m s ws = do
    let cands = filter (isXOf m s) ws
    if length cands == 1
        then Just $ head cands
        else Nothing

pager :: PagerConfig -> (String -> X ()) -> [String] -> X ()
pager c viewFunc as = newPager c as >>= ps_Mode viewFunc c


ps_Mode :: (String -> X ()) -> PagerConfig -> PagerState -> X ()
ps_Mode viewFunc c p =
    case match (pc_matchmethod c) (ps_search p) (init $ ps_strings p) of
        Nothing -> redraw c p >> submapString def keys
        Just i -> removePager p >> viewFunc i
    where
    def (ch:[]) | isPrint ch =
        incSearchPushChar ch p >>= ps_Mode viewFunc c

    def _ =
        failbeep >> ps_Mode viewFunc c p

    keys = fromList $
        [ ((0, xK_BackSpace ), incSearchPopChar p >>= ps_Mode viewFunc c)
        , ((0, xK_Escape    ), removePager p)
        , ((0, xK_Menu      ), removePager p)
        , ((0, xK_Left      ), goto c (-1, 0) p >>= ps_Mode viewFunc c)
        , ((0, xK_Right     ), goto c ( 1, 0) p >>= ps_Mode viewFunc c)
        , ((0, xK_Up        ), goto c ( 0,-1) p >>= ps_Mode viewFunc c)
        , ((0, xK_Down      ), goto c ( 0, 1) p >>= ps_Mode viewFunc c)
        , ((0, xK_Return    ), removePager p >> return (selectFocused p) >>= viewFunc)
        ]


failbeep = spawn "beep -l 100 -f 500"


goto :: PagerConfig -> (Position, Position) -> PagerState -> X PagerState
goto PagerConfig{pc_wrap=True}  = wrapFocus
goto PagerConfig{pc_wrap=False} = moveFocus


moveFocus :: (Position, Position) -> PagerState -> X PagerState
moveFocus (dx, dy) p = do
    let (x, y) = ps_focus p
        focus' = (x + dx, y + dy)

    if elem focus' (reachableCoords p)
        then return p { ps_focus = focus' }
        else failbeep >> return p


wrapFocus :: (Position, Position) -> PagerState -> X PagerState

wrapFocus (0, dy) p = do
    let focus = ps_focus p
        column = sortBy (comparing snd) $ filter ((==) (fst focus) . fst) (reachableCoords p)
        i = fromJust (elemIndex focus column)
    return p { ps_focus = column `modIndex` (i + fromIntegral dy) }

wrapFocus (dx, 0) p = do
    let focus = ps_focus p
        column = sortBy (comparing fst) $ filter ((==) (snd focus) . snd) (reachableCoords p)
        i = fromJust (elemIndex focus column)
    return p { ps_focus = column `modIndex` (i + fromIntegral dx) }


wrapFocus _ p = failbeep >> return p


selectFocused :: PagerState -> String
selectFocused p =
    -- TODO the pager must never "focus" something inexistent
    fromJust $ lookup (ps_focus p) $ zip wave (ps_strings p)


incSearchPushChar :: Char -> PagerState -> X PagerState
incSearchPushChar c p = return p { ps_search = ps_search p ++ [c] }


incSearchPopChar :: PagerState -> X PagerState

-- only rubout if we have at least one char
incSearchPopChar p@PagerState{ps_search=xs@(_:_)} =
    return p { ps_search = init xs }

incSearchPopChar p = return p


data TagState = Current | Candidate | Other
    deriving (Eq)



redraw :: PagerConfig -> PagerState -> X ()
redraw c p = do
    let w   = pc_cellwidth c
        h   = pc_cellheight c
        bw  = pc_borderwidth c
        bc  = pc_bordercolor c
        mc  = pc_matchcolor c

    let wsTags = ps_strings p
    let currentTag = last wsTags

    forM_ (zip4 [1..] wsTags (ps_windows p) wave)
          (\(i, tag, win, pos) -> do

            (bg, fg) <- if pos == ps_focus p
                            then defaultColorizer tag True
                            else
                                if isXOf (pc_matchmethod c) (ps_search p) tag
                                    then colorizer c (ps_search p) currentTag tag
                                    else return $ pc_uncolor c

            let matchStuff = if tag == currentTag
                                 then Nothing
                                 else Just (pc_matchmethod c, ps_search p, mc)

            my_paintAndWrite win (ps_font p) w h bw bg bc fg bg [AlignCenter] [tag] matchStuff)


newPager :: PagerConfig -> [String] -> X PagerState
newPager c as = do
    fn <- initXMF (pc_font c)

    ss <- gets windowset

    let Screen _ _ (SD (Rectangle _ _ s_width s_height)) = current ss
        x  = fromIntegral $ s_width `div` 2
        y  = fromIntegral $ s_height `div` 2
        w  = pc_cellwidth c
        h  = pc_cellheight c
        dx = fromIntegral (pc_cellwidth c) - 1
        dy = fromIntegral (pc_cellheight c) - 1

    let wsTags = as
    let currentTag = last wsTags

    pws <- zipWithM
            (\ tag (ox, oy) -> do
                (bg, fg) <- colorizer c "" currentTag tag
                createNewWindow (Rectangle (x + ox * dx) (y + oy * dy) w h) Nothing bg True)
            wsTags
            wave

    showWindows pws

    return $ PagerState pws "" fn (0,0) wsTags


removePager :: PagerState -> X ()
removePager (PagerState pws _ fn _ _) = do
    releaseXMF fn
    deleteWindows pws


tagState searchInput currentTag t
    | t == currentTag = Current
    | length searchInput > 0 && searchInput `isPrefixOf` t = Candidate
    | otherwise = Other


colorizer :: PagerConfig -> String -> String -> String -> X (String, String)
colorizer c searchInput currentTag tag =
    case tagState searchInput currentTag tag of
        Current     -> return $ pc_uncolor c
        --Candidate   -> defaultColorizer tag True
        _           -> defaultColorizer tag False





wave :: [(Position, Position)]
wave = zip (0:(concat $ map (\i -> [0..i]++[i-1,i-2..1] ++ [0,-1..(-i)]++[-i,-i+1..(-1)]) [1..])) (concat $ map (\i -> [0..i]++[i-1,i-2..1] ++ [0,-1..(-i)]++[-i+1,-i+2..(-1)]) [1..])
    where
        wave1 = 0:(concat $ map (\i -> [0..i]++[i-1,i-2..1] ++ [0,-1..(-i)]++[-i,-i+1..(-1)]) [1..])
        wave2 = concat $ map (\i -> [0..i]++[i-1,i-2..1] ++ [0,-1..(-i)]++[-i+1,-i+2..(-1)]) [1..]


-- | Fill a window with a rectangle and a border, and write
-- | a number of strings to given positions
my_paintAndWrite :: Window     -- ^ The window where to draw
                -> XMonadFont -- ^ XMonad Font for drawing
                -> Dimension  -- ^ Window width
                -> Dimension  -- ^ Window height
                -> Dimension  -- ^ Border width
                -> String     -- ^ Window background color
                -> String     -- ^ Border color
                -> String     -- ^ String color
                -> String     -- ^ String background color
                -> [Align]    -- ^ String 'Align'ments
                -> [String]   -- ^ Strings to be printed
                -> Maybe (PagerMatch, String, String) -- ^ TODO
                -> X ()
my_paintAndWrite w fs wh ht bw bc borc ffc fbc als strs matchStuff = do
    d <- asks display
    strPositions <- forM (zip als strs) $ \(al, str) ->
        stringPosition d fs (Rectangle 0 0 wh ht) al str
    let ms = Just (fs,ffc,fbc, zip strs strPositions)
    my_paintWindow' w (Rectangle 0 0 wh ht) bw bc borc ms Nothing matchStuff

-- | Paints a titlebar with some strings and icons
-- drawn inside it.
-- Not exported.
my_paintWindow' :: Window -> Rectangle -> Dimension -> String -> String
                -> Maybe (XMonadFont,String,String,[(String, (Position, Position))])
                -> Maybe (String, String, [((Position, Position), [[Bool]])])
                -> Maybe (PagerMatch, String, String) -- ^ TODO
                -> X ()
my_paintWindow' win (Rectangle _ _ wh ht) bw color b_color strStuff iconStuff matchStuff = do
  d  <- asks display
  p  <- io $ createPixmap d win wh ht (defaultDepthOfScreen $ defaultScreenOfDisplay d)
  gc <- io $ createGC d p
  -- draw
  io $ setGraphicsExposures d gc False
  [color',b_color'] <- mapM (stringToPixel d) [color,b_color]
  -- we start with the border
  io $ setForeground d gc b_color'
  io $ fillRectangle d p gc 0 0 wh ht
  -- and now again
  io $ setForeground d gc color'
  io $ fillRectangle d p gc (fi bw) (fi bw) (wh - (bw * 2)) (ht - (bw * 2))
  -- paint strings
  when (isJust strStuff) $ do
    let (xmf,fc,bc,strAndPos) = fromJust strStuff
    forM_ strAndPos $ \(s, (x, y)) -> do
        printStringXMF d p xmf gc fc bc x y s

        when (isJust matchStuff) $ do
            let Just (matchMethod, searchInput, matchColor) = matchStuff
                mbi = findXIndex matchMethod searchInput s
            when (isJust mbi) $ do
                let i = fromJust mbi
                skip_width <- textWidthXMF d xmf (take i s)

                printStringXMF d p xmf gc matchColor bc (x + fromIntegral skip_width) y searchInput

  -- paint icons
  when (isJust iconStuff) $ do
    let (fc, bc, iconAndPos) = fromJust iconStuff
    forM_ iconAndPos $ \((x, y), icon) ->
      drawIcon d p gc fc bc x y icon
  -- copy the pixmap over the window
  io $ copyArea      d p win gc 0 0 wh ht 0 0
  -- free the pixmap and GC
  io $ freePixmap    d p
  io $ freeGC        d gc


commonPrefix (x:xs) (y:ys) | x == y = x:commonPrefix xs ys
commonPrefix _ _ = [] 


isXOf :: PagerMatch -> String -> String -> Bool
isXOf PagerMatchInfix  = isInfixOf
isXOf PagerMatchPrefix = isPrefixOf


findXIndex :: (Eq a) => PagerMatch -> [a] -> [a] -> Maybe Int
findXIndex PagerMatchInfix  = findInfixIndex
findXIndex PagerMatchPrefix = findPrefixIndex


findInfixIndex :: (Eq a) => [a] -> [a] -> Maybe Int
findInfixIndex needle haystack
    = (\x -> if null x then Nothing else Just (fst $ head x))
      . dropWhile (\(_,x) -> not $ isPrefixOf needle x)
        $ zip [0..] (tails haystack)

findPrefixIndex :: (Eq a) => [a] -> [a] -> Maybe Int
findPrefixIndex needle haystack =
    if isPrefixOf needle haystack
        then Just 0
        else Nothing


modIndex :: Integral i => [a] -> i -> a
modIndex xs i = xs `genericIndex` (i `mod` genericLength xs)
