module Pager
    ( pager
    , defaultPagerConfig
    , PagerConfig(..)
    ) where

import Control.Monad ( forM, forM_, when, zipWithM )
import Data.List
import Data.Map ( fromList )
import Data.Maybe ( isJust, fromJust )
import XMonad
import XMonad.Actions.GridSelect ( defaultColorizer )
import XMonad.Actions.Submap
import XMonad.StackSet hiding ( filter )
import XMonad.Util.Font
import XMonad.Util.Image ( drawIcon )
import XMonad.Util.XUtils

import Debunk


data PagerConfig = PagerConfig
    { p_font        :: String
    , p_cellwidth   :: Dimension
    , p_cellheight  :: Dimension
    -- , p_cellpadding :: Dimension
    , p_borderwidth :: Dimension
    , p_bordercolor :: String
    , p_prefixcolor :: Maybe String
    , p_suffixcolor :: Maybe String
    }


defaultPagerConfig = PagerConfig "xft:Sans-8" 100 30 2 "white" Nothing Nothing


data PagerState = PagerState
    { pagerWindows  :: [Window]
    , search        :: String
    , pagerXMF      :: XMonadFont
    }


match :: String -> [String] -> Maybe String
match s ws = do
    let cands = filter (isPrefixOf s) ws
    if length cands == 1
        then Just $ head cands
        else Nothing


pager :: PagerConfig -> X ()
pager c = newPager c >>= pagerMode c


pagerMode :: PagerConfig -> PagerState -> X ()
pagerMode c p = do

    ss <- gets windowset

    case match (search p) (map tag $ hidden ss) of
        Just i -> removePager p >> windows (greedyView i)
        Nothing -> do
            redraw c p
            submapDefault (failbeep >> pagerMode c p) . fromList $
                        zipWith (\ k chr -> ((0, k), incSearchPushChar chr p >>= pagerMode c))
                                ([xK_0..xK_9] ++ [xK_a..xK_z] ++ [xK_space])
                                (['0' .. '9'] ++ ['a' .. 'z'] ++ [' '])
                        ++
                        zipWith (\ k chr -> ((shiftMask, k), incSearchPushChar chr p >>= pagerMode c))
                                [xK_a..xK_z]
                                ['A'..'Z']
                        ++
                        [ ((0, xK_BackSpace ), incSearchPopChar p >>= pagerMode c)
                        , ((0, xK_Escape    ), removePager p)
                        , ((0, xK_Menu      ), removePager p)
                        ]

failbeep = spawn "beep -l 100 -f 500"
    

incSearchPushChar c p = return p { search = search p ++ [c] }

-- only rubout if we have at least one char
incSearchPopChar p@PagerState{search=xs@(_:_)} = return p { search = init xs }
incSearchPopChar p = return p

data TagState = Current | Candidate | Other
    deriving (Eq)



redraw :: PagerConfig -> PagerState -> X ()
redraw c p = do
    ss <- gets windowset

    let w   = p_cellwidth c
        h   = p_cellheight c
        bw  = p_borderwidth c
        bc  = p_bordercolor c
        pc  = p_prefixcolor c
        sc  = p_suffixcolor c

    let currentTag = tag $ workspace $ current ss
    let hiddenTags = map tag $ hidden ss

    let wsTags = hiddenTags ++ [currentTag]

    forM_ (zip wsTags $ pagerWindows p)
          (\(tag, win) -> do

            (bg, fg) <- colorizer (search p) currentTag tag

            my_paintAndWrite win (pagerXMF p) w h bw bg bc fg bg [AlignCenter] [tag] (search p) pc sc
            )


newPager :: PagerConfig -> X PagerState
newPager c = do
    fn <- initXMF (p_font c)

    ss <- gets windowset

    let Screen _ _ (SD (Rectangle _ _ s_width s_height)) = current ss
        x  = fromIntegral $ s_width `div` 2
        y  = fromIntegral $ s_height `div` 2
        w  = p_cellwidth c
        h  = p_cellheight c
        dx = fromIntegral (p_cellwidth c) - 1
        dy = fromIntegral (p_cellheight c) - 1

    let currentTag = tag $ workspace $ current ss
    let hiddenTags = map tag $ hidden ss

    let wsTags = hiddenTags ++ [currentTag]


    pws <- zipWithM
            (\ tag (ox, oy) -> do
                (bg, fg) <- colorizer "" currentTag tag
                createNewWindow (Rectangle (x + ox * dx) (y + oy * dy) w h) Nothing bg True)
            wsTags
            wave

    showWindows pws

    return $ PagerState pws "" fn


removePager :: PagerState -> X ()
removePager (PagerState pws _ fn) = do
    releaseXMF fn
    deleteWindows pws


tagState searchInput currentTag t
    | t == currentTag = Current
    | length searchInput > 0 && searchInput `isPrefixOf` t = Candidate
    | otherwise = Other


colorizer searchInput currentTag tag =
    case tagState searchInput currentTag tag of
        Current     -> return ("#030303", "#0f0f0f")
        Candidate   -> defaultColorizer tag True
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
              -> String     -- ^----TODO
              -> Maybe String     -- ^----TODO
              -> Maybe String     -- ^----TODO
              -> X ()
my_paintAndWrite w fs wh ht bw bc borc ffc fbc als strs searchInput mbPfxColor mbSfxColor = do
    d <- asks display
    strPositions <- forM (zip als strs) $ \(al, str) ->
        stringPosition d fs (Rectangle 0 0 wh ht) al str
    let ms = Just (fs,ffc,fbc, zip strs strPositions)
    my_paintWindow' w (Rectangle 0 0 wh ht) bw bc borc ms Nothing searchInput mbPfxColor mbSfxColor

-- | Paints a titlebar with some strings and icons
-- drawn inside it.
-- Not exported.
my_paintWindow' :: Window -> Rectangle -> Dimension -> String -> String
                -> Maybe (XMonadFont,String,String,[(String, (Position, Position))])
                -> Maybe (String, String, [((Position, Position), [[Bool]])])
                -> String           -- ^----TODO
                -> Maybe String     -- ^----TODO
                -> Maybe String     -- ^----TODO
                -> X ()
my_paintWindow' win (Rectangle _ _ wh ht) bw color b_color strStuff iconStuff searchInput mbPfxColor mbSfxColor = do
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

        let pfx = commonPrefix s searchInput
            sfx = drop (length pfx) searchInput

        pfx_width <- textWidthXMF d xmf pfx

        when (isJust mbPfxColor) $ do
            printStringXMF d p xmf gc (fromJust mbPfxColor) bc x y pfx

        when (isJust mbSfxColor) $ do
            printStringXMF d p xmf gc (fromJust mbSfxColor) bc (x + (fromIntegral pfx_width)) y sfx

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
