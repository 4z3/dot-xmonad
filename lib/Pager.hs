module Pager
    ( pager
    , defaultPagerConfig
    , PagerConfig(..)
    , PagerState(..)
    , PagerMatch(..)
    ) where

import Control.Monad ( forM_, zipWithM_ )
import Data.Char
import Data.List
import Data.Ord
import Data.Map ( fromList )
import Data.Maybe ( isJust, fromJust )
import XMonad
import XMonad.StackSet hiding ( filter )
import XMonad.Util.Font
import XMonad.Util.Image ( drawIcon )
import XMonad.Util.XUtils

import Debunk
import Submap
import Util.XUtils
import Util.Font


data PagerMatch = PagerMatchInfix | PagerMatchPrefix

data PagerConfig = PagerConfig
    { pc_font           :: String
    , pc_cellwidth      :: Dimension
    , pc_matchmethod    :: PagerMatch
    , pc_wrap           :: Bool
    , pc_colors         :: Bool -> Bool -> Bool -> (String, String, String)
    , pc_paint          :: PagerConfig -> PagerState -> Display -> Pixmap -> GC -> String -> Rectangle -> Bool -> Bool -> Bool -> X ()
    }


-- TODO currently xft is broken
defaultPagerConfig = PagerConfig "xft:Sans-8" 100 PagerMatchInfix True stupidColors noPaint
    where
    stupidColors _ _ _ = ("red", "magenta", "yellow")
    noPaint _ _ _ _ _ _ _ _ _ _ = return ()


data PagerState = PagerState
    { ps_window     :: Window
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
pager pc viewFunc as = do
    ps <- newPager pc as
    --redraw pc ps
    showWindow (ps_window ps)
    pagerMode viewFunc pc ps


pagerMode :: (String -> X ()) -> PagerConfig -> PagerState -> X ()
pagerMode viewFunc c p =
    case match (pc_matchmethod c) (ps_search p) (init $ ps_strings p) of
        Nothing -> redraw c p >> submapString def keys
        Just i -> removePager p >> viewFunc i
    where
    def (ch:[]) | isPrint ch =
        incSearchPushChar ch p >>= pagerMode viewFunc c

    def _ =
        failbeep >> pagerMode viewFunc c p

    keys = fromList $
        [ ((0, xK_BackSpace ), incSearchPopChar p >>= pagerMode viewFunc c)
        , ((0, xK_Escape    ), removePager p)
        , ((0, xK_Menu      ), removePager p)
        , ((0, xK_Left      ), goto c (-1, 0) p >>= pagerMode viewFunc c)
        , ((0, xK_Right     ), goto c ( 1, 0) p >>= pagerMode viewFunc c)
        , ((0, xK_Up        ), goto c ( 0,-1) p >>= pagerMode viewFunc c)
        , ((0, xK_Down      ), goto c ( 0, 1) p >>= pagerMode viewFunc c)
        , ((0, xK_Return    ), removePager p >> return (selectFocused p) >>= viewFunc)
        ]


-- TODO make failbeep configurable
failbeep = spawn "beep -l 100 -f 500"


goto :: PagerConfig -> (Position, Position) -> PagerState -> X PagerState
goto PagerConfig{pc_wrap=True}  xy p = maybe (failbeep >> return p) return $ wrapFocus xy p
goto PagerConfig{pc_wrap=False} xy p = maybe (failbeep >> return p) return $ moveFocus xy p


moveFocus :: (Position, Position) -> PagerState -> Maybe PagerState
moveFocus (dx, dy) p@PagerState{ps_focus=(x,y)} = do
    let focus' = (x + dx, y + dy)
    if elem focus' (reachableCoords p)
        then Just p { ps_focus = focus' }
        else Nothing


wrapFocus :: (Position, Position) -> PagerState -> Maybe PagerState

wrapFocus (0, dy) p@PagerState{ps_focus=focus} = do
    let column = sortBy (comparing snd) $ filter ((==) (fst focus) . fst) (reachableCoords p)
    i <- elemIndex focus column
    return p { ps_focus = column `modIndex` (i + fromIntegral dy) }

wrapFocus (dx, 0) p@PagerState{ps_focus=focus} = do
    let column = sortBy (comparing fst) $ filter ((==) (snd focus) . snd) (reachableCoords p)
    i <- elemIndex focus column
    return p { ps_focus = column `modIndex` (i + fromIntegral dx) }

wrapFocus _ _ = Nothing


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


redraw :: PagerConfig -> PagerState -> X ()
redraw pc ps = do
    ss <- gets windowset

    let Screen _ _ (SD (Rectangle _ _ s_width s_height)) = current ss

    -- TODO this let is duplicated in newPager
    let scale x = x * cell_w `div` s_width -- TODO use bw
        cell_w  = pc_cellwidth pc
        cell_h  = scale s_height

        -- txy is the top-left corner of the first (center) cell
        -- XXX div and (-) are not distributive
        --     we could round $ (s_* - cell_*) / 2, though...
        tx = fi $ s_width  `div` 2 - cell_w `div` 2
        ty = fi $ s_height `div` 2 - cell_h `div` 2

        -- dxy are the outer cell dimensions (i.e. including the border)
        -- TODO currently we can only use 2 here...
        dx = fi $ cell_w + 2
        dy = fi $ cell_h + 2

        paint = pc_paint pc
        xmf   = ps_font ps
        tags  = ps_strings ps
        --currentTag = last tags

    withDisplay $ \ d -> do
        -- XXX we cannot use withPixmapAndGC because pc_paint is an X monad
        p <- io $ createPixmap d (ps_window ps) s_width s_height (defaultDepthOfScreen $ defaultScreenOfDisplay d)
        g <- io $ createGC d p

        -- TODO fixme
        color_black <- stringToPixel d "black"

        forZipWithM_ tags (reachableCoords ps) $ \ tag oxy@(ox, oy) -> do

            let focus   = oxy == ps_focus ps
                match   = isXOf (pc_matchmethod pc) (ps_search ps) tag
                current = tag == last tags
                (_b_color, _bg_color, _fg_color) = pc_colors pc focus match current
                --cell_x = (ox * dx) + x - fi (cell_w `div` 2)
                --cell_y = (oy * dy) + y - fi (cell_h `div` 2)
                cell_x = (ox * dx) + tx
                cell_y = (oy * dy) + ty

            b_color <- stringToPixel d _b_color
            bg_color <- stringToPixel d _bg_color
            fg_color <- stringToPixel d _fg_color

            -- draw background
            io $ setForeground d g bg_color
            io $ fillRectangle d p g (cell_x + 1) (cell_y + 1) cell_w cell_h

            -- draw border
            io $ setForeground d g b_color
            io $ drawLines d p g
                    [ Point cell_x cell_y
                    , Point (fi cell_w + 1) 0
                    , Point 0 (fi cell_h + 1)
                    , Point (-(fi cell_w + 1)) 0
                    , Point 0 (-(fi cell_h + 1))
                    ]
                    coordModePrevious

            -- custom draw
            paint pc ps d p g tag (Rectangle (cell_x + 1) (cell_y + 1) cell_w cell_h) focus match current

            -- paint text
            -- TODO custom paint text?
            -- TODO withCopyArea
            io $ withPixmapAndGC d p s_width s_height (defaultDepthOfScreen $ defaultScreenOfDisplay d) $ \ f_pm f_gc -> do
                withPixmapAndGC d f_pm s_width s_height 1 $ \ clip_mask clip_gc -> do
                    setForeground d clip_gc 0
                    setBackground d clip_gc 0
                    fillRectangle d clip_mask clip_gc 0 0 s_width s_height
                    setForeground d clip_gc 1

                    let r = Rectangle cell_x cell_y cell_w cell_h

                    printStringCentered d clip_mask xmf clip_gc r tag

                    setForeground d f_gc fg_color
                    setBackground d f_gc color_black -- TODO

                    printStringCentered d f_pm xmf f_gc r tag

                    setClipMask d f_gc clip_mask

                    copyArea d f_pm p f_gc 0 0 s_width s_height 0 0

        io $ copyArea d p (ps_window ps) g 0 0 s_width s_height 0 0
        io $ freePixmap d p
        io $ freeGC d g


newPager :: PagerConfig -> [String] -> X PagerState
newPager c tags = do
    ss <- gets windowset

    let Screen _ _ (SD (Rectangle _ _ s_width s_height)) = current ss
        (_, def_win_bg, _) = pc_colors c False True False

    -- TODO this let is duplicated in redraw
    let scale x = x * cell_w `div` s_width -- TODO use bw
        cell_w  = pc_cellwidth c
        cell_h  = scale s_height

        -- TODO don't delete this let but use it instead of s_{width,height}
        -- (xcoords, ycoords) = unzip $ take (length tags) wave -- this is reachableCoords
        -- win_width  = (maximum xcoords - minimum xcoords) * dx
        -- win_height = (maximum ycoords - minimum ycoords) * dy

        -- txy is the top-left corner of the first (center) cell
        -- XXX div and (-) are not distributive
        --     we could round $ (s_* - cell_*) / 2, though...
        tx = fi $ s_width  `div` 2 - cell_w `div` 2
        ty = fi $ s_height `div` 2 - cell_h `div` 2

        -- dxy are the outer cell dimensions (i.e. including the border)
        -- TODO currently we can only use 2 here...
        dx = fi $ cell_w + 2
        dy = fi $ cell_h + 2

    fn <- initXMF (pc_font c)
    win <- createNewWindow (Rectangle 0 0 s_width s_height) Nothing def_win_bg True

    withDisplay $ \ d ->
        io $ shapeWindow d win $ \ p g ->
            forZipWithM_ tags wave $ \ _ (ox, oy) ->
                fillRectangle d p g (tx + ox * dx) (ty + oy * dy) (fi dx) (fi dy)

    return $ PagerState win "" fn (0,0) tags


removePager :: PagerState -> X ()
removePager (PagerState w _ fn _ _) = do
    deleteWindow w
    releaseXMF fn

wave :: [(Position, Position)]
wave = zip (0:(concat $ map (\i -> [0..i]++[i-1,i-2..1] ++ [0,-1..(-i)]++[-i,-i+1..(-1)]) [1..])) (concat $ map (\i -> [0..i]++[i-1,i-2..1] ++ [0,-1..(-i)]++[-i+1,-i+2..(-1)]) [1..])
    where
        wave1 = 0:(concat $ map (\i -> [0..i]++[i-1,i-2..1] ++ [0,-1..(-i)]++[-i,-i+1..(-1)]) [1..])
        wave2 = concat $ map (\i -> [0..i]++[i-1,i-2..1] ++ [0,-1..(-i)]++[-i+1,-i+2..(-1)]) [1..]

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


forZipWithM_ a b f = zipWithM_ f a b
