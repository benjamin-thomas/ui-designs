{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Text qualified as T
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Static qualified as Static
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Web.Atomic ((-.))
import Web.Atomic.CSS hiding (style)
import Web.Atomic.CSS qualified as A
import Web.Hyperbole as Hyperbole
import Prelude hiding (div)

{-

rg --files -t hs app-hyper/ *.cabal | entr -rc cabal run hyperbole-main

 -}

data Counter = MkCounter
    deriving (Generic, ViewId)

instance HyperView Counter es where
    data Action Counter
        = Inc Int
        deriving (Generic, ViewAction)

    update (Inc n) = do
        pure $ viewCard (n + 1)

data AppRoute
    = Home
    deriving (Generic, Eq)

instance Route AppRoute where
    baseRoute = Just Home

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    let port = 4013
    putStrLn $ "Starting on http://localhost:" ++ show port ++ "/"
    Warp.run port $
        Static.staticPolicy
            (Static.addBase "app-hyper/assets")
            app

app :: Application
app = do
    liveApp
        (basicDocument "Counter")
        (routeRequest router)
  where
    router Home = runPage cardPage

cardPage :: (Hyperbole :> es) => Eff es (Page '[Counter])
cardPage = do
    pure $ hyper MkCounter (viewCard 0)

screenCenter :: (Styleable h) => CSS h -> CSS h
screenCenter =
    utility
        "screen-center"
        [ "display" :. "flex"
        , "min-height" :. "100vh"
        , "justify-content" :. "center"
        , "align-items" :. "center"
        , "background-color" :. A.style (colorValue PageBackground)
        , "color" :. "white"
        , "font-family" :. "system-ui"
        ]

marginAuto :: (Styleable h) => CSS h -> CSS h
marginAuto = utility "margin-auto" ["margin" :. "auto"]

fontWeight :: (Styleable h) => Int -> CSS h -> CSS h
fontWeight n = utility ("fw" -. n) ["font-weight" :. A.style n]

cursorPointer :: (Styleable h) => CSS h -> CSS h
cursorPointer = utility "cursor-pointer" ["cursor" :. "pointer"]

userSelectNone :: (Styleable h) => CSS h -> CSS h
userSelectNone = utility "select-none" ["user-select" :. "none"]

link' :: View a () -> View a ()
link' =
    el
        ~ bg LinkBackground
            . border 1
            . borderColor LinkBorder
            . pad (XY 0 13)
            . rounded 9
            . margin (TRBL 15 30 0 30)
            . fontWeight 600
            . hover
                ( cursorPointer
                    . transition 100 (BgColor $ colorValue Primary)
                    . color LinkBackground
                )

lineHeight :: (Styleable h) => Float -> CSS h -> CSS h
lineHeight n = utility ("line-height" -. n) ["line-height" :. A.style n]

viewCard :: Int -> View Counter ()
viewCard n = do
    el ~ screenCenter . flexDirection Column $ do
        el @ onClick (Inc n)
            ~ margin 15
                . italic
                . color PageText
                . minWidth 440
                . cursorPointer
                . userSelectNone
            $ text ("Hyperbole (" <> T.pack (show n) <> ")")
        el
            ~ bg CardBackground
                . pad 10
                . border 1
                . borderColor CardBorder
                . minWidth 440
                . rounded 10
            $ col
                ( el
                    ~ textAlign AlignCenter
                        . pad (Y 50)
                    $ col
                        ( do
                            img "/images/avatar-jessica.jpeg" -- cSpell:disable-line
                                ~ rounded (Pct 0.5)
                                    . width 120
                                    . marginAuto
                            el "Jessica Randall" ~ fontSize 24 . bold . pad (T 55)
                            el "London, United Kingdom" ~ fontSize 14 . bold . color Primary ~ pad (T 7)
                            el "Front-end developer and avid reader." ~ fontSize 14 . pad (T 30)
                            el
                                ~ pad (T 20)
                                $ do
                                    link' "GitHub"
                                    link' "Frontend Mentor"
                                    link' "LinkedIn"
                                    link' "Twitter"
                                    link' "Instagram"
                        )
                )
        el
            ~ pad (T 20)
                . lineHeight 1.3
            $ do
                row $ do
                    el "Challenge by "
                    tag "a"
                        ~ ( cursorPointer
                                . color Primary
                                . underline
                                . italic
                                . pad (L 10)
                          )
                            @ att "href" "https://www.frontendmentor.io?ref=challenge"
                        $ "Frontend Mentor"
                row ~ margin 0 . pad 0 . gap 0 $ do
                    el "Coded by:"
                    el
                        ~ pad (L 10)
                            . color (HexColor "#C4F82A")
                            . underline
                            . italic
                        $ "Benjamin THOMAS."

data AppColor
    = CardBorder
    | PageBackground
    | CardBackground
    | LinkBorder
    | LinkBackground
    | PageText
    | Primary
    | White
    deriving (Show)

{- FOURMOLU_DISABLE -}
instance ToColor AppColor where
    colorValue :: AppColor -> HexColor
    colorValue PageBackground    = "#141414"   -- #141414 main background
    colorValue PageText          = "#808080"   -- #808080 gray text
    colorValue CardBackground    = "#1F1F1F"   -- #1F1F1F card background
    colorValue CardBorder        = "#101010"   -- #101010 darkest - card border
    colorValue LinkBackground    = "#333333"   -- #333333 link background
    colorValue LinkBorder        = "#202020"   -- #202020 link border
    colorValue Primary           = "#C4F82A"   -- #C4F82A accent color
    colorValue White             = "#FFFFFF"   -- #FFFFFF white text
{- FOURMOLU_ENABLE -}