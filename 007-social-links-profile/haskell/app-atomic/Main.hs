{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main, debug, debug') where

import Data.ByteString.Lazy (fromStrict)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Static qualified as Static
import Web.Atomic

main :: IO ()
main = do
    let port = 4012
    putStrLn $ "Starting on http://localhost:" ++ show port ++ "/"
    Warp.run port $
        Static.staticPolicy
            (Static.addBase "app-atomic/assets")
            app

debug :: (Styleable a) => CSS a -> CSS a
debug =
    utility
        "debug"
        [ "border" :. "1px solid red"
        ]

debug' :: (Styleable a) => CSS a -> CSS a
debug' =
    mconcat
        [ border 4
        , borderColor (HexColor "#00F")
        ]

lineHeight :: (Styleable h) => Float -> CSS h -> CSS h
lineHeight n = utility ("line-height" -. n) ["line-height" :. style n]

screenCenter :: (Styleable h) => CSS h -> CSS h
screenCenter =
    utility
        "screen-center"
        [ "display" :. "flex"
        , "min-height" :. "100vh"
        , "justify-content" :. "center"
        , "align-items" :. "center"
        , "background-color" :. style (colorValue PageBackground)
        , "color" :. "white"
        , "font-family" :. "system-ui"
        ]

fontWeight :: (Styleable h) => Int -> CSS h -> CSS h
fontWeight n = utility ("fw" -. n) ["font-weight" :. style n]

src :: (Attributable h) => AttValue -> Attributes h -> Attributes h
src = att "src"

img :: AttValue -> Html ()
img x = tag "img" @ src x $ ""

marginAuto :: (Styleable h) => CSS h -> CSS h
marginAuto = utility "margin-auto" ["margin" :. "auto"]

data Profile = Profile
    { profileImage :: Text
    , profileName :: Text
    , profileLocation :: Text
    , profileBio :: Text
    , profileSocialLinks :: [Text] -- platform names
    }

sampleProfile :: Profile
sampleProfile =
    Profile
        { profileImage = "avatar-jessica.jpeg"
        , profileName = "Jessica Randall"
        , profileLocation = "London, United Kingdom"
        , profileBio = "Front-end developer and avid reader."
        , profileSocialLinks = ["GitHub", "Frontend Mentor", "LinkedIn", "Twitter", "Instagram"]
        }

profileCard :: Profile -> Html ()
profileCard profile = do
    el ~ screenCenter . flexDirection Column $ do
        el
            ~ margin 15
                . italic
                . color PageText
                . minWidth 440
            $ text "Atomic CSS"
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
                            img ("/images/" <> profileImage profile)
                                ~ rounded (Pct 0.5)
                                    . width 120
                                    . marginAuto
                            el (text $ profileName profile) ~ fontSize 24 . bold . pad (T 55)
                            el (text $ profileLocation profile) ~ fontSize 14 . bold . color Primary ~ pad (T 7)
                            el (text $ profileBio profile) ~ fontSize 14 . pad (T 30)
                            el
                                ~ pad (T 20)
                                $ mapM_ link' (profileSocialLinks profile)
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
                            . color Primary
                            . underline
                            . italic
                        $ "Benjamin THOMAS."

cursorPointer :: (Styleable h) => CSS h -> CSS h
cursorPointer = utility "cursor-pointer" ["cursor" :. "pointer"]

{-

el ~ transition 100 (Height 400) $ "Tall"
el ~ transition 100 (Height 100) $ "Small"

 -}

link' :: Text -> Html ()
link' name =
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
        $ text name

app :: Application
app req respond = do
    case pathInfo req of
        [] -> view (profileCard sampleProfile)
        ["static", "reset.css"] -> reset
        _ -> notFound
  where
    html h =
        respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] h

    notFound =
        respond $ responseLBS status404 [("Content-Type", "text/plain; charset=utf-8")] "Not Found"

    view v =
        html $ document $ renderLazyByteString v

    document cnt =
        [i|<html>
      <head>
        <link rel="stylesheet" type="text/css" href="static/reset.css"></link>
        <script>
          // Smart auto-reload: only reload if page content changed
          let lastContent = '';

          function checkForChanges() {
            fetch(window.location.href)
              .then(response => response.text())
              .then(html => {
                if (lastContent === '') {
                  lastContent = html;
                  return;
                }

                if (html !== lastContent) {
                  console.log('Page changed, reloading...');
                  window.location.reload();
                }
              })
              .catch(err => {
                // Server might be restarting, try again soon
                console.log('Server unavailable, retrying...');
              });
          }

          // Check every 500ms
          setInterval(checkForChanges, 500);
        </script>
      </head>
      <body>#{cnt}</body>
    </html>|]

    reset =
        respond $
            responseLBS
                status200
                [ ("Content-Type", "text/css; charset=utf-8")
                ]
                (fromStrict cssResetEmbed)

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