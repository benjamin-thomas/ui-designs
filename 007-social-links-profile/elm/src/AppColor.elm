module AppColor exposing
    ( pageBackground
    , pageText
    , cardBackground
    , cardBorder
    , linkBackground
    , linkBorder
    , primary
    , white
    )

import Element exposing (Color, rgb255)


pageBackground : Color
pageBackground =
    rgb255 20 20 20 -- #141414


pageText : Color
pageText =
    rgb255 128 128 128 -- #808080


cardBackground : Color
cardBackground =
    rgb255 31 31 31 -- #1F1F1F


cardBorder : Color
cardBorder =
    rgb255 16 16 16 -- #101010


linkBackground : Color
linkBackground =
    rgb255 51 51 51 -- #333333


linkBorder : Color
linkBorder =
    rgb255 32 32 32 -- #202020


primary : Color
primary =
    rgb255 196 248 42 -- #C4F82A


white : Color
white =
    rgb255 255 255 255 -- #FFFFFF