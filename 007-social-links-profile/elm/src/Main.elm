module Main exposing (main)

import AppColor
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HA


type alias Profile =
    { image : String
    , fullName : String
    , location : String
    , bio : String
    , socialLinks : List String
    }


type alias Model =
    { hoveredButton : Maybe String
    }


type Msg
    = ButtonHover String
    | ButtonLeave


init : Model
init =
    { hoveredButton = Nothing }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ButtonHover buttonText ->
            { model | hoveredButton = Just buttonText }

        ButtonLeave ->
            { model | hoveredButton = Nothing }


col : List (Attribute msg) -> List (Element msg) -> Element msg
col =
    column


elf : List (Attribute msg) -> Element msg -> Element msg
elf attrs =
    el (width fill :: attrs)


sampleProfile : Profile
sampleProfile =
    { image = "avatar-jessica.jpeg"
    , fullName = "Jessica Randall"
    , location = "London, United Kingdom"
    , bio = "Front-end developer and avid reader"
    , socialLinks = [ "GitHub", "Frontend Mentor", "LinkedIn", "Twitter", "Instagram" ]
    }


profileCard : { a | hoveredButton : Maybe String } -> Profile -> Element Msg
profileCard { hoveredButton } profile =
    col
        [ Background.color AppColor.cardBackground
        , padding 30
        , Border.rounded 10
        , width fill
        , Font.center
        ]
        [ col
            [ centerX
            , width fill
            , Font.center
            ]
            [ image
                [ centerX
                , Border.rounded 60
                , width (px 120)
                , height (px 120)
                , clip
                ]
                { src = "http://localhost:4010/assets/images/" ++ profile.image
                , description = "avatar"
                }
            , elf
                [ Font.size 25
                , Font.bold
                , paddingEach
                    { top = 50
                    , right = 0
                    , bottom = 0
                    , left = 0
                    }
                ]
                (text profile.fullName)
            , elf
                [ Font.size 16
                , Font.color AppColor.primary
                , paddingEach
                    { top = 15
                    , bottom = 0
                    , left = 0
                    , right = 0
                    }
                ]
                (text profile.location)
            , elf
                [ Font.size 16
                , paddingXY 0 30
                ]
                (text profile.bio)
            , col
                [ centerX
                , width fill
                , Font.center
                , paddingXY 0 15
                , spacing 15
                ]
                (List.map
                    (\str ->
                        let
                            hoverStyles =
                                if hoveredButton == Just str then
                                    (++)
                                        [ Background.color AppColor.primary
                                        , Font.color AppColor.linkBackground
                                        ]

                                else
                                    (::) (Background.color AppColor.linkBackground)
                        in
                        el
                            (hoverStyles
                                [ width fill
                                , paddingXY 0 18
                                , Border.rounded 9
                                , Font.semiBold
                                , Font.size 18
                                , Events.onMouseEnter (ButtonHover str)
                                , Events.onMouseLeave ButtonLeave
                                , Element.htmlAttribute (HA.style "cursor" "pointer")
                                ]
                            )
                            (text str)
                    )
                    profile.socialLinks
                )
            ]
        ]


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color AppColor.pageBackground ]
        (centerScreen <|
            col
                [ Font.color AppColor.white
                ]
                [ col
                    [ width (minimum 440 fill)
                    , centerX
                    , Font.center
                    ]
                    [ el
                        [ paddingXY 0 20
                        , Font.color AppColor.pageText
                        , Font.italic
                        ]
                        (text "Elm-UI")
                    , profileCard model sampleProfile
                    , col
                        [ width fill
                        , Font.size 17
                        ]
                        [ row
                            [ centerX
                            , paddingEach
                                { top = 20
                                , bottom = 0
                                , left = 0
                                , right = 0
                                }
                            ]
                            [ text "Challenge by\u{00A0}"
                            , link
                                [ Font.color AppColor.primary
                                , Font.underline
                                , Font.italic
                                ]
                                { url = "https://www.frontendmentor.io?ref=challenge"
                                , label = text "Frontend Mentor"
                                }
                            ]
                        , row [ centerX, padding 2 ]
                            [ text "Coded by "
                            , el
                                [ Font.color AppColor.primary
                                , Font.italic
                                ]
                                (text "Benjamin THOMAS")
                            ]
                        ]
                    ]
                ]
        )


centerScreen : Element msg -> Element msg
centerScreen x =
    el
        [ width fill
        , height fill
        ]
        (el
            [ centerX
            , centerY
            ]
            x
        )


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
