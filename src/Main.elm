module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Random exposing (generate)
import Time exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Star =
    { posX : Int
    , posY : Int
    , pixelSize : Int
    , speed : Int
    }


type alias Model =
    { stars : List Star }


model : Model
model =
    { stars = [] }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( model, initialNumStars )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (20 * millisecond) Tick



-- UPDATE


type Msg
    = GenerateNumStars Int
    | GeneratePositions (List ( Int, Int ))
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNumStars num ->
            let
                positions =
                    randomPositions num
            in
                ( model, positions )

        GeneratePositions posPairs ->
            let
                stars =
                    List.map
                        (\( a, b ) ->
                            { posX = a
                            , posY = b
                            , pixelSize = (getStarSize a)
                            , speed = (getStarSize a)
                            }
                        )
                        posPairs
            in
                ( { model | stars = stars }, Cmd.none )

        Tick time ->
            let
                stars =
                    List.map (\s -> { s | posX = calculateXPosition s }) model.stars
            in
                ( { model | stars = stars }, Cmd.none )



-- VIEW


starStyle : Star -> Attribute msg
starStyle star =
    let
        x =
            pixelString star.posX

        y =
            pixelString star.posY

        pix =
            pixelString star.pixelSize
    in
        Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "top", y )
            , ( "left", x )
            , ( "width", pix )
            , ( "height", pix )
            , ( "min-width", pix )
            , ( "min-height", pix )
            , ( "background-color", "#FFF" )
            ]


pixelString : Int -> String
pixelString x =
    (toString x) ++ "px"


listStars : List Star -> Html Msg
listStars stars =
    div [] (List.map (\star -> div [ starStyle star ] []) stars)


view : Model -> Html Msg
view model =
    div [] [ listStars model.stars ]



-- HELPERS


initialNumStars : Cmd Msg
initialNumStars =
    Random.generate GenerateNumStars (Random.int 500 800)


randomPositions : Int -> Cmd Msg
randomPositions numStars =
    Random.generate GeneratePositions (listGenerator numStars)


listGenerator : Int -> Random.Generator (List ( Int, Int ))
listGenerator num =
    Random.list num (Random.pair (Random.int 0 maxSize) (Random.int 0 maxSize))


calculateXPosition : Star -> Int
calculateXPosition s =
    let
        newPos =
            s.posX - s.speed

        reset =
            newPos < 0
    in
        case reset of
            True ->
                maxSize

            False ->
                newPos


getStarSize : Int -> Int
getStarSize x =
    (x % 3) + 1


maxSize : Int
maxSize =
    1000
