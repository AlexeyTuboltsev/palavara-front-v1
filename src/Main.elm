module Main exposing (main)

import Constants exposing (sectionList, tagList)
import Html exposing (text)
import Browser
import Browser.Navigation as Navigation
import Browser.Dom exposing (Viewport,getViewport)
import Browser.Events exposing (onResize)
import Url
import Url.Builder exposing (absolute)
import Url.Parser exposing (Parser, parse, int, string, map, oneOf, s, top,custom, (</>))
import Task
import Debug

type alias SectionId = String
type alias TagId = String
type alias ImageId = String

type Route =
    Root
    | InfoRoute
    | SectionRoute SectionId
    | SectionImageRoute SectionId ImageId
    | TagRoute SectionId TagId
    | TagImageRoute SectionId TagId ImageId

type Page = StartPage

type alias ReadyModelData = {
    viewport: Viewport
    , key: Navigation.Key
    , route: Route
    , page: Page
    }

type alias InitModelData = {
    key: Navigation.Key
    , route: Route
    }

type Model = ReadyModel ReadyModelData | InitModel InitModelData

type Msg
  = NoOp
  | SetViewport Viewport
  | GetViewport
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case model of
        InitModel {key,route} ->
            case message of
                SetViewport viewport ->
                    (ReadyModel {viewport = viewport, key = key, route = route, page = StartPage}, Cmd.none)
                _ ->
                    (model, Cmd.none)
        ReadyModel readyModelData ->
            case message of
                GetViewport ->
                    (ReadyModel readyModelData, Task.perform SetViewport getViewport)
                SetViewport viewport ->
                    (ReadyModel {readyModelData | viewport = viewport}, Cmd.none)
                _ ->
                    (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
  onResize (\_ _ -> GetViewport)


main = Browser.application
   { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }

init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        _ = Debug.log "key" key
        route = parseToRoute url
        newUrl = routeToUrl route
    in
    --Task.perform SetViewport getViewport
      (InitModel {
      key = key
      , route = route
      }, Cmd.batch [
        Navigation.replaceUrl key newUrl
        ,Task.perform SetViewport getViewport
        ]
        )

view : Model -> Browser.Document Msg
view model =
    case model of
        InitModel initModelData ->
            Browser.Document "init" [text "init"]
        ReadyModel readyModelData ->
                    Browser.Document "ready" [text "ready"]

sectionParser =
    custom "SECTION" <| \segment ->
        if(List.member segment sectionList) then
            Just segment
        else
            Nothing

tagParser =
    custom "TAG" <| \segment ->
        if(List.member segment tagList) then
            Just segment
        else
            Nothing

routeParser : Parser (Route -> a) a
routeParser = oneOf
    [ map Root top
    , map InfoRoute (s "info")
    , map TagImageRoute (sectionParser </> tagParser </> string)
    , map TagRoute (sectionParser </> tagParser )
    , map SectionImageRoute (sectionParser </> string )
    , map SectionRoute (sectionParser)
    ]

parseToRoute url =
    Maybe.withDefault Root (parse routeParser url)

routeToUrl route =
    case route of
        Root ->
            absolute [] []
        InfoRoute ->
            absolute ["info"] []
        SectionRoute sectionId ->
            absolute [sectionId] []
        SectionImageRoute sectionId imageId ->
            absolute [sectionId,imageId] []
        TagRoute sectionId tagId ->
            absolute [sectionId,tagId] []
        TagImageRoute sectionId tagId imageId ->
            absolute [sectionId,tagId, imageId] []