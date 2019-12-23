module Main exposing (main)

import Constants exposing (sectionList, tagList)
import Html exposing (Html,text, div, a, span, br)
import Html.Attributes exposing (class, href)
import Html.Events.Extra exposing (onClickPreventDefault)
import Browser
import Browser.Navigation as Navigation
import Browser.Dom exposing (Viewport,getViewport)
import Browser.Events exposing (onResize)
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser as UrlParser exposing (Parser, parse, int, string, oneOf, s, top,custom, (</>))
import Task
import Http exposing (get, expectJson)
import Array exposing (Array)
import Json.Decode as JD
import Result exposing (Result)
import Dict exposing (Dict)
import Icons
import Debug

type alias Flags = {
    apiBaseUrl : String
    , apiPort: String
    ,apiUrl : String
    }

type alias SectionId =
    String

type alias TagId = String
type alias ItemId = String

type alias GroupData =
    { label : String
    , tagId : TagId
    , itemOrder : List ItemId
    }

type alias ItemData =
    { itemId : ItemId
    , width : Int
    , height : Int
    }

type alias SectionData =
    { label : String
    , sectionId : SectionId
    , items: Dict ItemId ItemData
    , itemOrder: List ItemId
    , groups : List GroupData
    }

--type alias ItemDataWithId = {itemId:ItemId, ItemData}

type alias AppData = List SectionData

type Route =
    Root
    | InfoRoute
    | SectionRoute SectionId
    | SectionImageRoute SectionId ItemId
    | TagRoute SectionId TagId
    | TagImageRoute SectionId TagId ItemId

type alias MenuTagData = {
    tagId: TagId
    ,sectionId: SectionId
    ,tagLabel: String
    ,tagIsActive:Bool
    ,onClickMessage: Msg
    }

type alias MenuSectionData = {
    sectionId: SectionId
    ,sectionLabel: String
    ,sectionIsActive: Bool
    ,tags: List MenuTagData
    ,onClickMessage: Msg
    }

type alias MenuData = List MenuSectionData

type StartPageData = StartPageData MenuData
type ListPageData = ListPageData MenuData
type ContentPageData = ContentPageData MenuData
type InfoPageData = InfoPageData MenuData

type Page =
    StartPage StartPageData
    | InfoPage InfoPageData
    | ListPage ListPageData
    | ContentPage ContentPageData

type alias ReadyModelData = {
    viewport: Viewport
    , key: Navigation.Key
    , route: Route
    , page: Page
    , data: AppData
    }

type alias InProgressModelData = {
    key: Navigation.Key
    , route: Route
    , data: AppData
    }

type alias InitModelData ={
    url: Url
    , key: Navigation.Key
    }

type Model = ReadyModel ReadyModelData
    | InitProgressModel InProgressModelData
    | InitModel InitModelData
    | InitErrorModel Http.Error

type Msg
  = NoOp
  | GetViewport
  | SetViewport Viewport
  | SetData (Result Http.Error AppData)
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url
  | SetRoute Route


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case model of
        InitModel {key, url} ->
            case message of
                SetData result ->
                    case result of
                        Err err ->
                            (InitErrorModel err, Cmd.none)
                        Ok data ->
                            initProgress url key data
                _ ->
                    (model, Cmd.none)
        InitProgressModel {key,route, data} ->
            case message of
                SetViewport viewport ->
                    (ReadyModel {viewport = viewport, key = key, route = route, page = routeToPage route data, data = data}, Cmd.none)
                _ ->
                    (model, Cmd.none)
        InitErrorModel errData ->
            case message of
                _ ->
                    (model, Cmd.none)
        ReadyModel readyModelData ->
            case message of
                GetViewport ->
                    (ReadyModel readyModelData, Task.perform SetViewport getViewport)
                SetViewport viewport ->
                    (ReadyModel {readyModelData | viewport = viewport}, Cmd.none)
                --SetRoute    TODO
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

initProgress url key data =
    let
        route = parseToRoute url data
        newUrl = routeToUrl route
    in
      (InitProgressModel {
      key = key
      , route = route
      , data = data
      },Cmd.batch
         [
            Navigation.replaceUrl key newUrl
            , Task.perform SetViewport getViewport
        ]
        )

init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init {apiBaseUrl, apiUrl, apiPort} url key =
    (InitModel (InitModelData url key)
    ,get {url = ("http://" ++ apiBaseUrl ++ ":" ++ apiPort ++ "/" ++ apiUrl), expect = expectJson SetData appDataDecoder}
    )

view : Model -> Browser.Document Msg
view model =
    case model of
        InitModel _ ->
           Browser.Document "starting" initPage
        InitProgressModel _ ->
           Browser.Document "init" initPage
        InitErrorModel _ ->
           Browser.Document "init" [text "error"]
        ReadyModel readyModelData ->
           Browser.Document "ready" <| contentPage readyModelData.page


initPage = [div [ class "loader" ]
              [ div [ class "logo-image" ]
                  [ Icons.logo ]
              ]
          ]

contentPage: Page -> List (Html Msg)
contentPage page =
    case page of
        StartPage data ->
           startPage data
        InfoPage _ ->
            []
        ListPage _ ->
            []
        ContentPage _ ->
            []


startPage data =
    case data of
        StartPageData menuData ->
            [div [ class "start" ]
                [ div [ class "image-start" ] []
                , buildFullMenu menuData
                ]
            ]


buildFullMenu : MenuData -> Html Msg
buildFullMenu menuData =
    div [ class "menu" ] (buildEntries menuData)

buildEntries : MenuData -> List (Html Msg)
buildEntries menuData =
    buildLogo
        --:: buildInfoEntry
        :: List.map (\sectionData -> buildEntry sectionData) menuData


buildLogo : Html Msg
buildLogo =
    div [ class "logo" ]
        [ div [ class "logo-label" ]
            [ text "Varvara Polyakova"
            , span [ class "logo-label-byline" ]
                [ br [] []
                , text "illustration, graphics, ceramics "
                ]
            ]
        , div [ class "logo-image", onClickPreventDefault <| SetRoute Root ]
            [ Icons.logo
            ]
        ]

--buildInfoEntry : Html Msg
--buildInfoEntry =
--    div [ class "menu-entry info" ]
--        [ a [ class "menu-entry-label", href "info", onClickPreventDefault <| SetRoute InfoRoute ] [ text "info" ] ]

buildEntry: MenuSectionData -> Html Msg
buildEntry sectionData =
    div [ class "menu-entry", onClickPreventDefault sectionData.onClickMessage ]
        [ div [ class ("menu-entry-label " ++ (if sectionData.sectionIsActive == True then "active" else "")) ] [ text <| sectionData.sectionLabel ++ ":" ]
        , div [ class "menu-entry-groups" ] (buildGroups sectionData.tags)
        ]

buildGroups : List MenuTagData -> List (Html Msg)
buildGroups tagDataList =
    List.map (\tagData -> buildGroup tagData) tagDataList
        |> List.intersperse
            (span [ class "pipe" ]
                [ text "  "
                , text "|"
                , text "  "
                ]
            )

buildGroup : MenuTagData -> Html Msg
buildGroup tagData =
    a [
    class (if tagData.tagIsActive == True then "active" else "")
    ,href tagData.tagId
    ,onClickPreventDefault tagData.onClickMessage
    ] [ text tagData.tagLabel ]


-- URL PARSING --

tagImageParserGenerator: AppData -> List (Parser (Route -> b) b)
tagImageParserGenerator input =
    List.concatMap (\{sectionId,groups} ->
        List.map (\{tagId} -> UrlParser.map (\itemId -> TagImageRoute sectionId tagId itemId ) (s sectionId </> s tagId </> string)) groups
    ) input

tagParserGenerator: AppData ->  List (Parser (Route -> b) b)
tagParserGenerator input =
    List.concatMap (\{sectionId,groups} ->
        List.map (\{tagId} -> UrlParser.map (TagRoute sectionId tagId ) (s sectionId </> s tagId)) groups
    ) input

sectionImageParserGenerator: AppData -> List (Parser (Route -> b) b)
sectionImageParserGenerator input =
        List.map (\{sectionId} -> UrlParser.map (\itemId -> SectionImageRoute sectionId itemId ) (s sectionId </> string)) input

sectionParserGenerator: AppData -> List (Parser (Route -> b) b)
sectionParserGenerator input =
        List.map (\{sectionId} -> UrlParser.map (SectionRoute sectionId ) (s sectionId)) input


routeParser : AppData -> Parser (Route -> a) a
routeParser data = oneOf
    [ UrlParser.map Root top
    , UrlParser.map InfoRoute (s "info")
    , oneOf (tagImageParserGenerator data)
    , oneOf (tagParserGenerator data)
    , oneOf (sectionImageParserGenerator data)
    , oneOf (sectionParserGenerator data)
    ]

parseToRoute url data =
    Maybe.withDefault Root (parse (routeParser data) url )

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

routeToPage: Route -> AppData -> Page
routeToPage route appData =
    case route of
        Root ->
            let menuData =  List.map
                    (\{sectionId,label,groups} ->
                        MenuSectionData sectionId label False [] NoOp
                    ) appData
            in
            StartPage <| StartPageData menuData
        InfoRoute ->
            let menuData =  List.map
                    (\{sectionId,label,groups} ->
                        MenuSectionData sectionId label True [] NoOp
                    ) appData
            in
            InfoPage <| InfoPageData menuData
        SectionRoute activeSectionId ->
             let menuData =  List.map
                    (\{sectionId,label,groups} ->
                        MenuSectionData sectionId label (isSectionActive sectionId activeSectionId) [] NoOp
                    ) appData
             in
             ListPage <| ListPageData menuData
        TagRoute activeSectionId activeTagId ->
            let menuData =  List.map
                    (\{sectionId,label,groups} ->
                        MenuSectionData sectionId label (isSectionActive sectionId activeSectionId) [] NoOp
                    ) appData
            in
            ListPage <| ListPageData menuData
        SectionImageRoute activeSectionId imageId ->
            let menuData =  List.map
                    (\{sectionId,label,groups} ->
                        MenuSectionData sectionId label (isSectionActive sectionId activeSectionId) [] NoOp
                    ) appData
            in
            ContentPage <| ContentPageData menuData
        TagImageRoute activeSectionId activeTagId imageId ->
            let menuData =  List.map
                    (\{sectionId,label,groups} ->
                        MenuSectionData sectionId label (isSectionActive sectionId activeSectionId) [] NoOp
                    ) appData
            in
            ContentPage <| ContentPageData menuData

isSectionActive sectionId activeSectionId =
    sectionId == activeSectionId

-- JSON --

appDataDecoder : JD.Decoder (List SectionData)
appDataDecoder =
    JD.field "sections" (JD.list sectionDataDecoder)

itemDataDecoder: JD.Decoder ItemData
itemDataDecoder =
   JD.map3 ItemData
        itemIdDecoder
        (JD.field "width" JD.int)
        (JD.field "height" JD.int)

itemIdDecoder =
     (JD.field "itemId" JD.string)

groupDataDecoder: JD.Decoder GroupData
groupDataDecoder =
    JD.map3 GroupData
        (JD.field "label" JD.string)
        (JD.field "groupId" JD.string)
        (JD.field "items" (JD.list itemIdDecoder))

itemsDecoder =
    JD.list itemDataDecoder
    |> JD.map (\decodedList -> List.map (\item -> (item.itemId,item)) decodedList )
    |> JD.map Dict.fromList

itemOrderDecoder =
    JD.list itemDataDecoder
    |> JD.map (\decodedList -> List.map (\item -> item.itemId) decodedList )

sectionDataDecoder: JD.Decoder SectionData
sectionDataDecoder =
    JD.map5 SectionData
        (JD.field "label" JD.string)
        (JD.field "sectionId" JD.string)
        (JD.field "items" itemsDecoder)
        (JD.field "items" itemOrderDecoder)
        (JD.field "groups" (JD.list groupDataDecoder))
