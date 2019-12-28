module Main exposing (..)

import AppData exposing (..)
import Browser
import Browser.Dom exposing (Viewport, getElement, getViewport, getViewportOf)
import Browser.Events exposing (onResize)
import Browser.Navigation as Navigation
import Debug
import Html exposing (Html, a, br, div, img, span, text)
import Html.Attributes exposing (class, href, id, src, style)
import Html.Events exposing (onMouseOut, onMouseOver)
import Html.Events.Extra exposing (onClickPreventDefault)
import Html.Events.Extra.Pointer as Pointer
import Http exposing (expectJson, get)
import Icons
import List.Extra exposing (find, findIndex, indexedFoldl)
import Result exposing (Result)
import Task
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser as UrlParser exposing ((</>), Parser, custom, int, oneOf, parse, s, string, top)


type alias Flags =
    { apiBaseUrl : String
    , apiPort : String
    , apiUrl : String
    }


type Route
    = Root
    | InfoRoute
    | SectionRoute SectionId
    | SectionImageRoute SectionId ItemId
    | TagRoute SectionId TagId
    | TagImageRoute SectionId TagId ItemId


type alias MenuTagData =
    { tagId : TagId
    , tagLabel : String
    , tagIsActive : Bool
    , onClickMessage : Msg
    }


type alias MenuSectionData =
    { sectionType : MenuSectionType
    , sectionId : SectionId
    , sectionLabel : String
    , sectionIsActive : Bool
    , tags : List MenuTagData
    , onClickMessage : Msg
    , urlString : String
    }


type MenuSectionType
    = GalleryWithTags
    | Gallery
    | Info


type alias MenuData =
    List MenuSectionData


type alias ItemContentData =
    { itemId : ItemId
    , urlString : String
    , onClickMessage : Msg
    , width : Int
    , height : Int
    , isActive : Bool
    }


type alias ContentData =
    { items : List ItemContentData, activeItemIndex : Int }


type StartPageData
    = StartPageData MenuData


type ListPageData
    = ListPageData MenuData


type GalleryPageData
    = GalleryPageData MenuData ContentData


type InfoPageData
    = InfoPageData MenuData


type Page
    = StartPage StartPageData
    | InfoPage InfoPageData
    | ListPage ListPageData
    | GalleryPage GalleryPageData


type alias ReadyModelData =
    { viewport : Viewport
    , key : Navigation.Key
    , route : Route
    , page : Page
    , data : AppData
    }


type alias InProgressModelData =
    { key : Navigation.Key
    , route : Route
    , data : AppData
    }


type alias InitModelData =
    { url : Url
    , key : Navigation.Key
    }


type Model
    = ReadyModel ReadyModelData
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
    | SetRoutePageData Route String (Result Browser.Dom.Error ( Viewport, Browser.Dom.Element ))
    | DownMsg ( Float, Float )
    | MoveMsg ( Float, Float )
    | UpMsg ( Float, Float )



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case model of
        InitModel { key, url } ->
            case message of
                SetData result ->
                    case result of
                        Err err ->
                            ( InitErrorModel err, Cmd.none )

                        Ok data ->
                            initProgress url key data

                _ ->
                    ( model, Cmd.none )

        InitProgressModel { key, route, data } ->
            case message of
                SetViewport viewport ->
                    ( ReadyModel { viewport = viewport, key = key, route = route, page = routeToPage route data, data = data }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InitErrorModel errData ->
            case message of
                _ ->
                    ( model, Cmd.none )

        ReadyModel readyModelData ->
            case message of
                GetViewport ->
                    ( ReadyModel readyModelData, Task.perform SetViewport getViewport )

                SetViewport viewport ->
                    ( ReadyModel { readyModelData | viewport = viewport }, Cmd.none )

                SetRoute route ->
                    let
                        newUrl =
                            routeToUrlPath route
                    in
                    ( model
                    , Cmd.batch
                        [ Task.attempt (SetRoutePageData route newUrl) (Task.map2 (\x y -> ( x, y )) (getViewportOf "slider-window") (getElement "27.jpg"))
                        ]
                    )

                SetRoutePageData route newUrl r ->
                    let
                        _ =
                            Debug.log "bla" r
                    in
                    ( ReadyModel { readyModelData | route = route, page = routeToPage route readyModelData.data }
                    , Cmd.batch
                        [ Navigation.pushUrl readyModelData.key newUrl
                        ]
                    )

                UrlChanged url ->
                    if url.path == routeToUrlPath readyModelData.route then
                        ( model, Cmd.none )

                    else
                        let
                            route =
                                parseToRoute url readyModelData.data

                            page =
                                routeToPage route readyModelData.data
                        in
                        ( ReadyModel { readyModelData | route = route, page = page }, Navigation.pushUrl readyModelData.key <| Url.toString url )

                _ ->
                    ( model, Cmd.none )



-- MAIN --


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


initProgress url key data =
    let
        route =
            parseToRoute url data

        newUrl =
            routeToUrlPath route
    in
    ( InitProgressModel
        { key = key
        , route = route
        , data = data
        }
    , Cmd.batch
        [ Navigation.replaceUrl key newUrl
        , Task.perform SetViewport getViewport
        ]
    )


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init { apiBaseUrl, apiUrl, apiPort } url key =
    ( InitModel (InitModelData url key)
    , get { url = "http://" ++ apiBaseUrl ++ ":" ++ apiPort ++ "/" ++ apiUrl, expect = expectJson SetData appDataDecoder }
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize (\_ _ -> GetViewport)



-- VIEW --


view : Model -> Browser.Document Msg
view model =
    case model of
        InitModel _ ->
            Browser.Document "starting" initPage

        InitProgressModel _ ->
            Browser.Document "init" initPage

        InitErrorModel _ ->
            Browser.Document "init" [ text "error" ]

        ReadyModel readyModelData ->
            Browser.Document "*-PALAVARA-*" <| contentPage readyModelData.page


initPage =
    [ div [ class "loader" ]
        [ div [ class "logo-image" ]
            [ Icons.logo ]
        ]
    ]


contentPage : Page -> List (Html Msg)
contentPage page =
    case page of
        StartPage data ->
            startPage data

        InfoPage data ->
            infoPage data

        ListPage data ->
            listPage data

        GalleryPage data ->
            galleryPage data


startPage : StartPageData -> List (Html Msg)
startPage data =
    case data of
        StartPageData menuData ->
            [ div [ class "start" ]
                [ div [ class "image-start" ] []
                , buildFullMenu menuData
                ]
            ]


infoPage : InfoPageData -> List (Html Msg)
infoPage data =
    case data of
        InfoPageData menuData ->
            [ div [ class "layout" ]
                [ div [] []
                , buildFullMenu menuData
                ]
            ]


listPage : ListPageData -> List (Html Msg)
listPage data =
    case data of
        ListPageData menuData ->
            [ div [ class "layout" ]
                [ div [] []
                , buildFullMenu menuData
                ]
            ]


galleryPage : GalleryPageData -> List (Html Msg)
galleryPage data =
    case data of
        GalleryPageData menuData contentData ->
            [ div [ class "layout" ]
                [ buildFullMenu menuData
                , buildPictures contentData
                ]
            ]


initialOffset =
    150.0


pictureHeight =
    609.96


buildPictures contentData =
    div
        [ class "slider-window"
        , id "slider-window"
        , Pointer.onDown (relativePos >> DownMsg)
        , Pointer.onMove (relativePos >> MoveMsg)
        , Pointer.onUp (relativePos >> UpMsg)
        , Pointer.onCancel (relativePos >> UpMsg)
        ]
        [ div
            [ class "image-group"
            , style "top" (String.fromFloat (-1 * (pictureHeight * toFloat contentData.activeItemIndex - initialOffset)) ++ "px")

            --, onMouseOver FadeInPictures
            --, onMouseOut FadeOutPictures
            , id "image-group"
            ]
            (List.map
                (\itemData ->
                    buildSectionPicture
                        itemData.urlString
                        itemData.onClickMessage
                        itemData.isActive
                        itemData.itemId
                )
             <|
                contentData.items
            )
        ]


buildSectionPicture urlString onClickMessage isActive itemId =
    a
        [ id itemId
        , class
            (if isActive then
                "image active-image"

             else
                "image"
            )
        , onClickPreventDefault onClickMessage
        ]
        [ img [ src urlString ] []
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


buildEntry : MenuSectionData -> Html Msg
buildEntry sectionData =
    case sectionData.sectionType of
        Info ->
            buildInfoEntry sectionData

        _ ->
            buildGalleryWithTagsEntry sectionData


buildInfoEntry : MenuSectionData -> Html Msg
buildInfoEntry sectionData =
    div [ class "menu-entry info" ]
        [ a [ class "menu-entry-label", onClickPreventDefault sectionData.onClickMessage, href sectionData.urlString ] [ text sectionData.sectionLabel ] ]


buildGalleryWithTagsEntry : MenuSectionData -> Html Msg
buildGalleryWithTagsEntry sectionData =
    div [ class "menu-entry" ]
        [ div
            [ class
                ("menu-entry-label "
                    ++ (if sectionData.sectionIsActive == True then
                            "active"

                        else
                            ""
                       )
                )
            ]
            [ a [ onClickPreventDefault sectionData.onClickMessage, href sectionData.urlString ] [ text <| sectionData.sectionLabel ++ ":" ] ]
        , div [ class "menu-entry-groups" ] (buildTags sectionData.tags)
        ]


buildTags : List MenuTagData -> List (Html Msg)
buildTags tagDataList =
    List.map (\tagData -> buildTag tagData) tagDataList
        |> List.intersperse
            (span [ class "pipe" ]
                [ text "\u{00A0}\u{00A0}"
                , text "|"
                , text " \u{00A0}"
                ]
            )


buildTag : MenuTagData -> Html Msg
buildTag tagData =
    a
        [ class
            (if tagData.tagIsActive == True then
                "active"

             else
                ""
            )
        , href tagData.tagId
        , onClickPreventDefault tagData.onClickMessage
        ]
        [ text tagData.tagLabel ]



-- ROUTING


routeToUrlPath : Route -> String
routeToUrlPath route =
    case route of
        Root ->
            absolute [] []

        InfoRoute ->
            absolute [ "info" ] []

        SectionRoute sectionId ->
            absolute [ sectionId ] []

        SectionImageRoute sectionId imageId ->
            absolute [ sectionId, imageId ] []

        TagRoute sectionId tagId ->
            absolute [ sectionId, tagId ] []

        TagImageRoute sectionId tagId imageId ->
            absolute [ sectionId, tagId, imageId ] []


makeRootMenuData section =
    case section of
        GalleryWithTagsSectionType { sectionId, label, tags } ->
            makeMenuEntryData GalleryWithTags False sectionId label (SectionRoute sectionId) (makeMenuTagData tags sectionId)

        GallerySectionType { sectionId, label } ->
            MenuSectionData Gallery sectionId label False [] (SetRoute <| SectionRoute sectionId) (routeToUrlPath <| SectionRoute sectionId)

        InfoSectionType { sectionId, label } ->
            makeMenuEntryData Info False sectionId label InfoRoute []


makeInfoMenuData section =
    case section of
        GalleryWithTagsSectionType { sectionId, label, tags } ->
            makeMenuEntryData GalleryWithTags False sectionId label (SectionRoute sectionId) (makeMenuTagData tags sectionId)

        GallerySectionType { sectionId, label } ->
            MenuSectionData Gallery sectionId label False [] NoOp ""

        InfoSectionType { sectionId, label } ->
            makeMenuEntryData Info True sectionId label InfoRoute []


makeSectionMenuData activeSectionId sections =
    makeSectionMenuDataInternal activeSectionId sections makeMenuTagData


makeTagMenuData activeSectionId activeTagId sections =
    makeMenuTagDataWithActiveTag activeTagId
        |> makeSectionMenuDataInternal activeSectionId sections


filterMenuInfoSection sections =
    List.filter
        (\section ->
            case section of
                InfoSectionType _ ->
                    True

                _ ->
                    False
        )
        sections


makeSectionMenuDataInternal activeSectionId sections tagMenuGeneratingFn =
    filterMenuInfoSection sections
        |> List.map
            (\section ->
                case section of
                    InfoSectionType { sectionId, label } ->
                        makeMenuEntryData Info False sectionId label InfoRoute []

                    GalleryWithTagsSectionType { sectionId, label, tags } ->
                        let
                            sectionIsActive =
                                isSectionActive activeSectionId sectionId
                        in
                        tagMenuGeneratingFn tags sectionId
                            |> makeMenuEntryData GalleryWithTags sectionIsActive sectionId label (SectionRoute sectionId)

                    GallerySectionType { sectionId, label } ->
                        MenuSectionData Gallery sectionId label False [] NoOp ""
            )


generateMenuEntryAttributes : Bool -> Route -> { sectionIsActive : Bool, onClickMessage : Msg, urlString : String }
generateMenuEntryAttributes sectionIsActive nextRoute =
    case sectionIsActive of
        False ->
            { sectionIsActive = False, onClickMessage = SetRoute <| nextRoute, urlString = routeToUrlPath nextRoute }

        True ->
            { sectionIsActive = False, onClickMessage = NoOp, urlString = "" }


makeMenuEntryData : MenuSectionType -> Bool -> SectionId -> String -> Route -> List MenuTagData -> MenuSectionData
makeMenuEntryData menuSectionType sectionIsActive sectionId label nextRoute tagMenuData =
    let
        menuEntryAttributes =
            generateMenuEntryAttributes sectionIsActive nextRoute
    in
    MenuSectionData menuSectionType sectionId label menuEntryAttributes.sectionIsActive tagMenuData menuEntryAttributes.onClickMessage menuEntryAttributes.urlString


routeToPage : Route -> AppData -> Page
routeToPage route appData =
    case route of
        Root ->
            List.map makeRootMenuData appData
                |> StartPageData
                |> StartPage

        InfoRoute ->
            List.map makeInfoMenuData appData
                |> InfoPageData
                |> InfoPage

        SectionRoute activeSectionId ->
            GalleryPageData
                (makeSectionMenuData activeSectionId appData)
                (makeSectionContentData activeSectionId appData zeroItemIndex (SectionImageRoute activeSectionId))
                |> GalleryPage

        TagRoute activeSectionId activeTagId ->
            GalleryPageData
                (makeTagMenuData activeSectionId activeTagId appData)
                (makeTagContentData activeSectionId activeTagId appData zeroItemIndex (TagImageRoute activeSectionId activeTagId))
                |> GalleryPage

        SectionImageRoute activeSectionId imageId ->
            GalleryPageData
                (makeSectionMenuData activeSectionId appData)
                (makeSectionContentData activeSectionId appData (findItemIndex imageId) (SectionImageRoute activeSectionId))
                |> GalleryPage

        TagImageRoute activeSectionId activeTagId imageId ->
            GalleryPageData
                (makeTagMenuData activeSectionId activeTagId appData)
                (makeTagContentData activeSectionId activeTagId appData (findItemIndex imageId) (TagImageRoute activeSectionId activeTagId))
                |> GalleryPage


findSection sectionList sectionId =
    find
        (\section ->
            case section of
                GalleryWithTagsSectionType data ->
                    data.sectionId == sectionId

                _ ->
                    False
        )
        sectionList


findTag tagId section =
    case section of
        GalleryWithTagsSectionType { tags } ->
            find (\tag -> tag.tagId == tagId) tags

        _ ->
            Nothing


findGalleryWithTagsSectionType section =
    case section of
        GalleryWithTagsSectionType data ->
            Just data

        _ ->
            Nothing


makeItemContentDataInternal activeItemIndex onClickMessage =
    \i { itemId, width, height } acc ->
        { acc
            | items =
                List.append
                    acc.items
                    [ ItemContentData
                        itemId
                        ("/img/" ++ itemId)
                        (onClickMessage itemId)
                        width
                        height
                        (activeItemIndex == i)
                    ]
            , activeItemIndex = activeItemIndex
        }


makeItemContentData nextRoute itemIndexFn items =
    (\is -> itemIndexFn is) items
        |> Maybe.map
            (\activeItemIndex ->
                let
                    onClickMessage =
                        \itemId -> SetRoute <| nextRoute itemId
                in
                indexedFoldl
                    (makeItemContentDataInternal activeItemIndex onClickMessage)
                    { items = [], activeItemIndex = 0 }
                    items
            )


findItemIndex activeItemId items =
    findIndex (\x -> x.itemId == activeItemId) items


zeroItemIndex _ =
    Just 0


makeContentDataInternal activeSectionId sectionFn itemsFn appData =
    findSection appData activeSectionId
        |> Maybe.andThen
            (\section -> sectionFn section)
        |> Maybe.andThen
            (\{ items } -> itemsFn items)
        |> Maybe.withDefault { items = [], activeItemIndex = 0 }


makeSectionContentData activeSectionId appData itemIndexFn nextRoute =
    makeContentDataInternal activeSectionId findGalleryWithTagsSectionType (makeItemContentData nextRoute itemIndexFn) appData


makeTagContentData activeSectionId activeTagId appData itemIndexFn nextRoute =
    makeContentDataInternal activeSectionId (findTag activeTagId) (makeItemContentData nextRoute itemIndexFn) appData


isSectionActive sectionId activeSectionId =
    sectionId == activeSectionId


isTagActive tagId activeTagId =
    tagId == activeTagId


makeMenuTagData tags sectionId =
    List.map
        (\{ tagId, label } ->
            MenuTagData tagId label False (SetRoute <| TagRoute sectionId tagId)
        )
        tags


makeMenuTagDataWithActiveTag : TagId -> List TagData -> SectionId -> List MenuTagData
makeMenuTagDataWithActiveTag activeTagId tags sectionId =
    List.map
        (\{ tagId, label } ->
            let
                tagIsActive =
                    isTagActive tagId activeTagId

                onClickMessage =
                    if tagIsActive then
                        NoOp

                    else
                        SetRoute <| TagRoute sectionId tagId
            in
            MenuTagData tagId label tagIsActive onClickMessage
        )
        tags



-- URL PARSING --


tagImageParserGenerator : AppData -> List (Parser (Route -> b) b)
tagImageParserGenerator input =
    List.filter
        (\section ->
            case section of
                GalleryWithTagsSectionType _ ->
                    True

                _ ->
                    False
        )
        input
        |> List.concatMap
            (\section ->
                case section of
                    GalleryWithTagsSectionType { sectionId, tags } ->
                        List.map
                            (\{ tagId } -> UrlParser.map (\itemId -> TagImageRoute sectionId tagId itemId) (s sectionId </> s tagId </> string))
                            tags

                    GallerySectionType _ ->
                        [ UrlParser.map Root top ]

                    InfoSectionType _ ->
                        [ UrlParser.map Root top ]
            )


tagParserGenerator : AppData -> List (Parser (Route -> b) b)
tagParserGenerator input =
    List.filter
        (\section ->
            case section of
                GalleryWithTagsSectionType _ ->
                    True

                _ ->
                    False
        )
        input
        |> List.concatMap
            (\section ->
                case section of
                    GalleryWithTagsSectionType { sectionId, tags } ->
                        List.map (\{ tagId } -> UrlParser.map (TagRoute sectionId tagId) (s sectionId </> s tagId)) tags

                    GallerySectionType _ ->
                        [ UrlParser.map Root top ]

                    InfoSectionType _ ->
                        [ UrlParser.map Root top ]
            )


sectionImageParserGenerator : AppData -> List (Parser (Route -> b) b)
sectionImageParserGenerator input =
    List.filter
        (\section ->
            case section of
                GalleryWithTagsSectionType _ ->
                    True

                _ ->
                    False
        )
        input
        |> List.map
            (\section ->
                case section of
                    GalleryWithTagsSectionType { sectionId } ->
                        UrlParser.map (\itemId -> SectionImageRoute sectionId itemId) (s sectionId </> string)

                    GallerySectionType _ ->
                        UrlParser.map Root top

                    InfoSectionType _ ->
                        UrlParser.map Root top
            )


sectionParserGenerator : AppData -> List (Parser (Route -> b) b)
sectionParserGenerator input =
    List.map
        (\section ->
            case section of
                GalleryWithTagsSectionType { sectionId } ->
                    UrlParser.map (SectionRoute sectionId) (s sectionId)

                GallerySectionType { sectionId } ->
                    UrlParser.map (SectionRoute sectionId) (s sectionId)

                InfoSectionType { sectionId } ->
                    UrlParser.map (SectionRoute sectionId) (s sectionId)
        )
        input


routeParser : AppData -> Parser (Route -> a) a
routeParser data =
    oneOf
        [ UrlParser.map Root top
        , UrlParser.map InfoRoute (s "info")

        --, UrlParser.map DesignRoute (s "design")
        , oneOf (tagImageParserGenerator data)
        , oneOf (tagParserGenerator data)
        , oneOf (sectionImageParserGenerator data)
        , oneOf (sectionParserGenerator data)
        ]


parseToRoute url data =
    Maybe.withDefault Root (parse (routeParser data) url)


relativePos : Pointer.Event -> ( Float, Float )
relativePos event =
    event.pointer.offsetPos
