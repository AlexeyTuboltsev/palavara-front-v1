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


mobileBreakpoint =
    1024


type alias Flags =
    { apiBaseUrl : String
    , apiProtocol : String
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


type alias MobileContentDataType =
    { items : List ItemContentData, activeItemIndex : Int, sliderHeight : Float }


type alias ContentDataType =
    { items : List ItemContentData, activeItemIndex : Int }


type ContentData
    = MobileContentData MobileContentDataType
    | ContentData ContentDataType


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
    , viewport : Viewport
    }


type alias InitModelData =
    { url : Url
    , key : Navigation.Key
    , data : Maybe AppData
    , viewport : Maybe Viewport
    }


type Model
    = ReadyModel ReadyModelData
    | InitProgressModel InProgressModelData
    | InitModel InitModelData
    | InitErrorModel Http.Error


type Msg
    = NoOp
    | AppIsReady AppData
    | GetViewport
    | SetViewport Viewport
    | SetData (Result Http.Error AppData)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | SetRoute Route
    | SetInitialRoute Route
    | SetSectionRouteWithPageDimensions Route (Result Browser.Dom.Error Float)
    | DownMsg ( Float, Float )
    | MoveMsg ( Float, Float )
    | UpMsg ( Float, Float )



--| FadeInPictures
--| FadeOutPictures
-- UPDATE --


allFieldsPresent newModel =
    case newModel of
        InitModel d ->
            Maybe.map4
                (\appData vp key url ->
                    let
                        route =
                            parseToRoute url appData
                    in
                    ( route, InitProgressModel { data = appData, viewport = vp, key = key, route = route } )
                )
                d.data
                d.viewport
                (Just d.key)
                (Just d.url)

        _ ->
            Nothing


setRoute modelData route =
    if modelData.viewport.viewport.width <= mobileBreakpoint then
        case route of
            SectionRoute _ ->
                mobileSetRoute modelData route

            SectionImageRoute _ _ ->
                mobileSetRoute modelData route

            TagRoute _ _ ->
                mobileSetRoute modelData route

            TagImageRoute _ _ _ ->
                mobileSetRoute modelData route

            _ ->
                ( ReadyModel { modelData | route = route, page = routeToPage route modelData.data }, Navigation.pushUrl modelData.key <| routeToUrlPath route )

    else
        ( ReadyModel { modelData | route = route, page = routeToPage route modelData.data }, Navigation.pushUrl modelData.key <| routeToUrlPath route )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case model of
        InitModel initModel ->
            case message of
                SetData result ->
                    case result of
                        Err err ->
                            ( InitErrorModel err, Cmd.none )

                        Ok data ->
                            let
                                newModel =
                                    InitModel { initModel | data = Just data }
                            in
                            case allFieldsPresent newModel of
                                Just ( route, initProgressModel ) ->
                                    update (SetInitialRoute route) initProgressModel

                                Nothing ->
                                    ( newModel, Cmd.none )

                SetViewport viewport ->
                    let
                        newModel =
                            InitModel { initModel | viewport = Just viewport }
                    in
                    case allFieldsPresent newModel of
                        Just ( route, initProgressModel ) ->
                            update (SetInitialRoute route) initProgressModel

                        Nothing ->
                            ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InitProgressModel initProgressModel ->
            case message of
                SetInitialRoute route ->
                    let
                        newModelData =
                            { viewport = initProgressModel.viewport
                            , key = initProgressModel.key
                            , route = route
                            , page = routeToPage route initProgressModel.data
                            , data = initProgressModel.data
                            }
                    in
                    setRoute newModelData route

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
                    setRoute readyModelData route

                SetSectionRouteWithPageDimensions route dimensions ->
                    case dimensions of
                        Err _ ->
                            --TODO this is a loop!
                            let
                                page =
                                    routeToMobilePage route 0 readyModelData.data

                                newModel =
                                    ReadyModel { readyModelData | route = route, page = page }
                            in
                            update (SetRoute route) newModel

                        Ok viewportHeight ->
                            let
                                page =
                                    routeToMobilePage route viewportHeight readyModelData.data

                                newModel =
                                    ReadyModel { readyModelData | route = route, page = page }
                            in
                            ( newModel, Navigation.pushUrl readyModelData.key <| routeToUrlPath route )

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


mobileSetRoute readyModelData route =
    ( ReadyModel { readyModelData | route = route, page = routeToMobilePage route 0 readyModelData.data }
    , Task.attempt
        (SetSectionRouteWithPageDimensions route)
        (Task.map
            (\viewport ->
                viewport.viewport.height
            )
            (getViewportOf "slider-window")
        )
    )



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


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init { apiBaseUrl, apiUrl, apiPort, apiProtocol } url key =
    ( InitModel (InitModelData url key Nothing Nothing)
    , Cmd.batch
        [ get
            { url =
                apiProtocol
                    ++ "://"
                    ++ apiBaseUrl
                    ++ (if apiPort /= "none" then
                            ":" ++ apiPort

                        else
                            ""
                       )
                    ++ "/"
                    ++ apiUrl
            , expect = expectJson SetData appDataDecoder
            }
        , Task.perform SetViewport getViewport
        ]
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
                , case contentData of
                    ContentData desktopContentData ->
                        buildPictures desktopContentData

                    MobileContentData mobileContentData ->
                        buildMobilePictures mobileContentData
                ]
            ]


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
            [ class "image-group" ]
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


buildMobilePictures contentData =
    let
        imageHeight =
            contentData.sliderHeight * 0.7

        initialOffset =
            (contentData.sliderHeight - imageHeight) / 2

        gridGap =
            20
    in
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
            , style "top" (String.fromFloat (initialOffset - ((imageHeight + gridGap) * toFloat contentData.activeItemIndex)) ++ "px")

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


makeRootMenuData : SectionData -> MenuSectionData
makeRootMenuData section =
    case section of
        GalleryWithTagsSectionType { sectionId, label, tags } ->
            makeMenuEntryData GalleryWithTags False sectionId label (SectionRoute sectionId) (makeMenuTagData tags sectionId)

        GallerySectionType { sectionId, label } ->
            MenuSectionData Gallery sectionId label False [] (SetRoute <| SectionRoute sectionId) (routeToUrlPath <| SectionRoute sectionId)

        InfoSectionType { sectionId, label } ->
            makeMenuEntryData Info False sectionId label InfoRoute []


makeInfoMenuData : SectionData -> MenuSectionData
makeInfoMenuData section =
    case section of
        GalleryWithTagsSectionType { sectionId, label, tags } ->
            makeMenuEntryData GalleryWithTags False sectionId label (SectionRoute sectionId) (makeMenuTagData tags sectionId)

        GallerySectionType { sectionId, label } ->
            MenuSectionData Gallery sectionId label False [] NoOp ""

        InfoSectionType { sectionId, label } ->
            makeMenuEntryData Info True sectionId label InfoRoute []


makeSectionMenuData : SectionId -> List SectionData -> MenuData
makeSectionMenuData activeSectionId sections =
    makeSectionMenuDataInternal activeSectionId sections makeMenuTagData


makeTagMenuData : SectionId -> TagId -> List SectionData -> MenuData
makeTagMenuData activeSectionId activeTagId sections =
    makeMenuTagDataWithActiveTag activeTagId
        |> makeSectionMenuDataInternal activeSectionId sections


makeMobileSectionMenuData : SectionId -> List SectionData -> MenuData
makeMobileSectionMenuData activeSectionId sections =
    makeSectionMenuDataInternal activeSectionId (filterMenuInfoSection sections) makeMenuTagData


makeMobileTagMenuData : SectionId -> TagId -> List SectionData -> MenuData
makeMobileTagMenuData activeSectionId activeTagId sections =
    makeMenuTagDataWithActiveTag activeTagId
        |> makeSectionMenuDataInternal activeSectionId (filterMenuInfoSection sections)


filterMenuInfoSection : List SectionData -> List SectionData
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


makeSectionMenuDataInternal : SectionId -> List SectionData -> (List TagData -> SectionId -> List MenuTagData) -> MenuData
makeSectionMenuDataInternal activeSectionId sections tagMenuGeneratingFn =
    List.map
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
        sections


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



--routeToPageInternal : (SectionId -> AppData -> (List ItemData -> Maybe Int) -> (ItemId -> Route) -> ContentData) -> (SectionId -> TagId -> AppData -> (List ItemData -> Maybe Int) -> (ItemId -> Route) -> ContentData) -> Route -> AppData -> Page


routeToPageInternal sectionMenuFn tagMenuFn sectionFn tagFn route appData =
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
                (sectionMenuFn activeSectionId appData)
                (sectionFn activeSectionId appData zeroItemIndex (SectionImageRoute activeSectionId))
                |> GalleryPage

        TagRoute activeSectionId activeTagId ->
            GalleryPageData
                (tagMenuFn activeSectionId activeTagId appData)
                (tagFn activeSectionId activeTagId appData zeroItemIndex (TagImageRoute activeSectionId activeTagId))
                |> GalleryPage

        SectionImageRoute activeSectionId imageId ->
            GalleryPageData
                (sectionMenuFn activeSectionId appData)
                (sectionFn activeSectionId appData (findItemIndex imageId) (SectionImageRoute activeSectionId))
                |> GalleryPage

        TagImageRoute activeSectionId activeTagId imageId ->
            GalleryPageData
                (tagMenuFn activeSectionId activeTagId appData)
                (tagFn activeSectionId activeTagId appData (findItemIndex imageId) (TagImageRoute activeSectionId activeTagId))
                |> GalleryPage


routeToPage : Route -> AppData -> Page
routeToPage route appData =
    routeToPageInternal makeSectionMenuData makeTagMenuData makeSectionContentData makeTagContentData route appData


routeToMobilePage : Route -> Float -> AppData -> Page
routeToMobilePage route sliderHeight appData =
    routeToPageInternal makeMobileSectionMenuData makeMobileTagMenuData (makeMobileSectionContentData sliderHeight) (makeMobileTagContentData sliderHeight) route appData


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


findTag : TagId -> SectionData -> Maybe TagData
findTag tagId section =
    case section of
        GalleryWithTagsSectionType { tags } ->
            find (\tag -> tag.tagId == tagId) tags

        _ ->
            Nothing


findGalleryWithTagsSectionType : SectionData -> Maybe GalleryWithTagsSectionData
findGalleryWithTagsSectionType section =
    case section of
        GalleryWithTagsSectionType data ->
            Just data

        _ ->
            Nothing


makeItemContentDataInternal : Int -> (ItemId -> Msg) -> (Int -> ItemData -> { x | items : List ItemContentData, activeItemIndex : Int } -> { x | items : List ItemContentData, activeItemIndex : Int })
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


makeItemContentData : (ItemId -> Route) -> (List ItemData -> Maybe Int) -> List ItemData -> Maybe ContentDataType
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


makeMobileItemContentData : Float -> (ItemId -> Route) -> (List ItemData -> Maybe Int) -> List ItemData -> Maybe MobileContentDataType
makeMobileItemContentData sliderHeight nextRoute itemIndexFn items =
    (\is -> itemIndexFn is) items
        |> Maybe.map
            (\activeItemIndex ->
                let
                    onClickMessage =
                        \itemId -> SetRoute <| nextRoute itemId
                in
                indexedFoldl
                    (makeItemContentDataInternal activeItemIndex onClickMessage)
                    { items = [], activeItemIndex = 0, sliderHeight = sliderHeight }
                    items
            )


findItemIndex : ItemId -> List ItemData -> Maybe Int
findItemIndex activeItemId items =
    findIndex (\x -> x.itemId == activeItemId) items


zeroItemIndex : x -> Maybe Int
zeroItemIndex _ =
    Just 0


getSectionItems : SectionId -> (SectionData -> Maybe { a | items : List ItemData }) -> (List ItemData -> Maybe y) -> AppData -> Maybe y
getSectionItems activeSectionId sectionFn itemsFn appData =
    findSection appData activeSectionId
        |> Maybe.andThen
            (\section -> sectionFn section)
        |> Maybe.andThen
            (\{ items } -> itemsFn items)


makeSectionContentData : SectionId -> AppData -> (List ItemData -> Maybe Int) -> (ItemId -> Route) -> ContentData
makeSectionContentData activeSectionId appData itemIndexFn nextRoute =
    getSectionItems activeSectionId findGalleryWithTagsSectionType (makeItemContentData nextRoute itemIndexFn) appData
        |> Maybe.withDefault { items = [], activeItemIndex = 0 }
        |> ContentData


makeTagContentData : SectionId -> TagId -> AppData -> (List ItemData -> Maybe Int) -> (ItemId -> Route) -> ContentData
makeTagContentData activeSectionId activeTagId appData itemIndexFn nextRoute =
    getSectionItems activeSectionId (findTag activeTagId) (makeItemContentData nextRoute itemIndexFn) appData
        |> Maybe.withDefault { items = [], activeItemIndex = 0 }
        |> ContentData


makeMobileSectionContentData : Float -> SectionId -> AppData -> (List ItemData -> Maybe Int) -> (ItemId -> Route) -> ContentData
makeMobileSectionContentData sliderHeight activeSectionId appData itemIndexFn nextRoute =
    getSectionItems activeSectionId findGalleryWithTagsSectionType (makeMobileItemContentData sliderHeight nextRoute itemIndexFn) appData
        |> Maybe.withDefault { items = [], activeItemIndex = 0, sliderHeight = 0 }
        |> MobileContentData


makeMobileTagContentData : Float -> SectionId -> TagId -> AppData -> (List ItemData -> Maybe Int) -> (ItemId -> Route) -> ContentData
makeMobileTagContentData sliderHeight activeSectionId activeTagId appData itemIndexFn nextRoute =
    getSectionItems activeSectionId (findTag activeTagId) (makeMobileItemContentData sliderHeight nextRoute itemIndexFn) appData
        |> Maybe.withDefault { items = [], activeItemIndex = 0, sliderHeight = 0 }
        |> MobileContentData


isSectionActive : SectionId -> SectionId -> Bool
isSectionActive sectionId activeSectionId =
    sectionId == activeSectionId


isTagActive : TagId -> TagId -> Bool
isTagActive tagId activeTagId =
    tagId == activeTagId


makeMenuTagData : List TagData -> SectionId -> List MenuTagData
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


parseToRoute : Url.Url -> AppData -> Route
parseToRoute url data =
    Maybe.withDefault Root (parse (routeParser data) url)


relativePos : Pointer.Event -> ( Float, Float )
relativePos event =
    event.pointer.offsetPos



--
--debugLog s x =
--    let
--        _ =
--            Debug.log s x
--    in
--    x
