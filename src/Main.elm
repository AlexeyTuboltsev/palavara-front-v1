module Main exposing (..)

import AppData exposing (..)
import Browser
import Browser.Dom exposing (Viewport, getViewport, getViewportOf)
import Browser.Events exposing (onResize)
import Browser.Navigation as Navigation
import Constants exposing (mobileBreakpoint)
import Html exposing (Html, a, br, div, img, span, text)
import Html.Attributes exposing (class, href, id, src, style)
import Html.Events.Extra exposing (onClickPreventDefault)
import Html.Events.Extra.Pointer as Pointer
import Http exposing (expectJson, get)
import Icons
import List.Extra exposing (find, findIndex, getAt, indexedFoldl)
import Message exposing (Msg(..))
import Page exposing (GalleryContentData(..), GalleryPageData, InfoPageData, ListPageData, MenuData, MenuSectionData, MenuSectionType(..), MenuTagData, Page(..), StartPageData, calculateTopOffset, routeToMobilePage, routeToPage)
import Result exposing (Result)
import Route exposing (Route(..), parseToRoute, routeToUrlPath)
import Task
import Url exposing (Url)


type alias Flags =
    { apiBaseUrl : String
    , apiProtocol : String
    , apiPort : String
    , apiUrl : String
    }


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



-- UPDATE --


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
                        page =
                            case initProgressModel.viewport.viewport.width <= mobileBreakpoint of
                                True ->
                                    routeToMobilePage route 0 initProgressModel.data

                                False ->
                                    routeToPage route initProgressModel.data

                        newModelData =
                            { viewport = initProgressModel.viewport
                            , key = initProgressModel.key
                            , route = route
                            , page = page
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

                GoToRoute route ->
                    setRoute readyModelData route

                SetSectionRouteWithPageDimensions route dimensions ->
                    case dimensions of
                        Err _ ->
                            let
                                page =
                                    -- TODO add an error page
                                    routeToMobilePage Root 0 readyModelData.data

                                newModel =
                                    ReadyModel { readyModelData | route = route, page = page }
                            in
                            update (GoToRoute route) newModel

                        Ok sliderHeight ->
                            let
                                page =
                                    routeToMobilePage route sliderHeight readyModelData.data

                                newModel =
                                    ReadyModel { readyModelData | route = route, page = page }
                            in
                            ( newModel, Navigation.pushUrl readyModelData.key <| routeToUrlPath route )


                DownMsg ( xCoordinate, yCoordinate ) ->
                    --FIXME move this to a util fn
                    case readyModelData.page of
                        GalleryPage galleryPageData ->
                            case galleryPageData of
                                Page.GalleryPageData menuData contentData ->
                                    case contentData of
                                        MobileContentData mobileContentData ->
                                            let
                                                newMobileContentData =
                                                    MobileContentData { mobileContentData | pointerStart = Just ( yCoordinate, mobileContentData.topOffset ) }
                                            in
                                            ( ReadyModel { readyModelData | page = GalleryPage <| Page.GalleryPageData menuData newMobileContentData }, Cmd.none )

                                        _ ->
                                            ( ReadyModel readyModelData, Cmd.none )

                        _ ->
                            ( ReadyModel readyModelData, Cmd.none )

                MoveMsg ( xCoordinate, yCoordinate ) ->
                    case readyModelData.page of
                        GalleryPage galleryPageData ->
                            case galleryPageData of
                                Page.GalleryPageData menuData contentData ->
                                    case contentData of
                                        MobileContentData mobileContentData ->
                                            case mobileContentData.pointerStart of
                                                Just pointerStart ->
                                                    let
                                                        newTopOffset =
                                                            calculateSliderTopOffset pointerStart mobileContentData.topOffset yCoordinate

                                                        newMobileContentData =
                                                            MobileContentData { mobileContentData | pointerStart = Just pointerStart, topOffset = newTopOffset }
                                                    in
                                                    ( ReadyModel { readyModelData | page = GalleryPage <| Page.GalleryPageData menuData newMobileContentData }, Cmd.none )

                                                Nothing ->
                                                    ( ReadyModel readyModelData, Cmd.none )

                                        _ ->
                                            ( ReadyModel readyModelData, Cmd.none )

                        _ ->
                            ( ReadyModel readyModelData, Cmd.none )

                UpMsg ( xCoordinate, yCoordinate ) ->
                    case readyModelData.page of
                        GalleryPage galleryPageData ->
                            case galleryPageData of
                                Page.GalleryPageData menuData contentData ->
                                    case contentData of
                                        MobileContentData mobileContentData ->
                                            case mobileContentData.pointerStart of
                                                Just pointerStart ->
                                                    let
                                                        ( newTopOffset, maybeNewItem ) =
                                                            calculateFinalSliderTopOffset mobileContentData.sliderHeight mobileContentData.topOffset mobileContentData.activeItemIndex mobileContentData.items pointerStart

                                                        newRoute =
                                                            case maybeNewItem of
                                                                Nothing ->
                                                                    readyModelData.route

                                                                Just { itemId } ->
                                                                    updateRoute readyModelData.route itemId

                                                        newPage =
                                                            routeToMobilePage newRoute mobileContentData.sliderHeight readyModelData.data
                                                    in
                                                    ( ReadyModel { readyModelData | page = newPage }, Cmd.none )

                                                Nothing ->
                                                    ( ReadyModel readyModelData, Cmd.none )

                                        _ ->
                                            ( ReadyModel readyModelData, Cmd.none )

                        _ ->
                            ( ReadyModel readyModelData, Cmd.none )

                _ ->
                    ( model, Cmd.none )


setRoute modelData route =
    let
        cmdBatch =
            [ Navigation.pushUrl modelData.key <| routeToUrlPath route ]
    in
    if modelData.viewport.viewport.width <= mobileBreakpoint then
        let
            mobileModelData =
                ReadyModel { modelData | route = route, page = routeToMobilePage route 0 modelData.data }

            mobileUpdate =
                ( mobileModelData, getSliderViewport route :: cmdBatch |> Cmd.batch )
        in
        case route of
            SectionRoute _ ->
                mobileUpdate

            SectionImageRoute _ _ ->
                mobileUpdate

            TagRoute _ _ ->
                mobileUpdate

            TagImageRoute _ _ _ ->
                mobileUpdate

            _ ->
                ( mobileModelData, Cmd.batch cmdBatch )

    else
        ( ReadyModel { modelData | route = route, page = routeToPage route modelData.data }, Cmd.batch cmdBatch )


updateRoute oldRoute newImageId =
    case oldRoute of
        SectionRoute _ ->
            oldRoute

        SectionImageRoute sectionId _ ->
            SectionImageRoute sectionId newImageId

        TagRoute _ _ ->
            oldRoute

        TagImageRoute sectionId tagId _ ->
            TagImageRoute sectionId tagId newImageId

        _ ->
            oldRoute


calculateSliderTopOffset pointerStart topOffset yCoordinate =
    let
        ( initialPointerStart, oldTopOffset ) =
            pointerStart

        delta =
            yCoordinate - initialPointerStart
    in
    oldTopOffset + delta


calculateFinalSliderTopOffset : Float -> Float -> Int -> List Page.ItemContentData -> ( Float, Float ) -> ( Float, Maybe Page.ItemContentData )
calculateFinalSliderTopOffset sliderHeight topOffset activeItemIndex items pointerStart =
    let
        threshold =
            22

        ( _, oldTopOffset ) =
            pointerStart

        delta =
            oldTopOffset - topOffset
    in
    if abs delta > threshold then
        let
            newActiveItemIndex =
                case delta > 0 of
                    True ->
                        clamp 0 (List.length items) (activeItemIndex + 1)

                    False ->
                        clamp 0 (List.length items) (activeItemIndex - 1)

            maybeNewItem =
                getAt newActiveItemIndex items
        in
        ( calculateTopOffset sliderHeight newActiveItemIndex, maybeNewItem )

    else
        ( oldTopOffset, getAt activeItemIndex items )


getSliderViewport route =
    Task.attempt
        (SetSectionRouteWithPageDimensions route)
        (Task.map
            (\viewport ->
                viewport.viewport.height
            )
            (getViewportOf "slider-window")
        )


allFieldsPresent : Model -> Maybe ( Route, Model )
allFieldsPresent newModel =
    case newModel of
        InitModel data ->
            Maybe.map4
                (\appData vp key url ->
                    let
                        route =
                            parseToRoute url appData vp
                    in
                    ( route, InitProgressModel { data = appData, viewport = vp, key = key, route = route } )
                )
                data.data
                data.viewport
                (Just data.key)
                (Just data.url)

        _ ->
            Nothing



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
            Browser.Document "** palavara **" initPage

        InitProgressModel _ ->
            Browser.Document "** palavara **" initPage

        InitErrorModel _ ->
            Browser.Document "** palavara **" [ text "error" ]

        ReadyModel readyModelData ->
            Browser.Document "** palavara **" <| contentPage readyModelData.page


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


startPage : Page.StartPageData -> List (Html Msg)
startPage data =
    case data of
        Page.StartPageData menuData ->
            [ div [ class "start" ]
                [ div [ class "image-start" ] []
                , buildFullMenu menuData
                ]
            ]


infoPage : Page.InfoPageData -> List (Html Msg)
infoPage data =
    case data of
        Page.InfoPageData menuData ->
            [ div [ class "layout" ]
                [ div [] []
                , buildFullMenu menuData
                ]
            ]


listPage : Page.ListPageData -> List (Html Msg)
listPage data =
    case data of
        Page.ListPageData menuData ->
            [ div [ class "layout" ]
                [ div [] []
                , buildFullMenu menuData
                ]
            ]


galleryPage : Page.GalleryPageData -> List (Html Msg)
galleryPage data =
    case data of
        Page.GalleryPageData menuData contentData ->
            [ div [ class "layout" ]
                [ buildFullMenu menuData
                , case contentData of
                    GalleryContentData desktopContentData ->
                        buildPictures desktopContentData

                    GalleryImageContentData desktopContentImageData ->
                        buildPicturesAndImage desktopContentImageData

                    MobileContentData mobileContentData ->
                        buildMobilePictures mobileContentData
                ]
            ]


buildPictures contentData =
    div
        [ class "slider-window"
        , id "slider-window"
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
    div
        [ class "slider-window"
        , id "slider-window"
        , Pointer.onDown (relativePos >> DownMsg)
        , Pointer.onMove (relativePos >> MoveMsg)
        , Pointer.onUp (relativePos >> UpMsg)
        , Pointer.onCancel (relativePos >> UpMsg)
        , Html.Attributes.style "touch-action" "none"
        ]
        [ div
            [ class "image-group"
            , style "top" (String.fromFloat contentData.topOffset ++ "px")
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
    div [ class "menu-wrapper" ] [
    div [class "menu-background"] [ div [class "menu-background-inner"] [] ]
    , div [ class "menu" ] (List.append (buildEntries menuData) [buildLogo])
    ]



buildEntries : MenuData -> List (Html Msg)
buildEntries menuData =
    List.append
        [
        div [ class "logo-label" ] [ text "Varvara Polyakova"]
        , div [class "menu-line"] []
        ]
        (List.map (\sectionData -> buildEntry sectionData) menuData)



buildLogo : Html Msg
buildLogo =
        div [ class "logo", onClickPreventDefault <| GoToRoute Root ] [ Icons.logo  ]


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
        , href tagData.urlString
        , onClickPreventDefault tagData.onClickMessage
        ]
        [ text tagData.tagLabel ]


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
