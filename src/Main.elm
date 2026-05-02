module Main exposing (..)

import AppData exposing (..)
import Browser
import Browser.Dom exposing (Viewport, getViewport, getViewportOf)
import Browser.Events exposing (onResize)
import Browser.Navigation as Navigation exposing (Key, load)
import Constants exposing (mobileBreakpoint)
import Html exposing (Html, a, br, div, img, source, span, text)
import Html.Attributes exposing (alt, attribute, class, href, id, property, rel, src, style, target)
import Html.Events.Extra exposing (onClickPreventDefault, onClickPreventDefaultAndStopPropagation)
import Html.Events.Extra.Pointer as Pointer
import Html.Keyed exposing (node)
import Html.Parser.Util
import Http exposing (expectJson, get)
import Icons exposing (instagram, logo, minus, plus)
import Json.Encode as Json
import List.Extra exposing (find, getAt)
import Message exposing (Msg(..))
import Page exposing (ActiveItemContentData, GalleryContentData(..), GalleryContentDataType, GalleryImageContentDataType, GalleryPageData, InfoPageData, ItemContentData, ListPageData, MenuData(..), MenuSectionData, MenuSectionType(..), MenuTagData, MobileGalleryContentDataType, Page(..), StartPageData, calculateTopOffset, findItemIndex, findSection, findTag, generateGalleryContentData, generateGalleryItemContentData, generateInfoContentData, generateInfoMenuData, generateMobileGalleryItemContentData, generateMobileGalleryMenuData, generateMobileMenuData, generateRootMenuData, generateSectionMenuData, getGalleryWithTagsSectionData)
import Result exposing (Result)
import Route exposing (Route(..), parseToRoute, routeToUrlPath)
import Task
import Url exposing (Url)
import Html.Parser

etsyLink =
    "https://www.etsy.com/shop/palavara/"

studioLink =
    "https://studio.palavara.com/"

instagramLink =
    "https://www.instagram.com/palavara_ceramics/"


type alias Flags =
    { apiBaseUrl : String
    , apiProtocol : String
    , apiPort : String
    , dataPath : String
    , imagePath : String
    }


type alias ReadyModelData =
    { viewport : Viewport
    , key : Navigation.Key
    , apiUrl : String
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
    , dataUrl : String
    , key : Navigation.Key
    , data : Maybe AppData
    , viewport : Maybe Viewport
    }


type Model
    = ReadyModel ReadyModelData
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
                                Just ( route, readyModelData ) ->
                                    update (GoToRoute route) (ReadyModel readyModelData)

                                Nothing ->
                                    ( newModel, Cmd.none )

                SetViewport viewport ->
                    let
                        newModel =
                            InitModel { initModel | viewport = Just viewport }
                    in
                    case allFieldsPresent newModel of
                        Just ( route, readyModelData ) ->
                            update (GoToRoute route) (ReadyModel readyModelData)

                        Nothing ->
                            ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InitErrorModel _ ->
            case message of
                _ ->
                    ( model, Cmd.none )

        ReadyModel readyModelData ->
            case message of
                UrlChanged url ->
                    ( ReadyModel readyModelData, Cmd.none )

                GetViewport ->
                    ( ReadyModel readyModelData, Task.perform SetViewport getViewport )

                SetViewport viewport ->
                    -- Re-derive the page when the viewport crosses the
                    -- mobile-breakpoint boundary. Both the route parser
                    -- and generatePageData / generateMobilePageData
                    -- branch on `viewport.width < mobileBreakpoint`, so
                    -- without this re-derive the model's viewport flips
                    -- mode but the rendered page stays in the previous
                    -- mode — content and CSS disagree (e.g., desktop
                    -- grid markup with mobile CSS, or vice versa). Most
                    -- visible after orientation change on tablets, and
                    -- that's the bug the visual-regression item-route
                    -- tablet snapshots were exposing.
                    let
                        oldIsMobile =
                            readyModelData.viewport.viewport.width < mobileBreakpoint

                        newIsMobile =
                            viewport.viewport.width < mobileBreakpoint

                        newModelData =
                            { readyModelData | viewport = viewport }
                    in
                    if oldIsMobile == newIsMobile then
                        ( ReadyModel newModelData, Cmd.none )

                    else
                        update (GoToRoute newModelData.route) (ReadyModel newModelData)

                GoToRoute route ->
                    case readyModelData.viewport.viewport.width < mobileBreakpoint of
                        True ->
                            generateMobilePageData readyModelData readyModelData.viewport.scene.height route

                        False ->
                            generatePageData readyModelData route

                OpenMenu ->
                    case readyModelData.page of
                        GalleryPage galleryPageData ->
                            case galleryPageData of
                                Page.GalleryPageData menuData contentData ->
                                    case menuData of
                                        MobileTogglingMenuData mobileMenuData ->
                                            MobileTogglingMenuData { mobileMenuData | menuOpen = True }
                                                |> (\m -> Page.GalleryPageData m contentData)
                                                |> GalleryPage
                                                |> (\page -> ( ReadyModel { readyModelData | page = page }, Cmd.none ))

                                        _ ->
                                            ( ReadyModel readyModelData, Cmd.none )

                        _ ->
                            ( ReadyModel readyModelData, Cmd.none )

                CloseMenu ->
                    case readyModelData.page of
                        GalleryPage galleryPageData ->
                            case galleryPageData of
                                Page.GalleryPageData menuData contentData ->
                                    case menuData of
                                        MobileTogglingMenuData mobileMenuData ->
                                            MobileTogglingMenuData { mobileMenuData | menuOpen = False }
                                                |> (\m -> Page.GalleryPageData m contentData)
                                                |> GalleryPage
                                                |> (\page -> ( ReadyModel { readyModelData | page = page }, Cmd.none ))

                                        _ ->
                                            ( ReadyModel readyModelData, Cmd.none )

                        _ ->
                            ( ReadyModel readyModelData, Cmd.none )

                DownMsg ( _, yCoordinate ) ->
                    case readyModelData.page of
                        GalleryPage galleryPageData ->
                            case galleryPageData of
                                Page.GalleryPageData menuData contentData ->
                                    case contentData of
                                        MobileGalleryContentData mobileContentData ->
                                            let
                                                newMobileContentData =
                                                    MobileGalleryContentData { mobileContentData | pointerStart = Just ( yCoordinate, mobileContentData.topOffset ) }
                                            in
                                            ( ReadyModel { readyModelData | page = GalleryPage <| Page.GalleryPageData menuData newMobileContentData }, Cmd.none )

                                        _ ->
                                            ( ReadyModel readyModelData, Cmd.none )

                        _ ->
                            ( ReadyModel readyModelData, Cmd.none )

                MoveMsg ( _, yCoordinate ) ->
                    case readyModelData.page of
                        GalleryPage galleryPageData ->
                            case galleryPageData of
                                Page.GalleryPageData menuData contentData ->
                                    case contentData of
                                        MobileGalleryContentData mobileContentData ->
                                            case mobileContentData.pointerStart of
                                                Just pointerStart ->
                                                    let
                                                        newTopOffset =
                                                            calculateSliderTopOffset pointerStart mobileContentData.topOffset yCoordinate

                                                        newMobileContentData =
                                                            MobileGalleryContentData { mobileContentData | pointerStart = Just pointerStart, topOffset = newTopOffset }
                                                    in
                                                    ( ReadyModel { readyModelData | page = GalleryPage <| Page.GalleryPageData menuData newMobileContentData }, Cmd.none )

                                                Nothing ->
                                                    ( ReadyModel readyModelData, Cmd.none )

                                        _ ->
                                            ( ReadyModel readyModelData, Cmd.none )

                        _ ->
                            ( ReadyModel readyModelData, Cmd.none )

                UpMsg ( _, _ ) ->
                    case readyModelData.page of
                        GalleryPage galleryPageData ->
                            case galleryPageData of
                                Page.GalleryPageData _ contentData ->
                                    case contentData of
                                        MobileGalleryContentData mobileContentData ->
                                            let
                                                newRoute =
                                                    mobileContentData.pointerStart
                                                        |> Maybe.andThen (\pointerStart -> calculateFinalSliderTopOffset mobileContentData.sliderHeight mobileContentData.topOffset mobileContentData.activeItemIndex mobileContentData.items pointerStart)
                                                        |> Maybe.map (\{ itemId } -> updateRoute readyModelData.route itemId)
                                                        |> Maybe.withDefault readyModelData.route
                                            in
                                            update (GoToRoute newRoute) (ReadyModel readyModelData)

                                        _ ->
                                            ( ReadyModel readyModelData, Cmd.none )

                        _ ->
                            ( ReadyModel readyModelData, Cmd.none )

                GoToInstagram ->
                    ( model, load instagramLink )

                _ ->
                    ( model, Cmd.none )


generatePageData : ReadyModelData -> Route -> ( Model, Cmd Msg )
generatePageData modelData activeRoute =
    case activeRoute of
        Root ->
            let
                ( finalRoute, page ) =
                    List.map generateRootMenuData modelData.data
                        |> (\sd -> MenuData { menuSectionData = sd })
                        |> Page.StartPageData
                        |> StartPage
                        |> Tuple.pair activeRoute
            in
            ( ReadyModel { modelData | page = page }, Navigation.pushUrl modelData.key <| routeToUrlPath finalRoute )

        InfoRoute ->
            let
                maybeRouteAndPage =
                    findSection modelData.data "info"
                        |> Maybe.andThen
                            (\sd ->
                                case sd of
                                    InfoSectionType id ->
                                        Just id

                                    _ ->
                                        Nothing
                            )
                        |> Maybe.map
                            (\id ->
                                generateInfoContentData id.text modelData.apiUrl id.imageId id.lqip id.widths id.originalWidth id.originalHeight
                                    |> Page.InfoPageData (List.map generateInfoMenuData modelData.data |> (\sd -> MenuInfoData { menuSectionData = sd }))
                                    |> InfoPage
                                    |> Tuple.pair activeRoute
                            )
            in
            case maybeRouteAndPage of
                Just ( route, page ) ->
                    ( ReadyModel { modelData | page = page }, Navigation.pushUrl modelData.key <| routeToUrlPath route )

                Nothing ->
                    ( ReadyModel modelData, Navigation.pushUrl modelData.key <| routeToUrlPath Root )

        SectionRoute activeSectionId ->
            let
                maybeItems =
                    findSection modelData.data activeSectionId
                        |> Maybe.andThen (\sectionData -> getGalleryWithTagsSectionData sectionData)
                        |> Maybe.map (\galleryWithTagsSectionData -> galleryWithTagsSectionData.items)
            in
            case maybeItems of
                Just items ->
                    generateGalleryContentData modelData.apiUrl (SectionImageRoute activeSectionId) items
                        |> GalleryContentData
                        |> Page.GalleryPageData
                            (generateSectionMenuData activeSectionId Nothing modelData.data)
                        |> GalleryPage
                        |> (\page -> ( ReadyModel <| ReadyModelData modelData.viewport modelData.key modelData.apiUrl activeRoute page modelData.data, Navigation.pushUrl modelData.key <| routeToUrlPath activeRoute ))

                Nothing ->
                    ( ReadyModel modelData, Navigation.pushUrl modelData.key <| routeToUrlPath Root )

        SectionImageRoute activeSectionId itemId ->
            let
                maybeItems =
                    findSection modelData.data activeSectionId
                        |> Maybe.andThen (\sectionData -> getGalleryWithTagsSectionData sectionData)
                        |> Maybe.map (\galleryWithTagsSectionData -> galleryWithTagsSectionData.items)

                maybeActiveItem =
                    maybeItems
                        |> Maybe.andThen (\items -> find (\item -> item.itemId == itemId) items)

                maybeRouteAndBla =
                    Maybe.map2
                        (\items activeItem -> ( items, activeItem ))
                        maybeItems
                        maybeActiveItem
            in
            case maybeRouteAndBla of
                Just ( items, activeItem ) ->
                    generateGalleryItemContentData modelData.apiUrl (SectionImageRoute activeSectionId) activeItem items
                        |> GalleryImageContentData
                        |> Page.GalleryPageData
                            (generateSectionMenuData activeSectionId Nothing modelData.data)
                        |> GalleryPage
                        |> (\page -> ( ReadyModel { modelData | route = activeRoute, page = page }, Navigation.pushUrl modelData.key <| routeToUrlPath activeRoute ))

                Nothing ->
                    ( ReadyModel modelData, Navigation.pushUrl modelData.key <| routeToUrlPath Root )

        TagRoute activeSectionId activeTagId ->
            let
                maybeItems =
                    findSection modelData.data activeSectionId
                        |> Maybe.andThen (\sectionData -> findTag activeTagId sectionData)
                        |> Maybe.map (\tagData -> tagData.items)
            in
            case maybeItems of
                Just items ->
                    generateGalleryContentData modelData.apiUrl (TagImageRoute activeSectionId activeTagId) items
                        |> GalleryContentData
                        |> Page.GalleryPageData
                            (generateSectionMenuData activeSectionId (Just activeTagId) modelData.data)
                        |> GalleryPage
                        |> (\page -> ( ReadyModel { modelData | route = activeRoute, page = page }, Navigation.pushUrl modelData.key <| routeToUrlPath activeRoute ))

                Nothing ->
                    ( ReadyModel modelData, Navigation.pushUrl modelData.key <| routeToUrlPath Root )

        TagImageRoute activeSectionId activeTagId itemId ->
            let
                maybeItems =
                    findSection modelData.data activeSectionId
                        |> Maybe.andThen (\sectionData -> findTag activeTagId sectionData)
                        |> Maybe.map (\tagData -> tagData.items)

                maybeActiveItem =
                    maybeItems
                        |> Maybe.andThen (\items -> find (\item -> item.itemId == itemId) items)

                maybeItemsAndActiveItem =
                    Maybe.map2
                        (\items activeItem -> ( items, activeItem ))
                        maybeItems
                        maybeActiveItem
            in
            case maybeItemsAndActiveItem of
                Just ( items, activeItem ) ->
                    generateGalleryItemContentData modelData.apiUrl (TagImageRoute activeSectionId activeTagId) activeItem items
                        |> GalleryImageContentData
                        |> Page.GalleryPageData
                            (generateSectionMenuData activeSectionId (Just activeTagId) modelData.data)
                        |> GalleryPage
                        |> (\page -> ( ReadyModel { modelData | route = activeRoute, page = page }, Navigation.pushUrl modelData.key <| routeToUrlPath activeRoute ))

                Nothing ->
                    ( ReadyModel modelData, Navigation.pushUrl modelData.key <| routeToUrlPath Root )


generateMobilePageData : ReadyModelData -> Float -> Route -> ( Model, Cmd Msg )
generateMobilePageData modelData sliderHeight activeRoute =
    case activeRoute of
        Root ->
            let
                page =
                    List.map generateMobileMenuData modelData.data
                        |> (\sd -> MobileMenuData { menuSectionData = sd })
                        |> Page.StartPageData
                        |> StartPage
            in
            ( ReadyModel { modelData | page = page }, Navigation.pushUrl modelData.key <| routeToUrlPath activeRoute )

        InfoRoute ->
            let
                maybeRouteAndPage =
                    findSection modelData.data "info"
                        |> Maybe.andThen
                            (\sd ->
                                case sd of
                                    InfoSectionType id ->
                                        Just id

                                    _ ->
                                        Nothing
                            )
                        |> Maybe.map
                            (\id ->
                                generateInfoContentData id.text modelData.apiUrl id.imageId id.lqip id.widths id.originalWidth id.originalHeight
                                    |> Page.InfoPageData (List.map generateInfoMenuData modelData.data |> (\sd -> MobileTogglingMenuData { menuSectionData = sd, menuOpen = False }))
                                    |> InfoPage
                                    |> Tuple.pair activeRoute
                            )
            in
            case maybeRouteAndPage of
                Just ( route, page ) ->
                    ( ReadyModel { modelData | page = page }, Navigation.pushUrl modelData.key <| routeToUrlPath route )

                Nothing ->
                    ( ReadyModel modelData, Navigation.pushUrl modelData.key <| routeToUrlPath Root )

        SectionRoute activeSectionId ->
            let
                maybeItems =
                    findSection modelData.data activeSectionId
                        |> Maybe.andThen (\sectionData -> getGalleryWithTagsSectionData sectionData)
                        |> Maybe.map (\galleryWithTagsSectionData -> galleryWithTagsSectionData.items)

                maybeFirstItem =
                    Maybe.andThen (\items -> getAt 0 items) maybeItems

                maybeRouteAndPage =
                    Maybe.map2
                        (\items { itemId } ->
                            generateMobileGalleryItemContentData modelData.apiUrl sliderHeight (SectionImageRoute activeSectionId) itemId 0 items
                                |> MobileGalleryContentData
                                |> Page.GalleryPageData
                                    (generateMobileGalleryMenuData activeSectionId modelData.data)
                                |> GalleryPage
                                |> Tuple.pair (SectionImageRoute activeSectionId itemId)
                        )
            in
            case maybeRouteAndPage maybeItems maybeFirstItem of
                Just ( route, page ) ->
                    ( ReadyModel { modelData | route = activeRoute, page = page }, Navigation.pushUrl modelData.key <| routeToUrlPath route )

                Nothing ->
                    ( ReadyModel modelData, Navigation.pushUrl modelData.key <| routeToUrlPath Root )

        SectionImageRoute activeSectionId itemId ->
            let
                maybeItems =
                    findSection modelData.data activeSectionId
                        |> Maybe.andThen (\sectionData -> getGalleryWithTagsSectionData sectionData)
                        |> Maybe.map (\galleryWithTagsSectionData -> galleryWithTagsSectionData.items)

                maybeItemIndex =
                    Maybe.andThen (\items -> findItemIndex itemId items) maybeItems

                maybeRouteAndPage =
                    Maybe.map2
                        (\items itemIndex ->
                            generateMobileGalleryItemContentData modelData.apiUrl sliderHeight (SectionImageRoute activeSectionId) itemId itemIndex items
                                |> MobileGalleryContentData
                                |> Page.GalleryPageData
                                    (generateMobileGalleryMenuData activeSectionId modelData.data)
                                |> GalleryPage
                                |> Tuple.pair (SectionImageRoute activeSectionId itemId)
                        )
            in
            case maybeRouteAndPage maybeItems maybeItemIndex of
                Just ( route, page ) ->
                    ( ReadyModel { modelData | route = activeRoute, page = page }, Navigation.pushUrl modelData.key <| routeToUrlPath route )

                Nothing ->
                    ( ReadyModel modelData, Navigation.pushUrl modelData.key <| routeToUrlPath Root )

        TagRoute activeSectionId activeTagId ->
            let
                maybeItems =
                    findSection modelData.data activeSectionId
                        |> Maybe.andThen (\sectionData -> findTag activeTagId sectionData)
                        |> Maybe.map (\tagData -> tagData.items)

                maybeFirstItem =
                    Maybe.andThen (\items -> getAt 0 items) maybeItems

                maybeRouteAndPage =
                    Maybe.map2
                        (\items { itemId } ->
                            generateMobileGalleryItemContentData modelData.apiUrl sliderHeight (TagImageRoute activeSectionId activeTagId) itemId 0 items
                                |> MobileGalleryContentData
                                |> Page.GalleryPageData
                                    (generateMobileGalleryMenuData activeSectionId modelData.data)
                                |> GalleryPage
                                |> Tuple.pair (TagImageRoute activeSectionId activeTagId itemId)
                        )
            in
            case maybeRouteAndPage maybeItems maybeFirstItem of
                Just ( route, page ) ->
                    ( ReadyModel { modelData | route = activeRoute, page = page }, Navigation.pushUrl modelData.key <| routeToUrlPath route )

                Nothing ->
                    ( ReadyModel modelData, Navigation.pushUrl modelData.key <| routeToUrlPath Root )

        TagImageRoute activeSectionId activeTagId itemId ->
            let
                maybeItems =
                    findSection modelData.data activeSectionId
                        |> Maybe.andThen (\sectionData -> findTag activeTagId sectionData)
                        |> Maybe.map (\tagData -> tagData.items)

                maybeItemIndex =
                    Maybe.andThen (\items -> findItemIndex itemId items) maybeItems

                maybeRouteAndPage =
                    Maybe.map2
                        (\items itemIndex ->
                            generateMobileGalleryItemContentData modelData.apiUrl sliderHeight (TagImageRoute activeSectionId activeTagId) itemId itemIndex items
                                |> MobileGalleryContentData
                                |> Page.GalleryPageData
                                    (generateMobileGalleryMenuData activeSectionId modelData.data)
                                |> GalleryPage
                                |> Tuple.pair (TagImageRoute activeSectionId activeTagId itemId)
                        )
            in
            case maybeRouteAndPage maybeItems maybeItemIndex of
                Just ( route, page ) ->
                    ( ReadyModel { modelData | route = activeRoute, page = page }, Navigation.pushUrl modelData.key <| routeToUrlPath route )

                Nothing ->
                    ( ReadyModel modelData, Navigation.pushUrl modelData.key <| routeToUrlPath Root )


updateRoute : Route -> ItemId -> Route
updateRoute oldRoute newItemId =
    case oldRoute of
        SectionRoute _ ->
            oldRoute

        SectionImageRoute sectionId _ ->
            SectionImageRoute sectionId newItemId

        TagRoute _ _ ->
            oldRoute

        TagImageRoute sectionId tagId _ ->
            TagImageRoute sectionId tagId newItemId

        _ ->
            oldRoute


calculateSliderTopOffset : ( Float, Float ) -> Float -> Float -> Float
calculateSliderTopOffset pointerStart topOffset yCoordinate =
    let
        ( initialPointerStart, oldTopOffset ) =
            pointerStart

        delta =
            yCoordinate - initialPointerStart
    in
    oldTopOffset + delta


calculateFinalSliderTopOffset : Float -> Float -> Int -> List Page.ItemContentData -> ( Float, Float ) -> Maybe Page.ItemContentData
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
        maybeNewItem

    else
        getAt activeItemIndex items


allFieldsPresent : Model -> Maybe ( Route, ReadyModelData )
allFieldsPresent newModel =
    case newModel of
        InitModel data ->
            Maybe.map5
                (\appData vp urlString key url ->
                    let
                        route =
                            parseToRoute url appData vp
                    in
                    ( route, { data = appData, apiUrl = urlString, viewport = vp, key = key, route = route, page = InitialPage } )
                )
                data.data
                data.viewport
                (Just data.dataUrl)
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
init { apiBaseUrl, dataPath, imagePath, apiPort, apiProtocol } url key =
    let
        baseUrl =
            apiProtocol
                ++ "://"
                ++ apiBaseUrl
                ++ ":"
                ++ apiPort
                ++ "/"
    in
    ( InitModel (InitModelData url (baseUrl ++ imagePath ++ "/") key Nothing Nothing)
    , Cmd.batch
        [ get
            { url = baseUrl ++ dataPath
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

        _ ->
            []


startPage : Page.StartPageData -> List (Html Msg)
startPage data =
    case data of
        Page.StartPageData menuData ->
            [ div [ class "start" ]
                [ buildMenu menuData
                , div [ class "image-start" ] []
                ]
            ]


infoPage : Page.InfoPageData -> List (Html Msg)
infoPage data =
    case data of
        Page.InfoPageData menuData infoContentData ->
            [ div [ class "layout-info" ]
                [ buildMenu menuData
                , div [ class "info-wrapper" ]
                    [ div [ class "info-image" ]
                        [ pictureFor infoContentData.urlString infoContentData.lqip infoContentData.widths infoContentData.originalWidth infoContentData.originalHeight True "" ]
                    , div [ class "info-text" ]
                        (infoText infoContentData.text)

                    ]
                ]
            ]


infoText txt =
    Result.withDefault [text ""] (Html.Parser.run txt |> Result.map Html.Parser.Util.toVirtualDom)

listPage : Page.ListPageData -> List (Html Msg)
listPage data =
    case data of
        Page.ListPageData menuData ->
            [ div [ class "layout" ]
                [ div [] []
                , buildMenu menuData
                ]
            ]


galleryPage : Page.GalleryPageData -> List (Html Msg)
galleryPage data =
    case data of
        Page.GalleryPageData menuData contentData ->
            [ div [ class "layout" ]
                (case contentData of
                    GalleryContentData desktopContentData ->
                        [ buildMenu menuData
                        , buildPictures True desktopContentData
                        , div [ class "main-image off" ]
                            [ img [ src "" ] []
                            ]
                        ]

                    GalleryImageContentData desktopContentImageData ->
                        [ buildMenu menuData
                        , buildPictures False desktopContentImageData
                        , buildActiveImage desktopContentImageData.activeItem
                        ]

                    MobileGalleryContentData mobileContentData ->
                        [ buildMenu menuData
                        , buildMobilePictures mobileContentData
                        ]
                )
            ]


buildPictures : Bool -> { x | items : List ItemContentData } -> Html Msg
buildPictures firstItemIsLcp contentData =
    -- `firstItemIsLcp` marks the first thumb as the LCP candidate when
    -- there's no active image rendered alongside the gallery (i.e.,
    -- SectionRoute / TagRoute on desktop). When an active image is
    -- rendered (SectionImageRoute / TagImageRoute), pass False — the
    -- active image is the LCP and gets fetchpriority=high via
    -- buildActiveImage.
    node
        "div"
        [ class "image-group" ]
        (List.indexedMap
            (\i itemData ->
                buildSectionPicture
                    itemData.urlString
                    itemData.onClickMessage
                    itemData.isActive
                    itemData.itemId
                    itemData.lqip
                    itemData.widths
                    itemData.originalWidth
                    itemData.originalHeight
                    (firstItemIsLcp && i == 0)
            )
            contentData.items
        )


buildActiveImage : ActiveItemContentData -> Html Msg
buildActiveImage activeImageData =
    let
        prevAttributes =
            case activeImageData.prevRoute of
                Nothing ->
                    [ class "prev inactive" ]

                Just msg ->
                    [ class "prev", onClickPreventDefault msg ]

        nextAttributes =
            case activeImageData.nextRoute of
                Nothing ->
                    [ class "next inactive" ]

                Just msg ->
                    [ class "next", onClickPreventDefault msg ]
    in
    div [ class "main-image on" ]
        [ div prevAttributes [ minus ]
        , pictureFor activeImageData.urlString activeImageData.lqip activeImageData.widths activeImageData.originalWidth activeImageData.originalHeight True ""
        , div nextAttributes [ plus ]
        ]


buildMobilePictures : MobileGalleryContentDataType -> Html Msg
buildMobilePictures contentData =
    div
        [ class "slider-window"
        , id "slider-window"
        , onClickPreventDefault CloseMenu
        , Pointer.onDown (relativePos >> DownMsg)
        , Pointer.onMove (relativePos >> MoveMsg)
        , Pointer.onUp (relativePos >> UpMsg)
        , Pointer.onCancel (relativePos >> UpMsg)
        , Html.Attributes.style "touch-action" "none"
        ]
        [ node
            "div"
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
                        itemData.lqip
                        itemData.widths
                        itemData.originalWidth
                        itemData.originalHeight
                        -- On mobile the slider snap-centres on the active
                        -- item — that's the visible-on-load image, hence
                        -- the LCP candidate.
                        itemData.isActive
                )
             <|
                contentData.items
            )
        ]


buildSectionPicture : String -> Msg -> Bool -> ItemId -> String -> List Int -> Maybe Int -> Maybe Int -> Bool -> ( String, Html Msg )
buildSectionPicture urlString onClickMessage isActive itemId lqip widths originalWidth originalHeight isLcp =
    ( itemId
    , a
        [ id itemId
        , class
            (if isActive then
                "image active-image"

             else
                "image"
            )
        , onClickPreventDefault onClickMessage
        ]
        [ pictureFor urlString lqip widths originalWidth originalHeight isLcp ""
        ]
    )


buildMenu : MenuData -> Html Msg
buildMenu menuData =
    case menuData of
        MenuData { menuSectionData } ->
            div [ class "menu-wrapper", id "menu-wrapper" ]
                [ div [ class "menu-background" ] [ div [ class "menu-background-inner" ] [] ]
                , div [ class "menu" ] (List.append (buildEntries menuSectionData) [ buildLogo ])
                ]

        MenuInfoData { menuSectionData } ->
            div [ class "menu-wrapper", id "menu-wrapper" ]
                [ div [ class "menu-background" ] [ div [ class "menu-background-inner" ] [] ]
                , div [ class "menu" ] (List.append (buildEntries menuSectionData) [])
                ]

        MobileMenuData { menuSectionData } ->
            div [ class "menu-wrapper", id "menu-wrapper mobile" ]
                [ div [ class "menu-background" ] [ div [ class "menu-background-inner" ] [] ]
                , div [ class "menu" ]
                    (List.concat
                        [ buildMobileEntries menuSectionData
                        , [ a [ class "instagram", onClickPreventDefaultAndStopPropagation GoToInstagram, href instagramLink ] [ instagram ] ]
                        ]
                    )
                ]

        MobileTogglingMenuData { menuSectionData, menuOpen } ->
            case menuOpen of
                True ->
                    div [ class "menu-wrapper", id "menu-wrapper mobile" ]
                        [ div [ class "menu-background" ] [ div [ class "menu-background-inner" ] [] ]
                        , div [ class "menu" ]
                            (List.concat
                                [ [ buildLogo
                                  , div [ class "menu-line" ] []
                                  ]
                                , List.map (\sectionData -> buildMobileEntry sectionData) menuSectionData
                                , [ a [ class "instagram", onClickPreventDefaultAndStopPropagation GoToInstagram, href instagramLink ] [ instagram ] ]
                                ]
                            )
                        ]

                False ->
                    div [ class "menu-wrapper", id "menu-wrapper mobile" ]
                        [ div [ class "menu-background closed" ] [ div [ class "menu-background-inner" ] [] ]
                        , div [ class "menu closed", onClickPreventDefault OpenMenu ]
                            (List.concat
                                [ [ buildLogo
                                  , div [ class "menu-line" ] []
                                  ]
                                , List.map (\sectionData -> buildMobileEntry sectionData) menuSectionData
                                , [ a [ class "instagram", onClickPreventDefaultAndStopPropagation GoToInstagram, href instagramLink ] [ instagram ] ]
                                ]
                            )
                        ]


buildEntries : List MenuSectionData -> List (Html Msg)
buildEntries menuData =
    List.append
        [ div [ class "logo-label" ] [ text "Varvara Polyakova" ]
        , div [ class "menu-line" ] []
        ]
        (List.map (\sectionData -> buildEntry sectionData) menuData)

buildMobileEntries : List MenuSectionData -> List (Html Msg)
buildMobileEntries menuData =
    List.append
        [ div [ class "logo-label" ] [ text "Varvara Polyakova" ]
        , div [ class "menu-line" ] []
        ]
        (List.map (\sectionData -> buildMobileEntry sectionData) menuData)

buildLogo : Html Msg
buildLogo =
    div [ class "logo", onClickPreventDefault <| GoToRoute Root ] [ logo ]


buildEntry : MenuSectionData -> Html Msg
buildEntry sectionData =
    case sectionData.sectionType of
        Info ->
            buildInfoEntry sectionData

        _ ->
            buildGalleryWithTagsEntry sectionData


buildMobileEntry : MenuSectionData -> Html Msg
buildMobileEntry sectionData =
    case sectionData.sectionType of
        Info ->
            buildMobileInfoEntry sectionData

        _ ->
            buildGalleryWithTagsEntry sectionData


buildInfoEntry : MenuSectionData -> Html Msg
buildInfoEntry sectionData =
    div [ class "menu-entry info" ]
        [ a [ class "menu-entry-label", onClickPreventDefault sectionData.onClickMessage, href sectionData.urlString ] [ text sectionData.sectionLabel ]
        , span [ class "info-pipe" ] [ text "|" ]
        , a [ class "menu-entry-label", href etsyLink, target "_blank", rel "noopener noreferrer" ] [ text "shop" ]
        , span [ class "info-pipe" ] [ text "|" ]
        , a [ class "menu-entry-label", href studioLink, target "_blank", rel "noopener noreferrer" ] [ text "studio" ]
        , span [ class "info-pipe" ] [ text "|" ]
        , a [ class "instagram", onClickPreventDefaultAndStopPropagation GoToInstagram, href instagramLink ] [ instagram ]
        ]

buildMobileInfoEntry : MenuSectionData -> Html Msg
buildMobileInfoEntry sectionData =
    div [ class "menu-entry info" ]
        [ a [ class "menu-entry-label", onClickPreventDefault sectionData.onClickMessage, href sectionData.urlString ] [ text sectionData.sectionLabel ]
        , span [ class "info-pipe" ] [ text "|" ]
        , a [ class "menu-entry-label", href etsyLink, target "_blank", rel "noopener noreferrer" ] [ text "shop" ]
        , span [ class "info-pipe" ] [ text "|" ]
        , a [ class "menu-entry-label", href studioLink, target "_blank", rel "noopener noreferrer" ] [ text "studio" ]
        ]

buildGalleryWithTagsEntry : MenuSectionData -> Html Msg
buildGalleryWithTagsEntry sectionData =
    div [ class "menu-entry" ]
        [ div
            [ class
                ("menu-entry-label"
                    ++ (if sectionData.sectionIsActive == True then
                            " active"

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
                [ text "|"
                ]
            )
        |> List.intersperse (text "\u{00A0}")


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


lqipAttrs lqip =
    if String.isEmpty lqip then
        []

    else
        [ style "background-image" ("url(" ++ lqip ++ ")")
        , style "background-size" "contain"
        , style "background-repeat" "no-repeat"
        , style "background-position" "center"
        ]


variantSizesAttr : String
variantSizesAttr =
    -- Mirrors src/styles.scss: gallery items are 100 % width on mobile,
    -- ~half-width on the 1024+ desktop layout.
    "(max-width: 1024px) 100vw, 50vw"


splitExtension : String -> ( String, String )
splitExtension fname =
    case String.indexes "." fname |> List.reverse of
        i :: _ ->
            ( String.left i fname, String.dropLeft (i + 1) fname )

        [] ->
            ( fname, "" )


variantSrcSet : String -> String -> List Int -> Maybe Int -> String
variantSrcSet urlString ext widths originalWidth =
    -- urlString: "https://data.palavara.com/img/7-1.jpg"
    -- ext:       "avif" | "webp" | "jpg"
    -- widths:    only the widths actually generated for this image
    --            (skipped if larger than the source). Stored per-item
    --            in data.json so the browser never gets a 500 from
    --            srcset selecting a non-existent variant.
    -- originalWidth: width of the source image. Appended as the
    --            largest entry pointing at urlString itself, so the
    --            browser picks the unscaled original instead of CSS-
    --            upscaling a smaller variant when the slot is bigger
    --            than any generated variant. Only used for the JPEG
    --            srcset (the urlString points at the JPEG/PNG/WebP
    --            original) — AVIF/WebP <source> srcsets stay variant-
    --            only since the original isn't an AVIF/WebP.
    let
        ( base, _ ) =
            splitExtension urlString

        entry w =
            base ++ "-" ++ String.fromInt w ++ "." ++ ext ++ " " ++ String.fromInt w ++ "w"

        variantEntries =
            widths |> List.map entry

        entries =
            case ( ext, originalWidth ) of
                ( "jpg", Just w ) ->
                    variantEntries ++ [ urlString ++ " " ++ String.fromInt w ++ "w" ]

                _ ->
                    variantEntries
    in
    String.join ", " entries


{-| Loading hints for an `<img>`. Set `isLcp` for the one image per
route that's the largest contentful paint candidate (the active-image
slot, or the first thumbnail on a section-only gallery). Lighthouse
flags both `loading="lazy"` and the missing `fetchpriority="high"` on
LCP candidates — this swaps to eager + high priority for those, lazy
+ async for everything else.
-}
imgLoadingAttrs : Bool -> List (Html.Attribute msg)
imgLoadingAttrs isLcp =
    if isLcp then
        [ attribute "fetchpriority" "high"
        , attribute "decoding" "async"
        ]

    else
        [ attribute "loading" "lazy"
        , attribute "decoding" "async"
        ]


pictureFor : String -> String -> List Int -> Maybe Int -> Maybe Int -> Bool -> String -> Html msg
pictureFor urlString lqip widths originalWidth originalHeight isLcp altText =
    -- <picture> with AVIF + WebP <source> negotiation, JPEG <img>
    -- fallback. The inner <img> keeps the lqipAttrs background-image
    -- trick so the LQIP shows immediately and the full image fades in
    -- as it decodes.
    --
    -- Falls back to a plain <img> when widths is empty (no variants
    -- generated). The optimize pipeline records which widths it
    -- actually produced; an item with widths == [] hasn't been
    -- processed yet, so we serve the original to avoid 500s on
    -- non-existent variant URLs.
    --
    -- Width and height attributes (when known from the manifest) let
    -- the browser reserve a slot of the right aspect ratio before the
    -- image bytes arrive — eliminates the CLS shift Lighthouse flags
    -- as "Image elements do not have explicit width and height".
    if List.isEmpty widths then
        img
            (src urlString
                :: alt altText
                :: dimensionAttrs originalWidth originalHeight
                ++ lqipAttrs lqip
                ++ imgLoadingAttrs isLcp
            )
            []

    else
        Html.node "picture"
            []
            [ source
                [ attribute "type" "image/avif"
                , attribute "srcset" (variantSrcSet urlString "avif" widths originalWidth)
                , attribute "sizes" variantSizesAttr
                ]
                []
            , source
                [ attribute "type" "image/webp"
                , attribute "srcset" (variantSrcSet urlString "webp" widths originalWidth)
                , attribute "sizes" variantSizesAttr
                ]
                []
            , img
                (src urlString
                    :: attribute "srcset" (variantSrcSet urlString "jpg" widths originalWidth)
                    :: attribute "sizes" variantSizesAttr
                    :: alt altText
                    :: dimensionAttrs originalWidth originalHeight
                    ++ lqipAttrs lqip
                    ++ imgLoadingAttrs isLcp
                )
                []
            ]


{-| Width / height attributes for the `<img>` tag. Both must be present
for the browser to use them as an aspect-ratio hint (a single attribute
is ignored). Returns an empty list when either is unknown — better no
attribute than a misleading one.
-}
dimensionAttrs : Maybe Int -> Maybe Int -> List (Html.Attribute msg)
dimensionAttrs originalWidth originalHeight =
    case ( originalWidth, originalHeight ) of
        ( Just w, Just h ) ->
            [ attribute "width" (String.fromInt w)
            , attribute "height" (String.fromInt h)
            ]

        _ ->
            []


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
