module Page exposing (..)

import AppData exposing (AppData, GalleryWithTagsSectionData, ItemData, ItemId, SectionData(..), SectionId, TagData, TagId)
import List.Extra exposing (find, findIndex, getAt, indexedFoldl)
import Message exposing (Msg(..))
import Route exposing (Route(..), routeToUrlPath)

imgPath = "/img/"
type Page
    = StartPage StartPageData
    | InfoPage InfoPageData
    | ListPage ListPageData
    | GalleryPage GalleryPageData
    | InitialPage


type StartPageData
    = StartPageData MenuData


type ListPageData
    = ListPageData MenuData


type GalleryPageData
    = GalleryPageData MenuData GalleryContentData


type InfoPageData
    = InfoPageData MenuData InfoContentData


type MenuData
    = MenuData { menuSectionData : List MenuSectionData }
    | MenuInfoData { menuSectionData : List MenuSectionData }
    | MobileMenuData { menuSectionData : List MenuSectionData }
    | MobileTogglingMenuData { menuSectionData : List MenuSectionData, menuOpen : Bool }


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


type alias MenuTagData =
    { tagId : TagId
    , tagLabel : String
    , tagIsActive : Bool
    , onClickMessage : Msg
    , urlString : String
    }


type GalleryContentData
    = MobileContentData MobileGalleryContentDataType
    | GalleryContentData GalleryContentDataType
    | GalleryImageContentData GalleryImageContentDataType


type alias MobileGalleryContentDataType =
    { items : List ItemContentData
    , activeItemIndex : Int
    , sliderHeight : Float
    , topOffset : Float
    , pointerStart : Maybe ( Float, Float )
    , menuOpen : Bool
    }


type alias GalleryContentDataType =
    { items : List ItemContentData }


type alias GalleryImageContentDataType =
    { items : List ItemContentData, activeItem : ActiveItemContentData }


type alias ItemContentData =
    { itemId : ItemId
    , urlString : String
    , onClickMessage : Msg
    , width : Int
    , height : Int
    , isActive : Bool
    }


type alias ActiveItemContentData =
    { itemId : ItemId
    , urlString : String
    , prevRoute : Maybe Msg
    , nextRoute : Maybe Msg
    }


type alias InfoContentData =
    { imageId : String
    , text : String
    }



--routeToPage : Route -> AppData -> Page
--routeToPage route appData =
--    routeToPageInternal
--        generateRootMenuData
--        makeSectionMenuData
--        generateTagMenuData
--        (generateSectionImageContentData route)
--        (generateTagContentData route)
--        route
--        appData
--routeToMobilePage : Route -> Float -> AppData -> Page
--routeToMobilePage route sliderHeight appData =
--    routeToPageInternal
--        generateMobileRootMenuData
--        generateMobileSectionMenuData
--        generateMobileTagMenuData
--        (generateMobileSectionContentData route sliderHeight)
--        (generateMobileTagContentData route sliderHeight)
--        route
--        appData
--routeToPageInternal rootMenuFn sectionMenuFn tagMenuFn sectionFn tagFn route appData =
--    case route of
--        Root ->
--            List.map rootMenuFn appData
--                |> StartPageData
--                |> StartPage
--
--
--        InfoRoute ->
--            List.map generateInfoMenuData appData
--                |> InfoPageData
--                |> InfoPage
--
--
--        SectionRoute activeSectionId ->
--            let (finalRoute,contentData) = (sectionFn activeSectionId appData zeroItemIndex (SectionImageRoute activeSectionId))
--            in
--            GalleryPageData
--                (sectionMenuFn activeSectionId appData)
--                contentData
--                |> GalleryPage
--
--
--        TagRoute activeSectionId activeTagId ->
--            GalleryPageData
--                (tagMenuFn activeSectionId activeTagId appData)
--                (tagFn activeSectionId activeTagId appData zeroItemIndex (TagImageRoute activeSectionId activeTagId))
--                |> GalleryPage
--
--
--        SectionImageRoute activeSectionId imageId ->
--            let (finalRoute,contentData) = (sectionFn activeSectionId appData (findItemIndex imageId) (SectionImageRoute activeSectionId))
--            in
--            GalleryPageData
--                (sectionMenuFn activeSectionId appData)
--                contentData
--                |> GalleryPage
--
--
--        TagImageRoute activeSectionId activeTagId imageId ->
--            GalleryPageData
--                (tagMenuFn activeSectionId activeTagId appData)
--               (tagFn activeSectionId activeTagId appData (findItemIndex imageId) (TagImageRoute activeSectionId activeTagId))
--                |> GalleryPage
-- ROOT MENU  --


generateRootMenuData : SectionData -> MenuSectionData
generateRootMenuData section =
    case section of
        GalleryWithTagsSectionType { sectionId, label, tags } ->
            generateMenuEntryData GalleryWithTags False sectionId label (SectionRoute sectionId) (generateMenuTagData tags sectionId)

        GallerySectionType { sectionId, label } ->
            MenuSectionData Gallery sectionId label False [] (GoToRoute <| SectionRoute sectionId) (routeToUrlPath <| SectionRoute sectionId)

        InfoSectionType { sectionId, label } ->
            generateMenuEntryData Info False sectionId label InfoRoute []


generateMobileMenuData : SectionData -> MenuSectionData
generateMobileMenuData section =
    case section of
        GalleryWithTagsSectionType { sectionId, label, tags, items } ->
            let
                firstItem =
                    getAt 0 items
            in
            case firstItem of
                Nothing ->
                    generateMenuEntryData GalleryWithTags False sectionId label Root (generateMobileMenuTagData tags sectionId)

                Just { itemId } ->
                    generateMenuEntryData GalleryWithTags False sectionId label (SectionImageRoute sectionId itemId) (generateMobileMenuTagData tags sectionId)

        GallerySectionType { sectionId, label } ->
            MenuSectionData Gallery sectionId label False [] (GoToRoute <| SectionRoute sectionId) (routeToUrlPath <| SectionRoute sectionId)

        InfoSectionType { sectionId, label } ->
            generateMenuEntryData Info False sectionId label InfoRoute []


generateMobileGalleryMenuData : SectionId -> AppData -> MenuData
generateMobileGalleryMenuData activeSectionId sections =
    let
        bla =
            List.map
                (\section ->
                    case section of
                        GalleryWithTagsSectionType { sectionId, label, tags, items } ->
                            let
                                firstItem =
                                    getAt 0 items
                            in
                            case firstItem of
                                Nothing ->
                                    generateMenuEntryData GalleryWithTags (activeSectionId == sectionId) sectionId label Root (generateMobileMenuTagData tags sectionId)

                                Just { itemId } ->
                                    generateMenuEntryData GalleryWithTags (activeSectionId == sectionId) sectionId label (SectionImageRoute sectionId itemId) (generateMobileMenuTagData tags sectionId)

                        GallerySectionType { sectionId, label } ->
                            MenuSectionData Gallery sectionId label (activeSectionId == sectionId) [] (GoToRoute <| SectionRoute sectionId) (routeToUrlPath <| SectionRoute sectionId)

                        InfoSectionType { sectionId, label } ->
                            generateMenuEntryData Info (activeSectionId == sectionId) sectionId label InfoRoute []
                )
                sections
    in
    MobileTogglingMenuData { menuSectionData = bla, menuOpen = False }



-- INFO MENU --


generateInfoMenuData : SectionData -> MenuSectionData
generateInfoMenuData section =
    case section of
        GalleryWithTagsSectionType { sectionId, label, tags } ->
            generateMenuEntryData GalleryWithTags False sectionId label (SectionRoute sectionId) (generateMenuTagData tags sectionId)

        GallerySectionType { sectionId, label } ->
            MenuSectionData Gallery sectionId label False [] NoOp ""

        InfoSectionType { sectionId, label } ->
            generateMenuEntryData Info True sectionId label InfoRoute []



-- SECTION AND TAG MENU --


generateSectionMenuData : SectionId -> List SectionData -> MenuData
generateSectionMenuData activeSectionId sections =
    generateSectionMenuDataInternal activeSectionId sections generateMenuTagData



--generateMobileSectionMenuData : SectionId -> List SectionData -> MenuData
--generateMobileSectionMenuData activeSectionId sections =
--    generateSectionMenuDataInternal activeSectionId (filterMenuInfoSection sections) generateMenuTagData
--
--
--generateTagMenuData : SectionId -> TagId -> List SectionData -> MenuData
--generateTagMenuData activeSectionId activeTagId sections =
--    generateMenuTagDataWithActiveTag activeTagId
--        |> generateSectionMenuDataInternal activeSectionId sections
--
--
--generateMobileTagMenuData : SectionId -> TagId -> List SectionData -> MenuData
--generateMobileTagMenuData activeSectionId activeTagId sections =
--    generateMobileMenuTagDataWithActiveTag activeTagId
--        |> generateSectionMenuDataInternal activeSectionId (filterMenuInfoSection sections)


generateSectionMenuDataInternal : SectionId -> List SectionData -> (List TagData -> SectionId -> List MenuTagData) -> MenuData
generateSectionMenuDataInternal activeSectionId sections tagMenuGeneratingFn =
    List.map
        (\section ->
            case section of
                InfoSectionType { sectionId, label } ->
                    generateMenuEntryData Info False sectionId label InfoRoute []

                GalleryWithTagsSectionType { sectionId, label, tags } ->
                    let
                        sectionIsActive =
                            isSectionActive activeSectionId sectionId
                    in
                    tagMenuGeneratingFn tags sectionId
                        |> generateMenuEntryData GalleryWithTags sectionIsActive sectionId label (SectionRoute sectionId)

                GallerySectionType { sectionId, label } ->
                    MenuSectionData Gallery sectionId label False [] NoOp ""
        )
        sections
        |> (\s -> MenuData { menuSectionData = s })



--generateMenuTagDataWithActiveTag : TagId -> List TagData -> SectionId -> List MenuTagData
--generateMenuTagDataWithActiveTag activeTagId tags sectionId =
--    List.map
--        (\{ tagId, label } ->
--            let
--                tagIsActive =
--                    isTagActive tagId activeTagId
--
--                ( onClickMessage, urlString ) =
--                    if tagIsActive then
--                        ( NoOp, routeToUrlPath Root )
--
--                    else
--                        let
--                            route =
--                                TagRoute sectionId tagId
--                        in
--                        ( GoToRoute route, routeToUrlPath route )
--            in
--            MenuTagData tagId label tagIsActive onClickMessage urlString
--        )
--        tags
--generateMobileMenuTagDataWithActiveTag : TagId -> List TagData -> SectionId -> List MenuTagData
--generateMobileMenuTagDataWithActiveTag activeTagId tags sectionId =
--    List.map
--        (\{ tagId, label, items } ->
--            let
--                tagIsActive =
--                    isTagActive tagId activeTagId
--
--                firstItem =
--                    getAt 0 items
--
--                ( onClickMessage, urlString ) =
--                    if tagIsActive then
--                        ( NoOp, routeToUrlPath Root )
--
--                    else
--                        case firstItem of
--                            Nothing ->
--                                ( GoToRoute <| Root, routeToUrlPath Root )
--
--                            Just { itemId } ->
--                                let
--                                    route =
--                                        TagImageRoute sectionId tagId itemId
--                                in
--                                ( GoToRoute <| route, routeToUrlPath route )
--            in
--            MenuTagData tagId label tagIsActive onClickMessage urlString
--        )
--        tags
-- MENU ENTRY --


generateMenuEntryData : MenuSectionType -> Bool -> SectionId -> String -> Route -> List MenuTagData -> MenuSectionData
generateMenuEntryData menuSectionType sectionIsActive sectionId label nextRoute tagMenuData =
    let
        menuEntryAttributes =
            generateMenuEntryAttributes sectionIsActive nextRoute
    in
    MenuSectionData menuSectionType sectionId label menuEntryAttributes.sectionIsActive tagMenuData menuEntryAttributes.onClickMessage menuEntryAttributes.urlString


generateMenuEntryAttributes : Bool -> Route -> { sectionIsActive : Bool, onClickMessage : Msg, urlString : String }
generateMenuEntryAttributes sectionIsActive nextRoute =
    case sectionIsActive of
        False ->
            { sectionIsActive = False, onClickMessage = GoToRoute <| nextRoute, urlString = routeToUrlPath nextRoute }

        True ->
            { sectionIsActive = False, onClickMessage = NoOp, urlString = "" }



-------------
-- TODO this one is very similar to other tag menu generation fns


generateMenuTagData : List TagData -> SectionId -> List MenuTagData
generateMenuTagData tags sectionId =
    List.map
        (\{ tagId, label, items } ->
            let
                firstItem =
                    getAt 0 items
            in
            case firstItem of
                Nothing ->
                    MenuTagData tagId label False (GoToRoute <| Root) (routeToUrlPath Root)

                Just { itemId } ->
                    let
                        route =
                            TagRoute sectionId tagId
                    in
                    MenuTagData tagId label False (GoToRoute <| route) (routeToUrlPath route)
        )
        tags


generateMobileMenuTagData : List TagData -> SectionId -> List MenuTagData
generateMobileMenuTagData tags sectionId =
    List.map
        (\{ tagId, label, items } ->
            let
                firstItem =
                    getAt 0 items
            in
            case firstItem of
                Nothing ->
                    MenuTagData tagId label False (GoToRoute <| Root) (routeToUrlPath Root)

                Just { itemId } ->
                    let
                        route =
                            TagImageRoute sectionId tagId itemId
                    in
                    MenuTagData tagId label False (GoToRoute <| route) (routeToUrlPath route)
        )
        tags



----------------
-- SECTION CONTENT --


generateGalleryContentData : (ItemId -> Route) -> List ItemData -> GalleryContentDataType
generateGalleryContentData nextRoute items =
    let
        onClickMessage itemId =
            GoToRoute <| nextRoute itemId
    in
    List.map
        (\{ itemId, width, height } ->
            ItemContentData
                itemId
                (imgPath ++ itemId)
                (onClickMessage itemId)
                width
                height
                False
        )
        items
        |> GalleryContentDataType


generateGalleryItemContentData : (ItemId -> Route) -> ItemId -> List ItemData -> GalleryImageContentDataType
generateGalleryItemContentData nextRoute activeItemId items =
    let
        onClickMessage itemId =
            GoToRoute <| nextRoute itemId

        itemDataList =
            List.map
                (\{ itemId, width, height } ->
                    ItemContentData
                        itemId
                        (imgPath ++ itemId)
                        (onClickMessage itemId)
                        width
                        height
                        (itemId == activeItemId)
                )
                items

        prevOnClick =
            findIndex (\{ itemId } -> itemId == activeItemId) items
                |> Maybe.andThen (\i -> getAt (i - 1) items)
                |> Maybe.andThen (\{ itemId } -> Just <| onClickMessage itemId)

        nextOnClick =
            findIndex (\{ itemId } -> itemId == activeItemId) items
                |> Maybe.andThen (\i -> getAt (i + 1) items)
                |> Maybe.andThen (\{ itemId } -> Just <| onClickMessage itemId)

        activeItemData =
            ActiveItemContentData activeItemId ("/img/" ++ activeItemId) prevOnClick nextOnClick
    in
    GalleryImageContentDataType itemDataList activeItemData


generateMobileGalleryItemContentData : Float -> (ItemId -> Route) -> ItemId -> Int -> List ItemData -> MobileGalleryContentDataType
generateMobileGalleryItemContentData sliderHeight nextRoute activeItemId activeItemIndex items =
    let
        onClickMessage itemId =
            case itemId == activeItemId of
                True ->
                    NoOp

                False ->
                    GoToRoute <| nextRoute itemId

        topOffset =
            calculateTopOffset sliderHeight activeItemIndex

        itemDataList =
            List.map
                (\{ itemId, width, height } ->
                    ItemContentData
                        itemId
                        (imgPath ++ itemId)
                        (onClickMessage itemId)
                        width
                        height
                        (itemId == activeItemId)
                )
                items
    in
    MobileGalleryContentDataType itemDataList activeItemIndex sliderHeight topOffset Nothing False


generateInfoContentData : String -> String -> InfoContentData
generateInfoContentData text imageId =
    InfoContentData imageId text



--generateSectionContentData : Route -> SectionId -> AppData -> (List ItemData -> Maybe Int) -> (ItemId -> Route) -> GalleryContentData
--generateSectionContentData route activeSectionId appData itemIndexFn nextRoute =
--    getSectionItems activeSectionId getGalleryWithTagsSectionData (generateItemContentData nextRoute itemIndexFn) appData
--        |> Maybe.withDefault { items = [] }
--        |> GalleryContentData
--        |> (,) route
--generateSectionImageContentData : Route -> SectionId -> AppData -> (List ItemData -> Maybe Int) -> (ItemId -> Route) -> GalleryContentData
--generateSectionImageContentData route activeSectionId appData itemIndexFn nextRoute =
--    let maybeData =
--            findSection appData activeSectionId
--               |> Maybe.andThen (\maybeSection -> getGalleryWithTagsSectionData maybeSection)
--
--
--    --let (finalRoute, items) = getSectionItems activeSectionId getGalleryWithTagsSectionData (generateItemContentData nextRoute itemIndexFn) appData
--    in
--        GalleryImageContentData
--        |> Tuple.pair route
--generateMobileSectionContentData : Route -> Float -> SectionId -> AppData -> (List ItemData -> Maybe Int) -> (ItemId -> Route) -> ( Route, GalleryContentData )
--generateMobileSectionContentData route sliderHeight activeSectionId appData itemIndexFn nextRoute =
--    getSectionItems activeSectionId getGalleryWithTagsSectionData (generateMobileItemContentData sliderHeight nextRoute itemIndexFn) appData
--        |> Maybe.withDefault { items = [], activeItemIndex = 0, sliderHeight = 0, topOffset = 0, pointerStart = Nothing }
--        |> MobileContentData
--        |> Tuple.pair route
-- TAG CONTENT --
--generateTagContentData : Route -> SectionId -> TagId -> AppData -> (List ItemData -> Maybe Int) -> (ItemId -> Route) -> (Route,GalleryContentData)
--generateTagContentData route activeSectionId activeTagId appData itemIndexFn nextRoute =
--    getSectionItems activeSectionId (findTag activeTagId) (generateGalleryItemContentData nextRoute itemIndexFn) appData
--        |> Maybe.withDefault { items = [], activeItemIndex = 0 }
--        |> GalleryImageContentData
--        |> Tuple.pair route
--generateMobileTagContentData : Route -> Float -> SectionId -> TagId -> AppData -> (List ItemData -> Maybe Int) -> (ItemId -> Route) -> ( Route, GalleryContentData )
--generateMobileTagContentData route sliderHeight activeSectionId activeTagId appData itemIndexFn nextRoute =
--    getSectionItems activeSectionId (findTag activeTagId) (generateMobileItemContentData sliderHeight nextRoute itemIndexFn) appData
--        |> Maybe.withDefault { items = [], activeItemIndex = 0, sliderHeight = 0, topOffset = 0, pointerStart = Nothing }
--        |> MobileContentData
--        |> Tuple.pair route
--generateMobileItemContentData : Float -> (ItemId -> Route) -> (List ItemData -> Maybe Int) -> List ItemData -> Maybe MobileGalleryContentDataType
--generateMobileItemContentData sliderHeight nextRoute itemIndexFn items =
--    (\itms -> itemIndexFn itms) items
--        |> Maybe.map
--            (\activeItemIndex ->
--                let
--                    onClickMessage =
--                        \itemId -> GoToRoute <| nextRoute itemId
--
--                    topOffset =
--                        calculateTopOffset sliderHeight activeItemIndex
--                in
--                indexedFoldl
--                    (generateItemContentDataInternal activeItemIndex onClickMessage)
--                    { items = [], activeItemIndex = 0, sliderHeight = sliderHeight, topOffset = topOffset, pointerStart = Nothing }
--                    items
--            )
--generateItemContentDataInternal : Int -> (ItemId -> Msg) -> (Int -> ItemData -> { x | items : List ItemContentData, activeItemIndex : Int } -> { x | items : List ItemContentData, activeItemIndex : Int })
--generateItemContentDataInternal activeItemIndex onClickMessage =
--    \i { itemId, width, height } acc ->
--        { acc
--            | items =
--                List.append
--                    acc.items
--                    [ ItemContentData
--                        itemId
--                        (imgPath ++ itemId)
--                        (onClickMessage itemId)
--                        width
--                        height
--                        (activeItemIndex == i)
--                    ]
--            , activeItemIndex = activeItemIndex
--        }
-- UTILS --


isSectionActive : SectionId -> SectionId -> Bool
isSectionActive sectionId activeSectionId =
    sectionId == activeSectionId



--isTagActive : TagId -> TagId -> Bool
--isTagActive tagId activeTagId =
--    tagId == activeTagId
--
--
--getSectionItems : SectionId -> (SectionData -> Maybe { a | items : List ItemData }) -> (List ItemData -> Maybe y) -> AppData -> Maybe y
--getSectionItems activeSectionId sectionFn itemsFn appData =
--    findSection appData activeSectionId
--        |> Maybe.andThen
--            (\section -> sectionFn section)
--        |> Maybe.andThen
--            (\{ items } -> itemsFn items)


findSection : List SectionData -> SectionId -> Maybe SectionData
findSection sectionList sectionId =
    find
        (\section ->
            case section of
                GalleryWithTagsSectionType data ->
                    data.sectionId == sectionId

                InfoSectionType data ->
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


getGalleryWithTagsSectionData : SectionData -> Maybe GalleryWithTagsSectionData
getGalleryWithTagsSectionData section =
    case section of
        GalleryWithTagsSectionType data ->
            Just data

        _ ->
            Nothing


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


calculateTopOffset : Float -> Int -> Float
calculateTopOffset sliderHeight activeItemIndex =
    let
        imageHeight =
            sliderHeight * 0.7

        initialOffset =
            (sliderHeight - imageHeight) / 2

        gridGap =
            20
    in
    initialOffset - ((imageHeight + gridGap) * toFloat activeItemIndex)


findItemIndex : ItemId -> List ItemData -> Maybe Int
findItemIndex activeItemId items =
    findIndex (\x -> x.itemId == activeItemId) items


zeroItemIndex : x -> Maybe Int
zeroItemIndex _ =
    Just 0
