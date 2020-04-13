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
    = MobileGalleryContentData MobileGalleryContentDataType
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


-- UTILS --


isSectionActive : SectionId -> SectionId -> Bool
isSectionActive sectionId activeSectionId =
    sectionId == activeSectionId


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
