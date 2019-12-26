module AppData exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD


type alias SectionId =
    String


type alias TagId =
    String


type alias ItemId =
    String


type alias TagData =
    { label : String
    , tagId : TagId
    , items : List ItemData
    }


type alias ItemData =
    { itemId : ItemId
    , width : Int
    , height : Int
    }


type SectionData
    = GalleryWithTagsSectionType GalleryWithTagsSectionData
    | GallerySectionType GallerySectionData
    | InfoSectionType InfoSectionData


type alias GalleryWithTagsSectionData =
    { label : String
    , sectionId : SectionId
    , items : List ItemData --Dict ItemId ItemData

    --, itemOrder : List ItemId
    , tags : List TagData
    }


type alias InfoSectionData =
    { label : String
    , sectionId : SectionId
    }


type alias GallerySectionData =
    { label : String
    , sectionId : SectionId
    }


type alias AppData =
    List SectionData



-- JSON --


appDataDecoder : JD.Decoder (List SectionData)
appDataDecoder =
    JD.field "sections" (JD.list sectionDataDecoder)


itemDataDecoder : JD.Decoder ItemData
itemDataDecoder =
    JD.map3 ItemData
        itemIdDecoder
        (JD.field "width" JD.int)
        (JD.field "height" JD.int)


itemIdDecoder =
    JD.field "itemId" JD.string


tagDataDecoder : JD.Decoder TagData
tagDataDecoder =
    JD.map3 TagData
        (JD.field "label" JD.string)
        (JD.field "tagId" JD.string)
        (JD.field "items" (JD.list itemDataDecoder))


itemsDecoder =
    JD.list itemDataDecoder



--|> JD.map (\decodedList -> List.map (\item -> ( item.itemId, item )) decodedList)
--|> JD.map Dict.fromList


itemOrderDecoder =
    JD.list itemDataDecoder
        |> JD.map (\decodedList -> List.map (\item -> item.itemId) decodedList)


sectionDataDecoder : JD.Decoder SectionData
sectionDataDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\sectionType ->
                case sectionType of
                    "galleryWithTags" ->
                        JD.map4 GalleryWithTagsSectionData
                            (JD.field "label" JD.string)
                            (JD.field "sectionId" JD.string)
                            (JD.field "items" itemsDecoder)
                            --(JD.field "items" itemOrderDecoder)
                            (JD.field "tags" (JD.list tagDataDecoder))
                            |> JD.map GalleryWithTagsSectionType

                    "gallery" ->
                        JD.map2 GallerySectionData
                            (JD.field "label" JD.string)
                            (JD.field "sectionId" JD.string)
                            |> JD.map GallerySectionType

                    "info" ->
                        JD.map2 InfoSectionData
                            (JD.field "label" JD.string)
                            (JD.field "sectionId" JD.string)
                            |> JD.map InfoSectionType

                    _ ->
                        JD.fail "no luck today"
            )
