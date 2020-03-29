module Route exposing (..)

import AppData exposing (AppData, ItemId, SectionData(..), SectionId, TagId)
import Browser.Dom exposing (Viewport)
import Constants exposing (mobileBreakpoint)
import List.Extra exposing (getAt)
import Url
import Url.Builder exposing (absolute)
import Url.Parser as UrlParser exposing ((</>), Parser, oneOf, parse, s, string, top)


type Route
    = Root
    | InfoRoute
    | SectionRoute SectionId
    | SectionImageRoute SectionId ItemId
    | TagRoute SectionId TagId
    | TagImageRoute SectionId TagId ItemId


parseToRoute : Url.Url -> AppData -> Viewport -> Route
parseToRoute url data viewport =
    Maybe.withDefault Root (parse (routeParser data viewport) url)


routeParser : AppData -> Viewport -> Parser (Route -> a) a
routeParser data viewport =
    if viewport.viewport.width <= mobileBreakpoint then
        oneOf
            [ UrlParser.map Root top
            , UrlParser.map InfoRoute (s "info")

            --, UrlParser.map DesignRoute (s "design")
            , oneOf (tagImageParserGenerator data)
            , oneOf (mobileTagParserGenerator data)
            , oneOf (sectionImageParserGenerator data)
            , oneOf (mobileSectionParserGenerator data)
            ]

    else
        oneOf
            [ UrlParser.map Root top
            , UrlParser.map InfoRoute (s "info")

            --, UrlParser.map DesignRoute (s "design")
            , oneOf (tagImageParserGenerator data)
            , oneOf (tagParserGenerator data)
            , oneOf (sectionImageParserGenerator data)
            , oneOf (sectionParserGenerator data)
            ]



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


mobileTagParserGenerator : AppData -> List (Parser (Route -> b) b)
mobileTagParserGenerator input =
    let
        defaultRouteParser =
            UrlParser.map Root top
    in
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
                            (\{ tagId, items } ->
                                if List.length items > 0 then
                                    let
                                        firstItem =
                                            getAt 0 items
                                    in
                                    case firstItem of
                                        Just { itemId } ->
                                            UrlParser.map (TagImageRoute sectionId tagId itemId) (s sectionId </> s tagId)

                                        Nothing ->
                                            defaultRouteParser

                                else
                                    defaultRouteParser
                            )
                            tags

                    GallerySectionType _ ->
                        [ defaultRouteParser ]

                    InfoSectionType _ ->
                        [ defaultRouteParser ]
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


mobileSectionParserGenerator : AppData -> List (Parser (Route -> b) b)
mobileSectionParserGenerator input =
    List.map
        (\section ->
            case section of
                GalleryWithTagsSectionType { sectionId, items } ->
                    if List.length items > 0 then
                        let
                            firstItem =
                                getAt 0 items
                        in
                        case firstItem of
                            Just { itemId } ->
                                UrlParser.map (SectionImageRoute sectionId itemId) (s sectionId)

                            Nothing ->
                                UrlParser.map Root top

                    else
                        UrlParser.map Root top

                GallerySectionType { sectionId } ->
                    UrlParser.map (SectionRoute sectionId) (s sectionId)

                InfoSectionType { sectionId } ->
                    UrlParser.map (SectionRoute sectionId) (s sectionId)
        )
        input

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