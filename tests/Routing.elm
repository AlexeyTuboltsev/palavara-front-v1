module Routing exposing (..)

import AppData exposing (..)
import Browser.Dom exposing (Viewport)
import Expect exposing (Expectation)
import Main exposing (..)
import Route exposing (..)
import Test exposing (..)
import Url exposing (..)
import Url.Builder exposing (absolute)


testViewport : Viewport
testViewport =
    { scene = { width = 1920, height = 1080 }
    , viewport = { x = 0, y = 0, width = 1920, height = 1080 }
    }


testAppData =
    [ InfoSectionType <| InfoSectionData "info" "info" "" "" ""
    , GallerySectionType <| GallerySectionData "design" "design"
    , GalleryWithTagsSectionType <|
        GalleryWithTagsSectionData
            "graphics"
            "graphics"
            []
            [ TagData "graphics_tag1" "graphics_tag1" []
            , TagData "graphics_tag2" "graphics_tag2" []
            , TagData "graphics_tag3" "graphics_tag3" []
            ]
    , GalleryWithTagsSectionType <|
        GalleryWithTagsSectionData
            "illustrations"
            "illustrations"
            []
            [ TagData "illustrations_tag1" "illustrations_tag1" []
            , TagData "illustrations_tag2" "illustrations_tag2" []
            , TagData "illustrations_tag3" "illustrations_tag3" []
            ]
    , GalleryWithTagsSectionType <| GalleryWithTagsSectionData "ceramics" "ceramics" [] []
    ]


suite : Test
suite =
    describe "routing"
        [ test "root" <|
            \_ ->
                let
                    url =
                        Url.Url Http "localhost" (Just 3061) (absolute [] []) Nothing Nothing
                in
                Expect.equal Root (parseToRoute url testAppData testViewport)
        , test "info" <|
            \_ ->
                let
                    url =
                        Url.Url Http "localhost" (Just 3061) (absolute [ "info" ] []) Nothing Nothing
                in
                Expect.equal InfoRoute (parseToRoute url testAppData testViewport)
        , test "info - any" <|
            \_ ->
                let
                    url =
                        Url.Url Http "localhost" (Just 3061) (absolute [ "info", "bla" ] []) Nothing Nothing
                in
                Expect.equal Root (parseToRoute url testAppData testViewport)
        , test "section/ok - tag/ok - img" <|
            \_ ->
                let
                    url =
                        Url.Url Http "localhost" (Just 3061) (absolute [ "graphics", "graphics_tag1", "img.jpg" ] []) Nothing Nothing
                in
                Expect.equal (TagImageRoute "graphics" "graphics_tag1" "img.jpg") (parseToRoute url testAppData testViewport)
        , test "section/ok - tag/ok - img - any" <|
            \_ ->
                let
                    url =
                        Url.Url Http "localhost" (Just 3061) (absolute [ "graphics", "graphics_tag1", "img.jpg", "blabla" ] []) Nothing Nothing
                in
                Expect.equal Root (parseToRoute url testAppData testViewport)
        , test "section/err - tag/ok - img" <|
            \_ ->
                let
                    url =
                        Url.Url Http "localhost" (Just 3061) (absolute [ "bla", "graphics_tag1", "img.jpg" ] []) Nothing Nothing
                in
                Expect.equal Root (parseToRoute url testAppData testViewport)
        , test "section/ok - tag/err - img" <|
            \_ ->
                let
                    url =
                        Url.Url Http "localhost" (Just 3061) (absolute [ "graphics", "bla", "img.jpg" ] []) Nothing Nothing
                in
                Expect.equal Root (parseToRoute url testAppData testViewport)
        , test "section/ok - tag/ok" <|
            \_ ->
                let
                    url =
                        Url.Url Http "localhost" (Just 3061) (absolute [ "graphics", "graphics_tag1" ] []) Nothing Nothing
                in
                Expect.equal (TagRoute "graphics" "graphics_tag1") (parseToRoute url testAppData testViewport)
        , test "section/err - tag/ok" <|
            \_ ->
                let
                    url =
                        Url.Url Http "localhost" (Just 3061) (absolute [ "bla", "graphics_tag1" ] []) Nothing Nothing
                in
                Expect.equal Root (parseToRoute url testAppData testViewport)
        , test "section/ok - img" <|
            \_ ->
                let
                    url =
                        Url.Url Http "localhost" (Just 3061) (absolute [ "graphics", "bla" ] []) Nothing Nothing
                in
                Expect.equal (SectionImageRoute "graphics" "bla") (parseToRoute url testAppData testViewport)
        , test "section" <|
            \_ ->
                let
                    url =
                        Url.Url Http "localhost" (Just 3061) (absolute [ "graphics" ] []) Nothing Nothing
                in
                Expect.equal (SectionRoute "graphics") (parseToRoute url testAppData testViewport)
        ]
