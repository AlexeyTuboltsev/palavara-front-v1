module Message exposing (..)

import AppData exposing (AppData)
import Browser
import Browser.Dom exposing (Viewport)
import Http
import Route exposing (Route)
import Url exposing (Url)

type Msg
    = NoOp
    | GetViewport
    | SetViewport Viewport
    | SetData (Result Http.Error AppData)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GoToRoute Route
    | DownMsg ( Float, Float )
    | MoveMsg ( Float, Float )
    | UpMsg ( Float, Float )
    | CloseMenu
    | OpenMenu
    | GoToShop
    | GoToInstagram