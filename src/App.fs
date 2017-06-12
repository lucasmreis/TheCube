module TheCube

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

importSideEffects "./styles.css"

type Sides = Front | Left | Main | Right
type TopOrBottom = Top | Bottom

type ShowingSide =
    | Side of Sides
    | TopOrBottom of TopOrBottom

type TurnTo = ToRight | ToLeft | ToTop | ToBottom

let turn showing turnTo =
    match showing, turnTo with
    | TopOrBottom _, ToRight -> Side Right
    | TopOrBottom _, ToLeft -> Side Left

    | TopOrBottom Top, ToTop -> Side Main
    | TopOrBottom Top, ToBottom -> Side Front
    | TopOrBottom Bottom, ToTop -> Side Front
    | TopOrBottom Bottom, ToBottom -> Side Main

    | Side _, ToTop -> TopOrBottom Top
    | Side _, ToBottom -> TopOrBottom Bottom

    | Side Front, ToRight -> Side Right
    | Side Front, ToLeft -> Side Left

    | Side Right, ToRight -> Side Main
    | Side Right, ToLeft -> Side Front

    | Side Main, ToRight -> Side Left
    | Side Main, ToLeft -> Side Right

    | Side Left, ToRight -> Side Front
    | Side Left, ToLeft -> Side Main

let show position =
    match position with
    | Side Front -> "rotateX(0deg) rotateY(0deg)"
    | Side Left -> "rotateX(0deg) rotateY(90deg)"
    | Side Right -> "rotateX(0deg) rotateY(-90deg)"
    | Side Main -> "rotateX(0deg) rotateY(180deg)"
    | TopOrBottom Top -> "rotateX(-90deg) rotateY(0deg)"
    | TopOrBottom Bottom -> "rotateX(90deg) rotateY(0deg)"

let update (cube: Browser.HTMLElement) showing turnTo =
    let next = turn showing turnTo
    cube.style.transform <- show next
    Browser.console.log (sprintf "Showing: %A" next)
    next

let init () =
    let mutable showing = Side Front

    let cube = Browser.document.getElementById "cube"

    let up = Browser.document.getElementById "btn-up"
    let down = Browser.document.getElementById "btn-down"
    let left = Browser.document.getElementById "btn-left"
    let right = Browser.document.getElementById "btn-right"

    up.addEventListener_click(fun _ -> showing <- update cube showing ToTop; null)
    down.addEventListener_click(fun _ -> showing <- update cube showing ToBottom; null)
    left.addEventListener_click(fun _ -> showing <- update cube showing ToLeft; null)
    right.addEventListener_click(fun _ -> showing <- update cube showing ToRight; null)

let loaded count (videos: Browser.NodeListOf<Browser.HTMLVideoElement>) =
    Browser.console.log (sprintf "Loaded: %A" (count + 1))
    if (count >= 3) then
        [0..3]
            |> List.map (fun id -> (videos.Item id).play())
            |> ignore
        Browser.console.log "Ready!"
        init ()
        4
    else
        count + 1

let load () =
    let mutable count = 0
    let videos = Browser.document.getElementsByTagName_video ()
    [0..3]
        |> List.map (fun id -> (videos.Item id).addEventListener_canplaythrough (fun _ -> count <- loaded count videos; null))
        |> ignore

load ()
