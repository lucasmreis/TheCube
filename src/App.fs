module TheCube

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

importSideEffects "./styles.css"

type TopOrBottom = Top | Bottom

type ShowingSide =
    | TopOrBottom of TopOrBottom
    | Side of turns: int

type TurnTo = ToRight | ToLeft | ToTop | ToBottom

let turn showing turnTo =
    match showing, turnTo with
    | TopOrBottom _, ToRight -> Side -1
    | TopOrBottom _, ToLeft -> Side 1
    | TopOrBottom Top, ToTop -> Side 2
    | TopOrBottom Top, ToBottom -> Side 0
    | TopOrBottom Bottom, ToTop -> Side 0
    | TopOrBottom Bottom, ToBottom -> Side 2
    | Side _, ToTop -> TopOrBottom Top
    | Side _, ToBottom -> TopOrBottom Bottom
    | Side t, ToRight -> Side (t - 1)
    | Side t, ToLeft -> Side (t + 1)

let show position =
    match position with
    | TopOrBottom Top -> "rotateX(-90deg) rotateY(0deg)"
    | TopOrBottom Bottom -> "rotateX(90deg) rotateY(0deg)"
    | Side t ->
        let degrees = t * 90
        "rotateX(0deg) rotateY(" + degrees.ToString() + "deg)"

let update (cube: Browser.HTMLElement) showing turnTo =
    let next = turn showing turnTo
    cube.style.transform <- show next
    Browser.console.log (sprintf "Showing: %A" next)
    Browser.console.log (sprintf "Style: %A" (show next))
    Browser.console.log "---"
    next

let init () =
    let mutable showing = Side 0

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

let listenToLoad (f: Browser.EventListenerOrEventListenerObject) =
    let videos = Browser.document.getElementsByTagName "video"
    [0..3] |> List.map (fun id -> (videos.Item id).addEventListener("canplaythrough", f, false))

// load ()
// init ()

