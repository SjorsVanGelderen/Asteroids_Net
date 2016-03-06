(*
   Processes keyboard and mouse input
*)

module Input

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

//Find a key in a list of keys
let findKey key list =
    List.exists (fun k -> k = key) list

//Get a list of pressed keys
let getKeyboardInput () =
    let ks = Keyboard.GetState ()
    let keys =
        [ Keys.Up
          Keys.Down
          Keys.Left
          Keys.Right
          Keys.Space
          Keys.Escape ]
    List.filter (fun k -> ks.IsKeyDown (k)) keys

//Get a list of pressed mouse buttons
let getMouseInput () =
    let ms = Mouse.GetState ()
    let buttons =
        [ ms.LeftButton
          ms.RightButton ]
    List.filter (fun b -> b = ButtonState.Pressed) buttons

//Get the mouse posiiton
let getMousePosition () =
    let ms = Mouse.GetState ()
    Vector2 (float32 ms.Position.X, float32 ms.Position.Y)
