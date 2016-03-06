(*
    Basic media loading functionality
*)

module Media

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics

//Load a single asset
let loadAsset (content : ContentManager) (filename : string) =
    if not (System.String.IsNullOrEmpty filename) then
        try
            Some (content.Load filename)
        with
            | :? Microsoft.Xna.Framework.Content.ContentLoadException ->
                printfn "Failed to load %s" filename
                None
    else
        None
