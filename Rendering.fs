(*
   These functions are used to interpret render data the game logic generates
*)

module Rendering

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics

//Construct for communicating data to be interpreted by the rendering logic
type RenderData =
    | EntityRD     of string * Vector2 * double * double * int //Texture name, position, orientation, radius, frame
    | BackgroundRD of string

let renderEntity
    (textureName  : string)
    (position     : Vector2)
    (orientation  : double)
    (radius       : double)
    (frame        : int)
    (textures     : Map<string, Texture2D Option>)
    (spriteBatch  : SpriteBatch)
    (viewport     : Viewport) =    
    match textures.TryFind textureName with
    | Some (Some (t : Texture2D)) ->
        let srcRect = System.Nullable<Rectangle> (Rectangle (int (double frame * radius * 2.0),
                                                             0, int radius * 2, int radius * 2))
        spriteBatch.Draw (t,
                          position - Vector2(float32 radius, float32 radius),
                          srcRect,
                          Color.White,
                          float32 (orientation + System.Math.PI / 2.0),
                          Vector2(float32 radius, float32 radius),
                          float32 1.0,
                          SpriteEffects.None,
                          float32 0.0)
    | _ -> printfn "Missing texture: %s" textureName

//Renders a background image in fill mode
let renderBackground
    (filename    : string)
    (textures    : Map<string, Texture2D Option>)
    (viewport    : Viewport)
    (spriteBatch : SpriteBatch) =
    match textures.TryFind filename with
    | Some (Some (t : Texture2D)) ->
        let ratio =
            let widthRatio  = (float viewport.Width / float t.Bounds.Width)
            let heightRatio = (float viewport.Height / float t.Bounds.Height)
            if widthRatio < heightRatio then
                heightRatio
            else
                widthRatio
            
        spriteBatch.Draw (t,
                          Vector2.Zero,
                          System.Nullable<Rectangle> (Rectangle (0, 0, t.Bounds.Width, t.Bounds.Height)),
                          Color.White,
                          float32 0.0,
                          Vector2.Zero,
                          float32 ratio,
                          SpriteEffects.None,
                          float32 0.0)
    | _ -> printfn "Missing texture: %s" filename
