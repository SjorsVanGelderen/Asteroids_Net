(*
  MonoGame game class overrides
*)

module Game

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open Media
open Rendering
open Networking
open Logic

type PlutusGame () as context =
    inherit Game ()
    
    let mutable graphics    = new GraphicsDeviceManager (context)
    let mutable viewport    = Unchecked.defaultof<Viewport>
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable state       = State.Zero
    
    override context.Initialize () =
        do context.Content.RootDirectory <- "Content"
        do graphics.GraphicsProfile <- GraphicsProfile.HiDef
        do viewport <- graphics.GraphicsDevice.Viewport
        do spriteBatch <- new SpriteBatch (context.GraphicsDevice)

        //Disable window resizing
        do base.Window.AllowUserResizing <- false
        
        //Initialize networking
        do state <- State.StartNetworking state
        
        base.Initialize ()
        ()
        
    override context.LoadContent () =
        //Load the textures
        do state <- State.LoadAssets context.Content state
        
        base.LoadContent ()
        ()

    override context.Update gameTime =
        let dt = (float gameTime.ElapsedGameTime.TotalSeconds) * 1.0
        base.Update gameTime
        
        //Update the game state
        state <- State.Update state dt
        
        //Check if the user has requested to quit
        if state.ExitFlag then
            do State.StopNetworking state
            base.Exit ()
        ()

    override context.Draw gameTime =
        context.GraphicsDevice.Clear Color.Black
        spriteBatch.Begin()

        renderBackground
            "Space"
            state.Textures
            viewport
            spriteBatch

        //Render everything in the state's render collection
        List.iter
            (fun rd ->
                 match rd with
                 | EntityRD (textureName, position, orientation, radius, frame) ->
                       renderEntity
                           textureName
                           position
                           orientation
                           radius
                           frame
                           state.Textures
                           spriteBatch
                           viewport
                 | _ -> printfn "Unhandled render data: %A" rd ) state.RenderCollection
        spriteBatch.End()
        base.Draw gameTime
        ()
