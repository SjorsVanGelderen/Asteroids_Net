(*
   The main game logic
*)

module Logic

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics
open Lidgren.Network
open Media
open CoroutineMonad
open Lens
open Input
open Math
open Rendering
open Mailbox
open Networking

//Global game object container
type Entity<'w, 'fs, 'mailbox> =
    { Fields  : 'fs
      Rules   : List<'w -> 'fs -> float -> 'fs>
      Scripts : Coroutine<'w, 'mailbox, 'fs, unit>
      Render  : Coroutine<'w, RenderData, 'fs, unit> } with
    static member Create(fields, rules, scripts, render) =
        { Fields  = fields
          Rules   = rules
          Scripts = andPassMany_ scripts
          Render  = render }
    member this.Update(world, mailbox, dt) =
        let fsRules = this.Rules |> List.fold (fun fs rule -> rule world fs dt) this.Fields
        let mailbox', fsScripts, scripts' = step_ (this.Scripts world mailbox fsRules)
        let rd, fsRender, render' =
            step_ (this.Render world (EntityRD ("Player",
                                                Vector2.Zero,
                                                0.0,
                                                0.0,
                                                0)) fsScripts)

        { Fields  = fsRender
          Rules   = this.Rules
          Scripts = scripts'
          Render  = render' }, mailbox', rd

//Player data
and PlayerFields =
    { ID         : int
      Body       : Body
      Health     : int } with
    static member Zero =
        { ID         = newEntityID ()
          Body       = { Body.Zero with
                             Position    = Vector2(float32 300, float32 300)
                             Orientation = -System.Math.PI / 2.0
                             Radius      = 32.0
                             Friction    = -0.5 }
          Health     = 3 }
    static member Rules =
        [ fun w fs dt ->
              let fsRotate : PlayerFields =
                  if findKey Keys.Left w.KeyboardInput then
                      { fs with Body = Body.Rotate fs.Body -(System.Math.PI) dt }
                  else if findKey Keys.Right w.KeyboardInput then
                      { fs with Body = Body.Rotate fs.Body (System.Math.PI) dt }
                  else
                      fs

              let fsThrust : PlayerFields =
                  if findKey Keys.Up w.KeyboardInput then
                      { fsRotate with Body = Body.Thrust fsRotate.Body 256.0 dt }
                  else
                      fsRotate

              let fsMove : PlayerFields = { fsThrust with Body = Body.Move fsThrust.Body dt }
              fsMove ]
    static member SlaveRules =
        [ fun w fs dt ->
              let fsMove : PlayerFields = { fs with Body = Body.MoveToTarget fs.Body dt }
              let fsRotate : PlayerFields = { fsMove with Body = Body.RotateToTarget fsMove.Body dt }
              fsRotate ]
    static member Scripts =
        let initRoutine =
            repeatFor_ 1 (co { do! yield_
                               do! printfn_ "test"
                               let! fs = getInnerState_
                               let! mailbox = getOuterState_
                               let! message =
                                   co { return (NewPlayerSlave
                                                   (fs.ID,
                                                    Vector2 (float32 fs.Body.Position.X, float32 fs.Body.Position.Y))) }
                               do! setOuterState_ { mailbox with Outbox = message :: mailbox.Outbox } })
        
        let shootRoutine =
            co { do! wait_ 0.5
                 return! (guard_ (co { do! yield_
                                       let! w = getGlobalState_
                                       return findKey Keys.Space w.KeyboardInput })
                                 (co { do! yield_
                                       let! fs = getInnerState_
                                       let! inMessage =
                                           co { return (NewProjectile
                                                            (0,
                                                             Vector2 (float32 fs.Body.Position.X, float32 fs.Body.Position.Y),
                                                             Vector2 (float32 (cos (fs.Body.Orientation) * 1024.0),
                                                                      float32 (sin (fs.Body.Orientation) * 1024.0)))) }
                                       let! mailbox = getOuterState_
                                       do! setOuterState_ { mailbox with Inbox = inMessage :: mailbox.Inbox } })) } |> repeat_
                                       (*
                                       let! outMessage =
                                           co { return (NewSlaveProjectile
                                                            (0,
                                                             0,
                                                             Vector2 (float32 fs.Body.Position.X, float32 fs.Body.Position.Y),
                                                             Vector2 (float32 (cos (fs.Body.Orientation) * 1024.0),
                                                                      float32 (sin (fs.Body.Orientation) * 1024.0)))) }
                                       let! mailbox = getOuterState_
                                       do! setOuterState_ { mailbox with Inbox  = message :: mailbox.Inbox
                                                                         Outbox = message :: mailbox.Outbox } })) } |> repeat_*)

        let collisionRoutine =
            co { do! yield_
                 let! w = getGlobalState_
                 let! fs = getInnerState_
                 let! collision =
                     co { return List.exists
                              (fun (elem : Entity<State, AsteroidFields, Mailbox>) ->
                                   if Vector2.Distance (fs.Body.Position, elem.Fields.Body.Position) <
                                       float32 (fs.Body.Radius + elem.Fields.Body.Radius) then
                                       true
                                   else
                                       false) w.Asteroids }
                 let! mailbox = getOuterState_
                 if collision then
                     do! setOuterState_ { mailbox with Inbox = (Destruction (0, fs.ID)) :: mailbox.Inbox }
                 else
                     do! setOuterState_ mailbox } |> repeat_

        let outputMailbox = co { do! yield_
                                 let! mailbox = getOuterState_
                                 let! outbox = co { return mailbox.Outbox }
                                 do! printfn_ outbox }
        
        let frameLens = PlayerFields.body >>| Body.animationData >>| AnimationData.frame
        let animationRoutine =
            co { do! wait_ 0.02
                 let! fs = getInnerState_
                 let! fs' = co { return fs |> frameLens += 1 } //Go to the next frame
                 let! fs'' = co { return fs' |> frameLens.Set ((fs' |> frameLens.Get) % 8) } //Wrap to beginning
                 do! setInnerState_ fs''
                 do! yield_ } |> repeat_

        let networkRoutine =
            co { do! yield_
                 let! fs = getInnerState_
                 let! mailbox = getOuterState_
                 do! setOuterState_ { mailbox with
                                          Outbox = (Transform (fs.ID, fs.Body.Position,
                                                               fs.Body.Orientation, 1.0)) :: mailbox.Outbox } } |> repeat_

        [ initRoutine
          animationRoutine
          networkRoutine ]
    static member SlaveScripts =
        let shadowPositionLens    = PlayerFields.body >>| Body.shadowData >>| ShadowData.position
        let shadowOrientationLens = PlayerFields.body >>| Body.shadowData >>| ShadowData.orientation
        let shadowRoutine =
            co { do! yield_
                 let! fs = getInnerState_
                 let! mailbox = getOuterState_
                 let! transform =
                     co { return List.tryPick
                                     (fun e ->
                                          match e with
                                          | Transform (id, pos, orientation, scale) when id = fs.ID -> Some (pos, orientation)
                                          | _ -> None) mailbox.Inbox }
                 match transform with
                 | Some (p, o) -> do! setInnerState_ (fs |> shadowPositionLens.Set p
                                                         |> shadowOrientationLens.Set o)
                 | None -> () } |> repeat_
                 
        let frameLens = PlayerFields.body >>| Body.animationData >>| AnimationData.frame
        let animationRoutine =
            co { do! wait_ 0.02
                 let! fs = getInnerState_
                 let! fs' = co { return fs |> frameLens += 1 } //Go to the next frame
                 let! fs'' = co { return fs' |> frameLens.Set ((fs' |> frameLens.Get) % 8) } //Wrap to beginning
                 do! setInnerState_ fs''
                 do! yield_ } |> repeat_
                 
        [ shadowRoutine
          animationRoutine ]
    static member Render =
        co { let! fs = getInnerState_
             do! setOuterState_ (EntityRD ("Player",
                                           fs.Body.Position,
                                           fs.Body.Orientation,
                                           fs.Body.Radius,
                                           fs.Body.AnimationData.Frame))
             do! yield_ } |> repeat_
    static member id =
        { Get = fun x -> x.ID
          Set = fun v x -> {x with ID = v} }
    static member body =
        { Get = fun x -> x.Body
          Set = fun v x -> {x with Body = v} }

//Asteroid data
and AsteroidFields =
    { ID         : int
      Body       : Body } with
    static member Zero =
        { ID        = newEntityID ()
          Body      = { Body.Zero with
                            Position    = Vector2(float32 (space.Origin.X + (float32 (random.NextDouble ())) * space.Dimensions.X),
                                                  float32 (space.Origin.Y + (float32 (random.NextDouble ())) * space.Dimensions.Y))
                            Orientation = (random.NextDouble ()) * (System.Math.PI * 2.0)
                            Radius      = 32.0
                            Velocity    = (Vector2.Normalize (Vector2(float32 (random.NextDouble () - random.NextDouble ()),
                                                                      float32 (random.NextDouble () - random.NextDouble ())))) *
                                          (float32 (random.Next (32, 64))) } }
    static member Rules =
        [ fun w fs dt ->
              let fsMove : AsteroidFields = { fs with Body = Body.Move fs.Body dt }
              let fsRotate : AsteroidFields = { fsMove with Body = Body.Rotate fsMove.Body System.Math.PI dt }
              fsRotate ]
    static member SlaveRules =
        [ fun w fs dt -> fs ]
    static member Scripts =
        [ co { do! yield_ } |> repeat_ ]
    static member SlaveScripts =
        [ co { do! yield_ } |> repeat_ ]
    static member Render =
        co { let! fs = getInnerState_
             do! setOuterState_ (EntityRD ("Asteroid",
                                           fs.Body.Position,
                                           fs.Body.Orientation,
                                           fs.Body.Radius,
                                           fs.Body.AnimationData.Frame))
             do! yield_ } |> repeat_
    static member body =
        { Get = fun x -> x.Body
          Set = fun v x -> {x with Body = v} }

//Projectile data
and ProjectileFields =
    { ID         : int
      Owner      : int
      Body       : Body } with
    static member Zero =
        { ID    = newEntityID ()
          Owner = 0
          Body  = { Body.Zero with
                        Position = Vector2 (float32 (random.Next (0, 1000)),
                                              float32 (random.Next (0, 500)))
                        Radius   = 8.0 } }
    static member Rules =
        [ fun w fs dt ->
              let fs' : ProjectileFields = { fs with Body = Body.MoveWithoutWrap fs.Body dt }
              fs' ]
    static member SlaveRules =
        [ fun w fs dt ->
              let fsMove : ProjectileFields = { fs with Body = Body.MoveToTarget fs.Body dt }
              let fsRotate : ProjectileFields = { fsMove with Body = Body.RotateToTarget fsMove.Body dt }
              fsRotate ]
    static member Scripts =
        let frameLens = ProjectileFields.body >>| Body.animationData >>| AnimationData.frame

        let boundsRoutine =
            co { do! yield_
                 let! fs = getInnerState_
                 if not (space.Contains fs.Body.Position) then
                     let! mailbox = getOuterState_
                     do! setOuterState_ { mailbox with Inbox = (Destruction (0, fs.ID)) :: mailbox.Inbox } } |> repeat_

        let collisionRoutine =
            co { do! yield_
                 let! w = getGlobalState_
                 let! fs = getInnerState_
                 let! collisions =
                     co { return List.fold (fun acc (elem : Entity<State, AsteroidFields, Mailbox>) ->
                                                if Vector2.Distance (fs.Body.Position, elem.Fields.Body.Position) <
                                                    float32 (fs.Body.Radius + elem.Fields.Body.Radius) then
                                                    (Destruction (0, elem.Fields.ID)) :: acc
                                                else
                                                    acc) [] w.Asteroids }
                 let! mailbox = getOuterState_
                 let! mailbox' = co { return { mailbox with Inbox = collisions @ mailbox.Inbox } }
                 if not (List.isEmpty collisions) then
                     do! setOuterState_ { mailbox' with Inbox = (Destruction (0, fs.ID)) :: mailbox'.Inbox }
                 else
                     do! setOuterState_ mailbox' } |> repeat_

        let animationRoutine =
            co { do! wait_ 0.01
                 let! fs = getInnerState_
                 let! fs' = co { return fs |> frameLens += 1 } //Go to the next frame
                 let! fs'' = co { return fs' |> frameLens.Set ((fs' |> frameLens.Get) % 4) } //Wrap to beginning
                 do! setInnerState_ fs''
                 do! yield_ } |> repeat_

        [ boundsRoutine
          collisionRoutine
          animationRoutine ]
    static member SlaveScripts =
        [ co { do! yield_ } ]
    static member Render =
        co { let! fs = getInnerState_
             do! setOuterState_ (EntityRD ("Projectile",
                                           fs.Body.Position,
                                           fs.Body.Orientation,
                                           fs.Body.Radius,
                                           fs.Body.AnimationData.Frame))
             do! yield_ } |> repeat_
    static member body =
        { Get = fun x -> x.Body
          Set = fun v x -> {x with Body = v} }

//Used to keep track of sprite animations
and AnimationData =
    { Frame : int } with
    static member Zero =
        { Frame = 0 }
    static member frame =
        { Get = fun (x : AnimationData) -> x.Frame
          Set = fun v (x : AnimationData) -> {x with Frame = v} }

//Information about shadow copies of networked entities
and ShadowData =
    { Position    : Vector2
      Orientation : double } with
    static member Zero =
        { Position    = Vector2.Zero
          Orientation = 0.0  }
    static member position =
        { Get = fun (x : ShadowData) -> x.Position
          Set = fun v (x : ShadowData) -> {x with Position = v} }
    static member orientation =
        { Get = fun (x : ShadowData) -> x.Orientation
          Set = fun v (x : ShadowData) -> {x with Orientation = v} }

//Physical body for entities
and Body =
    { Position      : Vector2
      Orientation   : double
      Radius        : double
      Velocity      : Vector2
      Friction      : double
      AnimationData : AnimationData
      ShadowData    : ShadowData } with
    static member Zero =
        { Position      = Vector2.Zero
          Orientation   = 0.0
          Radius        = 0.0
          Velocity      = Vector2.Zero
          Friction      = 0.0
          AnimationData = AnimationData.Zero
          ShadowData    = ShadowData.Zero }
    static member Move (b : Body) (dt : double) =
        let position' = Vector2 (b.Position.X + b.Velocity.X * float32 dt,
                                 b.Position.Y + b.Velocity.Y * float32 dt)

        let updateX x =
            if x > space.Dimensions.X then
                x - space.Dimensions.X
            else if x < space.Origin.X then
                x + space.Dimensions.X
            else x

        let updateY y =
            if y > space.Dimensions.Y then
                y - space.Dimensions.Y
            else if y < space.Origin.Y then
                y + space.Dimensions.Y
            else
                y

        { b with Position = Vector2 (updateX position'.X, updateY position'.Y)
                 Velocity = Vector2 (b.Velocity.X * float32 (System.Math.Pow (System.Math.E, b.Friction * dt)),
                                     b.Velocity.Y * float32 (System.Math.Pow (System.Math.E, b.Friction * dt))) }
    static member MoveWithoutWrap (b : Body) (dt : double) =
        { b with Position = Vector2 (b.Position.X + b.Velocity.X * float32 dt,
                                     b.Position.Y + b.Velocity.Y * float32 dt)
                 Velocity = Vector2(b.Velocity.X * float32 (System.Math.Pow (System.Math.E, b.Friction * dt)),
                                    b.Velocity.Y * float32 (System.Math.Pow (System.Math.E, b.Friction * dt))) }
    static member MoveToTarget (b : Body) (dt : double) =
        //This lerp function hasn't been implemented properly yet
        { b with Position = Vector2.Lerp (b.Position, b.ShadowData.Position, float32 1.0) }
    static member RotateToTarget (b : Body) (dt : double) =
        { b with Orientation = double (MathHelper.Lerp (float32 b.Orientation, float32 b.ShadowData.Orientation, float32 0.9)) }
    static member AddForce (b : Body) (v : Vector2) (dt : double) =
        { b with Velocity = b.Velocity + Vector2 (v.X * float32 dt,
                                                  v.Y * float32 dt) }
    static member Rotate (b : Body) (r : double) (dt : double) =
        { b with Orientation = b.Orientation + (r * dt) }
    static member Thrust (b : Body) (f : double) (dt : double) =
        { b with Velocity = b.Velocity + Vector2 (float32 (cos (b.Orientation) * f * dt),
                                                  float32 (sin (b.Orientation) * f * dt)) }
    static member position =
        { Get = fun x -> x.Position
          Set = fun v x -> {x with Position = v} }
    static member orientation =
        { Get = fun x -> x.Orientation
          Set = fun v x -> {x with Orientation = v} }
    static member radius =
        { Get = fun x -> x.Radius
          Set = fun v x -> {x with Radius = v} }
    static member velocity =
        { Get = fun x -> x.Velocity
          Set = fun v x -> {x with Velocity = v} }
    static member friction =
        { Get = fun x -> x.Friction
          Set = fun v x -> {x with Friction = v} }
    static member animationData =
        { Get = fun x -> x.AnimationData
          Set = fun v x -> {x with AnimationData = v} }
    static member shadowData =
        { Get = fun x -> x.ShadowData
          Set = fun v x -> {x with ShadowData = v} }

//The global game state
and State =
    { Players          : Entity<State, PlayerFields,     Mailbox> List
      Asteroids        : Entity<State, AsteroidFields,   Mailbox> List
      Projectiles      : Entity<State, ProjectileFields, Mailbox> List
      Mailbox          : Mailbox
      RenderCollection : RenderData List
      KeyboardInput    : Keys List
      ExitFlag         : bool
      Textures         : Map<string, Texture2D Option>
      NetConfig        : NetPeerConfiguration Option
      NetPeer          : NetPeer Option } with
    static member Zero =
        let newPlayer () =
            NewPlayer (0, newEntityID (),
                       Vector2 (space.Origin.X + space.Dimensions.X / float32 2.0,
                                space.Origin.Y + space.Dimensions.Y / float32 2.0))

        let newAsteroid () =
            NewAsteroid (0,
                         Vector2 (space.Origin.X + space.Dimensions.X * float32 (random.NextDouble ()),
                                  space.Origin.Y + space.Dimensions.Y * float32 (random.NextDouble ())),
                         (Vector2.Normalize (Vector2(float32 (random.NextDouble () - random.NextDouble ()),
                                                     float32 (random.NextDouble () - random.NextDouble ())))) *
                              (float32 (random.Next (32, 64))))

        { Players          = List.empty
          Asteroids        = List.empty
          Projectiles      = List.empty
          Mailbox          = { Inbox  = [ newPlayer () ]
                               Outbox = List.empty } 
          RenderCollection = List.empty
          KeyboardInput    = List.empty
          ExitFlag         = false
          Textures         = Map.empty
          NetConfig        = None
          NetPeer          = None }
    static member LoadAssets (content : ContentManager) (s : State) =
        { s with
              Textures =
                  [ "Asteroid",   loadAsset content "Asteroid"
                    "Player",     loadAsset content "Player"
                    "Projectile", loadAsset content "Projectile"
                    "Ship",       loadAsset content "Ship"
                    "Space",      loadAsset content "Space"      ] |> Map.ofList }
    static member StartNetworking s =
        let config, peer = startNetworking ()
        { s with
              NetConfig = Some config
              NetPeer   = Some peer }
    static member StopNetworking s =
        match s.NetPeer with
        | Some peer -> peer.Shutdown ("Finished program")
        | _ -> ()
    static member SendMail s =
        match s.NetPeer with
        | Some peer ->
            { s with Mailbox = processOutgoingMessages peer s.Mailbox }
        | _ ->
            s
    static member ReceiveMail s =
        match s.NetPeer with
        | Some peer ->
            { s with Mailbox = processIncomingMessages peer s.Mailbox }
        | _ ->
            s
    static member Update s dt =
        let sClear = { s with RenderCollection = List.empty }
        let sInput = { sClear with KeyboardInput = getKeyboardInput () }
        let exit = findKey Keys.Escape sInput.KeyboardInput
        if exit then
            { sInput with ExitFlag = true }
        else
            let connected () =
                match s.NetPeer with
                | Some peer ->
                    (peer.ConnectionsCount > 0)
                | _ -> false
            
            if not (connected ()) then
                //Keep checking for at least one connection
                let sReceive = State.ReceiveMail sInput
                let sSend = State.SendMail sInput
                sSend
            else
                //Various lenses for easy data access
                let playerPositionLens     = PlayerFields.body     >>| Body.position
                let playerIDLens           = PlayerFields.id
                let asteroidPositionLens   = AsteroidFields.body   >>| Body.position
                let asteroidVelocityLens   = AsteroidFields.body   >>| Body.velocity
                let projectilePositionLens = ProjectileFields.body >>| Body.position
                let projectileVelocityLens = ProjectileFields.body >>| Body.velocity
                
                //Get messages
                let sReceive = State.ReceiveMail sInput
                
                //Update players and player slaves
                let players', mailbox', rc' =
                    List.fold (fun (players, mailbox, rc) (player : Entity<State, PlayerFields, Mailbox>) ->
                                   let player', mailbox', rd = player.Update (sReceive, mailbox, dt)
                                   ((player' :: players), mailbox', (rd :: rc))) (List.empty, sReceive.Mailbox, sReceive.RenderCollection) sReceive.Players
                
                //Update asteroids
                let asteroids', mailbox'', rc'' =
                    List.fold (fun (asteroids, mailbox, rc) (asteroid : Entity<State, AsteroidFields, Mailbox>) ->
                                   let asteroid', mailbox', rd = asteroid.Update (sReceive, mailbox, dt)
                                   ((asteroid' :: asteroids), mailbox', (rd :: rc))) (List.empty, mailbox', rc') sReceive.Asteroids

                //Update projectiles
                let projectiles', mailbox''', rc''' =
                    List.fold (fun (projectiles, mailbox, rc) (projectile : Entity<State, ProjectileFields, Mailbox>) ->
                                   let projectile', mailbox', rd = projectile.Update (sReceive, mailbox, dt)
                                   ((projectile' :: projectiles), mailbox', (rd :: rc))) (List.empty, mailbox'', rc'') sReceive.Projectiles
                
                let sSend = State.SendMail { sReceive with Mailbox = mailbox''' }

                //Destroy entities for each relevant message
                let players'', asteroids'', projectiles'' =
                    List.fold (fun (players, asteroids, projectiles) elem ->
                                   match elem with
                                   | Destruction (_, id) ->
                                         (List.filter (fun (p : Entity<State, PlayerFields, Mailbox>)     -> id <> p.Fields.ID) players,
                                          List.filter (fun (p : Entity<State, AsteroidFields, Mailbox>)   -> id <> p.Fields.ID) asteroids,
                                          List.filter (fun (p : Entity<State, ProjectileFields, Mailbox>) -> id <> p.Fields.ID) projectiles)
                                   | _ -> (players, asteroids, projectiles)) (players', asteroids', projectiles') sSend.Mailbox.Inbox

                //Spawn entities for each relevant message
                let newPlayers, newPlayerSlaves, newAsteroids, newProjectiles =
                    List.fold (fun (players, playerSlaves, asteroids, projectiles) elem ->
                                   match elem with
                                   | NewPlayer (target, id, position) ->
                                         let player =
                                             Entity<State, PlayerFields, Mailbox>.Create
                                                 (PlayerFields.Zero |> playerIDLens.Set id
                                                                    |> playerPositionLens.Set position,
                                                  PlayerFields.Rules,
                                                  PlayerFields.Scripts,
                                                  PlayerFields.Render)
                                         (player :: players', playerSlaves, asteroids, projectiles)
                                   | NewPlayerSlave (id, position) ->
                                         let playerSlave =
                                             Entity<State, PlayerFields, Mailbox>.Create
                                                 (PlayerFields.Zero |> playerIDLens.Set id
                                                                    |> playerPositionLens.Set position,
                                                  PlayerFields.SlaveRules,
                                                  PlayerFields.SlaveScripts,
                                                  PlayerFields.Render)
                                         (players, playerSlave :: playerSlaves, asteroids, projectiles)
                                   | NewAsteroid (id, position, velocity) ->
                                         let asteroid =
                                             Entity<State, AsteroidFields, Mailbox>.Create
                                                 (AsteroidFields.Zero |> asteroidPositionLens.Set position
                                                                      |> asteroidVelocityLens.Set velocity,
                                                  AsteroidFields.Rules,
                                                  AsteroidFields.Scripts,
                                                  AsteroidFields.Render)
                                         (players, playerSlaves, asteroid :: asteroids, projectiles)
                                   | NewProjectile (id, position, velocity) ->
                                         let projectile =
                                             Entity<State, ProjectileFields, Mailbox>.Create
                                                 (ProjectileFields.Zero |> projectilePositionLens.Set position
                                                                        |> projectileVelocityLens.Set velocity,
                                                  ProjectileFields.Rules,
                                                  ProjectileFields.Scripts,
                                                  ProjectileFields.Render)
                                         (players, playerSlaves, asteroids, projectile :: projectiles)
                                   | _ -> (players, playerSlaves, asteroids, projectiles)) ([], [], [], []) sSend.Mailbox.Inbox

                { sSend with
                      Players          = newPlayerSlaves @ newPlayers @ players''
                      Asteroids        = newAsteroids @ asteroids''
                      Projectiles      = newProjectiles @ projectiles''
                      Mailbox          = Mailbox.Zero //Shouldn't it save anything?
                      RenderCollection = rc''' }
