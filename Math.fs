(*
   Elementary math module
*)

module Math

open Microsoft.Xna.Framework

//Used for random number generation
let random = System.Random ((int) System.DateTime.Now.Ticks)

//A rectangular region in space
type Area =
    { Origin     : Vector2
      Dimensions : Vector2 }
    member this.Contains (point : Vector2) =
        (Rectangle (int this.Origin.X,
                    int this.Origin.Y,
                    int this.Dimensions.X,
                    int this.Dimensions.Y)).Contains point

//The game area
let space =
    { Origin     = Vector2 (float32 -96.0,
                            float32 -96.0)
      Dimensions = Vector2 (float32 (800.0 + 96.0),
                            float32 (600.0 + 96.0)) }

//Generates a unique entity ID with a margin of error (duplicates are possible but unlikely)
let newEntityID () = hash (random.Next ())
