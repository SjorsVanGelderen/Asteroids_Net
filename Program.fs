(*
   Contains the entry point and main program sequence
*)

module Program

open Game

[<EntryPoint>]
let main args =
    //Run the game
    use g = new PlutusGame ()
    g.Run ()
    0
