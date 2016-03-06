(*
   The lens provides easy record access and modification functionality
*)

module Lens

type Lens<'a, 'b> =
    { Get: 'a -> 'b
      Set: 'b -> 'a -> 'a }
    with
        member this.Update f a =
            let value = this.Get a
            let newValue = f value
            this.Set newValue a

let inline (>>|) (p : Lens<_, _>) (k : Lens<_, _>) =
    { Get = p.Get >> k.Get
      Set = k.Set >> p.Update }

let (+=) (p : Lens<_, _>) v = p.Update ((+) v)
let (-=) (p : Lens<_, _>) v = p.Update ((-) v)
let (-*=) (p : Lens<_, _>) v = p.Update ((*) v) //Asterisk has problems with operator overloading, hence the dash
let (/=) (p : Lens<_, _>) v = p.Update ((/) v)
let (%=) (p : Lens<_, _>) v = p.Update ((%) v) //Doesn't seem to work properly at the moment
