(*
   Coroutine monad logic
*)

module CoroutineMonad

//Factorizes objects and recomposes them, f3 is not used by any routines
type Factorization<'f, 'f1, 'f2, 'f3> =
    { Factorize : 'f -> 'f1 * 'f2 * 'f3
      Compose   : 'f1 * 'f2 * 'f3 -> 'f }

//The coroutine type
type Coroutine<'w, 'so, 'si, 'a> = 'w -> 'so -> 'si -> Result<'w, 'so, 'si, 'a>
and Result<'w, 'so, 'si, 'a> =
    | Done  of 'so * 'si * 'a
    | Yield of 'so * 'si * Coroutine<'w, 'so, 'si, 'a>
    
//Return operation
let return_ x = fun w so si -> Done (so, si, x)

//Bind operation
let rec bind_ p k =
    fun w so si ->
        match p w so si with
        | Done (so, si, a)   -> k a w so si
        | Yield (so, si, p') -> Yield (so, si, bind_ p' k)

//Infix operator for bind
let (>>=) = bind_

//The coroutine computation expression builder
type CoroutineBuilder() =
    member this.Return x = return_ x
    member this.ReturnFrom c = c
    member this.Bind (p, k) = p >>= k
    member this.Combine (p, k) = p >>= (fun _ -> k)
    member this.Zero () = return_ ()
    member this.Delay a = a ()
    member this.Run a = a
let co = CoroutineBuilder()

//Printf something from a routine
let printf_ x =
    co { do printf "%A" x
         return () }

//Printfn something from a routine
let printfn_ x =
    co { do printfn "%A" x
         return () }

//Read something from the command line
let read_ () = co{ return System.Console.ReadLine () }

//Pause
let yield_ = fun w so si -> Yield (so, si, (fun w so si -> Done (so, si, ())))

//Return the world state as the result
let getGlobalState_ = fun w so si -> Done(so, si, w)

//Return the inner state as the result
let getInnerState_ = fun w so si -> Done(so, si, si)

//Return the outer state as the result
let getOuterState_ = fun w so si -> Done(so, si, so)

//Set x as the inner state
let setInnerState_ x = fun w so si -> Done(so, x, ())

//Set x as the outer state
let setOuterState_ x = fun w so si -> Done(x, si, ())

//Ignore result
let ignore_ r =
    co{ let! _ = r
        return () }

//Process a step
let step_ r =
    match r with
    | Done (so, si, x)   -> so, si, return_ x
    | Yield (so, si, r') -> so, si, r'

//Execute k only if p is true
let rec guard_ p k =
    co { let! condition = p
         if condition then
             return! k
         else
             return! guard_ p k }

//Do nothing until p is true before executing k
let rec when_ p k =
    co { let! condition = p
         if condition then
             return! k
         else
             do! yield_
             return! when_ p k }

//Repeat the routine
let rec repeat_ r =
    co { do! r
         return! repeat_ r }

//Repeat the routine i times
let rec repeatFor_ i r =
    co { if i > 0 then
             do! r
             return! repeatFor_ (i - 1) r
         else
             return! (co { do! yield_ } |> repeat_) }

//Concurrent(logical OR)
let rec (.||) p k =
    fun w so si ->
        match p w so si with
        | Done (so', si', a) as result -> result //-> Done (so', si', a)
        | Yield (so', si', p') ->
            match k w so' si' with
            | Done (so'', si'', a) as result -> result
            | Yield (so'', si'', k') -> Yield (so'', si'', p' .|| k')

//Concurrent with factorization
let rec orFactorized_ fa p k =
    fun w so si ->
        let si1, si2, si3 = fa.Factorize si
        match p w so si1 with
        | Done (so1, si1', a)   -> Done (so1, fa.Compose (si1', si2, si3), Choice1Of2 a)
        | Yield (so1, si1', p') ->
            match k w so1 si2 with
            | Done (so2, si2', b)   -> Done (so2, fa.Compose (si1', si2', si3), Choice2Of2 b)
            | Yield (so2, si2', k') -> Yield (so2, fa.Compose (si1', si2', si3), orFactorized_ fa p' k') 

//Many concurrent routines
let orMany_ routines =
    co { match routines with
         | x :: xs -> return! Choice1Of2 (List.reduce (fun a b -> a .|| b) routines)
         | [] -> return! Choice2Of2 (co { return () }) }

//Parallel(logical AND) on routines ignoring their result
let rec (.&&) p k =
    fun w so si ->
        match p w so si, k w so si with
        | Done (_, _, _), Done (_, _, _)     -> Done (so, si, ())
        | Done (_, _, _), Yield (_, _, k')   -> Yield (so, si, co { return () } .&& k')
        | Yield (_, _, p'), Done (_, _, _)   -> Yield (so, si, p' .&& co { return () })
        | Yield (_, _, p'), Yield (_, _, k') -> Yield (so, si, p' .&& k')

//Parallel(logical AND) on routines ignoring their result, but passing their state
let rec (.&&>) p k =
    fun w so si ->
        match p w so si with
        | Done (so', si', _) as pStep ->
            match k w so' si' with
            | Done (so'', si'', _) -> Done (so'', si'', ()) //(a1, a2)) can't be given, as that conflicts with andPassMany_
            | Yield (so'', si'', k') -> Yield (so'', si'', (fun w so si -> pStep) .&&> k')
        | Yield (so', si', p') ->
            match k w so' si' with
            | Done (so'', si'', _) as kStep -> Yield (so'', si'', p' .&&> (fun w so si -> kStep))
            | Yield (so'', si'', k') -> Yield (so'', si'', p' .&&> k')

(*
//Parallel(logical AND), problems with andMany_
let rec (.&&) p k =
    fun w (so1, so2) (si1, si2) ->
        match p w so1 si1, k w so2 si2 with
        | Done (so1', si1', a1), Done (so2', si2', a2) ->
            Done ((so1', so2'), (si1', si2'), (a1, a2))
        | Yield (so1', si1', p), Yield (so2', si2', k) ->
            Yield ((so1', so2'), (si1', si2'), (.&&) p k)
        | Yield (so1', si1', p), Done (so2', si2', a2) ->
            Yield ((so1', so2'), (si1', si2'), (.&&) p (fun w so si -> Done (so2', si2', a2)))
        | Done (so1', si1', a1), Yield (so2', si2', k) ->
            Yield ((so1', so2'), (si1', si2'), (.&&) (fun w so si -> Done (so1', si1', a1)) k)
*)

(*
//Parallel with factorization
let rec parallelFactorized fa p k =
    fun w (so1, so2) si ->
        let si1, si2, si3 = fa.Factorize si
        match p w so1 si1, k w so2 si2 with
        | Done (so1', si1', a1), Done(so2', si2', a2) ->
            Done ((so1', so2'), fa.Compose (si1', si2', si3), (a1, a2))
        | Yield (so1', si1', p'), Yield (so2', si2', k') ->
            Yield ((so1', so2'), fa.Compose (si1', si2', si3), parallelFactorized fa p' k')
        | Yield (so1', si1', p'), Done (so2', si2', a2) ->
            Yield ((so1', so2'), fa.Compose (si1', si2', si3), parallelFactorized fa p' (fun w so si -> Done (so2', si2', a2)))
        | Done (so1', si1', a1), Yield (so2', si2', k') ->
            Yield ((so1', so2'), fa.Compose (si1', si2', si3), parallelFactorized fa (fun w so si -> Done (so1', si1', a1)) k')
*)

//Many parallel routines
let andManyIgnore_ routines =
    match routines with
    | h::t -> List.reduce (fun a b -> a .&& b) routines
    | [] -> return_ ()

let andPassMany_ routines =
    match routines with
    | h::t -> List.reduce (fun a b -> a .&&> b) routines
    | [] -> return_ ()

//Sequence two routines and ignore their results
let rec (.>-) p k =
    fun w so si ->
        match p w so si with
        | Yield (so', si', p') -> Yield (so', si', p' .>- k)
        | Done (so', si', _)   ->
            match k w so' si' with
            | Yield (so'', si'', k') -> Yield (so'', si'', (fun w so si -> Done (so'', si'', ())) .>- k')
            | Done (so'', si'', _)   -> Done (so'', si'', ())

//Sequence many routines and ignore their results
let sequenceIgnoreMany_ routines =
    match routines with
    | h::t -> List.reduce (fun a b -> a .>- b) routines
    | [] -> return_ ()

//Process a routine for a given amount of time
let timed_ s r =
    //let time = co { return System.DateTime.Now }
    let time = fun w so si -> Done(so, si, System.DateTime.Now)
    co { let! t0 = time
         let rec waitRoutine () =
             co { let! t = time
                  let dt = (t - t0).TotalSeconds
                  if dt < s then
                      do! yield_
                      do! r
                      return! waitRoutine () }
         do! waitRoutine () }

//Suspend a routine for a given amount of time
let wait_ s =
    timed_ s (co { return () })

//timeout that returns x when timing out
let timeoutWith_ s x =
  co { do! wait_ s
       return x }
