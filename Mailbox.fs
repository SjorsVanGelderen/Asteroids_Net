(*
   Communication functionality, including networking
*)

module Mailbox

open Microsoft.Xna.Framework
open Lens

//Local communication, some of these are still wrong
type Mail =
    | NewPlayer          of int * int * Vector2 //Recipient ID, entity ID, position
    | NewAsteroid        of int * Vector2 * Vector2 //Recipient ID, position, velocity
    | NewProjectile      of int * Vector2 * Vector2 //Recipient ID, position and velocity
    | NewPlayerSlave     of int * Vector2 //Entity ID, position
    | NewAsteroidSlave   of int * Vector2 * Vector2 //Recipient ID, position
    | NewProjectileSlave of int * Vector2 * Vector2 //Entity ID, position, velocity
    | Destruction        of int * int //Recipient ID, entity ID
    | Collision          of int * int //Recipient ID. entity ID
    | Transform          of int * Vector2 * double * double //Recipient ID, position, orientation, scale

//Incoming or outgoing
type MailboxType =
    | Inbox
    | Outbox

type Mailbox =
    { Inbox  : Mail List
      Outbox : Mail List } with
    static member Zero =
        { Inbox  = List.empty
          Outbox = List.empty }
    static member inbox =
        { Get = fun (x : Mailbox) -> x.Inbox
          Set = fun v (x : Mailbox) -> {x with Inbox = v} }
    static member outbox =
        { Get = fun (x : Mailbox) -> x.Outbox
          Set = fun v (x : Mailbox) -> {x with Outbox = v} }
