(*
   Functionality for communication across a network
*)

module Networking

open Microsoft.Xna.Framework
open System.Net
open Lidgren.Network
open Math
open Mailbox

//Mode of operation
type Mode =
    | Create = 0
    | Join   = 1

//Used to identify the type of incoming information
type MessageType =
    | PeerInformation    = 0
    | NewPlayerSlave     = 1
    | NewAsteroidSlave   = 2
    | NewProjectileSlave = 3
    | Transform          = 4

//Simple alias for cleaner match expression
type Incoming = NetIncomingMessageType

//Initialize networking
let startNetworking () =
    //Ask user for mode of operation
    let rec getMode () =
        printfn "0 - Create, 1 - Join"
        match System.Console.ReadLine () with
        | "0" ->
            printfn "Creating new session"
            Mode.Create
        | "1" ->
            printfn "Joining existing session"
            Mode.Join
        | _ ->
            printfn "Invalid input!"
            getMode ()

    let mode = getMode ()

    //Ask user for local port input
    let localPort =
        printfn "Type a local port number to use"
        System.Int32.Parse (System.Console.ReadLine ())

    printfn "Using local port: %A" localPort

    //Ask user for remote port input
    let remotePort =
        printfn "Type a remote port number to use"
        System.Int32.Parse (System.Console.ReadLine ())

    printfn "Using remote port: %A" remotePort

    //Generate a random identifier
    let identifier = (hash (random.Next ())).ToString ()

    //Set up networking configuration
    let config = new NetPeerConfiguration "Asteroids"
    do config.Port                      <- localPort
    do config.MaximumConnections        <- 128
    do config.LocalAddress              <- new IPAddress ((int64)0x0100007f) //NetUtility.Resolve("localhost")
    do config.AcceptIncomingConnections <- true

    //Enable Lidgren message types
    let messageTypes =
        [ Incoming.DiscoveryRequest
          Incoming.DiscoveryResponse
          Incoming.ConnectionApproval
          Incoming.StatusChanged
          Incoming.UnconnectedData
          Incoming.Data
          Incoming.VerboseDebugMessage
          Incoming.DebugMessage
          Incoming.WarningMessage
          Incoming.ErrorMessage        ]
    
    let rec enableMessageTypes list =
        match list with
        | x :: xs ->
            do config.EnableMessageType x
            enableMessageTypes xs
        | []      -> ()
    
    enableMessageTypes messageTypes

    //Start the peer object
    printfn "Starting peer"
    let peer = new NetPeer (config)
    peer.Start ()
    
    //Look for peers
    peer.DiscoverKnownPeer ("localhost", remotePort) |> ignore
    peer.DiscoverLocalPeers (remotePort)

    //Return the configuration and the peer so it can be saved in the state
    (config, peer)
    
let handleDebugMessage (message : NetIncomingMessage) =
    printfn "Debug: %s" (message.ReadString ())

let sendPeerInfo (peer : NetPeer) (ip : IPAddress) (port : int) =
    let peerMessage  = peer.CreateMessage ()
    peerMessage.Write ((int)MessageType.PeerInformation)
    
    let ipBytes = ip.GetAddressBytes ()
    peerMessage.Write ((int)MessageType.PeerInformation)
    peerMessage.Write (ipBytes.Length)
    peerMessage.Write (ipBytes)
    peerMessage.Write (port)
    
    if peer.Connections.Count > 0 then
        peer.SendMessage (peerMessage, peer.Connections, NetDeliveryMethod.ReliableOrdered, 0)
    else
        printfn "Couldn't send peer info, no connections!"

//Process received messages
let rec processIncomingMessages (peer : NetPeer) mailbox =
    let inbox = mailbox.Inbox
    let message = peer.ReadMessage ()
    let mail =
        match message with
        | null -> None
        | _    ->
            match message.MessageType with
            | Incoming.VerboseDebugMessage
            | Incoming.DebugMessage
            | Incoming.WarningMessage
            | Incoming.ErrorMessage        ->
                handleDebugMessage message
                None
                
            | Incoming.DiscoveryRequest ->
                peer.SendDiscoveryResponse (null, message.SenderEndPoint)
                None
                        
            | Incoming.DiscoveryResponse ->
                peer.Connect (message.SenderEndPoint) |> ignore
                None
                        
            | Incoming.ConnectionApproval ->
                message.SenderConnection.Approve ()
                printfn "Sending peer info"
                sendPeerInfo peer message.SenderEndPoint.Address message.SenderEndPoint.Port
                None
                
            | Incoming.StatusChanged ->
                let id     = message.SenderConnection.RemoteUniqueIdentifier.ToString ()
                let status = enum<NetConnectionStatus> (message.ReadInt32 ()) //(message.ReadByte ())
                if status = NetConnectionStatus.Connected then
                    let reason = message.SenderConnection.RemoteHailMessage.ReadString ()
                    printfn "%s reports: %A - %s" id status reason
                None
                        
            | Incoming.UnconnectedData ->
                printfn "Unconnected data: %s" (message.ReadString ())
                None

            | Incoming.Data ->
                let messageType = message.ReadInt32 ()
                match (enum<MessageType> messageType) with
                | MessageType.PeerInformation ->
                    let byteLength = message.ReadInt32 ()
                    let ip         = new IPAddress (message.ReadBytes (byteLength))
                    let port       = message.ReadInt32 ()
                    let endPoint   = new IPEndPoint (ip, port)
                        
                    match peer.GetConnection (endPoint) with
                    | null ->
                        let localHash  = peer.Configuration.LocalAddress.GetHashCode ()
                        let localPort  = peer.Configuration.Port.GetHashCode ()
                        let remoteHash = endPoint.Address.GetHashCode ()
                        let remotePort = endPoint.Port.GetHashCode ()
                        if  (localHash <> remoteHash) || (localPort <> remotePort)  then
                            printfn "Initiating new connection to %s:%s"
                                (endPoint.Address.ToString()) (endPoint.Port.ToString ())
                            peer.Connect (endPoint) |> ignore
                    | _  -> ()
                    None
                    
                | MessageType.NewPlayerSlave ->
                    printfn "Receiving player creation with ID %A" id
                    let id = message.ReadInt32 ()
                    let pos_x  = message.ReadFloat ()
                    let pos_y  = message.ReadFloat ()
                    Some (NewPlayerSlave (id, Vector2 (pos_x, pos_y))) //Add message to the mailbox
                    
                | MessageType.Transform ->
                    let target       = message.ReadInt32 ()
                    let pos_x        = message.ReadFloat ()
                    let pos_y        = message.ReadFloat ()
                    let orientation  = message.ReadFloat ()
                    let scale        = message.ReadFloat ()
                    Some (Transform (target, Vector2 (pos_x, pos_y), double orientation, double scale)) //Add message to the mailbox
                    
                | _  ->
                    printfn "Unhandled message type: %A!" messageType
                    None
            
            | _ ->
                printfn "Unhandled message type: %A! %s" message.MessageType (message.ReadString ())
                None
    
    if message <> null then
        peer.Recycle message
        match mail with
        | Some m ->
            processIncomingMessages peer { mailbox with Inbox = m :: mailbox.Inbox }
        | None -> processIncomingMessages peer mailbox
    else
        mailbox

//Send messages to all peers
let processOutgoingMessages (peer : NetPeer) mailbox =
    let outbox = mailbox.Outbox
    if List.isEmpty outbox then
        mailbox
    else
        match peer.Connections with
        | null -> mailbox
        | _ ->
            if peer.ConnectionsCount > 0 then
                List.iter
                    (fun elem ->
                         match elem with
                         | NewPlayerSlave (id, pos) ->
                             printfn "Sending player creation with ID %A" id
                             let message = peer.CreateMessage ()
                             message.Write (int MessageType.NewPlayerSlave)
                             message.Write (id)
                             message.Write (pos.X)
                             message.Write (pos.Y)
                             peer.SendMessage (message, peer.Connections, NetDeliveryMethod.ReliableOrdered, 0)
                         | Transform (id, pos, orientation, scale) ->
                             printfn "Sending position message! %A %A" id pos
                             let message = peer.CreateMessage ()
                             message.Write (int MessageType.Transform)
                             message.Write (id)
                             message.Write (pos.X)
                             message.Write (pos.Y)
                             message.Write (float32 orientation)
                             message.Write (float32 scale)
                             peer.SendMessage (message, peer.Connections, NetDeliveryMethod.ReliableOrdered, 0)
                         | _ ->
                             printfn "Unhandled net mail type: %A" elem) outbox
                { mailbox with Outbox = List.empty }
            else
                mailbox
