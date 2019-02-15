module WorldGeneration

open System
open DomainTypes
open DomainFunctions

let secretClosetChance = 0.20

// Check that all rooms are connected. May work, needs more testing.
let validateRoomConnectList (map:RoomConnect list) =
    let adjRoomNames room =
        let (name,adjRooms,overlook) = room
        adjRooms |> List.map fst

    let rec inner acc list visited =
        match list with
        | [] -> true // Exit recursion when all rooms have been visited and removed from the list.
        | x::xs -> 
            if acc |> List.containsBy (adjRoomNames >> List.contains (Tuple3.fst x)) then inner (x::acc) xs [] // Check next list item against the connections in acc.
            else 
                if visited |> List.contains x then printfn "\nFailed: %A\n" x; false else            
                inner acc (xs@[x]) (x::visited)

    let map = map |> List.map (fun rC -> rC.RoomMap)
    inner [List.head map] (List.tail map) []


let activateClosetChance roomType (rng:Random) =
    match roomType with
    | Closet false -> Closet (rng.Next(0, 2) = 0)
    | _ -> roomType
    
// Default rooms. Every setup has each of these.
let roomSpawnBase = 
    [ Spawn; Patio; Garden; Garage; Bathroom; Stairs; Hallway; CommonRoom; EntranceWay; PrivateRoom; Closet true; MissionRoom ]

let roomSpawnBaseCount = 12

// Get a list of RoomTypes and then map over them with actual data from RoomData.fs and assign extra data for the number of connections each room can have.
let getRoomSpawnData roomCount (rng:Random) =
    if roomCount <= roomSpawnBaseCount then 
        roomSpawnBase
        |> Seq.map (RoomData.spawnRoomNameDesc rng)
        |> Seq.map (RoomData.getRoomFromRoomData rng)
        |> Seq.map (fun (rData,rType) -> rData, SpawnRoom.initSpawnRoom rng rType RoomData.getConnectionLimit)
    else
    Seq.initInfinite ( fun _ -> RoomData.roomChanceWeights |> List.randomChoice rng )
    |> Seq.append roomSpawnBase
    |> Seq.map (RoomData.spawnRoomNameDesc rng)    // Get name and description for each room based on roomType.
    |> Seq.distinct // Duh! If you want the rooms to be distinct by name and description, don't add random items to them yet!!!
    |> Seq.map (RoomData.getRoomFromRoomData rng)
    |> Seq.map (fun (rData,rType) -> rData, activateClosetChance rType rng)
    |> Seq.map (fun (rData,rType) -> rData, SpawnRoom.initSpawnRoom rng rType RoomData.getConnectionLimit)
    |> Seq.take roomCount

/// Create to adjacency graphs for each room in the game.
let connectRooms (rng:Random) roomList =
    // Initial pass to connect all rooms in the order they appear in the list.
    let firstPass list =
        let rec inner list acc =
            match list with
            | [] -> acc // No rooms left to connect -> return list of connected rooms
            | x::[] -> x::acc // One room left with no rooms for it to connect to -> return list of connected rooms with this one added.
            | x::y::xs ->
                let (newX, newY) = SpawnRoom.connectRooms x y rng
                inner (newY::xs) (newX::acc)
        inner list []
    // Final pass to connect rooms randomly to rooms they are not yet connected to, within their connection limits.
    let finalPass list =
        let rec findFreeRoom room list =
            let list =
                match room.SpawnRoom.Type with
                | Closet _ -> list |> List.filter (fun rC -> match rC.SpawnRoom.Type with | MissionRoom -> false | _ -> true)
                | MissionRoom -> list |> List.filter (fun rC -> match rC.SpawnRoom.Type with | Closet _ -> false | _ -> true)
                | _ -> list
            match list |> List.filter (SpawnRoom.areConnected room >> not) |> List.filter (SpawnRoom.isMaxedOut >> not) with
            | [] -> None
            | x::[] -> Some x
            | xs -> xs |> List.randomChoice rng |> Some

        let rec inner list acc = // acc for fully finished rooms
            match list with
            | [] -> acc
            | x::xs when xs |> List.forall (SpawnRoom.areConnected x) -> inner xs (x::acc)
            | x::xs when x |> SpawnRoom.isMaxedOut -> inner xs (x::acc)
            | x::xs ->
                match findFreeRoom x xs with
                | None -> inner xs (x::acc)
                //| Some next when (x |> SpawnRoom.isCloset) && (next |> SpawnRoom.isMissionRoom) || (x |> SpawnRoom.isMissionRoom) && (next |> SpawnRoom.isCloset) ->
                  //  let newXS = xs |> List.remove next  // next is picked randomly so have to update the list.
                    //inner (x::next::newXS) acc
                | Some next ->  // A match has been found. Connect the two rooms and decide which, if any will be put back on the pile or moved to acc.
                    let (newX, newNext) = SpawnRoom.connectRooms x next rng
                    match SpawnRoom.isMaxedOut newX, SpawnRoom.isMaxedOut newNext with
                    | true, true ->   // Both are done -> move both to acc.
                        let newXS = xs |> List.removeOne next
                        inner newXS (newX::newNext::acc)
                    | false, true ->   // Randomly chosen room is done -> move it to acc.
                        let newXS = xs |> List.removeOne next
                        inner (newX::newXS) (newNext::acc)
                    | true, false ->   // Loop room is done -> move it to acc, leaving the chosen room on the list.
                        let newXS = xs |> List.removeOne next
                        inner (newNext::newXS) (newX::acc)
                    | false, false ->   // Both are not yet done -> leave both on the list.
                        let newXS = xs |> List.removeOne next
                        inner (newX::newNext::newXS) acc

        inner list []            
    
    roomList |> firstPass |> finalPass  // first pass links all rooms in their list order, final pass links based on rng.

let tryLinkSecretCloset (rng:Random) (rooms:RoomConnect list) =
    // Closet has the mission room added to its map -> add room Data to the hiddenPassageway.
    let rec linkPassageways mRoomNames (closet:RoomConnect) =
        match closet.Room.Items |> List.tryFind (function | HiddenPassageway (_,_) -> true | _ -> false) with
        | None -> linkPassageways mRoomNames {closet with Room = {closet.Room with Items = (ItemData.HiddenPassageway.getItem rng)::closet.Room.Items}}
        | Some (HiddenPassageway(info, roomNames)) ->
            let newPassage = HiddenPassageway (info, mRoomNames @ roomNames)
            let newItems = closet.Room.Items |> List.replaceByWith (function | HiddenPassageway (_,_) -> true | _ -> false) newPassage
            let newRoom = {closet.Room with Items = newItems}
            {closet with Room = newRoom}
        | _ -> failwith "Default clause hit in WorldGeneration.tryLinkSecretCloset.linkPassageways"

    let linkClosetToMissionRoom closet mRoom = // closet -> mRoom = secret. mRoom -> closet = unlocked
        SpawnRoom.connectRoomsWithLockState closet mRoom Secret Unlocked |> fst
    let linkMissionRoomToCloset mRoom closet =
        SpawnRoom.connectRoomsWithLockState closet mRoom Secret Unlocked |> snd

    // type RoomConnect = {Room: Room; RoomMap: RoomMap; SpawnRoom: SpawnRoom}
    let secretClosets = rooms |> List.filter (fun rC -> match rC.SpawnRoom.Type with | Closet true -> true | _ -> false)
    let missionRooms = rooms |> List.filter (fun rC -> match rC.SpawnRoom.Type with | MissionRoom -> true | _ -> false)

    let newSecretClosets = 
        secretClosets 
        |> List.map (fun c -> missionRooms |> List.fold (fun s m -> linkClosetToMissionRoom s m) c)
        |> List.map (linkPassageways (missionRooms |> List.map (fun rC -> rC.Room.Info.Name)))

    let newMissionRooms = missionRooms |> List.map (fun m -> secretClosets |> List.fold (fun s c -> linkMissionRoomToCloset s c) m)

    let restOfRooms = rooms |> List.filter (fun r -> secretClosets@missionRooms |> List.contains r |> not)
    newSecretClosets @ newMissionRooms @ restOfRooms

    //Makes more sense here as it's the last time all of the rooms will be loaded at once
/// Find mission items (targets and intel) for use by the Environment object for the game. 
let findMissionObjectives roomCList =
    
    let folder (roomC:RoomConnect) =
        let intelList = roomC.Room.Items |> List.filter (function | Intel _ -> true | _ -> false)
        let targetList = roomC.Room.People |> List.filter (fun p -> match p.Type with | Target -> true | _ -> false)
        let itemObjs = intelList |> List.map (fun i -> CollectIntel (false, Item.getName i), roomC.Room.Info.Name)
        let targetObjs = targetList |> List.map (fun t -> Kill (false, t.Info.Name, Alive), roomC.Room.Info.Name)
        itemObjs @ targetObjs

    roomCList |> List.fold (fun objList rInf -> (folder rInf) @ objList) []

// Create clues with info about the objectives and their locations and put them in the appropriate rooms.
let pairObjectivesWithClueRooms objectives (rng:Random) rooms =
    let rec inner objs rooms acc =
        match objs, rooms with
        | [], rs -> rs @ acc
        | os, [] -> acc
        | o::os, r::rs when ItemData.clueRooms |> List.contains r.SpawnRoom.Type ->
            let room = r.Room
            match ItemData.Clue.getItem rng with
            | Clue (info, clueInfo) ->
                let clueStr = sprintf "%s: %s" (snd o) (o |> fst |> Objective.getInfoStr)
                let newClue = Clue(info,clueStr)
                let newRoom = {room with Items = newClue::room.Items}
                let newRC = {r with Room = newRoom}
                inner os rs (newRC::acc)
            | _ -> failwith "Fail in WorldGeneration.pairObjectivesWithClueRooms"
        | o::os, r::rs -> inner os rs (r::acc)
    inner objectives rooms []

/// The main function of the WorldGeneration project.
let spawnRooms roomCount (rng:Random) =
    let roomInfo =
        getRoomSpawnData roomCount rng
        |> Seq.map SpawnRoom.initRoomConnect
        |> Seq.toList
        |> connectRooms rng
        |> tryLinkSecretCloset rng  // Forces a hidden passageway to "hopefully" eliminate the bug where (Closet true) doesn't get a hiddenpassageway.
        |> SpawnRoom.tryFilterClosetConnections // Remove duplicate connections occuring between closets and mision rooms.
        //|> Debug.printListPass "Connections"

    let objectives = roomInfo |> findMissionObjectives
    let roomData = 
        roomInfo
        |> pairObjectivesWithClueRooms objectives rng
        |> List.map SpawnRoom.roomConnectToRoomInfo

    
    roomData, (objectives |> List.map fst)

// Main game will call this function to generate game data, then use a new module in RoomIO.fs to save all game data to a special folder,
// erasing what was previously in the folder, in order to run the game.