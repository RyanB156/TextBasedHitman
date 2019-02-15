module RoomData

open System
open DomainTypes
open DomainFunctions


// Weight w 1-5 for each room type. Create a list where each type is repeated w times, then a random choice 
// from the entire list will take each weight into account by simple probability.
let roomChanceWeights =
    let roomWeights = [
        Patio, 2
        Garden, 3 
        Garage, 2 
        Storage, 1
        Bathroom, 2
        Stairs, 2 
        Hallway, 3
        CommonRoom, 5
        EntranceWay, 2
        PrivateRoom, 2 
        Closet false, 2
        Closet true, 1
        MissionRoom, 1
        ]
    [ for r in roomWeights do yield! (SpawnRoom.transformWeights r) ]

let getConnectionLimit (rng:Random) roomType = // Update this when adding extra rooms...
        match roomType with
        // Outide
        | Spawn -> rng.Next(2, 4) // 2-3 connections.
        | Patio | Garden | Garage | Storage -> rng.Next(3, 6) // Outside rooms have 3-5 connections.
        // Inside
        | Bathroom -> rng.Next(2, 4) // 2-3 connections.
        | Stairs -> rng.Next(4, 7) // 4-6 connections.
        | Hallway -> rng.Next(3, 7) // 3-6 connections.
        | CommonRoom -> rng.Next(2, 4) // 2-3 connections.
        | EntranceWay -> 4
        | PrivateRoom -> 2
        | Closet nameOpt -> 1 // 1 connection, maybe secret connection to mission room or other closets. Secret closets connected first.
        | MissionRoom -> rng.Next(2, 4) // Black key, 1-2 locked connections max, 1 secret access unlocked.

let initMap currentRoom adjacentRooms overlookRooms : RoomMap =
    match overlookRooms with
    | [] -> currentRoom, adjacentRooms, None
    | rs -> currentRoom, adjacentRooms, Some rs


module Spawn =
    let roomOptionsList = ["Spawn", "The Spawn Room"]

module Patio =
    let roomOptionsList = [
        "NorthPatio", "The North Patio"
        "SouthPatio", "The South Patio"
        "EastPatio", "The East Patio"
        "WestPatio", "The West Patio"
        "NortheastPatio", "The Northeast Patio"
        "SoutheastPatio", "The Southeast Patio"
        "SouthwestPatio", "The Southwest Patio"
        "NorthwestPatio", "The Northwest Patio"
        "DiningPatio", "The Dining Patio"
        ]

module Garage =
    let roomOptionsList = [
        "NorthGarage", "The North Garage"
        "SouthGarage", "The South Garage"
        "EastGarage", "The East Garage"
        "WestGarage", "The West Garage"
        "BlueGarage", "A Blue Garage"
        "GreenGarage", "A Green Garage"
        "HorseStable", "Fancy living quarters for horses"
        ]

module Garden =
    let roomOptionsList = [
        "NorthGarden", "The North Garden"
        "SouthGarden", "The South Garden"
        "EastGarden", "The East Garden"
        "WestGarden", "The West Garden"
        "FlowerGarden", "A garden full of brilliantly colored flowers"
        "HerGarden", "A garden full of rose bushes in full bloom"
        "GreenHouse", "A large glass structure. There are many tables covered with plants."
    ]

module Storage =
    let roomOptionsList = [
        "Workshop", "A workshop full of tools"
        "Workshed", "A shed for doing carpentry work"

    ]
    
module Bathroom =
    let roomOptionsList = [
        "LargeBathroom", "A Large Bathroom"
        "SonsBathroom", "The Sons Bathroom"
        "DaughterBathroom", "The Daughters Bathroom"
        "CenterBathroom", "The Central Bathroom"
        "UpperBathroom", "The Upper Bathroom"
        "NorthBathroom", "The North Bathroom"
        "SouthBathroom", "The South Bathroom"
        "EastBathroom", "The East Bathroom"
        "WestBathroom", "The West Bathroom"
    ]

module Stairs =
    let roomOptionsList = [
        "MainStairs", "The main staircase"
        "CentralStairs", "The central staircase"
        "ServiceStairs", "The service staircase"

    ]

module Hallway =
    let roomOptionsList = [
        "DiningHallway", "The dining hallway"
        "FamilyHallway", "The family hallway"
        "ServiceHallway", "The service hallway"
        "NorthHallway", "The North Hallway"
        "SouthHallway", "The South Hallway"
        "EastHallway", "The East Hallway"
        "WestHallway", "The West Hallway"
        ]

module CommonRoom =
    let roomOptionsList = [
        "LivingRoom", "The living room"
        "WhiteParlor", "A parlor. The walls are covered in a white decorative wallpaper"
        "GreenParlor", "A parlor. The walls are covered in a green decorative wallpaper"
        "Library", "A large library full of books"
        "DiningRoom", "A dining room"
        "DiningHall", "A large room for eating. The ceiling is very tall and decorated with paintings."
        "GreatHall", "A large hall"
        "GameRoom", "A hangout spot for playing games."
        "Cinema", "A room for watching movies. The back wall is lined with comfy chairs."
        "MusicRoom", "A room full of musical instruments. There is a small concert space in the center"
        "LargeKitchen", "A large kitchen with stations for several cooks"
        "SmallKitchen", "A quaint kitchen"

    ]

module EntranceWay =
    let roomOptionsList = [
        "NorthFoyer", "The north entrance to the house"
        "SouthFoyer", "The south entrance to the house"
        "EastFoyer", "The east entrance to the house"
        "WestFoyer", "The west entrance to the house"
        "GrandFoyer", "A large entrance to the house. The walls are lined with regal paintings of the family's ancestors."

    ]

module PrivateRoom =  //Family's individual rooms always have a bathroom attached.
    let roomOptionsList = [
        "ParentsRoom", "The parent's room"
        "SonsRoom", "The son's room"
        "DaughtersRoom", "The daughter's room"
        "Tower", "A secluded tower"
        "FathersStudy", "The father's study"
        "Observatory", "Windows all around give a great view of the forest and mountains nearby"
        "WineCellar", "A dark room underground for aging wine"
        "GuestBedroom", "A simple bedroom for anyone staying at the house"
        "GrandGuestBedroom", "A large bedroom for guests. There is a large bearskin rug in the center."
    ]

module Closet =
    let roomOptionsList = [
        "ParentsCloset", "The parent's closet"
        "SonsCloset", "The son's closet"
        "DaughtersCloset", "The daughter's closet"
        "ServiceCloset", "A closet for the servants"
        "SmallCloset", "A small closet for keeping odds and ends"
        "Pantry", "A small room for storing food"

    ]

module MissionRoom = // Spawn intel here.
    let roomOptionsList = [
        "TechLab", "A large room full of high tech machinery, gadgets, and computers"
        "Cellar", "A large cellar. Smuggled goods and people are stored here."
        "OperationsRoom", "The fathers room for planning illegal operations"
        "ControlRoom", "A room for controlling the house's security systems"
    ]
    
// Use to prioritize Consumable items in certain rooms.
let foodRooms = [
    "Pantry"; "LargeKitchen"; "WineCellar"; "SmallKitchen"; "DiningRoom"; "DiningHall"
    ]

/// Generate all of the information for each room. Calls for the creation of all people and items to be put in a room.
let spawnRoomNameDesc (rng:Random) (spawnRoomType:SpawnRoomType) =

    let roomChoice =
        match spawnRoomType with
        | Spawn -> Spawn.roomOptionsList
        | Patio -> Patio.roomOptionsList
        | Garden -> Garden.roomOptionsList
        | Garage -> Garage.roomOptionsList
        | Storage -> Storage.roomOptionsList
        // Inside
        | Bathroom -> Bathroom.roomOptionsList
        | Stairs -> Stairs.roomOptionsList
        | Hallway -> Hallway.roomOptionsList
        | CommonRoom -> CommonRoom.roomOptionsList
        | EntranceWay -> EntranceWay.roomOptionsList
        | PrivateRoom -> PrivateRoom.roomOptionsList
        | Closet _ -> Closet.roomOptionsList
        | MissionRoom -> MissionRoom.roomOptionsList
    
    let (name,desc) = roomChoice |> List.randomChoice rng

    ((name |> String.toLower),desc), spawnRoomType

/// Create the room using the randomly chosen name and description plus randomly generated people and items by roomType.
let getRoomFromRoomData (rng:Random) roomInfo =
        let ((name,desc),roomType) = roomInfo
        let people = PeopleData.spawnPeople roomType rng
        let info = Info.initInfo name desc
        let prioritizeFood = foodRooms |> List.contains name
        let items = ItemData.spawnItems roomType rng prioritizeFood  // Each person does not get their own items.

        let room = Room.initRoom people info items roomType
        (room, initMap name [] []), roomType