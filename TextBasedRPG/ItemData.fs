module ItemData

open System

open DomainTypes
open DomainFunctions

module MeleeWeapon =
    let f = Item.initMeleeWeapon
    let itemOptionsList = [
        
        f 30 None VMedium "Knife" "A knife"
        f 50 (Some 2) VMedium "Shovel" "A typical garden shovel"
        f 15 None VLow "ScrewDriver" "A well worn screwdriver"
        f 50 (Some 3) VHigh "MetalPipe" "A heavy piece of pipe"
        f 10 None VLow "Pen" "A calligraphy pen"
        f 40 None VMedium "Cleaver" "A large butcher knife"
        f 5 (Some 10) VLow "Mop" "An old mop"
        f 25 None VMedium "HedgeTrimmers" "A set of hedge trimmers. They look sharp"
        f 15 None VLow "Pencil" "A well sharpened writing utensil"
        f 60 None VMedium "Katana" "A traditional Japanese sword"
        f 45 (Some 4) VMedium "Claymore" "A large two handed sword"
        f 10 (Some 15) VMedium "TVRemote" "An everyday TV remote"

        ]
    let getItem (rng:Random) = itemOptionsList |> List.randomChoice rng

module RangedWeapon =
    let f = Item.initRangedWeapon
    let itemOptionsList = [
        
        f 100 VHigh 2 "Shotgun" "A trusty double barrelled shotgun"
        f 100 VMedium 1 "HarpoonGun" "A projectile weapon used for underwater fishing"
        f 15 VLow 20 "NailGun" "A seemingly harmless tool. In the right hands it can be a deadly weapon."
        f 30 VMedium 8 "1911" "An American classic"
        f 60 VHigh 30 "M4" "A standard issue military carbine"
        f 35 VMedium 30 "M4Suppressed" "The M4 you know and love, with a suppressor attached"
        f 30 VMedium 15 "P226" "A special forces pistol"
        f 20 VLow 15 "P226Suppressed" "The same pistol, but quieter"

        ]
    let getItem (rng:Random) = itemOptionsList |> List.randomChoice rng

module Key =
    
    // type DoorCode = Blue | Red | Green | White | Black
    let blueKey = Item.initKey Blue "BlueKey" "A BlueKey"
    let redKey = Item.initKey Red "RedKey" "A RedKey"
    let greenKey = Item.initKey Green "GreenKey" "A GreenKey"
    let whiteKey = Item.initKey White "WhiteKey" "A WhiteKey"
    let blackKey = Item.initKey Black "BlackKey" "A BlackKey"

    let getItem roomType = 
        let keyOpt =
            match roomType with
            | Spawn | Storage -> Some blueKey
            | Patio | Garden | Garage -> Some redKey
            | CommonRoom -> Some greenKey
            | EntranceWay -> Some whiteKey
            | PrivateRoom -> Some blackKey
            | Bathroom | Stairs | Hallway | Closet _ | MissionRoom -> None
        match keyOpt with
        | None -> failwithf "Failure generating key for RoomType: %A. %A cannot have keys" roomType roomType
        | Some key -> key

module Clue = // Create all rooms with people and items, find all targets and intel items, then create clues in the proper rooms with objective information.
    let itemOptionsList = 
        let f = Item.initClue
        [

        f "SecretDocument" "A shady piece of paper" ""
        f "Memo" "An internal communication from the family" ""
        f "Letter" "A letter from one of the family's contacts" ""
        
        ]

    let getItem (rng:Random) = itemOptionsList |> List.randomChoice rng

module HiddenPassageway =
    let f = Item.initHiddenPassageway
    let itemOptionsList = [
        
        f "Bookcase" "A dusty bookcase. One of the books has some fingerprints on it. Interesting" []
        f "Panel" "An access panel on the wall" []
        f "Vent" "An airconditioning vent" []

        ]

    let getItem (rng:Random) = itemOptionsList |> List.randomChoice rng

module Consumable =
    let f = Item.initConsumable

    let itemOptionsList = [

        f "Pizza" "A supreme pizza with all of the standard toppings" false 20 4
        f "Burrito" "A spicy burrito" false 15 5
        f "Apple" "A juicy apple" false 10 4
        f "MtDew" "A crisp cool beverage with all of the sugar and caffeine an assassin can handle" false 10 2
        f "Pepsi" "A refreshing soda" false 10 2
        f "Coke" "An American classic" false 10 2
        f "Goldy" "A brilliant goldfish. It looks tasty, if you're into that sort of thing." false 20 1
        f "Grapes" "A handful of grapes picked from the grape vines in the North Patio." false 5 6
        f "Cake" "A lemon cake. It looks delicious" false 15 4
        f "Wine" "A bottle of red wine. It looks expensive" true 10 6
        f "Cake" "A lemon cake. It looks delicious" false 15 4
        f "Wine" "A bottle of red wine. It looks expensive" true 10 6
        f "Vodka" "A high quality Russian liquor" true 25 3
        f "Rum" "A rich Caribbean booze" true 20 4
        f "Whiskey" "A strong American brew" true 30 3
        f "Water" "It's water" false 10 4
        f "Special" "A special mix for the father" true 100 2

        ]

    let getItem (rng:Random) = itemOptionsList |> List.randomChoice rng

module Display =
    
    let f = Item.initDisplay

    let itemOptionsList = [

        f "Lillies" "A small display of flowers on the patio table. They must have come from the Mother's garden."
        f "FishPicture" "A family fishing picture. The father and son are struggling to hold a huge marlin for the picture."
        f "AfricaPicture" "A picture of the father with a robed man in the desert"
        f "JunglePicture" "A picture of the father with a rich looking man in a jungle somewhere"
        f "SkiMagazine" "A magazine about skiing in the Swiss Alps. A skiier carving a corner is featured on the cover"
        f "CallOfDestiny" "There is a futuristic man on the cover with a gun. This must be what the kids are playing nowadays."
        f "ZStation5" "The latest game console"
        f "FatherPortrait" "A regal portrait of the father. He is in some sort of military dress uniform"
        f "MotherPortrait" "A portrait of the mother. She's in a large ball gown"
        f "SonPortrait" "A portrait of the son. He is in a nice tuxedo"
        f "DaughterPortrait" "She is a spitting image of her mother"
        f "AfricanHistory" "A large book with a map of Africa on the cover"

        ]
    let getItem (rng:Random) = itemOptionsList |> List.randomChoice rng

// Large items to be placed outside.
module LargeDisplay =
    let f = Item.initLargeDisplay

    let itemOptionsList = [
        f "JetSki" "A blue jet ski"
        f "HorseHedge" "A decorative hedge hand crafted to look like a horse"
        f "ElephantHedge" "A decorative hedge hand crafted to look like an elephant"
        f "DolphinHedge" "A decorative hedge hand crafted to look like a dolphin"
        f "Pool" "A fancy inground pool with a rock display on one side"
        f "FishingBoat" "A fancy fishing boat"
        ]

    let getItem (rng:Random) = itemOptionsList |> List.randomChoice rng

module Escape =
    let f = Item.initEscape

    let itemOptionsList = [
        f "Horse" "A large thoroughbred"
        f "Bugatti" "An expensive car"
        f "Tesla" "A sleek electric car"

        ]

    let getItem (rng:Random) = itemOptionsList |> List.randomChoice rng

// Furniture items for inside.
module Furniture =
    let f = Item.initFurniture
    let itemOptionsList = [
        f "Couch" "A large leather couch"
        f "TV" "A large flatscreen TV. Some sort of shooter game is on the screen."
        f "Bar" "A modern looking bar. It is very clean."
        f "LoveSeat" "A small couch"
        f "Stool" "A small stool"
        f "WoodenStool" "A wooden stool"
        f "BarSeat" "A plain seat for sitting at a bar"

        ]

    let getItem (rng:Random) = itemOptionsList |> List.randomChoice rng

module Intel =
    
    let f = Item.initIntel

    let itemOptionsList = [

        f "DrugManifest" "A listing of all of the father's drug shipments"
        f "PersonManifest" "A record of the father's human trafficking shipments"
        f "CocaineResidue" "Trace powder on the table. It seems like someone was doing lines recently"
        ]

    let getItem (rng:Random) = itemOptionsList |> List.randomChoice rng

module Poison =

    let f = Item.initPoison

    let itemOptionsList = [
        
        f "RattlesnakeVenom" "Potent venom from a local viper"
        f "Venom" "An unknown concoction"
        ]

    let getItem (rng:Random) = itemOptionsList |> List.randomChoice rng

module Container =
    let containerItemOptions = 
        let weights = ["MWeapon", 2; "RWeapon", 2; "Consumable", 4; "Poison", 1]
        [for i in weights do yield! SpawnRoom.transformWeights i]

    let capacity = 2

    let itemOptionsList = [

        "Chest", "A storage chest"
        "OrnateShelf", "An ornate shelf"
        "PlainCabinet", "A plain cabinet"
        "KoiPond", "A koi pond surrounded by decorative stones"
        "TrashCan", "A place for people to dispose of their garbage"
        "ToolBox", "A place for storing tools"
        "FancyCabinet", "A fancy cabinet"
        "CoffeeTable", "A table for setting drinks on"
        "Counter", "A counter for holding various items"
        "WoodTable", "A wooden table"
        ]

    let stringToItem (rng:Random) str =
        match str with
        | "MWeapon" -> MeleeWeapon.getItem rng
        | "RWeapon" -> RangedWeapon.getItem rng
        | "Consumable" -> Consumable.getItem rng
        | "Poison" -> Poison.getItem rng
        | s -> failwithf "%s unmatchable as stringToItem" s

    let getItem (rng:Random) = 
        let items = [1..capacity] |> List.map (fun _ -> containerItemOptions |> List.randomChoice rng |> stringToItem rng)
        
        let (name,desc) = itemOptionsList |> List.randomChoice rng
        Item.initContainer name desc items


let stringClassToItem roomType (rng:Random) str =
    match str with
    | "MWeapon" -> MeleeWeapon.getItem rng
    | "RWeapon" -> RangedWeapon.getItem rng
    | "Key" -> Key.getItem roomType
    | "Clue" -> Clue.getItem rng
    | "HiddenP" -> HiddenPassageway.getItem rng // -- This should never be matched here. This item created later.
    | "Consumable" -> Consumable.getItem rng
    | "Container" -> Container.getItem rng
    | "Display" -> Display.getItem rng
    | "LargeDisplay" -> LargeDisplay.getItem rng
    | "Escape" -> Escape.getItem rng
    | "Furniture" -> Furniture.getItem rng
    | "Intel" -> Intel.getItem rng
    | "Poison" -> Poison.getItem rng
    | s -> failwithf "%s unmatchable as stringToItem" s

let defaultItems roomType =
    match roomType with
    | Spawn -> ["MWeapon"; "Escape"; "Consumable"]
    | Patio -> ["LargeDisplay"]
    | Garden -> ["LargeDisplay"]
    | Garage -> ["Escape"]
    | Storage -> ["MWeapon"; "Consumable"]
    | Bathroom -> []
    | Stairs -> []
    | Hallway -> []
    | CommonRoom -> ["Furniture"; "Consumable"; "Consumable"]
    | EntranceWay -> []
    | PrivateRoom -> ["Consumable"]
    | Closet _ -> []
    | MissionRoom -> []

let itemTypeChanceByRoomType roomType = 
    // | Bathroom | Stairs | Hallway | Closet _ | MissionRoom -> will raise an exception if you try to put a key in them, so don't.
    let itemWeightsByRoom =
        match roomType with
        | Spawn ->          ["MWeapon", 1; "RWeapon", 2; "Key", 5; "Consumable", 2; "Container", 1; "Display", 1; "Poison", 1];
        | Patio ->          ["MWeapon", 1; "Key", 1; "Consumable", 2; "Container", 1; "Display", 1; "LargeDisplay", 2];
        | Garden ->         ["MWeapon", 1; "Key", 1; "Container", 1; "Furniture", 0; "Display", 1; "LargeDisplay", 2; "Poison", 2];
        | Garage ->         ["RWeapon", 1; "Key", 1; "Escape", 2; "Container", 1; "Furniture", 1; "Display", 1; "LargeDisplay", 1];
        | Storage ->        ["Key", 1; "Consumable", 1; "Container", 1; "Furniture", 1; "Display", 1]
        | Bathroom ->       ["MWeapon", 1; "Container", 1; "Furniture", 1; "Display", 2];
        | Stairs ->         ["Display", 3];
        | Hallway ->        ["RWeapon", 1; "Consumable", 1; "Container", 1; "Display", 1];
        | CommonRoom ->     ["MWeapon", 1; "Key", 1;  "Consumable", 2; "Container", 1; "Furniture", 2; "Display", 1];
        | EntranceWay ->    ["Key", 1; "Furniture", 1; "Display", 1]
        | PrivateRoom ->    ["MWeapon", 1; "RWeapon", 1; "Key", 2; "Furniture", 3; "Display", 1; "Intel", 1];
        | Closet _ ->       ["RWeapon", 1; "Consumable", 1; "Container", 1; "Display", 1];
        | MissionRoom ->    ["MWeapon", 1; "RWeapon", 2; "Consumable", 1; "Container", 1; "Furniture", 1; "Display", 1; "Intel", 3; "Poison", 1];

    [ for r in itemWeightsByRoom do yield! SpawnRoom.transformWeights r]

let clueRooms = [Spawn; Garage; Bathroom; Stairs; Hallway; CommonRoom; PrivateRoom; Closet true; Closet false] 

let getItemCountByRoom roomType (rng:Random) =
    let itemBaseCount = 2
    itemBaseCount + 
        match roomType with
        | Spawn -> rng.Next(4, 6) // 4-5 items.
        | Patio -> rng.Next(4, 7) // 4-6 items.
        | Garden -> rng.Next(2, 6) // 2-5 items.
        | Garage -> rng.Next(1, 3) // 1-2 items.
        | Storage -> rng.Next(5, 8) // 5-7 items.
        // Inside
        | Bathroom -> rng.Next(0, 2) // 0-1 items.
        | Stairs -> 0
        | Hallway -> rng.Next(2, 4) // 2-3 items.
        | CommonRoom -> rng.Next(2, 5) // 2-4 items.
        | EntranceWay -> rng.Next(2, 5) // 2-4 items.
        | PrivateRoom -> rng.Next(2, 4) // 2-3 items.
        | Closet _ -> rng.Next(2, 5) // 2-4 items.
        | MissionRoom -> rng.Next(3, 5) // 3-4 items.

// Adds a hiddenpassageway to closets that are linked to mission rooms.
let tryAddHiddenPassageway roomType rng items =
    match roomType with
    | Closet true -> (HiddenPassageway.getItem rng)::items
    | _ -> items

// Force more food items to spawn in food related rooms.
let tryPrioritizeFood b (rng:Random) itemTypeNames =
    if b then
        let mapper = function
            | "Intel" -> "Intel"
            | s -> if rng.Next(0, 2) = 0 then "Consumable" else s
        itemTypeNames |> List.map mapper
    else itemTypeNames

let spawnItems roomType (rng:Random) prioritizeFood =
    let itemSpawnCount = getItemCountByRoom roomType rng
    [1..itemSpawnCount]
    |> List.map (fun _ -> itemTypeChanceByRoomType roomType |> List.randomChoice rng)   // Rewire this so that default items are added, unchanged to the list.
    |> fun items -> (defaultItems roomType) @ items
    |> tryPrioritizeFood prioritizeFood rng
    |> List.map (stringClassToItem roomType rng)
    |> tryAddHiddenPassageway roomType rng
    |> List.distinct