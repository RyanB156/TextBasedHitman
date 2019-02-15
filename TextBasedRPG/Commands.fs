module Commands
open DomainTypes
open CommandTypes
open DomainFunctions
open FileIO


let commandList = [
    "amuse", "amuse <person> - tell a joke to lift a person's spirits, it may backfire"
    "apply", "apply <poisonName> to <itemName> - poison a weapon or consumable item"
    "approach", "approach <person> - get in close quarters to a person, giving them a chance to react. Required for melee attacks"
    "attack", "attack <person> - attack a person with the equipped weapon"
    "capture", "capture <person> - capture a terrified person to get an extra life. Only \"Fearful\" people can be made terrified."
    "cheerup", "cheerup <person> - increase a person's happiness"
    "chokeout", "chokeout <person> - render a person unconscious"
    "command", "command <person> <pickup/goto/attack/stop> <target> - command an ai to take an action"
    "compliment", "complement <person> - give a complement to increase happiness and attraction"
    "consume", "consume <item> - eat or drink an item to regain health"
    "describe", "describe <area/item/person> <_/itemName/personName> - display the description for <area/item/person>"
    "disguise", "disguise <person> - disguise yourself a worker by taking their clothes"
    "dishearten", "dishearten <person> - decrease a person's happiness"
    "drop", "drop <item> - drop the specified item"
    "escape", "escape <item> - make your escape after completing all of the objectives"
    "equip", "equip <item> - make a weapon ready to use"
    "followme", "followme <person> - ask a person to follow you. Only works if they trust you"
    "give", "give <item> to <person> - give an item to a person"
    "goto", "goto <room> - move to the specified room if possible"
    "forcegoto", "forcegoto <room> - force your way into a room. Ignores locked doors but alerts the guards in the next room."
    "help", "help <command> - display information on the command <arg> or lists commands if <arg> is empty"
    "inquire", "inquire <personStat/personInfo/items> person"
    "inspect", "inspect <clue> - reveal information about a clue"
    "intimidate", "intimidate <person> - Intimidate a person to make them afraid of you. Lower there resistance to your influence."
    "leaveme", "leaveme <person> - Causes a person to stop following you"
    "peek", "peek <room> - reveal items and people in an adjacent room"
    "pickup", "pickup <item> - add item in the area to inventory"
    "place", "place <item> in <item> - place an item into another item if possible"
    "punch", "punch <person> - hit a person with your fists"
    "quit", "quit <> - exit the game"
    "romance", "romance <person> - romance a person and generate a new life. Other person must have full attraction."
    "save", "save <> - save the game"
    "scout", "scout <room> - investigate a location that the current room overlooks"
    "search", "search <area> - reveal items and people in a room"
    "seduce", "seduce <person> - increase attraction by a lot. hance to fail based on morality."
    "survey", "survey <> - reveal buildings in an area"
    "talk", "talk <person> - have a conversation with a person. gives a piece of information and raises their trust."
    "takefrom", "takefrom <person/container> <item> - take an item from a person"
    "unequip", "unequip <item> - hide a weapon"
    "unlock", "unlock <door> - unlock a door if you have the right key"
    "view", "view <items/time/stats personName/my stats/my companion/objectives/visited rooms> - display <inventory/time/stats for a person>"
    "wait", "wait - Do nothing and allow the ai to take a turn"
    ]

let aiCommandList = ["pickup"; "attack"; "goto"; "stop"; "killyourself"]

/// Creates a list of the elements in a list that match a given pattern in ascending order.
let getSuggestions matchStrings pattern =
    [for elem in matchStrings do yield (Seq.checkStrDiff pattern elem),elem]
    |> List.sortWith (fun x y -> if x > y then -1 elif x < y then 1 else 0)

let getCmdSuggestion str minVal =
    let someOrNone (score,cmd) =
        if score < minVal then None else Some (cmd) // Input must match at least 2 characters to bring up a suggestion.
    getSuggestions (commandList |> List.map (fst)) str
    |> List.head
    |> someOrNone

let printSuggestion str =
    match getCmdSuggestion str 2 with
    | None -> ""
    | Some s -> sprintf "Did you mean %s?" (s.ToUpper())


/// Tell a joke to lift a person's spirits.
let amuse personName env =
    match env |> Environment.tryFindPersonByNameLower personName with
    | None -> Failure.personFindFailure personName
    | Some person when person.State <> Dead ->
        let adjStr, adj =
            match env.Rng.Next(0, 4) with
            | 0 -> "lowered", (Down 4)
            | _ -> "raised", (Up 2)
        if env.Rng.NextDouble() < person.Responsiveness then
            printfn "You %s %s's spirits" adjStr (person.Info.Name)
            match person.Personality.Mood |> Personality.adjustMood adj with
            | Failure f -> Failure (Failure.printPersonalityAdjFailureStr person f)
            | Success newMood ->
                let newPerson = person |> Person.setMood newMood |> Person.trySetAwareness Aware
                let newEnvironment = env |> Environment.updatePerson newPerson
                Success (newEnvironment, AIMove)
        else printfn "%s did not respond" person.Info.Name; Success (env, AIMove)
    | Some person -> Failure (sprintf "%s is dead" person.Info.Name)

/// Poison a weapon or consumable item.
let apply poisonName targetName env =
    let tryPoisonItem item =
        match item with
            | Consumable (false,a,healthBonus,uses,info) -> Success (Consumable (true,a,healthBonus,uses,info))
            | Weapon (MWeapon weapon,info) when not weapon.IsPoisoned -> Success (Weapon (MWeapon {weapon with IsPoisoned = true},info))
            | Consumable (true,a,healthBonus,uses,info) -> Failure (sprintf "%s is already poisoned" info.Name)
            | _ -> Failure (sprintf "%s is not a poisonable item" (Item.getName item))
    match env.Player |> Player.tryFindItemByName poisonName with
    | None -> Failure.inventoryItemFindFailure poisonName
    | Some (Poison info) ->
        match env.Player |> Player.tryFindItemByName targetName with
        | None -> Failure.roomItemFindFailure targetName env.Room.Info.Name
        | Some item ->
            match tryPoisonItem item with
            | Failure f -> Failure f
            | Success newItem ->
                printfn "You applied poison %s to %s" info.Name (Item.getName item)
                let newPlayer = 
                    env.Player
                    |> Player.removeFromInventory (Poison info)
                    |> Player.updateInventory item newItem
                    |> (fun p -> match env.Player.EquippedItem with 
                        | Some i when i |> Item.getName |> String.toLower |> (=) targetName -> {p with EquippedItem = Some newItem}
                        | _ -> p)
                let newEnvironment = env |> Environment.updatePlayer newPlayer
                Success (newEnvironment, AIMove)
    | _ -> Failure (sprintf "%s is not a poison. It cannot be applied to an item" poisonName)

/// Get in close quarters to a person for melee attacks.
let approach personName env =
    match env |> Environment.tryFindPersonByNameLower personName with
    | None -> Failure.personFindFailure personName
    | Some person ->
        printfn "You approached %s" personName
        let newPlayer = {env.Player with CloseTarget = Some (person.Info.Name)}
        Success (env |> Environment.updatePlayer newPlayer, AIMove)

/// Attack the specified person with the equipped weapon.
let attack personName env =
    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person ->
        match env.Player.EquippedItem with
        | None -> Failure "You do not have a weapon equipped"
        | Some (Weapon (wType, wInfo)) ->
            match wType with
            | MWeapon mWeapon ->
                match env.Player.CloseTarget with
                | None -> Failure "No targets are in range for melee attacks"
                | Some cTarget when cTarget |> String.toLower = personName ->
                    match person |> Person.applyAttack mWeapon.Damage env.Rng mWeapon.KOChance mWeapon.IsPoisoned with
                    | Failure f -> Failure f
                    | Success newPerson ->
                        printfn "You melee attacked %s with %s for %d damage" cTarget wInfo.Name mWeapon.Damage
                        printfn "%s:\nHealth: %d, State: %s, Awareness: %s" personName newPerson.Health (newPerson |> Person.getStateAsString) (newPerson.Awareness |> Person.getAwarenessAsString)
                        let newPlayer = env.Player |> Player.updateCloseTarget newPerson
                        let newEnvironment = env |> Environment.updatePerson newPerson |> Environment.updatePlayer newPlayer |> Environment.applyBadActionToAll
                        let personIsDead = if newPerson.State = Dead then printfn "%s is dead" (person |> Person.getName); true else false
                        let newEnvironment = 
                            if personIsDead then newEnvironment |> Environment.checkPersonObjectives newPerson // Check if the dead person fills a mission objective.
                            else newEnvironment
                        let aiCall = match mWeapon.Visibility with
                                     | VLow -> AIMove
                                     | VMedium -> AIAlert TPlayer
                                     | VHigh -> AIAlertAll TPlayer
                        Success (newEnvironment, aiCall)
                | _ -> Failure (sprintf "%s is not in range for melee attacks" personName)
                
            | RWeapon rWeapon ->
                if rWeapon.AmmoCount = 0 then Failure (sprintf "%s is out of ammo" wInfo.Name) else
                match person |> Person.applyAttack rWeapon.Damage env.Rng None false with
                | Failure f -> Failure f
                | Success newPerson ->
                    printfn "You shot %s with %s for %d damage" personName wInfo.Name rWeapon.Damage
                    printfn "%s:\nHealth: %d, State: %s, Awareness: %A" personName newPerson.Health (newPerson |> Person.getStateAsString) newPerson.Awareness
                    let newWeapon = Weapon (RWeapon {rWeapon with AmmoCount = rWeapon.AmmoCount - 1}, wInfo) // New weapon with 1 less round.
                    let newPlayer = env.Player |> Player.updateInventory (Weapon (wType, wInfo)) newWeapon  // New player with updated weapon info for the ammo count.
                    let newEnvironment = 
                        env |> Environment.updatePerson newPerson |> Environment.updatePlayer {newPlayer with EquippedItem = Some newWeapon}
                        |> Environment.applyBadActionToAll
                    let personIsDead = if newPerson.State = Dead then printfn "%s is dead" (person |> Person.getName); true else false
                    let newEnvironment = 
                        if personIsDead then newEnvironment |> Environment.checkPersonObjectives newPerson // Check if the dead person fills a mission objective.
                        else newEnvironment
                    let aiCall = match rWeapon.Visibility with
                                 | VLow -> AIMove
                                 | VMedium -> AIAlert TPlayer
                                 | VHigh -> AIAlertAll TPlayer
                    Success (newEnvironment, aiCall)
        | Some item -> Failure (sprintf "%s cannot be used as a weapon" (item |> Item.getNameWithType))

/// Recruit a scared person to gain an extra life.
let capture personName env =
    let checkCompanion =
        match env.Player.CompanionName with
        | Some pName when pName = personName -> {env.Player with CompanionName = None}
        | _ -> env.Player

    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person ->
        match person |> Person.getFear |> fst with
        | Terrified ->
            printfn "You captured %s for an extra life" personName
            let newRoom = {env.Room with People = env.Room.People |> List.removeOne person}
            let newPlayer = checkCompanion
            let newEnvironment = 
                {env with Room = newRoom; ExtraLives = (person |> Person.getRespawnData)::env.ExtraLives}
                |> Environment.updatePlayer newPlayer 
                |> Environment.applyBadActionToAll    // Nearby people might not like this.

            Success (newEnvironment, AIMove)
        | _ -> Failure (sprintf "%s is not afraid enough to be captured and brainwashed into an assassin" personName)

/// Increase a person's happiness
let cheerup personName env =
    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person when person.State <> Dead ->
        if env.Rng.NextDouble() < person.Responsiveness then
            match person.Personality.Mood |> Personality.adjustMood (Up 2) with
            | Failure f -> Failure (Failure.printPersonalityAdjFailureStr person f)
            | Success newMood ->
                printfn "You lifted %s's spirits" (person.Info.Name)
                let newPerson = person |> Person.setMood newMood |> Person.trySetAwareness Aware
                let newEnvironment = env |> Environment.updatePerson newPerson
                Success (newEnvironment, AIMove)
        else printfn "%s did not respond" person.Info.Name; Success (env, AIMove)
    | Some person -> Failure (sprintf "%s is dead" person.Info.Name)


/// Chokeout a person to render them unconscious.
let chokeOut personName env =
    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person ->
        match person.State with
        | Unconscious -> Failure (sprintf "%s is already unconscious" person.Info.Name)
        | Dead -> Failure (sprintf "%s is dead" person.Info.Name)
        | _ ->
            match person.Awareness with
            | Unaware | _ when env.Rng.NextDouble() < Person.awareKnockoutChance ->
                printfn "You knocked %s unconscious" (person |> Person.getName)
                let newPerson = {person with State = Unconscious} |> Person.trySetAwareness Aware
                let newEnvironment = env |> Environment.updatePerson newPerson
                let ret = if env.Rng.NextDouble() < 0.25 then AIAlert TPlayer else AIMove
                Success (newEnvironment, ret)
            
            | _ ->
                printfn "%s resisted your attempts to knock %s unconscious" person.Info.Name (person |> Person.getGenderObjectiveString)
                Success (env, AIMove)

/// Command an ai to take an action. Sets the ai's "Action" for them to carry out.
let command personName command env =
    // Abstract the logic for updating the person based on the command into a function.
    let aiActionFunc (person:Person) = 
        match command with
        | AIAttack targetName ->
            match env |> Environment.tryFindPersonByName targetName with
            | None -> Failure (sprintf "%s is not a valid person for %s to attack" targetName person.Info.Name)
            | _ -> Success (Person.setAwareness (Hostile <| TPerson targetName) >> Person.setAction AAttack >> Person.setIsCommanded true)
        | AIGoto roomName ->
            let (_,adjRooms,_) = env.Map
            match adjRooms |> List.map fst |> List.tryFind ((=) roomName) with
            | None -> Failure (sprintf "%s is not a valid location for %s to move to" roomName person.Info.Name)
            | _ -> Success (Person.setAction (AGoto roomName) >> Person.setIsCommanded true)
        | AIPickup itemName ->
            match env.Room |> Room.tryFindItemByName itemName with
            | None -> Failure (sprintf "%s is not a valid item for %s to pick up" itemName person.Info.Name)
            | _ -> Success (Person.setAction (APickupItem itemName) >> Person.setIsCommanded true)
        | AIStop -> Success (Person.setAction (ANeutralAction) >> Person.setIsCommanded false >> Person.setAwareness Aware)  // Have an ai stop their action. Keeps them from killing people.
        | AISuicide -> 
            if person.Personality.Mood |> fst = Depressed then 
                printfn "%s has lost the will to live" person.Info.Name
                Success (Person.setAction (ASuicide) >> Person.setIsCommanded true)
            else Failure (sprintf "%s is not sad enough to kill themselves" person.Info.Name)
    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person when person |> Person.isCompliant && person.State <> Dead ->  // Check to see if the person will listen to your commands.
        match aiActionFunc person with // Get Result<Person Update Function> based on the command passed into "Commands.command".
        | Failure f -> Failure f
        | Success func -> 
            let newPerson = person |> func |> Person.trySetAwareness Aware
            let newEnvironment = env |> Environment.updatePerson newPerson
            Success (newEnvironment, AIMove)
    | Some person -> printfn "%s will not take orders from you" person.Info.Name; Success (env, AIMove)

/// Give a compliment to increase happiness and attraction.
let compliment personName env =
    let tryRaiseAttraction person =
        match person |> Person.getCompatability env.Player.Gender with
        | false -> 
            printfn "%s does not swing your way" (person.Info.Name)
            person
        | true ->
            match person.Personality.Attraction |> Personality.adjustAttraction (Up 2) with
            | Failure f -> Failure.printPersonalityAdjFailure person f; person
            | Success newAttraction ->
                printfn "You increased %s's attraction towards you" (person.Info.Name)
                person |> Person.setAttraction newAttraction
    let tryRaiseMood person =
        match person.Personality.Mood |> Personality.adjustMood (Up 2) with
        | Failure f -> Failure.printPersonalityAdjFailure person f; person
        | Success newMood ->
            printfn "You lifted %s's spirits" (person.Info.Name)
            person |> Person.setMood newMood

    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person when person.State <> Dead ->
        if env.Rng.NextDouble() < person.Responsiveness then
            let newPerson = person |> tryRaiseAttraction |> tryRaiseMood |> Person.trySetAwareness Aware
            let newEnvironment = env |> Environment.updatePerson newPerson
            Success (newEnvironment, AIMove)
        else printfn "%s did not respond to your compliment" person.Info.Name; Success (env, AIMove)
    | Some person -> Failure (sprintf "%s is dead" person.Info.Name)
        
/// Eat or drink an item if it is a consumable.
let consume itemName env =
    match env.Player |> Player.tryFindConsumableByName itemName with
    | None -> Failure.inventoryItemFindFailure itemName
    | Some item ->
        match item with
        | Consumable (false, a, healthBonus, uses, info) ->
            printfn "You consumed %s for %d health" (item |> Item.getName) healthBonus
            let newPlayer = 
                {env.Player with Health = env.Player.Health + healthBonus}
                |> (if uses-1 > 0 then Player.updateInventory item (Consumable (false, a, healthBonus, uses-1, info)) // Check if item is used up.
                   else printfn "%s was used up" (item |> Item.getName); Player.removeFromInventory item)
            let newEnvironment = env |> Environment.updatePlayer newPlayer
            Success (newEnvironment, AIMove)
        | Consumable (true, _, _, _, _) ->
            Failure (sprintf "Item %s is poisoned. You cannot consume it." (item |> Item.getName))
        | _ -> Failure (sprintf "Item %s is not consumable" (item |> Item.getName))

/// Display the description for items in the player's inventory or in the room.
let describe describeArg env =
    let getDescribeString =
        match describeArg with
        | DescribeArea -> Success env.Room.Info.Description
        | DescribeItem itemName ->
            match Player.tryFindItemByName itemName env.Player with
            | Some playerItem -> Success (Item.getDescription playerItem)
            | None ->
                match Room.tryFindItemByName itemName env.Room with
                | Some roomItem -> Success (Item.getDescription roomItem)
                | None -> Failure (sprintf "%s is not a valid item to get a description of" itemName)
        | DescribePerson personName ->
            match Room.tryFindPersonByName personName env.Room with
            | Some person -> Success (Person.getDescription person)
            | None -> Failure (sprintf "%s is not an describeable person" personName)
    match getDescribeString with
    | Failure f -> Failure f
    | Success str -> printfn "Description: %s" str; Success (env, AIWait)
    

/// Disguise yourself as a worker.
let disguise personName env =
    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person ->
        match person.State with
        | NormalS -> Failure (sprintf "%s is not in a condition where you can take %s clothes" (person |> Person.getName) (person |> Person.getPossessiveGenderString))
        | _ ->
            match Person.getJobClothes person with
            | Failure f -> Failure f
            | Success jobType ->
                printfn "You are now disguised as a %A" jobType
                let newPlayer = {env.Player with Disguise = Some jobType}
                let newEnvironment = env |> Environment.updatePlayer newPlayer
                Success (newEnvironment, AIMove)

/// Decrease a person's happiness
let dishearten personName env =
    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person when person.State <> Dead ->
        match person.Personality.Mood |> Personality.adjustMood (Down 2) with
        | Failure f -> Failure (sprintf "%s's " (person.Info.Name) + f)
        | Success newMood when env.Rng.NextDouble() < person.Responsiveness ->
            printfn "You lowered %s's spirits" (person.Info.Name)
            let newPersonality = {person.Personality with Mood = newMood}
            let newPerson = {person with Personality = newPersonality} |> Person.trySetAwareness Aware
            let newEnvironment = env |> Environment.updatePerson newPerson |> Environment.applyBadActionToAll // Nearby people might not like that.
            Success (newEnvironment, AIMove)
        | _ -> printfn "%s's mood did not change" person.Info.Name; Success (env, AIMove)
    | Some person -> Failure (sprintf "%s is dead" person.Info.Name)

/// Drop the specified item.
let drop itemName env =
    let player = env.Player
    match Player.tryFindItemByName itemName player with
    | None -> Failure.inventoryItemFindFailure itemName
    | Some item -> 
        printfn "You dropped %s " itemName
        let newPlayer = 
            {player with Items = player.Items |> List.removeOne item}
            |> Player.removeEquippedItemCheck item
        let newEnvironment = 
            env
            |> Environment.updateItems (List.add item)
            |> Environment.updatePlayer newPlayer
        Success (newEnvironment, AIMove)

/// Make your escape after completing the objectives.
let escape itemName env =
    match env |> Environment.tryFindItemByName itemName with
    | None -> Failure.roomItemFindFailure itemName env.Room.Info.Name
    | Some item ->
        match item with
        | EscapeItem info ->
            let totalObjs = env.Objectives |> List.length
            let finishedObjs = env.Objectives |> List.filter Objective.isCompleted |> List.length
            if totalObjs = finishedObjs then
                printfn "You have completed all objectives and escaped using %s" (item |> Item.getName)
                Success ({env with GameStatus = Win}, AIWait)
            else
                printf "You have completed only %d/%d objectives. Are you sure you want to leave (y/n)? : " finishedObjs totalObjs
                if ["Y"; "y"] |> List.contains (System.Console.ReadLine()) then Success ({env with GameStatus = PartialWin}, AIWait)
                else Success (env, AIMove)
        | _ -> Failure (sprintf "%s is not an escape route" (item |> Item.getName))

/// Equip the specified weapon.
let equip itemName env =
    match env.Player |> Player.tryFindItemByName itemName with
    | None -> Failure.inventoryItemFindFailure itemName
    | Some (Weapon (weapon,info)) ->
        printfn "You equipped %s" itemName
        let newPlayer = {env.Player with EquippedItem = Some (Weapon (weapon,info))}
        Success ({env with Player = newPlayer}, AIMove)
    | _ -> Failure (sprintf "%s is not a weapon" itemName)

/// Ask a person to follow you.
let followMe personName env =
    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person when person.Type <> Guard -> 
        match person |> Person.queryTrust with
        | x when x < 8 -> Failure (sprintf "%s does not trust you enough to follow you" personName)
        | _ -> 
            printfn "%s is your new Companion" person.Info.Name
            let newPlayer = env.Player |> Player.updateCompanion (Some (person.Info.Name |> String.toLower))
            let newPerson = person |> Person.trySetAwareness Aware
            let newEnvironment = env |> Environment.updatePlayer newPlayer |> Environment.updatePerson newPerson
            Success (newEnvironment, AIMove)
    | _ -> Failure (sprintf "%s will not be your companion" personName)

/// Give an item to a person.
let give itemName personName env =
    // Check if the person is already holding a weapon or food item. If so, cannot give them another one.
    let tryGiveItem person item =
        match item with
        | Consumable _ ->   // Giving a person a consumable item will increase their trust regardless of their trust level.
            if not person.IsHoldingFood then 
                printfn "You increased %s's trust in you" person.Info.Name
                {person with IsHoldingFood = true; Items = item::person.Items} |> Person.addTrust (Up 4)
                |> Person.setAction AUseFood, true
                
            else printfn "%s is already holding a consumable item" person.Info.Name; person, false
        | Weapon _ ->
            if not person.IsHoldingWeapon then {person with IsHoldingWeapon = true; Items = item::person.Items}, true
            else printfn "%s is already holding a weapon" person.Info.Name; person, false
        | _ -> {person with Items = item::person.Items}, true

    match env.Player |> Player.tryFindItemByName itemName with
    | None -> Failure.inventoryItemFindFailure itemName
    | Some item ->
        match env.Room |> Room.tryFindPersonByName personName with
        | None -> Failure.personFindFailure personName
        | Some person when Person.queryTrust person > 2 ->

            let (newPerson, newPlayer) =
                match tryGiveItem person item with
                | newPerson, true -> 
                    printfn "You gave %s to %s" itemName personName
                    newPerson, ({env.Player with Items = env.Player.Items |> List.removeOne item} |> Player.removeEquippedItemCheck item)
                | newPerson, false -> newPerson, env.Player

            let newEnvironment = 
                env
                |> Environment.updatePerson (newPerson |> Person.trySetAwareness Aware)
                |> Environment.updatePlayer newPlayer
            Success (newEnvironment, AIMove)
        | _ -> 
            printfn "%s does not trust you enough to accept item %s.\n%s has alerted the guards" personName itemName personName
            Success (env, AIAlert TPlayer)


/// Move to the specified location, if possible.
let goto arg force env = 

    // Process moving into the room with a companion and alerting any guards if the area is restricted.
    let travelToRoom loadedRoom roomName map alertGuards =
        let rec takeCompanion tempEnv = 
            match tempEnv.Player.CompanionName with
            | None ->
                RoomIO.writeToFile (tempEnv.Room, env.Map)  // Save old room information to a file.
                printfn "Moved to %s" roomName; 
                // Apply status updates if the player has been in this room already and then returns.
                {tempEnv with Room = loadedRoom; Map = map}
            | Some companionName ->                                    // Player's companion follows them to the next room.
                match tempEnv |> Environment.tryFindPersonByName companionName with
                | None -> failwithf "Error finding companion %s in the room. Commands.goto.travelToRoom" companionName
                | Some companion when companion.State = NormalS ->
                    printfn "Moved to %s with %s" roomName companionName
                    let newPrevRoom = {tempEnv.Room with People = tempEnv.Room.People |> List.removeOne companion} // Take the companion out of the current room,
                    RoomIO.writeToFile (newPrevRoom, tempEnv.Map)                                                 
                         
                    let newLoadedRoom = {loadedRoom with People = companion::loadedRoom.People} // and add it to the loaded room. 
                    {tempEnv with Room = newLoadedRoom; Map = map}  // Return updated environment.
                | _ -> 
                    let newPlayer = {tempEnv.Player with CompanionName = None}
                    let newEnvironment = tempEnv |> Environment.updatePlayer newPlayer
                    takeCompanion newEnvironment
        // Remove stored status people upon entering the new room.
        let removeOldStatusPeople env =                             // Person not updated when returning to the room. Dead from poison -> still alive...
            {env with UpdatePeople = env.UpdatePeople 
                    |> List.filter (fun p -> loadedRoom.People |> List.map Person.getName |> List.contains (p.Info.Name) |> not)}
        // Add new stored status people from the old room (env.Room)
        let addNewStatusPeople env =                                // This one works though...
            {env with UpdatePeople = env.UpdatePeople @ (env.Room.People |> List.filter Person.hasStatusEffect)}
        // Add status effected people to env.UpdatePeople from env.Room.People. Replace people in newRoom.People with people in env.UpdatePeople.
        let applyUpdateStatus (env:Environment) = 
            {env with Room = env.UpdatePeople |> List.fold (fun room p -> room |> Room.updatePerson p) env.Room}

        let newEnvironment = env |> addNewStatusPeople |> takeCompanion |> applyUpdateStatus |> removeOldStatusPeople |> Environment.addVisited roomName

        if alertGuards || force then Success (newEnvironment, AIAlert TPlayer)
        else Success (newEnvironment, AIMove)

    let (_, adjacentRooms, _) = env.Map
    match arg with
    | Empty -> 
        adjacentRooms |> List.filter (fun (n,l) -> match l with | Secret -> false | _ -> true) 
            |> List.sortBy (fun (n,l) -> match l with | Unlocked -> 0 | _ -> 10)
            |> List.iter (fun (name, lockState) -> printfn "%s - %A" name lockState)
        Success (env, AIWait) // Viewing nearby rooms does not trigger the ai.
    | Arg roomName -> 
        if roomName = (env.Room |> Room.getName |> String.toLower) then Failure ("You are already there") else
        let roomOpt = adjacentRooms |> List.tryFind (fst >> String.toLower >> (=) roomName) // Gets the CamelCase version of the name by searching the adjacent rooms list.

        let successF =
            match RoomIO.readFromFile roomName with // Load new room information from a file using name from List.tryFind.
            | Failure s -> Failure s // Internal matching error.
            | Success (loadedRoom, map) ->      // New room loaded from file.
                match loadedRoom.People |> List.tryFind (Person.getAwareness >> (=) Warn) with
                | Some guard when env.Player.Disguise.IsSome |> not -> 
                    printfn "%s: You are not allowed to be here" guard.Info.Name // Guards can patrol certain restricted areas. They will become hostile on entering.
                    printf "Are you sure? : "
                    let input = System.Console.ReadLine()
                    if List.contains (input.ToLower()) ["yes"; "y"] then
                        travelToRoom loadedRoom roomName map true
                    else
                        Failure (sprintf "Did not move. %s is being guarded by %s" roomName guard.Info.Name)
                | _ -> travelToRoom loadedRoom roomName map false
        match roomOpt with
        | None -> Failure (sprintf "%s is not a nearby location" roomName) // Not listed in nearby locations.
        | Some (_,Unlocked) | Some (_,Secret) -> successF   // Is nearby and accessible.
        | Some (_,(Locked code)) when force -> 
            // Forcing the room open, ignore locked doors, alerts guards in next room.
            printfn "You broke into %s. The guards have been alerted." roomName; successF 
            
        | Some (_,Locked code) -> Failure (sprintf "%s is locked. %A key required" roomName code) // Is locked.

/// Force your way into a room. Uses "goto arg (true) env".
let forceGoto arg env = goto arg true env
        
/// Display list of possible commands.
let help arg env =
    match arg with
    | Empty -> commandList |> List.iter (snd >> printfn "%s"); Success (env, AIWait)
    | Arg c ->
        match List.tryFind (fst >> (=) c) commandList with
        | None -> Failure (sprintf "The command %s is not listed under HELP\n" c + printSuggestion c)
        | Some (name,info) -> printfn "%s" info; Success (env, AIWait)

/// Get information from/about nearby people.
let inquire question personName env =
    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person when person.State <> Dead -> 
        if Person.queryTrust person <= 2 then Failure (sprintf "%s does not trust you enough to disclose their %s" personName question) else
        match Person.stringToDataString question person with
        | Failure f -> Failure f
        | Success response -> 
            printfn "Response: \n%s" response
            let newPerson = person |> Person.trySetAwareness Aware
            let newEnvironment = env |> Environment.updatePerson newPerson
            Success (newEnvironment, AIMove)
    | Some person -> Failure (sprintf "%s is dead" person.Info.Name)

/// Reveal information about clues.
let inspect itemName env =
    let findItem () =
        match env.Player |> Player.tryFindItemByName itemName with
        | Some item -> Some item
        | None ->
            match env.Room |> Room.tryFindItemByName itemName with
            | Some item -> Some item
            | None -> None
    match findItem () with
    | None -> Failure.roomItemFindFailure itemName env.Room.Info.Name
    | Some (Clue (info, clueInfo)) ->
        printfn "Clue - %s:\n%s" itemName clueInfo
        Success (env, AIWait)
    | Some (HiddenPassageway (info, roomNames)) -> 
        printfn "Hidden Passageway revealed: \n"; roomNames |> List.iter (printfn "%s")
        let newEnvironment = env |> Environment.revealPassageways
        Success (newEnvironment, AIWait)
    | _ -> Failure (sprintf "%s is not a clue" itemName)

/// Intimidate a person to make them afraid of you. Lower there resistance to your influence.
let intimidate personName env =
    let getFearIncrease person =
        match person.Personality.Bravery with
        | Fearful -> 3, true
        | NeutralB -> 1, true
        | Brave -> 0, false
    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person when person.State <> Dead ->
        match getFearIncrease person with
        | _, false -> printfn "%s will not be intimidated by you" person.Info.Name; Success (env, AIMove)
        | x, true ->
            match person.Personality.Fear |> Personality.adjustFear (Up x) with
            | Failure f -> Failure f
            | Success newFear ->
                if env.Rng.Next(0, x+1) = 0 then    // Random chance for the person to attack based on their bravery stat.
                    
                    printfn "The guards have been alerted"
                    let (newPlayer,damage) = env.Player |> Player.applyAngryAttack env.Rng
                    printfn "%s resisted your attempts to intimidate %s. %s attacked you for %d damage" 
                        person.Info.Name (person |> Person.getGenderObjectiveString) (person |> Person.getGenderPronounString) damage
                    let newEnvironment = env |> Environment.updatePlayer newPlayer |> Environment.applyBadActionToAll
                    Success (newEnvironment, AIAlert TPlayer)   // Alert guards on a failed "intimidate" command.
                else
                    let name = person.Info.Name
                    printfn "You increased %s's fear of you. %s is now %s" name name (newFear |> fst |> Personality.getFearAsString)
                    let newPerson = person |> Person.setFear newFear |> Person.trySetAwareness Aware
                    let newEnvironment = env |> Environment.updatePerson newPerson |> Environment.applyBadActionToAll
                    Success (newEnvironment, AIMove)
    | Some person -> Failure (sprintf "%s is dead" person.Info.Name)

/// Tell a person to stop following you.
let leaveMe env =
    match env.Player.CompanionName with
    | None -> Failure ("You do not have a companion")
    | Some companionName ->
        printfn "You have left your companion"
        let newPlayer = {env.Player with CompanionName = None}
        let newEnvironment = env |> Environment.updatePlayer newPlayer
        Success (newEnvironment, AIMove)
        

/// View people and items in the next room.
let peek arg env =
    let _, adjRooms, _ = env.Map
    let adjRoomNames = adjRooms |> List.map (fst >> String.toLower)
    if List.contains arg adjRoomNames then
        match RoomIO.readFromFile arg with
        | Failure f -> Failure f
        | Success (adjRoom, _) ->
            printfn "Items:"
            adjRoom.Items |> List.iter (Item.getNameWithType >> printfn "%s")
            printfn "People:"
            adjRoom.People |> List.iter (Person.getFullInfoStr >> printfn "%s")
            Success (env, AIMove)
    else Failure (sprintf "%s is not an adjacent area" arg)


/// Add item from the environment to inventory.
let pickup itemName env =
    let intelCheck item env =
        match item with
        | Intel _ -> Environment.checkItemObjectives item env
        | _ -> env
    match Room.tryFindItemByName itemName env.Room with
    | None -> Failure.roomItemFindFailure itemName env.Room.Info.Name
    | Some item -> 
        if item |> Item.isHeavy then Failure (sprintf "The item %s is too heavy to pick up" itemName) else
        printfn "You picked up %s" itemName
        let player = env.Player
        let newPlayer = {player with Items = item::player.Items}
        let newEnvironment = 
            env
            |> Environment.updateItems (List.removeOne item)
            |> Environment.updatePlayer newPlayer
            |> intelCheck item
        Success (newEnvironment, AIMove)

/// Place an item in a container.
let place itemName targetName env =
    match Room.tryFindItemByName targetName env.Room with
    | None -> Failure.roomItemFindFailure itemName env.Room.Info.Name
    | Some (Container (items,info)) -> 
        match Player.tryFindItemByName itemName env.Player with
        | None -> Failure.inventoryItemFindFailure itemName
        | Some playerItem -> 
            printfn "You put %s in %s" itemName targetName
            let newPlayer =
                {env.Player with Items = env.Player.Items |> List.removeOne playerItem}
                |> Player.removeEquippedItemCheck playerItem
            let newContainer = Container (playerItem::items, info)
            let newEnvironment =
                env
                |> Environment.updateItems (List.replaceByWith (Item.getName >> String.toLower >> (=) targetName) newContainer)
                |> Environment.updatePlayer newPlayer
            Success (newEnvironment, AIMove)
    | Some _ -> Failure (sprintf "The item %s cannot store any items" targetName)

/// Hit a person with your fists. Does not require ammo. Never misses. Small amount of damage. Must be close to the person.
let punch personName env =
    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person ->
        match env.Player.CloseTarget with
        | Some cTarget when cTarget |> String.toLower = personName ->
            let attackDamage = 10
            match person |> Person.applyAttack attackDamage env.Rng (Some 8) false with
            | Failure f -> Failure f
            | Success newPerson ->
                printfn "You punched %s for %d damage" cTarget attackDamage
                printfn "%s:\nHealth: %d, State: %s, Awareness: %s" personName newPerson.Health (newPerson |> Person.getStateAsString) (newPerson.Awareness |> Person.getAwarenessAsString)
                
                let newPlayer = env.Player |> Player.updateCloseTarget newPerson
                let newEnvironment = env |> Environment.updatePerson newPerson |> Environment.updatePlayer newPlayer
                let personIsDead = if newPerson.State = Dead then printfn "%s is dead" (person |> Person.getName); true else false
                let newEnvironment =
                    match personIsDead with
                    | true -> newEnvironment |> Environment.checkPersonObjectives newPerson // Check if the dead person fills a mission objective.
                    | false -> newEnvironment
                Success (newEnvironment |> Environment.applyBadActionToAll , AIMove)
        | _ -> Failure (sprintf "%s is not in range for melee attacks" personName)

/// Exit the game.
let quit env = Success ({env with GameStatus = Exit}, AIWait)

/// Romance a person and generate a new life.
let romance personName env =

    let getNewLife =    // Get a randomly selected name and gender from the WorldGeneration file PeopleData.fs
        let name = PeopleData.names |> List.randomChoice env.Rng
        let gender = PeopleData.getGenderByChance env.Rng
        name, gender
    let getResultStr genderA genderB =
        if genderA = genderB then "adopted" else "birthed"
    let gCheck person env =
        let newLife = getNewLife
        if Person.getCompatability env.Player.Gender person then printfn "New life added: %A" newLife; env |> Environment.addLife newLife
        else env

    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person ->
        if person.CreatedNewLife then Failure (sprintf "You have already created a new life with %s" person.Info.Name) else
        if env.Room.People 
            |> List.filter (fun p -> match p.State with | NormalS -> true | _ -> false) 
            |> List.length > 1 then Failure ("There are too many people around to do that") else
        match person |> Person.getAttraction |> fst with
        | Love ->
            let newLife = getNewLife
            printfn "You and %s %s a new person" person.Info.Name (getResultStr env.Player.Gender person.Gender)
            printfn "New life added: %A" newLife
            let newPerson = person |> Person.setCreatedNewLife true
            let newEnvironment = env |> Environment.addLife newLife |> Environment.updatePerson newPerson
            Success (newEnvironment, AIMove)
        | _ ->
            match person.State with
            | Dead -> Failure (sprintf "You perve, %s is dead!" person.Info.Name)
            | NormalS ->
                printf "%s does not like you enough for romance. Are you sure? (y/n): " person.Info.Name
                match System.Console.ReadLine() with    // Give the player the option to commit rape or not.
                | "y" | "Y" ->
                    let (newPlayer, damage) = env.Player |> Player.applyAngryAttack env.Rng
                    printfn "%s did not take kindly to that. %s attacked you for %d damage" person.Info.Name (person |> Person.getGenderPronounString) damage
                    let newPerson = person |> Person.setAttraction (Hate, 0) |> Person.setCreatedNewLife true
                    let newEnvironment = 
                        env |> Environment.updatePlayer newPlayer 
                        |> Environment.updatePerson newPerson |> gCheck person
                        |> Environment.applyBadActionToAll 
                    Success (newEnvironment, AIAlert TPlayer)
                | _ -> Success (env, AIMove)
            | _ ->  // The person is asleep/unconscious/drunk so they do not resist. Affects nearby people though.
                let newPerson = person |> Person.setCreatedNewLife true
                let newEnvironment = env |> Environment.updatePerson newPerson |> gCheck person |> Environment.applyBadActionToAll 
                Success (newEnvironment, AIMove)
            

/// Save the game.
let save env = 
    printfn "You saved the game"
    EnvironmentIO.writeToFile env
    RoomIO.writeToFile (env.Room, env.Map)
    Success ({env with GameStatus = Continue}, AIWait)

/// Scout a distant area.
let scout roomName env =
    let (_,_,oRoomsOpt) = env.Map
    match oRoomsOpt with
    | None -> Failure "The current location does not have any scoutable locations"
    | Some oRooms ->
        match oRooms |> List.tryFind (String.toLower >> (=) roomName) with
        | None -> Failure (sprintf "%s is not a scoutable location" roomName)
        | Some roomStr ->
            match RoomIO.readFromFile roomStr with
            | Failure f -> Failure f
            | Success (room,_) ->
                printfn "Scout:"
                room.People |> List.iter (Person.getName >> printfn "%s")
                Success (env, AIWait)


/// Search for items and people in a location.
let search arg env = 
    match arg with
    | SearchArea -> 
        printfn "Items:"
        env.Room.Items |> List.iter (Item.getNameWithType >> printfn "%s")
        printfn "People:"
        env.Room.People |> List.sortBy (fun p -> match p.State with | NormalS -> 0 | _ -> 10) |> List.iter (Person.getFullInfoStr >> printfn "%s")
        Success (env, AIWait)
    | SearchItem itemName ->
        match Room.tryFindItemByName itemName env.Room with
        | None -> Failure.roomItemFindFailure itemName env.Room.Info.Name
        | Some s -> 
            match Item.getItems s with
            | None -> Failure (sprintf "The item %s does not contain any items" itemName)
            | Some [] -> printfn "%s is empty" itemName; Success (env, AIWait)
            | Some items -> 
                printfn "%s Items:" itemName
                items |> List.iter (Item.getNameWithType >> printfn "%s")
                Success (env, AIWait)

/// Seduce a person. Large increase to attraction. Chance for failure.
let seduce personName env =
    let trySetFullAttraction person =
        match person |> Person.getCompatability env.Player.Gender with
        | false -> printfn "%s does not swing your way" (person.Info.Name); person
        | true ->
            printfn "%s accepted your advances" person.Info.Name
            if Person.queryAttraction person = 10 then printfn "%s's attraction is already maxed out" person.Info.Name; person
            else printfn "You maxed out %s's attraction and trust to you" person.Info.Name; 
                    person |> Person.setAttraction (Love, 10) |> Person.setTrust (FullTrust, 10)

    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person ->
        match person |> Person.getMoralityBasedChance env.Rng with
        | false ->
            printfn "%s did not take kindly to your advances. You lost all of %s attraction and trust." person.Info.Name (Person.getPossessiveGenderString person)
            let (newPlayer, damage) = env.Player |> Player.applyAngryAttack env.Rng
            printfn "%s attacked you for %d damage" person.Info.Name damage
            let newPerson = person |> Person.setAttraction (Hate, 0) |> Person.setTrust (Mistrust, 0)
            let newEnvironment = env |> Environment.updatePerson newPerson |> Environment.updatePlayer newPlayer
            Success (newEnvironment, AIMove)
        | true ->
            let newPerson = person |> trySetFullAttraction |> Person.trySetAwareness Aware
            let newEnvironment = env |> Environment.updatePerson newPerson
            Success (newEnvironment, AIMove)

/// View buildings in the area and nearby locations by checking the World Map
let survey env = 
    let (currentRoom, adjacentRooms, overlookRooms) = env.Map
    printfn "Current Room: %s" currentRoom
    printfn "Adjacent Rooms:"
    adjacentRooms |> List.filter (fun (name, lockState) -> lockState <> Secret) |> List.iter (AdjacentRoom.getRoomStateStr >> printfn "%s")
    overlookRooms |> function Some rs -> printfn "Overlook Rooms:"; rs |> List.iter (printfn "%s") | None -> ()
    Success (env, AIWait)


/// Take an item from a person or container.
let takeFrom targetName itemName env =
    let tryTakeSpecialItem item person = // 1 food, and 1 weapon are special cases for the person. Change the "IsHolding_" flags accordingly.
        match item with
        | Consumable _ -> {person with IsHoldingFood = false}
        | Weapon _ -> {person with IsHoldingWeapon = false}
        | _ -> person
    match env.Room |> Room.tryFindPersonByName targetName with
    | Some person ->
        let takeItem item =
            let newPerson = {person with Items = person.Items |> List.removeOne item} |> tryTakeSpecialItem item
            let newPlayer = {env.Player with Items = item::env.Player.Items}
            printfn "You took item %s from %s" (item |> Item.getName) (person.Info.Name)
            env |> Environment.updatePerson newPerson |> Environment.updatePlayer newPlayer

        match person |> Person.tryFindItemByName itemName with
        | None -> Failure (sprintf "%s does not have the item %s" targetName itemName)
        | Some item when person.State <> NormalS -> Success (takeItem item, AIMove)
        | Some item when person.Type = Guard -> Failure ("You cannot take items from guards when they are conscious")
        | Some item when Person.queryTrust person > 2 -> Success (takeItem item, AIMove)
        | Some item -> Failure (sprintf "%s does not trust you enough to give you %s" targetName itemName)
    | None -> 
        match Room.tryFindItemByName targetName env.Room with
        | None -> Failure (sprintf "%s is not a valid person or container to take items from" targetName)
        | Some (Container (items,info)) ->
            match items |> List.tryFind (Item.getName >> String.toLower >> (=) itemName) with
            | None -> Failure (sprintf "The item %s does not contain %s" targetName itemName)
            | Some containerItem -> 
                printfn "You took %s from %s" itemName targetName
                let newPlayer = {env.Player with Items = containerItem::env.Player.Items} // Add item to player inventory.
                let newContainer = Container (items |> List.removeOne containerItem, info) // Remove item from targetItem's inventory.
                let newEnvironment =
                    env
                    |> Environment.updateItems (List.replaceByWith (Item.getName >> String.toLower >> (=) targetName) newContainer)
                    |> Environment.updatePlayer newPlayer
                Success (newEnvironment, AIMove)
        | Some _ -> Failure (sprintf "The item %s cannot store any items" targetName)

/// Talk to a person to get information and increase trust.
let talk personName env =
    match env |> Environment.tryFindPersonByName personName with
    | None -> Failure.personFindFailure personName
    | Some person ->
        if person |> Person.queryTrust <= 2 then
            printfn "%s does not trust you enough to talk to you" person.Info.Name
            Success (env, AIMove)
        else
            match Person.stringToDataString Person.inquireInfo.[env.Rng.Next(0, (Person.inquireInfo |> List.length) - 1)] person with
            | Failure f -> failwith f
            | Success infoString ->
                printfn "%s gave you some information:" person.Info.Name
                printfn "%s" infoString
                if env.Rng.NextDouble() < person.Responsiveness then    // Each person has a parameter to limit the effects of "talking" with them.
                    match person.Personality.Trust |> Personality.adjustTrust (Up 1) with
                    | Failure f -> Failure (sprintf "%s" (person.Info.Name) + "'s " + f)
                    | Success newTrust ->
                        printfn "You increased %s's trust in you" person.Info.Name
                        let newPerson = person |> Person.setTrust newTrust |> Person.trySetAwareness Aware
                        let newEnvironment = env |> Environment.updatePerson newPerson
                        Success (newEnvironment, AIMove)
                else Success (env, AIMove)

/// Teleport to a room. Will throw an exception if the room is not found in a file. Only used for debugging purposes.
let teleport roomName env =
    let result = RoomIO.readFromFile roomName
    match result with
    | Failure f -> failwithf "%s" f
    | Success (room,roomMap) ->
        RoomIO.writeToFile (env.Room, env.Map)
        let newEnvironment = {env with Room = room; Map = roomMap} |> Environment.addVisited roomName
        Success (newEnvironment, AIWait)


/// Unequip a weapon and hides it from other people.
let unequip env =
    match env.Player.EquippedItem with
    | None -> Failure ("You do not have a weapon equipped")
    | Some item ->
        printfn "You unequipped %s" (item |> Item.getName)
        let newPlayer = {env.Player with EquippedItem = None}
        Success ({env with Player = newPlayer}, AIMove)
    

/// Unlock an adjacent room if the player has the right key.
let unlock roomName env =
    let (cRoom,adjRooms,oRooms) = env.Map
    match adjRooms |> List.tryFind (fst >> String.toLower >> (=) roomName) with
    | None -> Failure.roomFindFailure roomName
    | Some (name,lockState) ->
        match lockState with
        | Unlocked | Secret -> Failure (sprintf "The door to %s is not locked" roomName)
        | Locked code ->
            let changeLockState roomName roomInfo : RoomInfo =
                let (room,map) = roomInfo
                let (cRoom,adjRooms,oRooms) = map
                let newAdjRoom = (roomName, Unlocked)
                let newAdjRooms = adjRooms |> List.replaceByWith (fst >> String.toLower >> (=) roomName) newAdjRoom
                room, (cRoom,newAdjRooms,oRooms)

            // Unlock the target room for all rooms attached to the target room.
            match RoomIO.readFromFile roomName with     // Worked first try!!!
            | Failure f -> printfn "%s" f
            | Success (loadedRoom,loadedRoomMap) ->
                let (cRoom,adjRooms,oRooms) = loadedRoomMap
                let otherRoomNames = adjRooms |> List.map fst |> List.filter ((<>) env.Room.Info.Name)
                otherRoomNames |> List.iter (fun n ->   // Loop through all other rooms connected to the target room and change the lock state.
                    match RoomIO.readFromFile n with
                    | Failure f -> printfn "%s" f
                    | Success rInfo ->
                        let newRInfo = changeLockState roomName rInfo
                        RoomIO.writeToFile newRInfo)

            if env.Player.Items |> List.containsBy (function | Key (keyCode,_) when code = keyCode -> true | _ -> false) then
                printfn "You unlocked %s" roomName
                let (_,map) = changeLockState roomName (env.Room,env.Map)
                Success ({env with Map = map}, AIMove)
            else
                Failure (sprintf "The door could not be unlocked. You do not have a %O key" code)

/// View player's status and inventory.
let view arg env =
    match arg with
    | Inventory -> printfn "Items:"; env.Player.Items |> List.map Item.getNameWithType |> List.iter (printfn "%s"); Success (env, AIWait)
    | Time -> printfn "Time: %s" (env.Time |> Time.asString); Success (env, AIWait)
    | PersonStats personName ->
        match env |> Environment.tryFindPersonByNameLower personName with
        | None -> Failure (sprintf "%s is not a valid person in this room" personName)
        | Some person ->
            person |> Person.printStats
            Success (env, AIWait)
    | PlayerStats ->
        env.Player |> Player.printStats
        Success (env, AIWait)
    | CompanionName ->
        match env.Player.CompanionName with
        | None -> Failure ("You do not have a companion")
        | Some companionName -> printfn "Companion: %s" companionName; Success (env, AIWait)
    | Objectives ->
        env.Objectives |> List.map Objective.toString |> List.iter (printfn "%s"); Success (env, AIWait)
    | VisitedRooms ->
        env.VisitedRooms |> List.iter (printfn "%s"); Success (env, AIWait)
            

/// Wait and allow the ai to take a turn.
let wait env =
    printfn "You let the AI take a turn"
    Success (env, AIMove)