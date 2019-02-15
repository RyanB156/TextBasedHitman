module AI

open DomainTypes
open DomainFunctions

let attack ai target env = 
    let damage, newAI =
        if ai.IsHoldingWeapon then
            match ai.Items |> List.tryFind (function | Weapon _ -> true | _ -> false) with
            | None -> failwithf "AI holding weapon failure %s" ai.Info.Name
            | Some (Weapon (weaponType, info)) ->
                match weaponType with
                | MWeapon m -> m.Damage, ai
                | RWeapon r when r.AmmoCount-1 > 0 -> 
                    let newWeapon = RangedWeapon.fire r info
                    let newAI = ai |> Person.removeFromInventory (Weapon (weaponType, info)) |> Person.addToInventory newWeapon
                    r.Damage, newAI
                | RWeapon r -> r.Damage, {ai with Items = ai.Items |> List.removeOne (Weapon (weaponType, info)); IsHoldingWeapon = false} // All ammo used up.
            | _ -> printfn "Internal Error: ai attack, match failure for %s" (Person.getName ai); Person.defaultDamage, ai
        else Person.defaultDamage, ai

    let newEnvironment = env |> Environment.updatePerson {newAI with AttackDamage = damage}

    match target with
    | TPlayer -> 
        printfn "%s %s attacked you for %d damage" (Person.getTypeAsString newAI) (Person.getName newAI) damage
        let newAI = ai |> Person.setIsCommanded false
        let newPlayer = {newEnvironment.Player with Health = newEnvironment.Player.Health - damage}
        newEnvironment |> Environment.updatePlayer newPlayer |> Environment.updatePerson newAI
    | TPerson personName -> 
        match newEnvironment |> Environment.tryFindPersonByNameLower personName with
        | Some person -> 
            printfn "%s %s attacked %s for %d damage" (Person.getTypeAsString newAI) (Person.getName newAI) (Person.getName person) damage
            let newPerson = person |> Person.takeDamage damage env.Rng (Some 10) |> Person.attackResponse (TPerson personName)
            let newAI = (if newPerson.State = Dead then newAI |> Person.setAwareness Aware else newAI) |> Person.setIsCommanded false
            newEnvironment |> Environment.updatePerson newPerson |> Environment.updatePerson newAI |> Environment.checkPersonObjectives newAI
        | None -> newEnvironment
    | NoTarget -> printf "AI targeting error for %s" newAI.Info.Name; env

let useFood (ai:Person) env =
    match ai.Items |> List.tryFind (function | Consumable _ -> true | _ -> false) with
    | None -> failwithf "AI consume food error for %s" ai.Info.Name // IsHoldingFood and having a food item should always match up. Else -> break.
    | Some food ->
        match food with
        | Consumable (isPoisoned, isAlcohol, healthBonus, uses, info) ->
            printfn "%s consumed some of %s" ai.Info.Name (Item.getName food)
            let tryPoison person = if isPoisoned && person.IsPoisoned |> not then person |> Person.setIsPoisoned true else person
            // Allows giving multiple alcoholic drinks: Give then take, repeat; or wait for the person to consume the alcohol when they take damage.
            let tryGetDrunk person = if isAlcohol then person |> Person.makeDrunk else person
            let newAI = 
                {ai with Health = ai.Health + healthBonus}
                |> tryPoison |> tryGetDrunk
                |> (if uses-1 > 0 then Person.updateInventory food (Consumable (isPoisoned, isAlcohol, healthBonus, uses-1, info))
                    else printfn "%s was used up" (food |> Item.getName); Person.removeFromInventory food >> Person.setHoldingFood false)
                |> Person.setIsCommanded false
            env |> Environment.updatePerson newAI
        | _ -> failwithf "AI consume food error for %s" ai.Info.Name

let goto ai roomName env =
    match FileIO.RoomIO.readFromFile roomName with
    | Failure f -> failwithf "AI goto failure: %s" f
    | Success (loadedRoom, loadedRoomMap) ->
        let newAI = ai |> Person.setIsCommanded false |> Person.setAction ANeutralAction   // Make sure that the ai will not be forced to do the same command again.
        let newLoadedRoom = {loadedRoom with People = newAI::loadedRoom.People}
        FileIO.RoomIO.writeToFile (newLoadedRoom, loadedRoomMap)
        printfn "%s moved to %s" ai.Info.Name roomName
        let newPrevRoom = {env.Room with People = env.Room.People |> List.removeOne ai}
        let newPlayer =
            match env.Player.CompanionName with
            | Some cName when cName |> String.toLower |> (=) (ai.Info.Name |> String.toLower) -> {env.Player with CompanionName = None}
            | _ -> env.Player
        env |> Environment.updateRoom newPrevRoom |> Environment.updatePlayer newPlayer

let tryWakeUp ai env =
        if env.Rng.NextDouble() < 0.10 then
            printfn "%s regained consciousness" (Person.getName ai)
            env |> Environment.updatePerson ({ai with State = NormalS})
        else env

let pickupItem ai itemName env =
    match env.Room |> Room.tryFindItemByName itemName with
    | None -> failwithf "Internal Error: \"pickupItem\" for %s. Cannot find item %s" (Person.getName ai) itemName
    | Some item ->
        let newAI =
            match item with
            | Consumable _ ->
                if not ai.IsHoldingFood then 
                    printfn "%s picked up %s" (Person.getName ai) itemName; {ai with IsHoldingFood = true; Items = item::ai.Items} |> Person.setIsCommanded false
                else printfn "%s is already holding a consumable item" ai.Info.Name; ai
            | Weapon _ ->
                if not ai.IsHoldingWeapon then 
                    printfn "%s picked up %s" (Person.getName ai) itemName; {ai with IsHoldingWeapon = true; Items = item::ai.Items} |> Person.setIsCommanded false
                else printfn "%s is already holding a weapon" ai.Info.Name; ai
            | _ -> {ai with Items = item::ai.Items} |> Person.setIsCommanded false
        env |> Environment.updatePerson newAI |> Environment.updateItems (List.removeOne item)

let commitSuicide (ai:Person) env =
    printfn "%s commited suicide" ai.Info.Name
    let newAI = {ai with Health = 0; State = Dead}
    env |> Environment.updatePerson newAI |> Environment.checkPersonObjectives newAI

let takeAction env ai =
    match ai.Action with
    | AAttack -> attack ai (match ai.Awareness with | Hostile target -> target | _ -> NoTarget) env
    | AUseFood -> useFood ai env
    | AGoto roomName -> goto ai roomName env
    | ATryWakeUp -> tryWakeUp ai env
    | APickupItem itemName -> pickupItem ai itemName env
    | ASuicide -> commitSuicide ai env
    | ANeutralAction -> env

let decideAction env person =
    match person.IsCommanded with // Commands given to people override their decision making.
    | false ->
        match person.Action with // May change this to stick to the same action after the last move...
        | AGoto roomName -> env, AGoto roomName
        | _ -> 
            match person.State with
            | Unconscious -> 
                // Remove the player's companion if they are incapacitated.
                match env.Player.CompanionName with
                | None -> env, ATryWakeUp
                | Some companionName ->
                    if companionName = (person.Info.Name |> String.toLower)
                        then {env with Player = env.Player |> Player.updateCompanion None}, ATryWakeUp else env, ATryWakeUp
            | Dead | Asleep ->  // Dead people are filtered out, but matched here for completeness; they can't do anything from here anyway.
                // Remove the player's companion if they are incapacitated.
                match env.Player.CompanionName with
                | None -> env, ANeutralAction
                | Some companionName ->
                    if companionName = (person.Info.Name |> String.toLower)
                        then {env with Player = env.Player |> Player.updateCompanion None}, ANeutralAction else env, ANeutralAction
            | _ when person.Health <= 50 ->
                if person.IsHoldingFood then env, AUseFood  // Player giving a person food commands them to use the food one time. Then the person can decide.
                else
                    match env.Room.Items |> List.tryFind (function | Consumable _ -> true | _ -> false) with
                    | Some food -> env, (APickupItem (food |> Item.getName |> String.toLower))
                    | None ->
                        match person.Awareness with
                        | Hostile _ -> env, AAttack
                        | Warn -> env, ANeutralAction
                        | _ -> env, ANeutralAction
            | _ ->
                match person.Awareness with
                | Hostile _ -> env, AAttack
                | Warn -> env, ANeutralAction
                | _ -> env, ANeutralAction
                    
    | true ->
        env, person.Action

let aiAction env callType =

    let aiMove env (person:Person) =
        match env |> Environment.tryFindPersonByNameLower person.Info.Name with  // Original use of aiAlert -> aiMove did not use the updated ai from aiAlert... Fix this later......
        | None -> failwithf "Error finding person %s in env inside AI.aiAction" person.Info.Name
        | Some person ->
            let (newEnvironment, action) = decideAction env person
            let (newPerson,objDone) = 
                {person with Action = action}
                |> Person.tryApplyPoisonDamage // apply possible poison damage to people in the room.
                |> (fun p -> match p.State with | Dead -> p, true | _ -> p, false)

            let newEnvironment = newEnvironment |> Environment.updatePerson newPerson |> (if objDone then Environment.checkPersonObjectives newPerson else id)
            takeAction newEnvironment newPerson

    let updateAwareness alertPerson person =
        match person.State with
        | NormalS -> 
            let newPerson =
                match person.Personality.Bravery with
                | _ when person.Type = Guard -> person |> Person.setAwareness (Hostile alertPerson)
                | Brave -> person |> Person.setAwareness (Hostile alertPerson)
                | Fearful -> person |> Person.setAwareness Afraid |> Person.addFear (Up 1)  // Increase fear when doing actions that alert the guards.
                | NeutralB -> person |> Person.setAwareness Aware
                        
            if person.Awareness <> newPerson.Awareness then printfn "%s is %s" (person.Info.Name) (newPerson.Awareness |> Person.getAwarenessAsString)        
            newPerson

        | _ -> person

    /// Alert people in the same room.
    let aiAlert alertPerson env person =
        let newPerson = updateAwareness alertPerson person
        match alertPerson with
        | TPlayer ->
            match env.Player.Disguise with
            | Some _ ->
                let newPlayer = {env.Player with Disguise = None}
                env |> Environment.updatePerson newPerson |> Environment.updatePlayer newPlayer
            | _ -> env |> Environment.updatePerson newPerson
        | _ -> env |> Environment.updatePerson newPerson
        
    
    /// Alert people in adjacent rooms.
    let aiAlertAdjacentRooms alertPerson env =
        let (_, adjRooms, _) = env.Map
        for roomName in adjRooms |> List.map fst do
            match FileIO.RoomIO.readFromFile roomName with  // Read all adjacent rooms from file and update them with alert people.
            | Failure f -> failwith f
            | Success (loadedRoom, loadedRoomMap) ->
                // Only display that adjacent room people have been updated if there are > 0 people that are not yet updated.
                if loadedRoom.People |> List.fold (fun acc p -> match p.Awareness with | Unaware | Warn -> (acc+1) | _ -> acc) 0 > 0 then
                    printfn "-%s:" loadedRoom.Info.Name
                let newLoadedRoom = loadedRoom |> Room.mapPeople (updateAwareness alertPerson)
                FileIO.RoomIO.writeToFile (newLoadedRoom, loadedRoomMap)

    
    match callType with
    | AIMove -> Success (env.Room.People |> List.filter (fun p -> p.State |> (<>) Dead) |> List.fold aiMove env)
    | AIAlert alertPerson -> 
        Success (env.Room.People 
        |> List.filter (fun p -> p.State |> (<>) Dead)  // First rendition of this used to have zombies (dead people would still move lol).
        |> List.fold (fun e person -> aiMove (aiAlert alertPerson e person) person) env // Combines alert update with allowing people to attack.
        )
    | AIAlertAll alertPerson ->
        let ret = 
            Success (env.Room.People
            |> List.filter (fun p -> p.State |> (<>) Dead)
            |> List.fold (fun e person -> aiMove (aiAlert alertPerson e person) person) env
            )
        aiAlertAdjacentRooms alertPerson env
        ret
    | _ -> Success env