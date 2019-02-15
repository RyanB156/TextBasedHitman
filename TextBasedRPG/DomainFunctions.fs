module DomainFunctions
open DomainTypes
open System

module List =
    /// Removes all of the specified element from a list.
    let inline removeAll element list =
        let rec inner acc list =
            match list with
            | [] -> acc
            | x::xs when x = element -> inner acc xs
            | x::xs -> inner (x::acc) xs
        inner [] list |> List.rev

    /// Removes one of the specified element from a list. Doesn't preserve order.
    let inline removeOne element list =
        let rec inner acc list foundOne =
            if foundOne then list @ acc else
            match list with
            | [] -> acc
            | x::xs when x = element -> inner acc xs true // Leaves out the element that is to be removed, then signal that the function is done with "true"
            | x::xs -> inner (x::acc) xs false
        inner [] list false |> List.rev

    /// Make a random selection from a list.
    let inline randomChoice (rng:Random) (list:'a list) =
        let inline makeChoice (rng:Random) (arr:'a []) =
            arr.[rng.Next(0, Array.length arr)]
        list |> List.toArray |> makeChoice rng

    /// Add the specified element to a list.
    let inline add element list =
        element::list

    /// Same as List.contains but with the list given first for piping elements through a list search.
    let inline containsBack list element = List.contains element list

    /// Tests if the specified element matches the first of any pair in a list.
    let inline containsFst element list =
        List.unzip list
        |> fst
        |> List.contains element

    /// Tests if the specified element matches the second of any pair in a list.
    let inline containsSnd element list =
        List.unzip list
        |> snd
        |> List.contains element

    /// Checks if an element exists in a list by applying the specified function to it.
    let inline containsBy f list =
        match list |> List.tryFind f with
            | Some _ -> true
            | None -> false

    /// For each element for which the given function returns true, replace that element with the specified item.
    let inline replaceByWith f item list =
        let rec inner acc list =
            match list with
            | [] -> acc
            | x::xs when f x -> inner (item::acc) xs
            | x::xs -> inner (x::acc) xs
        inner [] list

module Seq =
    /// Returns the number of matching characters in the specified strings.
    let checkStrDiff (str1:string) (str2:string) =
        let folder count x y =
            if x = y then count + 1 else count
        Seq.fold2 folder 0 str1 str2

    /// Creates a string from the elements of a sequence of strings separated by a newline.
    let seqStringBuilder (list:seq<string>) =
        let adder str1 str2 =
            str1 + "\n" + str2
        if list |> Seq.length = 0 then "" else
        list |> Seq.fold adder ""

    let toString (cs:char seq) : string =
        cs |> Seq.fold (fun s c -> s + (string c)) ""

module Tuple3 =
    let fst tuple3 = 
        let (a,_,_) = tuple3
        a

    let snd tuple3 =
        let (_,b,_) = tuple3
        b

    let trd tuple3 =
        let (_,_,c) = tuple3
        c

module String =
    let toLower (str:string) = str.ToLower()

    let toPascalCase (str:string) =
        match str |> toLower |> Seq.toList with
        | [] -> str
        | c::cs -> Char.ToUpper(c)::cs |> Seq.toString

    /// Returns an entry to be "printf" ed to the command line. None -> no newLine, Some s -> s with newLine.
    let printOption strOpt =
        match strOpt with
        | None -> ""
        | Some s -> sprintf "%s\n" s

module Failure =

    /// Error finding the specified person in the room.
    let personFindFailure personName =
        Failure (sprintf "%s is not a valid person in this location" personName)

    /// Error finding the specified item in the player's inventory.
    let inventoryItemFindFailure itemName =
        Failure (sprintf "The item %s is not in your inventory" itemName)

    /// Error finding the specified item or the specified item in the player's inventory is poisoned.
    let inventoryConsumableFindFailure itemName =
        Failure (sprintf "The item %s is not in your inventory or is poisoned" itemName)

    /// Error finding the specified item in the room.
    let roomItemFindFailure itemName roomName =
        Failure (sprintf "The item %s does not exist in %s" itemName roomName)

    /// Error finding the specified room in the room map.
    let roomFindFailure roomName =
        Failure (sprintf "%s is not a nearby location" roomName)

    /// Print failure based on trying to adjust a person's mood/trust/attraction.
    let printPersonalityAdjFailure (person:Person) failureMsg =
        printfn "%s" (person.Info.Name + "'s " + failureMsg)

    let printPersonalityAdjFailureStr (person:Person) failureMsg =
        sprintf "%s" (person.Info.Name + "'s " + failureMsg)

module Input =
    let rec inputMapLoop prompt (table:Map<string,'a>) =
        let options = table |> Map.toList |> List.map fst
        printf "("; options |> List.iter (printf "%s "); printfn ")"
        printf "%s" prompt
        match table.TryFind (Console.ReadLine()) with
        | None -> inputMapLoop prompt table
        | Some input -> input

module Time = 
    let splitTime time = time.Hour,time.Minute

    let addTime amount time = 
        let hour,minute = splitTime time
        let minute = minute + amount
        if minute > 59 then {Hour = hour + 1; Minute = minute - 60}
        else {Hour = hour; Minute = minute}

    let asString time =
        let hour,minute = splitTime time
        (sprintf "%d:%d" hour minute) + if minute % 10 = 0 then "0" else ""

module Info =
    let initInfo name description = {Name = name; Description = description}

module Item =
    
    let initMeleeWeapon damage kOChance visibility name description =
        {Damage = damage; Visibility = visibility; KOChance = kOChance; IsPoisoned = false} 
        |> MWeapon |> (fun wT -> Weapon (wT, Info.initInfo name description))

    let initRangedWeapon damage visibility ammoCount name description =
        {Damage = damage; Visibility = visibility; AmmoCount = ammoCount} 
        |> RWeapon |> (fun wT -> Weapon (wT, Info.initInfo name description))

    let initKey code name description =
        Key (code, Info.initInfo name description)

    let initClue name description clueInfo =
        Clue ((Info.initInfo name description), clueInfo)

    let initHiddenPassageway name description roomName =
        HiddenPassageway ((Info.initInfo name description), roomName)

    let initConsumable name description isAlcohol healthBonus uses =
        Consumable (false, isAlcohol, healthBonus, uses, Info.initInfo name description)

    let initContainer name description items =
        Container (items, Info.initInfo name description)

    let initDisplay name description =
        Display (Info.initInfo name description)

    let initEscape name description =
        EscapeItem (Info.initInfo name description)

    let initLargeDisplay name description =
        LargeDisplay (Info.initInfo name description)

    let initFurniture name description =
        Furniture (Info.initInfo name description)

    let initIntel name description =
        Intel (Info.initInfo name description)

    let initPoison name description = 
        Poison (Info.initInfo name description)
    
    let getInfo = function
    | Weapon (_, info) -> info
    | Key (_, info) -> info
    | Clue (info, clueInfo) -> info
    | Consumable (_, _, _, _, info) -> info
    | Container (_, info) -> info
    | Display info -> info
    | EscapeItem info -> info
    | LargeDisplay info -> info
    | Furniture info -> info
    | HiddenPassageway (info, roomName) -> info
    | Intel info -> info
    | Poison info -> info

    let isClue = function
        | Clue (_, _) -> true
        | _ -> false

    /// Get the name of items.
    let getName item = (getInfo item).Name

    /// Get the ammo count for an item, if applicable.
    let getAmmoCount = function
        | Weapon (wType, _) ->
            match wType with
            | RWeapon rWeapon -> Some rWeapon.AmmoCount
            | _ -> None
        | _ -> None

    /// Get the damage stat for a weapon.
    let getWeaponDamage = function
        | Weapon (weaponType, info) ->
            match weaponType with
            | MWeapon m -> m.Damage
            | RWeapon r -> r.Damage
        | _ -> 0

    /// Get the damage state for a weapon, adjusted for the possible ammo count for the weapon.
    let getWeaponDamageWithUse = function
        | Weapon (weaponType, info) ->
            match weaponType with
            | MWeapon m -> m.Damage, true
            | RWeapon r when r.AmmoCount-1 > 0 -> r.Damage, true
            | RWeapon r -> r.Damage, false
        | _ -> 0, false

    let visibilityToStr = function
        | VHigh -> "HighProfile"
        | VMedium -> "LowProfile"
        | VLow -> "Silent"

    /// Get the name of items decorated with the item type.
    let getNameWithType item =
        let matchTypeString =
            match item with
            | Weapon (wType, info) ->
                match wType with
                | MWeapon weapon -> sprintf "Melee Weapon V=%s D=%d:" (weapon.Visibility |> visibilityToStr) 
                                        weapon.Damage  + 
                                        (weapon.KOChance |> function | Some c -> sprintf " KOC:1/%d" c | None -> "") +
                                        (if weapon.IsPoisoned then " Poisoned" else "")
                | RWeapon weapon -> sprintf "RangedWeapon V=%s A=%d D=%d:" (weapon.Visibility |> visibilityToStr) weapon.AmmoCount weapon.Damage
            | Key _ -> "Key:"
            | Clue _ -> "Clue:"
            | Consumable (b,a,_,_,_) -> "Consumable:" + (if a then " Alcohol" else "") + (if b then " Poisoned" else "")
            | Container _ -> "Container:"
            | Display _ -> "Display:"
            | EscapeItem _ -> "Escape:"
            | LargeDisplay _ -> "LargeDisplay:"
            | Furniture _ -> "Furniture:"
            | HiddenPassageway _ -> "Display:"
            | Intel _ -> "Intel:"
            | Poison _ -> "Poison:"
        (getName item) |> sprintf "%s %s" matchTypeString

    let getDescription item = sprintf "%s %s" (getNameWithType item) ((getInfo item).Description)

    let isHeavy item =
        match item with
        | Container _ | LargeDisplay _ | EscapeItem _ | Furniture _ -> true
        | _ -> false

    let getItems item =
        match item with
        | Container (items, _) -> Some items
        | _ -> None

module RangedWeapon = 

    let fire weapon info = 
        Weapon (RWeapon {weapon with AmmoCount = weapon.AmmoCount - 1}, info) // New weapon with 1 less round.

module Personality =

    let adjuster f adjustment min max stat statStr =
        let adjustMin x = if x < 0 then 0 else x
        let adjustMax x = if x > 10 then 10 else x
        let level = snd stat
        match adjustment with
        | Down x -> 
            if level <= min then Failure (sprintf "%s cannot go any lower" statStr) 
            else Success (f <| adjustMin (level - x), adjustMin (level - x))
        | Up x -> 
            if level >= max then Failure (sprintf "%s cannot go any higher" statStr) 
            else Success (f <| adjustMax (level + x), adjustMax (level + x))

    let adjustAttraction adjustment attrStat =
        let matcher = function
        | 0 -> Hate
        | x when x >= 10 -> Love
        | _ -> NeutralA
        let level = snd attrStat
        adjuster matcher adjustment 0 10 attrStat "Attraction"

    let adjustTrust adjustment trustStat =
        let matcher = function
        | 0 -> Mistrust
        | 1 | 2 -> Doubt
        | 3 | 4 | 5 | 6 | 7 -> NeutralT // >= 3 is not suspicious. AI will listen to the player.
        | 8 | 9 -> Trust
        | _ -> FullTrust
        let level = snd trustStat
        adjuster matcher adjustment 0 10 trustStat "Trust"

    let adjustMood adjustment moodStat =
        let matcher = function
        | 0 -> Depressed
        | 1 | 2 | 3 -> Sad
        | 4 | 5 | 6 -> NeutralM
        | 7 | 8 | 9 -> Happy
        | _ -> Elated
        let level = snd moodStat
        adjuster matcher adjustment 0 10 moodStat "Mood"

    let adjustFear adjustment fearStat =
        let matcher = function
        | 0 | 1 -> NormalF
        | 2 | 3 | 4 | 5 -> Timid
        | 6 | 7 | 8 | 9 -> Shaken
        | _ -> Terrified
        let level = snd fearStat
        adjuster matcher adjustment 0 10 fearStat "Fear"

    let getEthicsAsString = function
        | ELawful -> "Lawful"
        | ENeutral -> "Neutral"
        | EChaotic -> "Chaotic"

    let getMoralityAsString = function
        | MBlue -> "Blue"
        | MGrey -> "Grey"
        | MRed -> "Red"

    let getAttractionAsString = function
        | Love -> "Love"
        | NeutralA -> "Neutral"
        | Hate -> "Hate"
        
    let getTrustAsString = function
        | FullTrust -> "Full Trust"
        | Trust -> "Trust"
        | NeutralT -> "Neutral"
        | Doubt -> "Doubt"
        | Mistrust -> "Mistrust"

    let getMoodAsString = function
        | Elated -> "Elated"
        | Happy -> "Happy"
        | NeutralM -> "Neutral"
        | Sad -> "Sad"
        | Depressed -> "Depressed"

    let getStateAsString = function
        | NormalS -> "Normal"
        | state -> sprintf "%A" state

    let getBraveryAsString = function
        | Fearful -> "Fearful"
        | NeutralB -> "Neutral"
        | Brave -> "Brave"

    let getFearAsString = function
        | NormalF -> "Normal"
        | x -> sprintf "%A" x

module Person =

    let getTypeAsString (person:Person) = 
        match person.Type with
        | Player -> "Player"
        | Civilian -> "Civilian"
        | Guard -> "Guard"
        | Target -> "Target"
        | Barkeep -> "Barkeep"
        | Chef -> "Chef"
        | Groundskeeper -> "Groundskeeper"
        | Janitor -> "Janitor"
        | Maid -> "Maid"

    let initPerson name description personType gender sexuality bravery ethics morality responsiveness = 
        let info = Info.initInfo name description
        let personality = { Attraction = NeutralA, 5; Trust = NeutralT, 5; Mood = NeutralM, 5; Fear = NormalF, 0;
            Ethics = ethics; Morality = morality; Bravery = bravery}

        {Info = info; ClueInfo = ""; 
        Type = personType; Gender = gender; 
        IsHoldingWeapon = false; IsHoldingFood = false; IsPoisoned = false; IsCommanded = false; CreatedNewLife = false;
        Sexuality = sexuality; State = NormalS; Health = 100; Action = ANeutralAction; Responsiveness = responsiveness;
        Awareness = Unaware; Personality = personality; Items = []; AttackDamage = 10}

    let addClue clue person =
        {person with ClueInfo = clue}

    let initStandingGuard name description personType gender sexuality bravery ethics morality responsiveness = 
        let person = initPerson name description personType gender sexuality bravery ethics morality responsiveness
        {person with Awareness = Warn}

    let setTrust newTrust person =
        let newPersonality = {person.Personality with Trust = newTrust}
        {person with Personality = newPersonality}

    let setAttraction newAttraction person =
        let newPersonality = {person.Personality with Attraction = newAttraction}
        {person with Personality = newPersonality}

    let setMood newMood person =
        let newPersonality = {person.Personality with Mood = newMood}
        {person with Personality = newPersonality}

    let setFear newFear person =
        let newPersonality = {person.Personality with Fear = newFear}
        {person with Personality = newPersonality}

    let setAwareness newAwareness person = {person with Awareness = newAwareness}

    let setDescription newDesc (person:Person) =
        let newInfo = {person.Info with Description = newDesc}
        {person with Info = newInfo}

    let setCreatedNewLife b (person:Person) = {person with CreatedNewLife = b}

    let getRespawnData (person:Person) =
        let name = person.Info.Name
        let gender = person.Gender
        name, gender

    let trySetAwareness newAwareness person =
        match person.Awareness with
        | Unaware -> person |> setAwareness newAwareness
        | _ -> person

    let setIsCommanded b person = {person with IsCommanded = b}

    let setIsPoisoned b (person:Person) = {person with IsPoisoned = b}

    let hasStatusEffect (person:Person) = person.IsPoisoned

    /// Once a person consumes alcohol, they become drunk which gives a boost to their responsiveness.
    let makeDrunk (person:Person) = 
        printfn "%s is inebriated" (person.Info.Name)
        {person with State = Drunk; Responsiveness = person.Responsiveness + 0.20}

    let setAction newAction person = {person with Action = newAction} |> setIsCommanded true

    let printStats person =
        printfn "Attraction: %s" <| Personality.getAttractionAsString (person.Personality.Attraction |> fst)
        printfn "Trust: %s" <| Personality.getTrustAsString (person.Personality.Trust |> fst)
        printfn "Mood: %s" <| Personality.getMoodAsString (person.Personality.Mood |> fst)
        printfn "Sexuality: %A" person.Sexuality
        printfn "State: %s" <| Personality.getStateAsString person.State
        printfn "Health: %d" person.Health
        printfn "Awareness: %A" person.Awareness
        printfn "Bravery: %s" <| Personality.getBraveryAsString (person.Personality.Bravery)
        printfn "Morality: %A" person.Personality.Morality
        printfn "Ethics: %A" person.Personality.Ethics
        printfn "Responsiveness: %f" person.Responsiveness

    let getAttraction person = person.Personality.Attraction
    let getTrust person = person.Personality.Trust
    let getMood person = person.Personality.Mood
    let getFear person = person.Personality.Fear
    let getEthics person = person.Personality.Ethics
    let getMorality person = person.Personality.Morality
    let getAwareness person = person.Awareness

    let getHealth (person:Person) = person.Health
    let getState person = person.State

    let getName (person:Person) = person.Info.Name
    let getDescription (person:Person) = person.Info.Description

    let getInfo (person:Person) = person.Info    

    let queryPersonality person = person.Personality.Attraction |> snd
    let queryTrust person = person.Personality.Trust |> snd
    let queryMood person = person.Personality.Mood |> snd
    let queryAttraction person = person.Personality.Attraction |> snd

    let fearWeight = 3 |> double
    let moodWeight = 2 |> double // High or low increases chance.
    let trustWeight = 5 |> double
    let attractionWeight = 5 |> double


    let adjustChanceWithStats (person:Person) chance =
        let weights = [
            fearWeight * (person |> getFear |> snd |> double)
            moodWeight * (person |> getMood |> snd |> double)
            trustWeight * (person |> getTrust |> snd |> double)
            attractionWeight * (person |> getAttraction |> snd |> double)
            ]
        weights |> List.fold (+) chance

    let adjustChanceFixed (person:Person) =
        let weights = [
            fearWeight * (person |> getFear |> snd |> double)
            moodWeight * (person |> getMood |> snd |> double)
            trustWeight * (person |> getTrust |> snd |> double)
            attractionWeight * (person |> getAttraction |> snd |> double)
            ]
        weights |> List.reduce (+)

    /// Random chance for a person's response based on their ethics.
    let getEthicsBasedChance (rng:Random) person =
        let chance = 
            match person.Personality.Ethics with
            | ELawful -> 0.20
            | ENeutral -> 0.50
            | EChaotic -> 0.80
        let chance = chance |> adjustChanceWithStats person
        rng.NextDouble() < chance

    /// Random chance for a person's response based on their morality.
    let getMoralityBasedChance (rng:Random) person =
        let chance = 
            match person.Personality.Morality with
            | MBlue -> 0.20
            | MGrey -> 0.50
            | MRed -> 0.80
        let chance = chance |> adjustChanceWithStats person
        rng.NextDouble() < chance

    /// Random chance for a person's response based on their ethics and morality.
    let getEthicsAndMoralityBasedChance (rng:Random) person =
        getEthicsBasedChance rng person = getMoralityBasedChance rng person // Anding the results of both is equivalent to doing P(chanceA * chanceB).

    let getStateAsString person =
        match person.State with
        | NormalS -> "Normal"
        | x -> sprintf "%A" x

    let getPossessiveGenderString (person:Person) =
        match person.Gender with
        | Male -> "his"
        | Female -> "her"

    let getReflexiveGenderString (person:Person) =
        match person.Gender with
        | Male -> "himself"
        | Female -> "herself"

    let getGenderPronounString (person:Person) =
        match person.Gender with
        | Male -> "he"
        | Female -> "she"

    let getGenderObjectiveString (person:Person) =
        match person.Gender with
        | Male -> "him"
        | Female -> "her"

    let getAwarenessAsString = function
        | Unaware -> "Unaware of you"
        | Afraid -> "Afraid of you"
        | Aware -> "Aware of you"
        | Warn -> "Warning you"
        | Hostile alertPerson ->
            match alertPerson with
            | TPerson p -> sprintf "Hostile to %s" p
            | TPlayer -> "Hostile to You"
            | NoTarget -> "No Target"

    /// Get all search info about a person.
    let getFullInfoStr (person:Person) =
        sprintf "Name:%s - Gender:%A - Type:%s - State:%s - Awareness:%s" person.Info.Name person.Gender (getTypeAsString person) (getStateAsString person) 
            (getAwarenessAsString person.Awareness)

    let getJobClothes (person:Person) =
        match person.Type with
            | Barkeep -> Success Barkeep
            | Chef -> Success Chef
            | Groundskeeper -> Success Groundskeeper
            | Janitor -> Success Janitor
            | Maid -> Success Maid
            | Guard -> Success Guard
            | _ -> Failure (sprintf "%s is not a person you can take clothes from" (person |> getName))

    let getCompatability gender person =
        match person.Sexuality, gender, person.Gender with
        | Bisexual, _, _ -> true
        | Straight, Male, Female -> true
        | Straight, Female, Male -> true
        | Gay, x, y when x = y -> true
        | _ -> false

    let getClueFromPerson person =
        match getTrust person with
        | Trust, _ | FullTrust, _ -> sprintf "%s Clue:\n%s" (person.Info.Name) (person.ClueInfo)
        | _ -> sprintf "%s does not trust you enough to give you any information" (person.Info.Name)

    let inquireInfo = ["name"; "description"; "type"; "gender"; "sexuality"; "attraction"; 
        "trust"; "mood"; "ethics"; "morality"; "bravery"; "health"; "clue"; "items"]

    let stringToDataString arg (person:Person) =
        let inner = function
            | "name" -> "Name: " + person.Info.Name
            | "description" -> "Description: " + person.Info.Description
            | "type" -> "Type: " + (person |> getTypeAsString)
            | "gender" -> "Gender: " + sprintf "%O" person.Gender
            | "sexuality" -> "Sexuality: " + sprintf "%O" person.Sexuality
            | "attraction" -> "Attraction: " + (person |> getAttraction |> fst |> Personality.getAttractionAsString)
            | "trust" -> "Trust: " + (person |> getTrust |> fst |> Personality.getTrustAsString)
            | "fear" -> "Fear: " + (person |> getFear |> fst |> Personality.getFearAsString)
            | "mood" -> "Mood: " + (person |> getMood |> fst |> Personality.getMoodAsString)
            | "ethics" -> "Ethics: " + (person |> getEthics |> Personality.getEthicsAsString)
            | "morality" ->  "Morality: " + (person |> getMorality |> Personality.getMoralityAsString)
            | "bravery" -> "Bravery: " + (person.Personality.Bravery |> Personality.getBraveryAsString)
            | "health" -> "Health: " + (sprintf "%d" <| getHealth person)
            | "state" -> "State: " + (person |> getStateAsString)
            | "clue" -> person |> getClueFromPerson     // Function already includes the formatting string.
            | "items" -> (sprintf "%s items:\n" person.Info.Name) + (person.Items |> List.map Item.getNameWithType |> Seq.seqStringBuilder)
                 
            | _ -> "" // Will never be matched. Only elements in the above list can be matched.

        if inquireInfo |> List.contains arg then Success <| inner arg   // Command safety from checking the list first. Ignore the above incomplete match warning.
        else Failure (sprintf "%s is not a valid question" arg)

    let addToInventory item (person:Person) =
        let inv = person.Items
        {person with Items = inv @ [item]}

    let removeFromInventory item (person:Person) =
        {person with Items = List.removeOne item person.Items}

    let updateInventory oldItem newItem (person:Person) =
        person |> removeFromInventory oldItem |> addToInventory newItem

    let setHoldingWeapon newBool (person:Person) = {person with IsHoldingWeapon = newBool}
    let setHoldingFood newBool (person:Person) = {person with IsHoldingFood = newBool}

    let tryFindItem item (person:Person) =
        person.Items |> List.tryFind (fun i -> i = item)

    /// Finds the item in the person's inventory by checking its name in lower case.
    let tryFindItemByName itemName (person:Person) =
        person.Items |> List.tryFind (Item.getName >> String.toLower >> (=) itemName)

    /// Update the specified person's awareness level with the given value.
    let updateAwareness newLevel person =
        {person with Awareness = newLevel}

    let addFear adj person =
        match person.Personality.Fear |> Personality.adjustFear adj with
        | Success fearStat -> person |> setFear fearStat
        | Failure s -> printfn "%s" s; person

    let addTrust adj person =
        match person.Personality.Trust |> Personality.adjustTrust adj with
        | Success trustStat -> person |> setTrust trustStat
        | Failure s -> printfn "%s" s; person

    /// Bad action by the player can upset people based on their morality. Prints a message based on the action.
    let badActionResponse person =
        let (adj,responseMsg) = 
            match person.Personality.Morality with
            | MBlue -> Down 2, sprintf "%s hated that" person.Info.Name
            | MGrey -> Down 1, sprintf "%s didn't like that" person.Info.Name
            | MRed -> Up 1, sprintf "%s liked that" person.Info.Name
        let adjAttraction adj person =
            match person.Personality.Attraction |> Personality.adjustAttraction adj with
            | Failure f -> Failure.printPersonalityAdjFailure person f; person
            | Success newAttraction -> person |> setAttraction newAttraction
        let adjTrust adj person =
            match person.Personality.Trust |> Personality.adjustTrust adj with
            | Failure f -> Failure.printPersonalityAdjFailure person f; person
            | Success newTrust -> person |> setTrust newTrust
        let adjMood adj person =
            match person.Personality.Mood |> Personality.adjustMood adj with
            | Failure f -> Failure.printPersonalityAdjFailure person f; person
            | Success newMood -> person |> setMood newMood
        if List.forall ((<)8) [person |> getTrust |> snd; person |> getAttraction |> snd] then person // If the person likes the player enough, ignore the action.
        else
            printfn "%s" responseMsg
            person |> adjAttraction adj |> adjTrust adj |> adjMood adj

    let updateState newState person =
        {person with State = newState}


    let isCompliant person =
        // Return true/false based on the person's trust, attraction, state, bravery, etc
        (1.0 - adjustChanceFixed person) < person.Responsiveness

    /// The default attack damage for people.
    let defaultDamage = 10

    let awareKnockoutChance = 20.0 |> double

    /// The default damage taken from poison per turn.
    let defaultPoisonDamage = 10

    let deathCheck (p:Person) =
        if p.Health <= 0 then printfn "%s is dead" p.Info.Name; p |> updateState Dead else p

    /// Try to apply poison damage to a person.
    let tryApplyPoisonDamage person =
        match person.State with
        | Dead -> person
        | _ ->
            if person.IsPoisoned then 
                printfn "%s took damage from poison. Health: %d" person.Info.Name (person.Health - defaultPoisonDamage)
                {person with Health = person.Health - defaultPoisonDamage}
                |> deathCheck
            else person

    /// Change a person's awareness to a new value or return the same person if it already has that value.
    let attackResponse attacker (person:Person) =
        match person.State with
        | NormalS ->
            let checkBravery (person:Person) =
                match person.Type, person.Personality.Bravery with
                | Guard,_ -> Hostile attacker
                | _,Brave -> Hostile attacker
                | _,Fearful -> Afraid
                | _,NeutralB -> Aware
            let newPersonAwareness = checkBravery person
            printfn "%s is %s" (person.Info.Name) (newPersonAwareness |> getAwarenessAsString)
            {person with Awareness = newPersonAwareness}
        | _ -> person

    /// Subtract health from people based on attack damage.
    let takeDamage damage (rng:Random) koChance (person:Person) = 
        let knockOutChance p =
            match koChance with
            | None -> p
            | Some chance -> 
                if rng.Next(0,chance) = 0 then p |> updateState Unconscious |> setAction ATryWakeUp
                else p
        {person with Health = person.Health - damage}
        |> knockOutChance
        |> deathCheck


    /// Apply an attack with specific parameters for punch/attack/chokeout/etc.
    let applyAttack attackDamage (rng:Random) kOChanceOpt weaponIsPoisoned person : Result<Person> =
        if person.State = Dead then Failure (sprintf "%s is already dead" (person.Info.Name)) else 
        person
        |> takeDamage attackDamage rng kOChanceOpt
        |> fun p -> if weaponIsPoisoned then printfn "%s is poisoned" person.Info.Name; {p with IsPoisoned = true} else p
        |> attackResponse TPlayer
        |> setTrust (Mistrust, 0)
        |> Success

module Player =
    let initPlayer name description gender items =
        {Info = Info.initInfo name description; Gender = gender; CloseTarget = None; 
            CompanionName = None; Disguise = None; Health = 100; 
            Items = items; EquippedItem = None}

    let setHealth newHealth (player:Player) = {player with Health = newHealth}

    /// Another person did not like your actions towards them which makes them attack you.
    let applyAngryAttack (rng:Random) (player:Player) =
        let damage = 10 * rng.Next(1, 5)
        {player with Health = player.Health - damage}, damage

    let applyDamage damage (player:Player) = {player with Health = player.Health - damage}

    let setName newName (player:Player) = {player with Info = {player.Info with Name = newName}}

    let setGender newGender (player:Player) = {player with Gender = newGender}

    let addToInventory item (player:Player) =
        let inv = player.Items
        {player with Items = inv @ [item]}

    let removeFromInventory item (player:Player) =
        {player with Items = List.removeOne item player.Items}

    let updateInventory oldItem newItem (player:Player) =
        player |> removeFromInventory oldItem |> addToInventory newItem

    let tryFindItemByName itemName (player:Player) =
        player.Items |> List.tryFind (Item.getName >> String.toLower >> (=) itemName)

    ///Finds the item in player's inventory, making sure that it is not poisoned first.
    let tryFindConsumableByName itemName (player:Player) =
        player.Items 
        |> List.filter (function | Consumable (true,_,_,_,_) -> false | _ -> true) 
        |> List.tryFind (Item.getName >> String.toLower >> (=) itemName)

    let updateCloseTarget person player =
        if person.State = Dead then {player with CloseTarget = None}
        else player

    let updateCompanion newCompanion player = {player with CompanionName = newCompanion}

    let printStats (player:Player) =
        printfn "Name: %s" player.Info.Name
        printfn "Description: %s" player.Info.Description
        printfn "Gender: %A" player.Gender

        match player.CloseTarget with
        | None -> ()
        | Some target -> printfn "Close Target: %s" target

        match player.CompanionName with
        | None -> ()
        | Some companionName -> printfn "Companion: %s" companionName

        printfn "Health: %d" player.Health

    let getDisguiseString player =
        match player.Disguise with
        | Some Barkeep -> "(Barkeep)"
        | Some Chef -> "(Chef)"
        | Some Groundskeeper -> "(Groundskeeper)"
        | Some Janitor -> "(Janitor)"
        | Some Maid -> "(Maid)"
        | Some Guard -> "(Guard)"
        | _ -> ""

    let removeEquippedItemCheck item player =
        match player.EquippedItem with
        | Some equippedItem when equippedItem = item -> {player with EquippedItem = None}
        | _ -> player

module AdjacentRoom =
    let getRoomStateStr (room:AdjacentRoom) =
        let name, lockState = room
        match lockState with
        | Unlocked -> name
        | Secret -> name
        | Locked code -> sprintf "%s Locked: %A key required" name code
        
module DoorCode =
    let toString = function
    | Blue -> "Blue"
    | Red -> "Red"
    | Green -> "Green"
    | White -> "White"
    | Black -> "Black"

    //type DoorCode = Red | Green | White | Black

module Room =

    let initRoom people info items roomType =
        {People = people; Info = info; Items = items; RoomType = roomType}

    let getInfo room = room.Info

    /// Get the name of a room.
    let getName room = room.Info.Name
    /// Get the description of a room.
    let getDescription room = room.Info.Description        

    let updateItems items (room:Room) =
        {room with Items = items}

    /// Finds the item in the room by checking its name in lower case.
    let tryFindItemByName itemName (room:Room) =
        room.Items |> List.tryFind (Item.getName >> String.toLower >> (=) itemName)

    /// Finds the person in the room by checking his/her name in lower case.
    let tryFindPersonByName (personName:string) room : Person option =
        let rec inner (acc:Person option) (people:Person list) =
            match people with
            | [] -> acc
            | p::ps when (p.Info.Name |> String.toLower = personName) -> Some p
            | p::ps -> inner None ps
        inner None room.People

    /// Update all people in a room using the specified function.
    let mapPeople f room =
        {room with People = room.People |> List.map f}

    let updatePerson (newPerson:Person) room =
        room |> mapPeople (fun p -> if p.Info.Name = newPerson.Info.Name then newPerson else p)

    let addPerson (person:Person) (room:Room) =
        {room with People = person::room.People}

    let addPeople (people:Person list) (room:Room) =
        {room with People = people @ room.People}


module Debug =
    let inline printPass msg (a:'a) = printfn "%s: %A" msg a; a

    let inline printListPass msg (a:'a list) = printfn "%s: " msg; a |> Seq.iter (printfn "%A"); a

module Objective =
    let getInfoStr obj =
        match obj with
        | Kill (_, personName ,_) -> sprintf "Kill %s" personName
        | CollectIntel (_,intelName) -> sprintf "Intel %s" intelName

    /// Check if an objective is completed
    let isCompleted = function
        | CollectIntel (true,_) -> true
        | Kill (true,_,_) -> true
        | _ -> false

    let toString = function
        | CollectIntel (b,name) -> sprintf "CollectIntel: %s" name + (if b then " Completed" else "")
        | Kill (b,name,state) -> sprintf "Kill: %s %A" name state

module SpawnRoom =

    let outsideRooms = [Spawn; Patio; Garden; Garage] // 
    let bathroomLockChance = double 0.25
    let stairsLockChance = double 0.10
    let commonRoomLockChance = double 0.10
    let closetLockChance = double 0.10

    let initSpawnRoom (rng:Random) roomType connectionF = 
        {Type = roomType; Connections = 0; MaxConnections = connectionF rng roomType}

    let initRoomConnect (room:(Room * RoomMap) * SpawnRoom) =
        let ((r,m),d) = room
        {Room = r; RoomMap = m; SpawnRoom = d}

    let roomConnectToRoomInfo (room:RoomConnect) = room.Room, room.RoomMap
    

    /// Takes an element x and returns a list containing n copies of x.
    let inline transformWeights (x, n) =
        if n = 0 then [] else
        [1..n] |> List.fold (fun s _ -> x::s) []

    let isCloset rC =
        match rC.SpawnRoom.Type with
        | Closet _ -> true
        | _ -> false

    let isMissionRoom rC =
        match rC.SpawnRoom.Type with
        | MissionRoom -> true
        | _ -> false

    /// Checks if a room has reached its maximum number of connections.
    let isMaxedOut (room:RoomConnect) = if room.SpawnRoom.Connections >= room.SpawnRoom.MaxConnections then true else false

    let getLockState (rng:Random) roomType =
        match roomType with
        | Spawn | Patio | Garden -> Unlocked
        | Garage | Storage -> Locked Blue
        | Bathroom -> if rng.NextDouble() < bathroomLockChance then [Locked White; Locked Green] |> List.randomChoice rng else Unlocked
        | Stairs -> if rng.NextDouble() < stairsLockChance then [Locked White; Locked Green; Locked Blue] |> List.randomChoice rng else Unlocked
        | CommonRoom -> if rng.NextDouble() < commonRoomLockChance then [Locked White; Locked Green; Locked Blue] |> List.randomChoice rng else Unlocked
        | EntranceWay | Hallway -> Locked Red
        | PrivateRoom -> Locked Green
        | Closet _ -> if rng.NextDouble() < closetLockChance then [Locked White; Locked Green; Locked Blue; Locked Red] |> List.randomChoice rng else Unlocked
        | MissionRoom -> Locked Black

    let remaining room = room.SpawnRoom.MaxConnections - room.SpawnRoom.Connections

    let areConnected roomA (roomB:RoomConnect) =
        let (_,aAdjRooms,_) = roomA.RoomMap
        match aAdjRooms |> List.tryFind (fst >> (=) roomB.Room.Info.Name) with
        | None -> false
        | _ -> true
        
    let connectRooms roomA roomB (rng:Random) =
        let aToBLockState = roomB.SpawnRoom.Type |> getLockState rng
        let bToALockState = roomA.SpawnRoom.Type |> getLockState rng

        let (acR,aAdjRooms,aoR) = roomA.RoomMap
        let newAAdjRooms = (roomB.Room.Info.Name, aToBLockState)::aAdjRooms
        let newASpawnRoom = {roomA.SpawnRoom with Connections = roomA.SpawnRoom.Connections + 1}
        let roomA = {roomA with RoomMap = (acR,newAAdjRooms,aoR); SpawnRoom = newASpawnRoom}

        let (bcR,bAdjRooms,boR) = roomB.RoomMap
        let newBAdjRooms = (roomA.Room.Info.Name, bToALockState)::bAdjRooms
        let newBSpawnRoom = {roomB.SpawnRoom with Connections = roomB.SpawnRoom.Connections + 1}
        let roomB = {roomB with RoomMap = (bcR,newBAdjRooms,boR); SpawnRoom = newBSpawnRoom}

        roomA, roomB

    let connectRoomsWithLockState (roomA:RoomConnect) (roomB:RoomConnect) aLockState bLockState =

        let (acR,aAdjRooms,aoR) = roomA.RoomMap
        let newAAdjRooms = (roomB.Room.Info.Name, aLockState)::aAdjRooms
        let newASpawnRoom = {roomA.SpawnRoom with Connections = roomA.SpawnRoom.Connections + 1}
        let roomA = {roomA with RoomMap = (acR,newAAdjRooms,aoR); SpawnRoom = newASpawnRoom}

        let (bcR,bAdjRooms,boR) = roomB.RoomMap
        let newBAdjRooms = (roomA.Room.Info.Name, bLockState)::bAdjRooms
        let newBSpawnRoom = {roomB.SpawnRoom with Connections = roomB.SpawnRoom.Connections + 1}
        let roomB = {roomB with RoomMap = (bcR,newBAdjRooms,boR); SpawnRoom = newBSpawnRoom}

        roomA, roomB

    // Filter out duplicated connections that may occur between closets and missionrooms when they are generated adjacent in the list and forced to connect.
    let tryFilterClosetConnections rCs =
        let mapper rC = 
            match rC.SpawnRoom.Type with
            | Closet true ->
                let (name,adj,over) = rC.RoomMap
                let newAdj = adj |> List.distinctBy (fst)
                {rC with RoomMap = (name,newAdj,over)}
            | MissionRoom ->
                let (name,adj,over) = rC.RoomMap
                let newAdj = adj |> List.distinct
                {rC with RoomMap = (name,newAdj,over)}
            | _ -> rC
        rCs |> List.map mapper

module Environment =
    /// Returns a new room based on applying f to each element.
    let updateItems f env =
        let newItems = env.Room.Items |> f
        {env with Room = env.Room |> Room.updateItems newItems}

    /// Update the player.
    let updatePlayer newPlayer env =
        let checkHealth env =
            if newPlayer.Health <= 0 then 
                match env.ExtraLives with
                | [] -> {env with GameStatus = PlayerDead}
                | (name,gender)::xs-> 
                    printfn "You have been reincarnated as %s, %A" name gender
                    let newPlayer = newPlayer |> Player.setHealth 100 |> Player.setName name |> Player.setGender gender
                    {env with Player = newPlayer; ExtraLives = xs}
            else env
        {env with Player = newPlayer} |> checkHealth

    /// Update a person in the room.
    let updatePerson newPerson env =
        let personName = newPerson |> Person.getName |> String.toLower
        let newRoom = {env.Room with People = env.Room.People |> List.replaceByWith (Person.getName >> String.toLower >> (=) personName) newPerson}
        {env with Room = newRoom}

    /// Bad action from the player affects any people that are aware.
    let applyBadActionToAll env =
        let newPeople = env.Room.People |> List.map (fun p -> if p.Awareness <> Unaware && p.State = NormalS then Person.badActionResponse p else p)
        let newRoom = {env.Room with People = newPeople}
        {env with Room = newRoom}

    /// Apply status effects to people that are no longer in the room. Only poison effects for right now.
    let updatePeopleStatus env =
        {env with UpdatePeople = 
                    env.UpdatePeople
                    |> List.map Person.tryApplyPoisonDamage}

    /// Add a new life for the player.
    let addLife newLife env = {env with ExtraLives = newLife::env.ExtraLives}

    /// Update the room.
    let updateRoom newRoom (env:DomainTypes.Environment) = {env with Room = newRoom}

    /// Find a person in the room.
    let tryFindPersonByName personName env =
        env.Room.People |> List.tryFind (Person.getName >> String.toLower >> (=) personName)

    let tryFindPersonByNameLower personName env =
        let personName = personName |> String.toLower
        env.Room.People |> List.tryFind (Person.getName >> String.toLower >> (=) personName)

    /// Find an item in the room.
    let tryFindItemByName itemName env =
        env.Room.Items |> List.tryFind (Item.getName >> String.toLower >> (=) itemName)

    /// Updates the map to reveal hidden passageways. World Generation ensures that all connected secret rooms are in the passageway list, so reveal all of them.
    let revealPassageways env =
        let revealDoor doorInfo =
            match doorInfo with
            | s, Secret -> (s, Unlocked)
            | d -> d
        let (roomName, doors, overlookRooms) = env.Map
        let newDoors = doors |> List.map revealDoor
        let newMap = (roomName,newDoors, overlookRooms)
        {env with Map = newMap}

    let addMove env = {env with MoveCount = env.MoveCount + 1}

    let addVisited roomName env = {env with VisitedRooms = roomName::env.VisitedRooms |> List.distinct}

    /// Check if the player has picked up a mission item (intel).
    let checkItemObjectives (item:Item) env =
        let rec inner list acc isDone =
            if isDone then list @ acc else
            match list with
            | [] -> acc
            | x::xs ->
                match x with
                | CollectIntel (completed, intelName) ->
                    if intelName = (Item.getName item) then 
                        printfn "You completed an Objective: CollectIntel-%s" intelName
                        inner xs ((CollectIntel (true, intelName))::acc) true
                    else inner xs (x::acc) false
                | _ -> inner xs (x::acc) false

        let objectives = inner env.Objectives [] false
        {env with Objectives = objectives}

    /// Check if the player has eliminated a mission target (person).
    let checkPersonObjectives (killedPerson:Person) env =
        let rec inner list acc isDone =
            if isDone then list @ acc else
            match list with
            | [] -> acc
            | x::xs ->
                match x with
                | Kill (completed, name, state) ->
                    if name = (killedPerson |> Person.getName) then 
                        printfn "You completed an Objective: Kill-%s" name
                        inner xs (Kill (true, name, Eliminated)::acc) true
                    else inner xs (x::acc) false
                | _ -> inner xs (x::acc) false
        
        let objectives = inner env.Objectives [] false
        {env with Objectives = objectives}