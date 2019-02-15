module DomainTypes

open System

type Result<'a> = Success of 'a | Failure of string
type Relationship<'Left, 'Right> = 'Left * 'Right


// Item types.
type Name = string

type AmmoCount = int
type Damage = int
type Visibility = VLow | VMedium | VHigh  // When attacking ... Low: Doesn't alert nearby people. Medium: Alert nearby people. High: Alert people in other rooms.
type Range = RShort | RMedium | RLong


type MeleeWeapon = {Damage: Damage; Visibility: Visibility; KOChance: int Option; IsPoisoned: bool}
type RangedWeapon = {Damage: Damage; Visibility: Visibility; AmmoCount: AmmoCount}

type WeaponType = 
    | MWeapon of MeleeWeapon
    | RWeapon of RangedWeapon

type Info = {Name: string; Description: string}
type ClueInfo = string

type DoorCode = Blue | Red | Green | White | Black

type HealthBonus = int
type RemainingUses = int

type Interactiontype = // TODO: Possibly have to interact with some items to complete objects and find or do things.
    | TurnOnOff of bool
    | Open
    | Close
    | EnterPassword of string

type IsPoisoned = bool
type IsAlcohol = bool

type Item = 
    | Weapon of WeaponType * Info
    | Key of DoorCode * Info
    | Clue of Info * ClueInfo
    | HiddenPassageway of Info * (string list)
    | Consumable of IsPoisoned * IsAlcohol * HealthBonus * RemainingUses * Info
    | Container of Item list * Info
    | Display of Info
    | Furniture of Info
    | LargeDisplay of Info
    | EscapeItem of Info
    | Intel of Info
    | Poison of Info

type Container = Item list * Info

// Person types.

type PersonType = 
    | Player 
    | Barkeep
    | Chef
    | Groundskeeper
    | Janitor
    | Maid
    | Civilian 
    | Guard 
    | Target

type Gender =
    | Male
    | Female

type Sexuality =
    | Straight
    | Gay
    | Bisexual

type Ethics =
    | ELawful
    | ENeutral
    | EChaotic
type Morality =
    | MBlue
    | MGrey
    | MRed

type Attraction =
    | Love
    | NeutralA
    | Hate

type Trust = 
    | FullTrust
    | Trust
    | NeutralT
    | Doubt
    | Mistrust

type Mood =
    | Elated
    | Happy
    | NeutralM
    | Sad
    | Depressed

type State =
    | NormalS
    | Asleep
    | Unconscious
    | Dead
    | Drunk

type Bravery =
    | Fearful
    | NeutralB
    | Brave

type Fear =
    | NormalF
    | Timid
    | Shaken
    | Terrified

type Target = 
    | TPerson of string
    | TPlayer
    | NoTarget

type Awareness =
    | Unaware
    | Aware
    | Afraid
    | Hostile of Target // Target's name
    | Warn


type Adjustment = | Up of int | Down of int

type Personality = {
    Attraction: Attraction * int // Value checking/ updating functions for each (_, int) pair based on 1..10 values...
    Trust: Trust * int
    Mood: Mood * int
    Fear: Fear * int
    Ethics: Ethics
    Morality: Morality
    Bravery: Bravery
    }

type PersonName = string
type RoomName = string

// AI routines.
type AIAction =
    | AAttack
    | ASuicide
    | AUseFood
    | APickupItem of string
    | AGoto of RoomName
    | ATryWakeUp // Attempt to regain consciousness.
    | ANeutralAction

type Person = {
    Info: Info
    ClueInfo: string
    Type: PersonType
    Gender: Gender
    Sexuality: Sexuality
    State: State
    Health: int
    Awareness: Awareness
    Personality: Personality
    Items: Item list
    IsHoldingWeapon: bool
    IsHoldingFood: bool
    IsPoisoned: bool
    Action: AIAction
    IsCommanded: bool
    AttackDamage: int
    CreatedNewLife: bool
    Responsiveness: double
    }
    
type IsDiscovered = bool

type Player = {
    Info: Info
    Gender: Gender
    CloseTarget: PersonName Option
    Disguise: PersonType Option
    CompanionName: PersonName Option
    Health: int
    Items: Item list
    EquippedItem: Item option
    }

type ExtraLife = Name * Gender

// Game Win/Loss types.
type TargetState = | Alive | Eliminated | Escaped
type Completed = bool
type IntelName = string
type Objective =
    | Kill of Completed * PersonName * TargetState
    | CollectIntel of Completed * IntelName

type Accolades = {Kills: int}   // TODO: Add more accolades for various actions.

// Environment types.
    
type Time = {Hour: int; Minute: int}
type GameStatus = Continue | Exit | PlayerDead | Win | PartialWin

type LockState = 
    | Unlocked
    | Secret
    | Locked of DoorCode

// Room Spawn types.

type SpawnRoomType =
    // Outside 1-4 connections, Always unlocked.
    | Spawn // Spawn location where the player starts the game.
    | Patio // People outside, large numbers.
    | Garden // Fewer people, find items.
    | Garage // Can hold escape vehicles, add new roomType data to each room for missions and global ai actions...
    | Storage // More weapons than other rooms. Guard inside.
    // Inside
    | Bathroom // 1-2 connections.
    | Stairs // 4-6 connections.
    | Hallway // 3-6 connections.
    | CommonRoom // 2-3 connections.
    | EntranceWay // 4 connections, can expand to accomodate extra rooms.
    | PrivateRoom // 1-2 connections, closet, maybe a bathroom.
    | Closet of bool // 1 connection, maybe secret connection to mission room or other closets.
    | MissionRoom // Black key, 2 locked connections max, 1 secret access unlocked.

type Room = {People: Person list; Info: Info; Items: Item list; RoomType: SpawnRoomType}

type ConnectionCount = int

type CurrentRoom = string
type AdjacentRoom = string * LockState
type OverlookRooms = (string list) option

type RoomMap = CurrentRoom * AdjacentRoom list * OverlookRooms

type SpawnRoom = {Type: SpawnRoomType; Connections: int; MaxConnections: int}
type RoomConnect = {Room: Room; RoomMap: RoomMap; SpawnRoom: SpawnRoom}


type RoomInfo = Room * RoomMap

// Wait: AI is not triggered. Move: AI is triggered. Alert: AI is triggered and alerted to the player's presence. 
// AlertAll: AI is triggered and all people in the room and adjacent rooms are alerted to the player's presence.
type AICall = AIWait | AIMove | AIAlert of Target | AIAlertAll of Target

type Environment = {Player: Player; Room: Room; Map: RoomMap; Time: Time; GameStatus: GameStatus; ExtraLives: ExtraLife list;
    UpdatePeople: Person list; Rng: Random; Objectives: Objective list; Accolades: Accolades; MoveCount: int; VisitedRooms: string list}

// Check adjacent areas by name using env.Map.