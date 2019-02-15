module CommandTypes

type ItemName = string
type PersonName = string
type CommandName = string
type RoomName = string
type Question = string

type OptionArg = | Empty | Arg of string
type ViewArg = | Inventory | Time | PlayerStats | PersonStats of PersonName | CompanionName | Objectives | VisitedRooms
type SearchArg = | SearchArea | SearchItem of ItemName
type DescribeArg = | DescribeArea | DescribeItem of ItemName | DescribePerson of PersonName

type AICommand =    // Update AICommandList and the Parser.fs page for any changes made here.
    | AIAttack of PersonName
    | AIGoto of RoomName
    | AIPickup of ItemName
    | AIStop
    | AISuicide

type Command = 
    | Amuse of PersonName
    | Apply of ItemName * ItemName
    | Approach of PersonName
    | Attack of PersonName
    | Capture of PersonName
    | CheerUp of PersonName
    | ChokeOut of PersonName
    | Command of PersonName * AICommand
    | Compliment of PersonName
    | Consume of ItemName
    | Diagnose
    | Describe of DescribeArg
    | Disguise of PersonName
    | Dishearten of PersonName
    | Drop of ItemName
    | Escape of ItemName
    | Equip of ItemName 
    | FollowMe of PersonName
    | Give of ItemName * PersonName
    | Goto of OptionArg
    | GotoForce of RoomName
    | Help of OptionArg
    | Inquire of Question * PersonName
    | Inspect of ItemName
    | Intimidate of PersonName
    | LeaveMe
    | Peek of RoomName
    | Pickup of ItemName
    | Place of ItemName * ItemName // place an item in a Container or other item with storage.
    | Punch of PersonName
    | Quit
    | Romance of PersonName
    | Save
    | Scout of RoomName
    | Search of SearchArg
    | Seduce of PersonName
    | Survey
    | TakeFrom of ItemName * string
    | Talk of PersonName
    | Teleport of RoomName
    | Unequip
    | Unlock of RoomName
    | View of ViewArg
    | Wait