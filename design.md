# ErlMUD Implementation Plan
This is a plan detailing the components which will be implemented, the sub-features which make them up, and the order of their dependencies.

**Question:**
Should I use the ECS (Entity-Component-System) paradigm?

**Table of Contents:**
- [ErlMUD Implementation Plan](#erlmud-implementation-plan)
- [Frontend (Client)](#frontend-client)
  - [User Interface](#user-interface)
    - [User Input](#user-input)
    - [User Output](#user-output)
- [Backend (Server)](#backend-server)
  - [Syntax Parser](#syntax-parser)
  - [Text Response Generator](#text-response-generator)
    - [Success messages](#success-messages)
    - [Failure messages](#failure-messages)
  - [WEM (World Event Manager/Dungeon Master)](#wem-world-event-managerdungeon-master)
  - [Player Registry](#player-registry)
  - [Door Registry](#door-registry)
  - [Object Registry](#object-registry)
- [Client/Server Communication](#clientserver-communication)
  - [Action Requests](#action-requests)
- [Things (Objects, Entities, Zones)](#things-objects-entities-zones)
  - [Object Hierarchy:](#object-hierarchy)
  - [Flags](#flags)
  - [JSON](#json)
  - [State Machine Structure:](#state-machine-structure)
- [Map Loader](#map-loader)

---
# Frontend (Client)
## User Interface
The user interface is a terminal interface that accepts typed commands from the user and displays the response from the game server. It consists of text input and text output.

### User Input
The user input interface runs on a process that listens for user input, sends it to the syntax parser, and then sends the parsed command to the game server.

### User Output
The user output interface runs on a process that listens for responses from the game server and displays them to the user. This allows for unprompted messages from the server to be displayed to the user, such as events not directly tied to a user action from the WEM.

---
# Backend (Server)

## Syntax Parser
The syntax parser is a system that converts typed commands from the user into requests that the game server can process.

A complete command is comprised of a verb (the action), a direct object (the acted upon), and sometimes, an instrumental object (the thing used in the action).

Basic rules to identify these:
- The direct object comes after the verb.
- An instrumental object is typically preceded by a word like 'with' or 'using'.

When parsing a command sentence, the algorithm is as follows:
1. Strip all definite and indefinite articles from the sentence. ('a', 'the')
2. Identify the verb.
3. Identify the direct object as the noun preceded by the verb.
4. Identify the instrumental object as the noun preceded by a word indicating instrumental use, like 'with' or 'using'.

The parser takes a sentence string and identifies the verb, direct object, and indirect object in the sentence. It then converts the verb into an action atom and the objects into strings. The parser then sends the parsed command to the game server.

The action thesaurus is a function that converts verb strings into valid action atoms. This allows for synonyms to be used in place of the standard form. For example, 'go' and 'move' are both string aliases for the 'move' action atom.

## Text Response Generator
The TRG is a system that generates descriptions of the game world for the user to read in response to user actions.
For example, if the user types 'look around', the TRG will generate a description of the current zone, including the entities and objects in it, as well as the doors in that room.

### Success messages
When a user inputs a command that succeeds, the TRG returns a description of the action taken, using the context of the specific noun acted upon or verb used to act. 
The message will ideally reflect the verb synonym used to perform it.
Ex. Input `move north`: `You move north.`
Ex. Input `go north`: `You go north.`

### Failure messages
When a user inputs a command that fails, either an unrecognized verb, absent noun, incorrect number of noun arguments to a verb, or action which isn't permitted on a certain object, a user error is returned which states:

For an unrecognized verb:
`I do not know how to '<verb>'.`

For a noun not found either in inventory or the current zone:
`There isn't a '<noun>' here.`

For a verb given an incorrect number of arguments:
`I don't know how to <verb> in that way.`

For an action not permitted on an object:
`You can't <verb> a <noun>, are you crazy?`

## WEM (World Event Manager/Dungeon Master)
Manages events in the game world, allowing for actions taken in one zone to affect another.
May communicate with the TRG notify user of events in other zones, ex. 
"You hear a deep rumbling from the banquet hall."

## Player Registry
Each player character is just a normal entity, but is linked to a particular client which is connected.
When a player logs in, they are prompted for a name, and then the player registry is updated with the new player entity. If the name already exists, the player is prompted to choose a different name.

## Door Registry
Stores the connections between zones.
Doors can be thought of as the edges in a graph of zones.

## Object Registry
Stores the behavior and properties of objects in the game world.


---
# Client/Server Communication

## Action Requests
A request sent by the client to the server to perform an action in the game world.
Action requests can both modify the game world and query the game world for information.

---
# Things (Objects, Entities, Zones)
All 'things' in the game are represented as state machines.
All attributes are stored in the machine's state.
'Things' are similar to Objects in an object oriented language, but with no inheritance rules.

## Object Hierarchy:
The game is organized similar to file directories on a computer. Each state machine may have the capacity to have other statems as children, such as when a house contains rooms, such as a kitchen, which contains a table, which contains a plate.

## Flags
Flags indicate that a thing has certain properties. This can be limitations on behavior, or it can guarantee the presence of particular properties in a state machine.
**ATTACKABLE**
**ALIVE**
**EDIBLE**
**HAS_HP**
**CONTAINER**:
Implies:
- 'children': A dictionary of child lists indicating the nature of the containing relationship 'on', 'under', 'in', etc.
- 'capacity': The maximum number of children the container can hold
- 

## JSON
Object JSON templates the creation of objects (meaning all entities, zones, and objects)

## State Machine Structure:
**Type:** A state machine's type can be either 'zone', 'entity', 'player_entity'.
**Parent:** The state machine this statem's objectis contained within.

---
# Map Loader
The map loader is a system that reads JSON files and creates the game world from them.
Because the game world is represented as a hierarchy of state machines, the representation to load in the game world is actually structured as a folder hierarchy, with folders representing all things in the game world, and JSON files representing the properties of those things.