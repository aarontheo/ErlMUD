# ErlMUD Implementation Plan
This is a plan detailing the components which will be implemented, the sub-features which make them up, and the order of their dependencies.

**Question:**
Should I use the ECS (Entity-Component-System) paradigm?

**Table of Contents:**
- [ErlMUD Implementation Plan](#erlmud-implementation-plan)
- [Frontend (Client)](#frontend-client)
  - [User Interface](#user-interface)
  - [Syntax Parser](#syntax-parser)
    - [Command Syntax](#command-syntax)
    - [Tokenizer](#tokenizer)
    - [Term Filter](#term-filter)
    - [Action Thesaurus](#action-thesaurus)
- [Backend (Server)](#backend-server)
  - [Text Response Generator](#text-response-generator)
  - [World Event Manager (Dungeon Master?)](#world-event-manager-dungeon-master)
  - [Map Loader](#map-loader)
  - [Door Registry](#door-registry)
  - [Object Registry](#object-registry)
- [Client/Server Communication](#clientserver-communication)
  - [Action Requests](#action-requests)
- [State Machines](#state-machines)
  - [Zones](#zones)
  - [Entities](#entities)
- [JSON Representation/Loading](#json-representationloading)
  - [Zones](#zones-1)
  - [Entities](#entities-1)
  - [Doors](#doors)
  - [Objects](#objects)

# Frontend (Client)
## User Interface
The user interface is a terminal interface that accepts typed commands from the user and displays the response from the game server.

The user interface runs on a process that listens for user input, sends it to the syntax parser, and then sends the parsed command to the game server. Then, it listens for the response from the game server and displays it to the user and repeats the process.

## Syntax Parser
The syntax parser is a system that converts typed commands from the user into requests to the game server.

### Command Syntax
Depending on the complexity I want to implement, this design may be simplified to only allow simple commands, such as 'go north', 'look around', or 'hit orc with sword'.

<!-- TODO: How to handle names made of multiple tokens? -->

A command looks like this:
```
attack the orc with the iron sword
<verb/2> <noun> <noun>

hit orc
<verb/1> <noun>

go north
<verb/1> <noun>
```

**A note on object names:**
It could be possible to have names for objects with spaces if nouns are tokenized according to separator words. For example, an item called `rusty sword` could be taken with `take rusty sword`, as the `take` command takes only 1 argument. The command `hit ugly goblin with rusty sword` works because it is tokenized into `hit`, `ugly goblin`, and `rusty sword`, separated by the filler word `with`.

A command is composed of the following parts:
- Verbs (Actions)
- Nouns (People, Places, Things)
Verbs are represented as atoms.
Nouns are represented as strings.
<!-- Adjectives are represented as strings. -->

It is composed of the following sub-systems:
- Tokenizer
- Preposition Filter
- Action Thesaurus
- Noun Identifier

### Tokenizer
The tokenizer is a function that simply splits a string into a list of words, removing any whitespace or punctuation. This prepares the user input for parsing.

### Term Filter
The term filter is a function that removes prepositions and articles from the list of tokens.

### Action Thesaurus
The action thesaurus is a function that converts verb strings into valid action atoms. This allows for synonyms to be used in place of the standard form. For example, 'go' and 'move' are both string aliases for the 'move' action atom.
If a verb is not recognized, the parser will propogate an error message to the user, informing that a verb is not recognized.

# Backend (Server)

## Text Response Generator
The TRG is a system that generates descriptions of the game world for the user to read in response to user actions.
For example, if the user types 'look around', the TRG will generate a description of the current zone, including the entities and objects in it, as well as the doors in that room.

## World Event Manager (Dungeon Master?)
Manages events in the game world, allowing for actions taken in one zone to affect another.
May communicate with the TRG notify user of events in other zones, ex. "You hear a deep rumbling from the banquet hall."

## Map Loader
Initializes the game world from JSON files.
When each door is loaded from the JSON file, it is added to the door registry and the door's source and destination zones are given a link to the door's registry entry.

## Door Registry
Stores the connections between zones.
Doors can be thought of as the edges in a graph of zones.

## Object Registry
Stores the behavior and properties of objects in the game world.

# Client/Server Communication

## Action Requests
A request sent by the client to the server to perform an action in the game world.
Action requests can both modify the game world and query the game world for information.

# State Machines
Zones and entities will be implemented as state machines.
All attributes are stored in the machine's state.

## Zones

## Entities

# JSON Representation/Loading

## Zones
Each zone is represented as a JSON object, with the following properties:
- 'id': A unique identifier for the zone.
- 'name': The display name of the zone.
- 'shortname': A short name for the zone.
- 'description': A description of the zone.
- 'shortdesc': A short description of the zone.
- 'entities': A list of entities to initialize in the zone.
- 'objects': A list of objects in the zone.

## Entities
Entities are represented as JSON objects, with the following properties:
- 'id': A unique identifier for the entity.
- 'name': The display name of the entity.
- 'shortname': A short name for the entity.
- 'description': A description of the entity.
- 'shortdesc': A short description of the entity.
- 'maxhp': The maximum hit points of the entity.
- 'inventory': A list of objects in the entity's inventory.

## Doors
Doors are represented as links between zones.
While referred to as a 'door', this can represent any connection between zones, be it a door, hallway, path, or other transition.
A door is represented as a JSON object, with the following properties:
- 'id': A unique identifier for the door.
- 'dest': The ID of the destination zone.
- 'source': The ID of the source zone.
- 'locked': A boolean indicating if the door is locked.
- 'directed': A boolean indicating if the door is one-way or bidirectional.
- 'name': The display name of the door.
- 'shortname': A short name for the door.
- 'description': A description of the door.

## Objects
Objects are represented in zone and entity inventories as strings.
An object string may or may not have a linked object in the object registry. This allows for objects to be created on the fly.
The behaviors and properties of objects are stored in a JSON file.
The file is a dictionary of item names to item properties.