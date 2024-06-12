# ErlMUD Implementation Plan
This is a plan detailing the components which will be implemented, the sub-features which make them up, and the order of their dependencies.

**Question:**
Should I use the ECS (Entity-Component-System) paradigm?

- [ErlMUD Implementation Plan](#erlmud-implementation-plan)
- [Frontend (Client)](#frontend-client)
  - [User Interface](#user-interface)
  - [Syntax Parser](#syntax-parser)
    - [Tokenizer](#tokenizer)
    - [Synonym Dictionary](#synonym-dictionary)
    - [Command List](#command-list)
- [Backend (Server)](#backend-server)
  - [World Event Manager (Dungeon Master?)](#world-event-manager-dungeon-master)
  - [Map Loader](#map-loader)
  - [Door Registry](#door-registry)
  - [Object Registry](#object-registry)
- [Client/Server Communication](#clientserver-communication)
  - [Information Requests](#information-requests)
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
## Syntax Parser
### Tokenizer
### Synonym Dictionary
### Command List

# Backend (Server)
## World Event Manager (Dungeon Master?)
Manages events in the game world, allowing for actions taken in one zone to affect another.
## Map Loader
Initializes the game world from JSON files.
When each door is loaded from the JSON file, it is added to the door registry and the door's source and destination zones are given a link to the door's registry entry.
## Door Registry
Stores the connections between zones.
Doors can be thought of as the edges in a graph of zones.
## Object Registry
Stores the behavior and properties of objects in the game world.

# Client/Server Communication
## Information Requests
A request sent by the client to the server to get information about the game world.
## Action Requests
A request sent by the client to the server to perform an action in the game world.

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