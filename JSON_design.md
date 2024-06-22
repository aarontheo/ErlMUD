
# JSON Representation/Loading

## Flags


## Things
A 'Thing' is what I will call the basic representation of all 'things' in the game. This means zones, entities, objects, even the Player. Because state machines have no inheritance rules, there are some common attributes given to all things.
Common 'Thing' properties:
- 'id': A unique identifier
- 'name': The display name of the thing.
- 'shortname': A short name for the thing.
- 'description': A description of the thing.
- 'shortdesc': A short description of the thing.
- 'flags': A set of flags which determines the properties of the Thing.


## Zone
Each zone is represented as a JSON object, with the following properties:
- 'id': A unique identifier for the zone.
- 'flags': A set of flags which determines the properties of the Thing.
- 'name': The display name of the zone.
- 'shortname': A short name for the zone.
- 'description': A description of the zone.
- 'shortdesc': A short description of the zone.

## Container


## Entity
Entities are represented as JSON objects, with the following properties:
- 'id': A unique identifier for the entity.
- 'name': The display name of the entity.
- 'shortname': A short name for the entity.
- 'description': A description of the entity.
- 'shortdesc': A short description of the entity.
- 'maxhp': The maximum hit points of the entity.
- 'children': A list of objects in the entity's inventory.

## Door
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