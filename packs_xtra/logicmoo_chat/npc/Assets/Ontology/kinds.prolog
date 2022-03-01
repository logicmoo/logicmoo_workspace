:- multifile(immediate_kind_of/2).
:- dynamic(immediate_kind_of/2).

immediate_kind_of(endurant, entity).
immediate_kind_of(actor, endurant).
immediate_kind_of(physical_endurant, endurant).
immediate_kind_of(physical_object, physical_endurant).
immediate_kind_of(nonphysical_endurant, endurant).
immediate_kind_of(nonphysical_object, nonphysical_endurant).

immediate_kind_of(living_thing, physical_object).
immediate_kind_of(male, living_thing).
immediate_kind_of(female, living_thing).
immediate_kind_of(creature, living_thing).
immediate_kind_of(plant, living_thing).
immediate_kind_of(creature, actor).
immediate_kind_of(person, creature).
immediate_kind_of(person, container).
immediate_kind_of(container, physical_object).
immediate_kind_of(human, person).
immediate_kind_of(boy, human).
immediate_kind_of(boy, male).
immediate_kind_of(girl, human).
immediate_kind_of(girl, female).

immediate_kind_of(prop, physical_object).

immediate_kind_of(furniture, prop).
immediate_kind_of(sittable, furniture).
immediate_kind_of(chair, sittable).
immediate_kind_of(shower, sittable).
immediate_kind_of(sofa, sittable).

immediate_kind_of(layable, furniture).
immediate_kind_of(bed, layable).

immediate_kind_of(physical_storage, furniture).
immediate_kind_of(physical_storage, container).
immediate_kind_of(closed_container, physical_storage).
immediate_kind_of(refridgerator, closed_container).
immediate_kind_of(sink, closed_container).
immediate_kind_of(bookshelf, closed_container).

immediate_kind_of(work_surface, furniture).
immediate_kind_of(work_surface, physical_storage).
immediate_kind_of(desk, work_surface).
immediate_kind_of(table, work_surface).

immediate_kind_of(room, physical_object).
immediate_kind_of(room, closed_container).
immediate_kind_of(kitchen, room).
immediate_kind_of(bedroom, room).
immediate_kind_of(bathroom, room).
immediate_kind_of(living_room, room).

immediate_kind_of(food, prop).
immediate_kind_of(fruit, food).
immediate_kind_of(apple, fruit).
immediate_kind_of(orange, fruit).

immediate_kind_of(mental_object, nonphysical_object).
immediate_kind_of(social_object, nonphysical_object).

immediate_kind_of(perdurant, entity).
immediate_kind_of(event, perdurant).
immediate_kind_of(action, event).
immediate_kind_of(question, event).
immediate_kind_of(assertion, event).
immediate_kind_of(favor, event).

immediate_kind_of(social_group, social_object).
immediate_kind_of(social_group, actor).
immediate_kind_of(sports_team, social_group).
immediate_kind_of(organization, social_group).
immediate_kind_of(community, organization).
immediate_kind_of(community_agency, organization).
immediate_kind_of(school, organization).
immediate_kind_of(science, organization).

immediate_kind_of(intentional_favor, favor).
immediate_kind_of(hug, intentional_favor).
immediate_kind_of(smile, intentional_favor).
immediate_kind_of(spreading_peace, intentional_favor).
immediate_kind_of(property_favor, favor).
immediate_kind_of(loaning, property_favor).
immediate_kind_of(giving, property_favor).
immediate_kind_of(gifting, giving).
immediate_kind_of(dignitary_favor, favor).
immediate_kind_of(complementing, dignitary_favor).
immediate_kind_of(thanking, dignitary_favor).
immediate_kind_of(telling_truth, dignitary_favor).
immediate_kind_of(honesty, telling_truth).
immediate_kind_of(honoring, dignitary_favor).


:- process_kind_hierarchy.
