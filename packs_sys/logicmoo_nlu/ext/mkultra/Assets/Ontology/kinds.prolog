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
immediate_kind_of(man, human).
immediate_kind_of(man, male).
immediate_kind_of(woman, human).
immediate_kind_of(woman, female).

immediate_kind_of(prop, physical_object).

immediate_kind_of(furniture, prop).
immediate_kind_of(sittable, furniture).
immediate_kind_of(chair, sittable).
immediate_kind_of(toilet, sittable).
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
immediate_kind_of(tort, event).

immediate_kind_of(social_group, social_object).
immediate_kind_of(social_group, actor).
immediate_kind_of(ethnic_group, social_group).
immediate_kind_of(organization, social_group).
immediate_kind_of(government, organization).
immediate_kind_of(government_agency, organization).
immediate_kind_of(business, organization).
immediate_kind_of(conspiracy, organization).

immediate_kind_of(intentional_tort, tort).
immediate_kind_of(assult, intentional_tort).
immediate_kind_of(battery, intentional_tort).
immediate_kind_of(false_imprisonment, intentional_tort).
immediate_kind_of(infliction_of_emotional_distress, intentional_tort).
immediate_kind_of(property_tort, tort).
immediate_kind_of(trespass, property_tort).
immediate_kind_of(conversion, property_tort).
immediate_kind_of(theft, conversion).
immediate_kind_of(dignitary_tort, tort).
immediate_kind_of(defamation, dignitary_tort).
immediate_kind_of(invasion_of_privacy, dignitary_tort).
immediate_kind_of(breach_of_confidence, dignitary_tort).
immediate_kind_of(abuse_of_process, dignitary_tort).
immediate_kind_of(malicious_prosecution, abuse_of_process).
immediate_kind_of(alienation_of_affections, dignitary_tort).
immediate_kind_of(economic_tort, tort).
immediate_kind_of(fraud, economic_tort).
immediate_kind_of(nuisance, tort).


:- process_kind_hierarchy.
