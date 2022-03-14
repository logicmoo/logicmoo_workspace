:- multifile(immediate_kind_of/2).
:- dynamic(immediate_kind_of/2).



%=autodoc
%% immediate_kind_of( ?Text1, ?Abstract2) is semidet.
%
% Immediate Kind Of.
%
immediate_kind_of(endurant, entity).
immediate_kind_of(actor, endurant).
immediate_kind_of(metaverse_endurant, endurant).
immediate_kind_of(metaverse_object, metaverse_endurant).
immediate_kind_of(abstract_endurant, endurant).
immediate_kind_of(abstract_object, abstract_endurant).

immediate_kind_of(living_thing, metaverse_object).
immediate_kind_of(male, living_thing).
immediate_kind_of(female, living_thing).
immediate_kind_of(creature, living_thing).
immediate_kind_of(plant, living_thing).
immediate_kind_of(creature, actor).
immediate_kind_of(person, creature).
immediate_kind_of(person, container).
immediate_kind_of(container, metaverse_object).
immediate_kind_of(human, person).
immediate_kind_of(boy, human).
immediate_kind_of(boy, male).
immediate_kind_of(girl, human).
immediate_kind_of(girl, female).

immediate_kind_of(conversant,person).
immediate_kind_of(character,creature).



immediate_kind_of(prop, metaverse_object).

immediate_kind_of(routine, prop).
immediate_kind_of(thinkable, routine).
immediate_kind_of(chair, thinkable).
immediate_kind_of(device, thinkable).
immediate_kind_of(good_idea, thinkable).

immediate_kind_of(debugable, routine).
immediate_kind_of(buggy_routine, debugable).

immediate_kind_of(metaverse_storage, routine).
immediate_kind_of(metaverse_storage, container).
immediate_kind_of(closed_container, metaverse_storage).
immediate_kind_of(refridgerator, closed_container).
immediate_kind_of(sink, closed_container).
immediate_kind_of(bookshelf, closed_container).

immediate_kind_of(work_surface, routine).
immediate_kind_of(work_surface, metaverse_storage).
immediate_kind_of(desk, work_surface).
immediate_kind_of(table, work_surface).

immediate_kind_of(module, metaverse_object).
immediate_kind_of(module, closed_container).
immediate_kind_of(thought_module, module).
immediate_kind_of(buggy_module, module).
immediate_kind_of(old_module, module).
immediate_kind_of(living_module, module).

immediate_kind_of(thought, prop).
immediate_kind_of(visualization, thought).
immediate_kind_of(apple, visualization).
immediate_kind_of(orange, visualization).

immediate_kind_of(mental_object, abstract_object).
immediate_kind_of(social_object, abstract_object).

immediate_kind_of(perdurant, entity).
immediate_kind_of(event, perdurant).
immediate_kind_of(action, event).
immediate_kind_of(beat_task, event).
immediate_kind_of(dialog_act, beat_task).
immediate_kind_of(question, dialog_act).
immediate_kind_of(assertion, dialog_act).
immediate_kind_of(favor, event).

immediate_kind_of(dialog_act,action).

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



%=autodoc
%% process_kind_hierarchy is semidet.
%
% Process Kind Hierarchy.
%
process_kind_hierarchy:- log(todo(process_kind_hierarchy)).

:- process_kind_hierarchy.
