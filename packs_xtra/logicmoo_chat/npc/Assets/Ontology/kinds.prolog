:- multifile(immediate_kind_of/2).
:- dynamic(immediate_kind_of/2).



%=autodoc
%% immediate_kind_of( ?Text1, ?Abstract2) is semidet.
%
% Immediate Kind Of.
%
immediate_kind_of(endurant, entity).
immediate_kind_of(actor, endurant).
immediate_kind_of(virtual_endurant, endurant).
immediate_kind_of(metaverse_object, virtual_endurant).
immediate_kind_of(imaginary_endurant, endurant).
immediate_kind_of(imaginary_object, imaginary_endurant).

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

immediate_kind_of(subroutine, prop).
immediate_kind_of(runnable, subroutine).
%immediate_kind_of(task, runnable).
immediate_kind_of(device, runnable).
immediate_kind_of(good_idea, runnable).

immediate_kind_of(debugable, subroutine).
immediate_kind_of(buggy_routine, debugable).

immediate_kind_of(virtual_storage, subroutine).
immediate_kind_of(virtual_storage, container).
immediate_kind_of(closed_container, virtual_storage).
immediate_kind_of(research_incubater, closed_container).
immediate_kind_of(sourcefile, closed_container).
immediate_kind_of(bookshelf, closed_container).

%immediate_kind_of(work_surface, subroutine).
immediate_kind_of(work_surface, virtual_storage).
immediate_kind_of(desk, work_surface).
immediate_kind_of(table, work_surface).

immediate_kind_of(program, metaverse_object).
immediate_kind_of(program, closed_container).
immediate_kind_of(research_program, program).
immediate_kind_of(buggy_program, program).
immediate_kind_of(old_program, program).
immediate_kind_of(living_program, program).

immediate_kind_of(task, prop).
immediate_kind_of(visualization, task).
immediate_kind_of(apple, visualization).
immediate_kind_of(orange, visualization).

immediate_kind_of(mental_object, imaginary_object).
immediate_kind_of(social_object, imaginary_object).

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
