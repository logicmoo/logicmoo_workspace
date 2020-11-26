%%% see /var/lib/myfrdcsa/codebases/minor/disaster-emergency-survival-preparedness/data-git/library/threats/coronavirus


%%% The Centers for Disease Control and Prevention (CDC) encourages household members to prepare for the possibility of a COVID-19 outbreak in their community.
general_advice ==>>
  isa(Household,household),
  forall(has_household(Agent,Household),prepare_for_the_possibility_of_a_covid_19_outbreak_in_their_community(Agent)).


%%% COVID-19 is caused by a new virus.
hasCause(covid19,new(virus)).


%%% • Before a COVID-19 outbreak occurs: Plan • During a COVID-19 outbreak: Act • After a COVID-19 outbreak has ended: Follow Up • Readiness Resources
before_a_covid_19_outbreak_occurs ==>>
  plan.

during_a_covid_19_outbreak ==>>
  act.

after_a_covid_19_outbreak_has_ended ==>>
  follow_up.


%%% A COVID-19 outbreak could last for a long time in your community.
plan ==>>
  prepare_for_long_duration_outbreak.


%%% Depending on the severity of the outbreak, public health officials may recommend community actions designed to help keep people healthy, reduce exposures to COVID-19, and slow the spread of the disease.
use_covid_19_planning_domain ==>>
  possibly_update_covid_19_planning_domain.


%%% Local public health officials may make recommendations appropriate to your local situation.
use_covid_19_planning_domain ==>>
  possibly_update_covid_19_planning_domain_with_local_recommendations.



%%% Creating a household plan can help protect your health and the health of those you care about in the event of an outbreak of COVID-19 in your community.
plan(Agent) ==>> cwc,
  has_household(Agent,Household),
  create_a_household_plan(Household).

household_has_members(Household,HouseholdMembers) ==>> cwc,
  findall(Agent0,has_household(Agent0,Household),HouseholdMembers).


%%% You should base the details of your household plan on the needs and daily routine of your household members.

:- discontiguous(create_a_household_plan/3).

%%% Talk with the people who need to be included in your plan.
create_a_household_plan(Household) ==>> cwc,
  household_has_members(Household,HouseholdMembers),
  generate_household_plan(groupFn(HouseholdMembers),Household,HouseholdPlan),
  %%% forall((member(Agent0,HouseholdMembers),member(Agent1,HouseholdMembers),Agent0 \= Agent1),
  %%%        talk_with_about(Agent0,Agent1,HouseholdPlan)).
  talk_together_about(groupFn(HouseholdMembers),HouseholdPlan).


%%% Meet with household members, other relatives, and friends to discuss what to do if a COVID-19 outbreak occurs in your community and what the needs of each person will be.
meet_with_household_members__other_relatives__and_friends(Household) ==>> cwc,
  household_has_members(Household,HouseholdMembers),
  get_household_members_comma_nearby_friends_or_relatives(groupFn(HouseholdMembers),groupFn(HouseholdMembersTheirNearbyFriendsAndRelatives)),
  meet_with_for(groupFn(HouseholdMembersTheirNearbyFriendsAndRelatives),
          discuss_what_to_do_if_a_covid_19_outbreak_occurs(groupFn(HouseholdMembersTheirNearbyFriendsAndRelatives))).


get_nearby_friends_or_relatives(groupFn(Agents),groupFn(NearbyFriendsAndRelatives)) ==>> cwc,
  findall(FriendOrRelative,
    (   
        member(Agent,Agents),
        (  has_relative(Agent,FriendOrRelative) ; has_friend(Agent,FriendOrRelative)),
        lives_nearby(Agent,FriendOrRelative)
    ),
    NearbyFriendsAndRelatives).

get_household_members_comma_nearby_friends_or_relatives(groupFn(HouseholdMembers),groupFn(HouseholdMembersTheirNearbyFriendsAndRelatives)) ==>> cwc,
  get_nearby_friends_or_relatives(groupFn(HouseholdMembers),groupFn(NearbyFriendsAndRelatives)),
  append(HouseholdMembers,NearbyFriendsAndRelatives,HouseholdMembersTheirNearbyFriendsAndRelatives).


%%% Plan ways to care for those who might be at greater risk for serious complications.

%%% If you or your household members are at increased risk for COVID-19 complications, please consult with your health care provider for more information about monitoring your health for symptoms suggestive of COVID-19.

%%% CDC will recommend actions to help keep people at high risk for complications healthy if a COVID-19 outbreak occurs in your community.

plan_ways_to_care_for_those_who_might_be_at_greater_risk_for_serious_complications(Household,Ways) ==>> cwc,
  household_has_members(Household,HouseholdMembers),
  get_household_members_comma_nearby_friends_or_relatives(
     groupFn(HouseholdMembers),
     groupFn(HouseholdMembersTheirNearbyFriendsAndRelatives)),
  forall(member(Agent,HouseholdMembersTheirNearbyFriendsAndRelatives),
    (   is_at_greater_risk_for_serious_complications(Agent) ->
        (  generate_care_plan_for(Household,Agent,Plan1),
      please_consult_with_your_health_care_provider_for_more_information_about_monitoring_your_health_for_symptoms_suggestive_of_covid_19(Household,Agent,Plan2),
      check_for_cdc_recommended_actions(Household,Agent,Plan3)))).


%%% From the data that are available for COVID-19 patients, and from data for related coronaviruses such as SARS-CoV and MERS-CoV, it is possible that older adults and persons who have underlying chronic medical conditions may be at risk for more serious complications.
is_at_greater_risk_for_serious_complications(Agent) ==>> cwc,
  (
   age(Agent,Age),
   Age >= 60
  ) ;
  has_underlying_chronic_health_condition(Agent).



%%% Get to know your neighbors.

%%% Talk with your neighbors about emergency planning.

create_a_household_plan(Household) ==>> cwc,
  household_has_members(Household,HouseholdMembers),
  has_neighboring_households(Household,NeighboringHouseholds),
  member(NeighboringHouseholds,Household0),
  household_has_members(Household0,Household0Members),
  get_to_know_your_neighbors(HouseholdMembers,Household0Members),
  talk_with_about_emergency_planning(HouseholdMembers,Household0Members).


%%% If your neighborhood has a website or social media page, consider joining it to maintain access to neighbors, information, and resources.
create_a_household_plan(Household) ==>> cwc,
  has_neighborhood(Household,Neighborhood),
  (   has_website(Neighborhood,NeighborhoodWebsite) ->
      (  consider_joining(Household,NeighborhoodWebsite),
    maintain_access_to_neighbors__information__and_resources(Household,NeighborhoodWebsite))),
  (   has_social_media_site(Neighborhood,NeighborhoodSocialMediaSite) ->
      (  consider_joining(Household,NeighborhoodSocialMediaSite),
    maintain_access_to_neighbors__information__and_resources(Household,NeighborhoodSocialMediaSite))).


%%% Identify aid organizations in your community.

%%% Create a list of local organizations that you and your household can contact in the event you need access to information, health care services, support, and resources.

%%% Consider including organizations that provide mental health or counseling services, food, and other supplies.

create_a_household_plan(Household) ==>> cwc,
  findall(Organization,
    (
     has_community(Household,Community),
     services(Organization,Community),
     (   isa(Organization,aidOrganization) ;
         isa(Organization,mentalHealthOrganization) ;
         isa(Organization,counselingService) ;
         isa(Organization,foodService))
    ),ListOfLocalAidOrganizations),
  contact_in_the_event_you_need_access_to_information__health_care_services__support__and_resources(Household,ListOfLocalAidOrganizations).


%%% Create an emergency contact list.

%%% Ensure your household has a current list of emergency contacts for family, friends, neighbors, carpool drivers, health care providers, teachers, employers, the local public health department, and other community resources.

create_a_household_plan(Household) ==>> cwc,
  create_an_emergency_contact_list(Household,_EmergencyContactList).
