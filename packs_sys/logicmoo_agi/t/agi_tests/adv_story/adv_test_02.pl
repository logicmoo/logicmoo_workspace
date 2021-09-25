/*
 Situation:
 */

:- include(adv_test_01).

adv_tst(200):- 
 nl_assert("Person is in Kitchen"),
 nl_assert("Floyd is in Pantry"),
 nl_assert("The Pantry is North of Kitchen"),!.

adv_tst(240):- 
 nl_assert("Person wants goal satification to be easy."),
 nl_assert("Person thinks if they know their environment, goal satisfiation will become easier."),
 nl_assert("Person goal is to know environment."),
 nl_assert("Person thinks if one is being an explorer, they will know their environment."),
 nl_assert("Person think doing what explorers do will make persons goal satisfaction easier."),
 nl_assert("Person thinks being an explorer means to find unexplored exits and travel to them."),
 nl_assert("Person thinks exits are known by looking."),
 nl_assert("Person goal is to have looked"),
 nl_assert("Person the way to satifiy the goal to have looked is to: add_intent( Person, ( act3('look',Person,[])))"),
 nl_assert("Person DOES ( act3('look',Person,[]))"),
 nl_assert("Person notices exits: north, south, east, west."),
 nl_assert("Person thinks north is unexplored"),
 nl_assert("Person thinks going north will be acting like an explorer"),
 nl_assert("Person goal is to go north"),
 nl_assert("Person makes plan to go north.. the plan is very simple: [ ( act3('go__dir',Person,[ walk, north]))]"),
 nl_assert("Person DOES do_go_dir(Person, walk, north)"),
 nl_assert("Person leaves kitchen to the north"),
 nl_assert("In Kitchen, thus Person, sees Person did_depart kitchen to the north"),
 nl_assert("Person enters pantry from the south"),
 nl_assert("In Pantry, thus Floyed and Person, sees Person enter pantry did_arrive from the south"),
 nl_assert("Floyd belives Person was somewhere other than pantry before"),
 nl_assert("Floyd belives Person traveled north and there might be an exit in the opposite dirrection (south) leading somewhere other than pantry"),
 nl_assert("Person belives pantry is where they end up if they go north from kitchen"),
 nl_assert("Person belives kitchen is where they end up if they go south from pantry"),
 !