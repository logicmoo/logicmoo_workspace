:- expects_dialect(lps).
maxTime(10).

% /mnt/sdc1/logicmoo_workspace.1/lib/swipl/library/prolog_xref loaded into prolog_xref 0.01 sec, 0 clauses
% /mnt/sdc1/logicmoo_workspace.1/lib/swipl/library/pldoc/doc_html loaded into pldoc_html 0.00 sec, 0 clauses
% /mnt/sdc1/logicmoo_workspace.1/lib/swipl/library/prolog_colour loaded into prolog_colour 0.00 sec, 0 clauses
% Updating GIT version stamps in the background.
% Updating GIT version stamps in the background.
% /mnt/sdc1/logicmoo_workspace.1/lib/swipl/library/prolog_xref loaded into prolog_xref 0.01 sec, 0 clauses
% /mnt/sdc1/logicmoo_workspace.1/lib/swipl/library/pldoc/doc_html loaded into pldoc_html 0.00 sec, 0 clauses
% /mnt/sdc1/logicmoo_workspace.1/lib/swipl/library/prolog_colour loaded into prolog_colour 0.00 sec, 0 clauses
% Updating GIT version stamps in the background.
% Updating GIT version stamps in the background.
% % From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
% include_e_lps_pddl_file(lps,ext('flp/worlds/flp/flp.d.pddl')).
% include_e_lps_pddl_file(lps,
%                       '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl').
% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
 %   [domain(flp), [requirement|':negative-preconditions']].
requirement(':negative_preconditions').
 %   [domain(flp), [requirement|':conditional-effects']].
requirement(':conditional_effects').
 %   [domain(flp), [requirement|':equality']].
requirement(':equality').
 %   [domain(flp), [requirement|':typing']].
requirement(':typing').
 %   [domain(flp), [requirement|':fluents']].
requirement(':fluents').
 %   [domain(flp), [requirement|':durative-actions']].
requirement(':durative_actions').
 %   [domain(flp), [requirement|':derived-predicates']].
requirement(':derived_predicates').
 %   [domain(flp), [subtype|typed(object, intelligentagent)]].
subtype(intelligentagent, object).
 %   [domain(flp), [subtype|typed(object, residence)]].
subtype(residence, object).
 %   [domain(flp), [subtype|typed(object, vehicle)]].
subtype(vehicle, object).
 %   [domain(flp), [subtype|typed(object, tool)]].
subtype(tool, object).
 %   [domain(flp), [subtype|typed(object, container)]].
subtype(container, object).
 %   [domain(flp), [subtype|typed(category, modeoftransportation)]].
subtype(modeoftransportation, category).
 %   [domain(flp), [subtype|typed(thing, object)]].
subtype(object, thing).
 %   [domain(flp), [subtype|typed(thing, physicallocation)]].
subtype(physicallocation, thing).
 %   [domain(flp), [subtype|typed(intelligentagent, person)]].
subtype(person, intelligentagent).
 %   [domain(flp), [subtype|typed(physicallocation, residence)]].
subtype(residence, physicallocation).
 %   [domain(flp), [subtype|typed(container, vehicle)]].
subtype(vehicle, container).
 %   [domain(flp), [predicate, autonomous, A, -, intelligentagent]].
fluents autonomous(intelligentagent).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
 /*   [ domain(flp),
          [predicate, location, O, -, object, L, -, physicallocation]
        ].
 */
fluents location(object, physicallocation).
 %   [domain(flp), [predicate, contains, C, -, container, O, -, object]].
fluents contains(container, object).
 %   [domain(flp), [predicate, mobile, Ob, -, object]].
fluents mobile(object).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
 /*   [ domain(flp),
          [predicate, 'directly-holding', A, -, intelligentagent, O, -, object]
        ].
 */
fluents directly_holding(intelligentagent, object).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
 /*   [ domain(flp),
          [ predicate,
            'travel-path',
            M,
             (-),
            modeoftransportation,
            L0,
            L1,
             (-),
            physicallocation
          ]
        ].
 */
fluents travel_path(modeoftransportation, physicallocation, physicallocation).
 %   [domain(flp), [predicate, 'driving-p', M, -, modeoftransportation]].
fluents driving_p(modeoftransportation).
 %   [domain(flp), [predicate, 'walking-p', M, -, modeoftransportation]].
fluents walking_p(modeoftransportation).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
 /*   [ domain(flp),
          [ functions,
            [ 'travel-distance',
              M,
               (-),
              modeoftransportation,
              L0,
              L1,
               (-),
              physicallocation
            ],
            [ 'travel-duration',
              M,
               (-),
              modeoftransportation,
              L0,
              L1,
               (-),
              physicallocation
            ]
          ]
        ].
 */
function(travel_distance(modeoftransportation, physicallocation, physicallocation), object).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
 /*   [ domain(flp),
          [ functions,
            [ 'travel-duration',
              M,
               (-),
              modeoftransportation,
              L0,
              L1,
               (-),
              physicallocation
            ]
          ]
        ].
 */
function(travel_duration(modeoftransportation, physicallocation, physicallocation), object).
 %   [domain(flp), [functions]].

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
 /*   [ domain(flp),
          [ 'durative-action',
            walk,
            ':parameters',
            [ A,
               (-),
              intelligentagent,
              L0,
              L1,
               (-),
              physicallocation,
              M,
               (-),
              modeoftransportation
            ],
            ':duration',
            [=, Duration, ['travel-duration', M, L0, L1]],
            ':condition',
            [ and,
              [over, all, ['walking-p', M]],
              [over, all, ['travel-path', M, L0, L1]],
              [over, all, [autonomous, A]],
              [at, start, [location, A, L0]]
            ],
            ':effect',
            [ and,
              [at, end, [not, [location, A, L0]]],
              [at, end, [location, A, L1]]
            ]
          ]
        ].
 */
fluents enabled_durative_action_walk(A, L0, L1, M, Duration).
actions walk(A, L0, L1, M).
enabled_durative_action_walk(A, L0, L1, M, Duration)at T0 if isa(A, intelligentagent), isa(L0, physicallocation), isa(L1, physicallocation), isa(M, modeoftransportation), walking_p(M)from T0 to T9, travel_path(M, L0, L1)from T0 to T9, autonomous(A)from T0 to T9, location(A, L0)at T0.
walk(A, L0, L1, M)initiates location(A, L1)if enabled_durative_action_walk(A, L0, L1, M, Duration).
walk(A, L0, L1, M)terminates location(A, L0)if enabled_durative_action_walk(A, L0, L1, M, Duration).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
 /*   [ domain(flp),
          [ 'durative-action',
            'pick-up',
            ':parameters',
            [ A,
               (-),
              intelligentagent,
              O,
               (-),
              object,
              L,
               (-),
              physicallocation
            ],
            ':duration',
            [=, Duration, 0],
            ':condition',
            [ and,
              [over, all, [autonomous, A]],
              [over, all, [mobile, O]],
              [at, start, [not, ['directly-holding', A, O]]],
              [at, start, [location, A, L]],
              [at, start, [location, O, L]]
            ],
            ':effect',
            [and, [at, end, ['directly-holding', A, O]]]
          ]
        ].
 */
fluents enabled_durative_action_pick_up(A, O, L, Duration).
actions pick_up(A, O, L).
enabled_durative_action_pick_up(A, O, L, Duration)at T0 if isa(A, intelligentagent), isa(O, object), isa(L, physicallocation), autonomous(A)from T0 to T9, mobile(O)from T0 to T9, location(A, L)at T0, location(O, L)at T0, not directly_holding(A, O)at T0.
pick_up(A, O, L)initiates directly_holding(A, O)if enabled_durative_action_pick_up(A, O, L, Duration).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
 /*   [ domain(flp),
          [ 'durative-action',
            'set-down',
            ':parameters',
            [ A,
               (-),
              intelligentagent,
              O,
               (-),
              object,
              L,
               (-),
              physicallocation
            ],
            ':duration',
            [=, Duration, 0],
            ':condition',
            [ and,
              [over, all, [autonomous, A]],
              [over, all, [mobile, O]],
              [at, start, ['directly-holding', A, O]],
              [at, start, [location, A, L]]
            ],
            ':effect',
            [ and,
              [at, end, [location, O, L]],
              [at, end, [not, ['directly-holding', A, O]]]
            ]
          ]
        ].
 */
fluents enabled_durative_action_set_down(A, O, L, Duration).
actions set_down(A, O, L).
enabled_durative_action_set_down(A, O, L, Duration)at T0 if isa(A, intelligentagent), isa(O, object), isa(L, physicallocation), autonomous(A)from T0 to T9, mobile(O)from T0 to T9, directly_holding(A, O)at T0, location(A, L)at T0.
set_down(A, O, L)initiates location(O, L)if enabled_durative_action_set_down(A, O, L, Duration).
set_down(A, O, L)terminates directly_holding(A, O)if enabled_durative_action_set_down(A, O, L, Duration).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
 /*   [ domain(flp),
          [ 'durative-action',
            carry,
            ':parameters',
            [ A,
               (-),
              intelligentagent,
              O,
               (-),
              object,
              L0,
              L1,
               (-),
              physicallocation,
              M,
               (-),
              modeoftransportation
            ],
            ':duration',
            [=, Duration, ['travel-duration', M, L0, L1]],
            ':condition',
            [ and,
              [over, all, ['walking-p', M]],
              [over, all, ['travel-path', M, L0, L1]],
              [over, all, [autonomous, A]],
              [over, all, [mobile, O]],
              [over, all, ['directly-holding', A, O]],
              [at, start, [location, A, L0]],
              [at, start, [location, O, L0]]
            ],
            ':effect',
            [ and,
              [at, end, [not, [location, A, L0]]],
              [at, end, [not, [location, O, L0]]],
              [at, end, [location, A, L1]],
              [at, end, [location, O, L1]]
            ]
          ]
        ].
 */
fluents enabled_durative_action_carry(A, O, L0, L1, M, Duration).
actions carry(A, O, L0, L1, M).
enabled_durative_action_carry(A, O, L0, L1, M, Duration)at T0 if isa(A, intelligentagent), isa(O, object), isa(L0, physicallocation), isa(L1, physicallocation), isa(M, modeoftransportation), walking_p(M)from T0 to T9, travel_path(M, L0, L1)from T0 to T9, autonomous(A)from T0 to T9, mobile(O)from T0 to T9, directly_holding(A, O)from T0 to T9, location(A, L0)at T0, location(O, L0)at T0.
carry(A, O, L0, L1, M)initiates location(A, L1)if enabled_durative_action_carry(A, O, L0, L1, M, Duration).
carry(A, O, L0, L1, M)initiates location(O, L1)if enabled_durative_action_carry(A, O, L0, L1, M, Duration).
carry(A, O, L0, L1, M)terminates location(A, L0)if enabled_durative_action_carry(A, O, L0, L1, M, Duration).
carry(A, O, L0, L1, M)terminates location(O, L0)if enabled_durative_action_carry(A, O, L0, L1, M, Duration).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
 /*   [ domain(flp),
          [ 'durative-action',
            'place-into',
            ':parameters',
            [ A,
               (-),
              intelligentagent,
              O,
               (-),
              object,
              C,
               (-),
              container,
              L,
               (-),
              physicallocation
            ],
            ':duration',
            [=, Duration, 0],
            ':condition',
            [ and,
              [over, all, [autonomous, A]],
              [over, all, [mobile, O]],
              [at, start, ['directly-holding', A, O]],
              [at, start, [location, A, L]],
              [at, start, [location, O, L]],
              [at, start, [location, C, L]],
              [at, start, [not, [contains, C, O]]]
            ],
            ':effect',
            [ and,
              [at, end, [contains, C, O]],
              [at, end, [not, ['directly-holding', A, O]]]
            ]
          ]
        ].
 */
fluents enabled_durative_action_place_into(A, O, C, L, Duration).
actions place_into(A, O, C, L).
enabled_durative_action_place_into(A, O, C, L, Duration)at T0 if isa(A, intelligentagent), isa(O, object), isa(C, container), isa(L, physicallocation), autonomous(A)from T0 to T9, mobile(O)from T0 to T9, directly_holding(A, O)at T0, location(A, L)at T0, location(O, L)at T0, location(C, L)at T0, not contains(C, O)at T0.
place_into(A, O, C, L)initiates contains(C, O)if enabled_durative_action_place_into(A, O, C, L, Duration).
place_into(A, O, C, L)terminates directly_holding(A, O)if enabled_durative_action_place_into(A, O, C, L, Duration).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
 /*   [ domain(flp),
          [ 'durative-action',
            drive,
            ':parameters',
            [ A,
               (-),
              intelligentagent,
              V,
               (-),
              vehicle,
              L0,
              L1,
               (-),
              physicallocation,
              M,
               (-),
              modeoftransportation
            ],
            ':duration',
            [=, Duration, ['travel-duration', M, L0, L1]],
            ':condition',
            [ and,
              [over, all, ['driving-p', M]],
              [over, all, ['travel-path', M, L0, L1]],
              [over, all, [autonomous, A]],
              [over, all, [mobile, V]],
              [at, start, [location, A, L0]],
              [at, start, [location, V, L0]]
            ],
            ':effect',
            [ and,
              [at, end, [not, [location, A, L0]]],
              [at, end, [not, [location, V, L0]]],
              [at, end, [location, A, L1]],
              [at, end, [location, V, L1]]
            ]
          ]
        ].
 */
fluents enabled_durative_action_drive(A, V, L0, L1, M, Duration).
actions drive(A, V, L0, L1, M).
enabled_durative_action_drive(A, V, L0, L1, M, Duration)at T0 if isa(A, intelligentagent), isa(V, vehicle), isa(L0, physicallocation), isa(L1, physicallocation), isa(M, modeoftransportation), driving_p(M)from T0 to T9, travel_path(M, L0, L1)from T0 to T9, autonomous(A)from T0 to T9, mobile(V)from T0 to T9, location(A, L0)at T0, location(V, L0)at T0.
drive(A, V, L0, L1, M)initiates location(A, L1)if enabled_durative_action_drive(A, V, L0, L1, M, Duration).
drive(A, V, L0, L1, M)initiates location(V, L1)if enabled_durative_action_drive(A, V, L0, L1, M, Duration).
drive(A, V, L0, L1, M)terminates location(A, L0)if enabled_durative_action_drive(A, V, L0, L1, M, Duration).
drive(A, V, L0, L1, M)terminates location(V, L0)if enabled_durative_action_drive(A, V, L0, L1, M, Duration).
 %   [].
% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
% Translated:
%
% [ define,
%   [domain,flp],
%   [':requirements',':negative-preconditions',':conditional-effects',':equality',':typing',':fluents',':durative-actions',':derived-predicates'],
%   [':types',intelligentagent,residence,vehicle,tool,container,-,object,modeoftransportation,-,category,object,physicallocation,-,thing,person,-,intelligentagent,residence,-,physicallocation,vehicle,-,container],
%   [ ':predicates',
%     [autonomous,A,-,intelligentagent],
%     [location,O,-,object,L,-,physicallocation],
%     [contains,C,-,container,O,-,object],
%     [mobile,Ob,-,object],
%     ['directly-holding',A,-,intelligentagent,O,-,object],
%     ['travel-path',M,-,modeoftransportation,L0,L1,-,physicallocation],
%     ['driving-p',M,-,modeoftransportation],
%     ['walking-p',M,-,modeoftransportation] ],
%   [ ':functions',
%     ['travel-distance',M,-,modeoftransportation,L0,L1,-,physicallocation],
%     ['travel-duration',M,-,modeoftransportation,L0,L1,-,physicallocation] ],
%   [ ':durative-action', walk, ':parameters',
%               [A,-,intelligentagent,L0,L1,-,physicallocation,M,-,modeoftransportation], ':duration', [=,Duration,['travel-duration',M,L0,L1]], ':condition',
%                                             [ and,
%                                               [ over,
%                                                 all,
%                                                 ['walking-p',M] ],
%                                               [ over,
%                                                 all,
%                                                 ['travel-path',M,L0,L1] ],
%                                               [ over,
%                                                 all,
%                                                 [autonomous,A] ],
%                                               [ at,
%                                                 start,
%                                                 [location,A,L0] ] ],
%                                             ':effect',
%                                             [ and,
%                                               [ at,
%                                                 end,
%                                                 [ not,
%                                                   [location,A,L0] ] ],
%                                               [ at,
%                                                 end,
%                                                 [location,A,L1] ] ] ],
%   [ ':durative-action', 'pick-up', ':parameters',
%                [A,-,intelligentagent,O,-,object,L,-,physicallocation], ':duration', [=,Duration,0], ':condition',
%                                     [ and,
%                                       [ over,
%                                         all,
%                                         [autonomous,A] ],
%                                       [ over,
%                                         all,
%                                         [mobile,O] ],
%                                       [ at,
%                                         start,
%                                         [ not,
%                                           ['directly-holding',A,O] ] ],
%                                       [ at,
%                                         start,
%                                         [location,A,L] ],
%                                       [ at,
%                                         start,
%                                         [location,O,L] ] ],
%                                     ':effect',
%                                     [ and,
%                                       [ at,
%                                         end,
%                                         ['directly-holding',A,O] ] ] ],
%   [ ':durative-action', 'set-down', ':parameters',
%                [A,-,intelligentagent,O,-,object,L,-,physicallocation], ':duration', [=,Duration,0], ':condition',
%                                     [ and,
%                                       [ over,
%                                         all,
%                                         [autonomous,A] ],
%                                       [ over,
%                                         all,
%                                         [mobile,O] ],
%                                       [ at,
%                                         start,
%                                         ['directly-holding',A,O] ],
%                                       [ at,
%                                         start,
%                                         [location,A,L] ] ],
%                                     ':effect',
%                                     [ and,
%                                       [ at,
%                                         end,
%                                         [location,O,L] ],
%                                       [ at,
%                                         end,
%                                         [ not,
%                                           ['directly-holding',A,O] ] ] ] ],
%   [ ':durative-action', carry, ':parameters',
%               [A,-,intelligentagent,O,-,object,L0,L1,-,physicallocation,M,-,modeoftransportation], ':duration', [=,Duration,['travel-duration',M,L0,L1]], ':condition',
%                                               [ and,
%                                                 [ over,
%                                                   all,
%                                                   ['walking-p',M] ],
%                                                 [ over,
%                                                   all,
%                                                   ['travel-path',M,L0,L1] ],
%                                                 [ over,
%                                                   all,
%                                                   [autonomous,A] ],
%                                                 [ over,
%                                                   all,
%                                                   [mobile,O] ],
%                                                 [ over,
%                                                   all,
%                                                   ['directly-holding',A,O] ],
%                                                 [ at,
%                                                   start,
%                                                   [location,A,L0] ],
%                                                 [ at,
%                                                   start,
%                                                   [location,O,L0] ] ],
%                                               ':effect',
%                                               [ and,
%                                                 [ at,
%                                                   end,
%                                                   [ not,
%                                                     [location,A,L0] ] ],
%                                                 [ at,
%                                                   end,
%                                                   [ not,
%                                                     [location,O,L0] ] ],
%                                                 [ at,
%                                                   end,
%                                                   [location,A,L1] ],
%                                                 [ at,
%                                                   end,
%                                                   [location,O,L1] ] ] ],
%   [ ':durative-action', 'place-into', ':parameters',
%                [A,-,intelligentagent,O,-,object,C,-,container,L,-,physicallocation], ':duration', [=,Duration,0], ':condition',
%                                        [ and,
%                                          [ over,
%                                            all,
%                                            [autonomous,A] ],
%                                          [ over,
%                                            all,
%                                            [mobile,O] ],
%                                          [ at,
%                                            start,
%                                            ['directly-holding',A,O] ],
%                                          [ at,
%                                            start,
%                                            [location,A,L] ],
%                                          [ at,
%                                            start,
%                                            [location,O,L] ],
%                                          [ at,
%                                            start,
%                                            [location,C,L] ],
%                                          [ at,
%                                            start,
%                                            [ not,
%                                              [contains,C,O] ] ] ],
%                                        ':effect',
%                                        [ and,
%                                          [ at,
%                                            end,
%                                            [contains,C,O] ],
%                                          [ at,
%                                            end,
%                                            [ not,
%                                              ['directly-holding',A,O] ] ] ] ],
%   [ ':durative-action', drive, ':parameters',
%               [A,-,intelligentagent,V,-,vehicle,L0,L1,-,physicallocation,M,-,modeoftransportation], ':duration', [=,Duration,['travel-duration',M,L0,L1]], ':condition',
%                                                [ and,
%                                                  [ over,
%                                                    all,
%                                                    ['driving-p',M] ],
%                                                  [ over,
%                                                    all,
%                                                    ['travel-path',M,L0,L1] ],
%                                                  [ over,
%                                                    all,
%                                                    [autonomous,A] ],
%                                                  [ over,
%                                                    all,
%                                                    [mobile,V] ],
%                                                  [ at,
%                                                    start,
%                                                    [location,A,L0] ],
%                                                  [ at,
%                                                    start,
%                                                    [location,V,L0] ] ],
%                                                ':effect',
%                                                [ and,
%                                                  [ at,
%                                                    end,
%                                                    [ not,
%                                                      [location,A,L0] ] ],
%                                                  [ at,
%                                                    end,
%                                                    [ not,
%                                                      [location,V,L0] ] ],
%                                                  [ at,
%                                                    end,
%                                                    [location,A,L1] ],
%                                                  [ at,
%                                                    end,
%                                                    [location,V,L1] ] ] ] ].
% 1,079,308 inferences, 0.164 CPU in 0.176 seconds (93% CPU, 6576137 Lips)
% /mnt/sdc1/logicmoo_workspace.1/lib/swipl/library/prolog_xref loaded into prolog_xref 0.00 sec, 0 clauses
% /mnt/sdc1/logicmoo_workspace.1/lib/swipl/library/pldoc/doc_html loaded into pldoc_html 0.00 sec, 0 clauses
% /mnt/sdc1/logicmoo_workspace.1/lib/swipl/library/prolog_colour loaded into prolog_colour 0.00 sec, 0 clauses
% Updating GIT version stamps in the background.
% Updating GIT version stamps in the background.
% % From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.d.pddl:1
% include_e_lps_pddl_file(lps,ext('flp/worlds/flp/flp.p.pddl')).
% include_e_lps_pddl_file(lps,
%                       '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl').
% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
 %   [problem(flp1), [domain|flp]].
domain(flp).
 %   [problem(flp1), [object|typed(modeoftransportation, driving)]].
isa(driving, modeoftransportation).
 %   [problem(flp1), [object|typed(modeoftransportation, walking)]].
isa(walking, modeoftransportation).
 %   [problem(flp1), [object|typed(object, bluetoothkeyboard)]].
isa(bluetoothkeyboard, object).
 %   [problem(flp1), [object|typed(object, tissues)]].
isa(tissues, object).
 %   [problem(flp1), [object|typed(person, andrewdougherty)]].
isa(andrewdougherty, person).
 %   [problem(flp1), [object|typed(person, meredithmcghan)]].
isa(meredithmcghan, person).
 %   [problem(flp1), [object|typed(physicallocation, auroraillinois)]].
isa(auroraillinois, physicallocation).
 %   [problem(flp1), [object|typed(physicallocation, flintmichigan)]].
isa(flintmichigan, physicallocation).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
 /*   [ problem(flp1),
          [object|typed(residence, townhomeofeleanorandandrewandmeredith)]
        ].
 */
isa(townhomeofeleanorandandrewandmeredith, residence).
 %   [problem(flp1), [object|typed(tool, bluetoothkeyboard)]].
isa(bluetoothkeyboard, tool).
 %   [problem(flp1), [object|typed(vehicle, andrewdoughertyshypotheticalcar)]].
isa(andrewdoughertyshypotheticalcar, vehicle).
 %   [problem(flp1), [object|typed(vehicle, meredithmcghanscar)]].
isa(meredithmcghanscar, vehicle).
 %   [problem(flp1), [initially, autonomous, andrewdougherty]].
initially autonomous(andrewdougherty).
 %   [problem(flp1), [initially, autonomous, meredithmcghan]].
initially autonomous(meredithmcghan).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
 /*   [ problem(flp1),
          [ initially,
            location,
            andrewdougherty,
            townhomeofeleanorandandrewandmeredith
          ]
        ].
 */
initially location(andrewdougherty, townhomeofeleanorandandrewandmeredith).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
 /*   [ problem(flp1),
          [initially, location, andrewdoughertyshypotheticalcar, auroraillinois]
        ].
 */
initially location(andrewdoughertyshypotheticalcar, auroraillinois).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
 /*   [ problem(flp1),
          [ initially,
            location,
            bluetoothkeyboard,
            townhomeofeleanorandandrewandmeredith
          ]
        ].
 */
initially location(bluetoothkeyboard, townhomeofeleanorandandrewandmeredith).
 %   [problem(flp1), [initially, location, meredithmcghan, flintmichigan]].
initially location(meredithmcghan, flintmichigan).
 %   [problem(flp1), [initially, location, meredithmcghanscar, flintmichigan]].
initially location(meredithmcghanscar, flintmichigan).
 %   [problem(flp1), [initially, mobile, andrewdoughertyshypotheticalcar]].
initially mobile(andrewdoughertyshypotheticalcar).
 %   [problem(flp1), [initially, mobile, bluetoothkeyboard]].
initially mobile(bluetoothkeyboard).
 %   [problem(flp1), [initially, mobile, meredithmcghanscar]].
initially mobile(meredithmcghanscar).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
 /*   [ problem(flp1),
          [ initially,
            'travel-path',
            driving,
            auroraillinois,
            townhomeofeleanorandandrewandmeredith
          ]
        ].
 */
initially travel_path(driving, auroraillinois, townhomeofeleanorandandrewandmeredith).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
 /*   [ problem(flp1),
          [initially, 'travel-path', driving, flintmichigan, auroraillinois]
        ].
 */
initially travel_path(driving, flintmichigan, auroraillinois).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
 /*   [ problem(flp1),
          [ initially,
             (=),
            [ 'travel-distance',
              driving,
              auroraillinois,
              townhomeofeleanorandandrewandmeredith
            ],
            5
          ]
        ].
 */
initially travel_distance(driving, auroraillinois, townhomeofeleanorandandrewandmeredith)=5.

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
 /*   [ problem(flp1),
          [initially, =, ['travel-distance', driving, flintmichigan, auroraillinois], 500]
        ].
 */
initially travel_distance(driving, flintmichigan, auroraillinois)=500.

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
 /*   [ problem(flp1),
          [ initially,
             (=),
            [ 'travel-duration',
              driving,
              auroraillinois,
              townhomeofeleanorandandrewandmeredith
            ],
            0.15
          ]
        ].
 */
initially travel_duration(driving, auroraillinois, townhomeofeleanorandandrewandmeredith)=0.15.

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
 /*   [ problem(flp1),
          [initially, =, ['travel-duration', driving, flintmichigan, auroraillinois], 7]
        ].
 */
initially travel_duration(driving, flintmichigan, auroraillinois)=7.

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
 /*   [ problem(flp1),
          [goal, and, ['directly-holding', andrewdougherty, bluetoothkeyboard]]
        ].
 */
ec:demo_test(problem(flp1), lps_demo, directly_holding(andrewdougherty, bluetoothkeyboard)).
 %   [problem(flp1), [metric, minimize, ['total-time']]].
metric(pddl_minimize(pddl_total_time)).
 %   [].
 %  assert_1pddl([problem(flp1)],[]).
problem(flp1, []).
% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/flp/worlds/flp/flp.p.pddl:1
% Translated:
%
% [ define,
%   [problem,flp1],
%   [':domain',flp],
%   [':objects',driving,walking,-,modeoftransportation,bluetoothkeyboard,tissues,-,object,andrewdougherty,meredithmcghan,-,person,auroraillinois,flintmichigan,-,physicallocation,townhomeofeleanorandandrewandmeredith,-,residence,bluetoothkeyboard,-,tool,andrewdoughertyshypotheticalcar,meredithmcghanscar,-,vehicle],
%   [ ':init',
%     [autonomous,andrewdougherty],
%     [autonomous,meredithmcghan],
%     [location,andrewdougherty,townhomeofeleanorandandrewandmeredith],
%     [location,andrewdoughertyshypotheticalcar,auroraillinois],
%     [location,bluetoothkeyboard,townhomeofeleanorandandrewandmeredith],
%     [location,meredithmcghan,flintmichigan],
%     [location,meredithmcghanscar,flintmichigan],
%     [mobile,andrewdoughertyshypotheticalcar],
%     [mobile,bluetoothkeyboard],
%     [mobile,meredithmcghanscar],
%     ['travel-path',driving,auroraillinois,townhomeofeleanorandandrewandmeredith],
%     ['travel-path',driving,flintmichigan,auroraillinois],
%     [ =,
%       ['travel-distance',driving,auroraillinois,townhomeofeleanorandandrewandmeredith], 5 ],
%     [ =,
%       ['travel-distance',driving,flintmichigan,auroraillinois], 500 ],
%     [ =,
%       ['travel-duration',driving,auroraillinois,townhomeofeleanorandandrewandmeredith], 0.15 ],
%     [ =,
%       ['travel-duration',driving,flintmichigan,auroraillinois], 7 ] ],
%   [ ':goal',
%     [ and,
%       ['directly-holding',andrewdougherty,bluetoothkeyboard] ] ],
%   [ ':metric',
%     minimize,
%     ['total-time'] ] ].

:- multifile(ec:demo_test/3).
ec:demo_test(lps_demo_test_9_run, lps_demo, [holds(directly_holding(andrewdougherty, bluetoothkeyboard),8)]).
baseKB:lps_demo_test_9_run :- abdemo([directly_holding(andrewdougherty, bluetoothkeyboard)at 8]).

