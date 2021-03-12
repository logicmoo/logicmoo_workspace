(define (domain midominio)

(:requirements
:typing 
:fluents 
:htn-expansion 
:durative-actions 
:negative-preconditions
:universal-preconditions
:disjuntive-preconditions
:derived-predicates
:metatags)


(:types
parameter - object
activity - object
participant - object
lane - object
boolean - object
)

(:constants
 true false - boolean
a0 a2 a4 a5 a6 a9 a8 a11 a12 a14 a15 a17 a19 a20 a1 - activity
Optimize - parameter
Training Authoring Formatting GraphicDesign Administration Quality  - lane
)

(:predicates
(completed ?a - activity)
(belongs_to_lane ?p - participant ?a - lane)
(value ?x - parameter ?v - boolean)
)


(:durative-action A0
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'StartEvent' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD StartEvent")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "1")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 1.0)
:condition (belongs_to_lane ?w Training)
:effect (completed a0))

(:durative-action A1
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'EndEvent' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD EndEvent")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "2")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 1.0)
:condition(belongs_to_lane ?w Administration)
:effect (completed a1))

(:durative-action A2
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'TrainingAuthors' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD TrainingAuthors")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "3")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 20.0)
:condition(belongs_to_lane ?w Training)
:effect (completed a2))

(:durative-action A4
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'TrainingTutorsLMS' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD TrainingTutorsLMS")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "4")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 20.0)
:condition(belongs_to_lane ?w Training)
:effect (completed a4))

(:durative-action A5
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'ContentAuthoring' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD ContentAuthoring")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "5")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 20.0)
:condition(belongs_to_lane ?w Authoring)
:effect (completed a5))

(:durative-action A6
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'ContentProcessing' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD ContentProcessing")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "6")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 20.0)
:condition(belongs_to_lane ?w Formatting)
:effect (completed a6))

(:durative-action A8
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'Optimization' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD Optimization")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "7")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 20.0)
:condition(belongs_to_lane ?w GraphicDesign)
:effect (completed a8))

(:durative-action A9
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'Creation' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD Creation")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "8")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 20.0)
:condition(belongs_to_lane ?w GraphicDesign)
:effect (completed a9))

(:durative-action A11
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'CSS' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD CSS")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "9")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 20.0)
:condition(belongs_to_lane ?w GraphicDesign)
:effect (completed a11))

(:durative-action A12
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'FlashAnimation' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD FlashAnimation")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "10")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 20.0)
:condition(belongs_to_lane ?w GraphicDesign)
:effect (completed a12))

(:durative-action A14
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'AuthorRevision' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD AuthorRevision")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "11")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 20.0)
:condition(belongs_to_lane ?w Authoring)
:effect (completed a14))

(:durative-action A15
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'QualityRevision' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD QualityRevision")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "12")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 10.0)
:condition(belongs_to_lane ?w Quality)
:effect (completed a15))

(:durative-action A17
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'AssemblyLMS' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD AssemblyLMS")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "13")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 20.0)
:condition(belongs_to_lane ?w Formatting)
:effect (completed a17))

(:durative-action A19
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'Registration' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD Registration")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "14")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 5.0)
:condition(belongs_to_lane ?w Administration)
:effect (completed a19))

(:durative-action A20
:parameters(?w - participant)
:meta (
(:tag prettyprint  "START: ?start; | END: ?end; | DURATION: ?duration; |   'Notification' ALLOCATED TO '?w' ")
(:tag short "ACTIVIDAD Notification")
(:tag resource "?w")
(:tag monitoring "manual")
(:tag UID "15")
(:tag Type "0")
(:tag OutlineLevel "1")
(:tag OutlineNumber "1")
(:tag WBS "1")
(:tag Summary "0")
)
:duration (= ?duration 5.0)
:condition(belongs_to_lane ?w Administration)
:effect (completed a20))
(:task BlockSB2
:parameters ()
(:method blsb2
:precondition ()
:tasks ((A0 ?w1) (A2 ?w2) (BlockPB3) (A20 ?w3) (A1 ?w4) )
))

(:task BlockSB1
:parameters ()
(:method blsb1
:precondition ()
:tasks ((A5 ?w1) (A6 ?w2) (BlockPB1 Optimize) (A11 ?w3) (A12 ?w4) (BlockPB2) )
))

(:task BlockPB3
:parameters ()
(:method blpb3
:precondition ()
:tasks ([(A4 ?w1) (BlockSB1) ](A19 ?w2))
))

(:task BlockPB1
:parameters (?x - parameter)
(:method if_A9
:precondition (value ?x false)
:tasks (A9 ?w1))
(:method if_A8
:precondition (value ?x true)
:tasks (A8 ?w1))
)

(:task BlockPB2
:parameters ()
(:method blpb2
:precondition ()
:tasks ([(A14 ?w1) (A15 ?w2) ](A17 ?w3))
))


)