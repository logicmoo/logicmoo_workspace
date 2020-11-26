(define (domain game)
  (:requirements :strips :equality :typing :conditional-effects)
(:types player monster treasure key weapon protection room passage)
  (:predicates 
              (roompos ?x1 - player ?x2 - room)
              (passage_pos ?x1 - player ?x2 - room ?x3 - passage)
              (in_room ?x1 - player ?x2 - room)
              (asleep ?x1 - monster)
              (awake ?x1 - monster)
              (killed ?x1 - monster ?x2 - player)
              (on_ground ?x1 - treasure ?x2 - room)
              (picked_up ?x1 - treasure ?x2 - player)
              (open ?x1 - passage)
              (locked ?x1 - passage ?x2 - key)
              (guarded ?x1 - passage ?x2 - monster)
              (contains ?x1 - room ?x2 - passage)
              (keypos ?x1 - key ?x2 - room ?x3 - passage)
              (in_use ?x1 - key ?x2 - player)
              (spent ?x1 - treasure)
              (weilding ?x1 - weapon ?x2 - player)
              (destroyed ?x1 - weapon)
              (protection_in_use ?x1 - protection ?x2 - player)
              (used_up ?x1 - protection)
              (in ?x1 - protection ?x2 - room)
   )



  (:action exchange
         :parameters ( ?x1 - player ?x2 - room ?x3 - passage ?x4 - treasure ?x5 - key)
         :precondition (and 
              (passage_pos ?x1 ?x2 ?x3)
              (contains ?x2 ?x3)
              (open ?x3)
              (picked_up ?x4 ?x1)
              (keypos ?x5 ?x2 ?x3)
              )
         :effect (and 
              (spent ?x4)
              (not (picked_up ?x4 ?x1))
              (in_use ?x5 ?x1)
              (not (keypos ?x5 ?x2 ?x3))
              )   )


  (:action fight_and_kill_monster
         :parameters ( ?x1 - player ?x2 - room ?x3 - passage ?x4 - protection ?x5 - weapon ?x6 - monster)
         :precondition (and 
              (roompos ?x1 ?x2)
              (contains ?x2 ?x3)
              (protection_in_use ?x4 ?x1)
              (weilding ?x5 ?x1)
              (awake ?x6)
              (guarded ?x3 ?x6)
              )
         :effect (and 
              (used_up ?x4)
              (not (protection_in_use ?x4 ?x1))
              (destroyed ?x5)
              (not (weilding ?x5 ?x1))
              (killed ?x6 ?x1)
              (not (awake ?x6))
              (open ?x3)
              (not (guarded ?x3 ?x6))
              )   )


  (:action get_protection
         :parameters ( ?x1 - player ?x2 - room ?x3 - protection)
         :precondition (and 
              (roompos ?x1 ?x2)
              (in ?x3 ?x2)
              )
         :effect (and 
              (protection_in_use ?x3 ?x1)
              (not (in ?x3 ?x2))
              )   )


  (:action kill_sleeping_monster
         :parameters ( ?x1 - player ?x2 - room ?x3 - passage ?x4 - weapon ?x5 - monster)
         :precondition (and 
              (roompos ?x1 ?x2)
              (contains ?x2 ?x3)
              (weilding ?x4 ?x1)
              (asleep ?x5)
              (guarded ?x3 ?x5)
              )
         :effect (and 
              (destroyed ?x4)
              (not (weilding ?x4 ?x1))
              (killed ?x5 ?x1)
              (not (asleep ?x5))
              (open ?x3)
              (not (guarded ?x3 ?x5))
              )   )


  (:action move_into_room
         :parameters ( ?x1 - passage ?x2 - player ?x3 - room ?x4 - room)
         :precondition (and 
              (open ?x1)
              (passage_pos ?x2 ?x3 ?x1)
              (contains ?x3 ?x1)
              (contains ?x4 ?x1)
              )
         :effect (and 
              (roompos ?x2 ?x4)
              (not (passage_pos ?x2 ?x3 ?x1))
              )   )


  (:action move_to_passage
         :parameters ( ?x1 - passage ?x2 - player ?x3 - room)
         :precondition (and 
              (open ?x1)
              (contains ?x3 ?x1)
              (roompos ?x2 ?x3)
              )
         :effect (and 
              (passage_pos ?x2 ?x3 ?x1)
              (not (roompos ?x2 ?x3))
              )   )


  (:action pick_up_treasure
         :parameters ( ?x1 - player ?x2 - room ?x3 - treasure)
         :precondition (and 
              (roompos ?x1 ?x2)
              (on_ground ?x3 ?x2)
              )
         :effect (and 
              (picked_up ?x3 ?x1)
              (not (on_ground ?x3 ?x2))
              )   )


  (:action unlock_passage
         :parameters ( ?x1 - player ?x2 - room ?x3 - passage ?x4 - key)
         :precondition (and 
              (roompos ?x1 ?x2)
              (contains ?x2 ?x3)
              (in_use ?x4 ?x1)
              (locked ?x3 ?x4)
              )
         :effect (and 
              (open ?x3)
              (not (locked ?x3 ?x4))
              )   )
)

