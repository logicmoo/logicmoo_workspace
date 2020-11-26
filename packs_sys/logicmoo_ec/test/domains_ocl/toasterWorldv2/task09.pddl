(define (problem task09)
   (:domain nonetw)
   (:objects
         bigPlate smallPlate - plates
         bread pita crumpet nullItem - item
         fridge side toaster servePlate - loc
         butter marmite honey - spreads
         smallKnife bigKnife - kitchenware
         blueToaster - toaster
        )
    (:init
        (isCold bread)
        (location bread fridge)
        (isCold pita)
        (location pita side)
        (isToasted crumpet)
        (location crumpet side)
        (cleanPlate bigPlate)
        (dirtyPlate smallPlate)
        (dirty smallKnife)
        (clean bigKnife)
        (notPluggedIn blueToaster)
        (next fridge side)
        (next side toaster)
        (next toaster side)
        (next side servePlate)
        (next servePlate side)
        )
    (:goal
      (and
        (clean bigKnife)
       ))
)

