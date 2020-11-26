(define (problem task12)
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
        (cleanPlate bigPlate)
        (cleanPlate smallPlate)
        (isCold bread)
        (location bread fridge)
        (isCold pita)
        (location pita fridge)
        (inJar butter)
        (item butter nullItem)
        (inJar marmite)
        (item marmite nullItem)
        (inJar honey)
        (item honey nullItem)
        (clean smallKnife)
        (clean bigKnife)
        (isCold crumpet)
        (location crumpet fridge)
        (notPluggedIn blueToaster)
        (next fridge side)
        (next side toaster)
        (next toaster side)
        (next side servePlate)
        (next servePlate side)
        )
    (:goal
      (and
        (isToasted pita)
        (location pita side)
       ))
)

