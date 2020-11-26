(define (problem task14)
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
        (dirtyPlate bigPlate)
        (dirtyPlate smallPlate)
        (isCold bread)
        (location bread side)
        (isCold pita)
        (location pita fridge)
        (isCold crumpet)
        (location crumpet fridge)
        (inJar butter)
        (item butter nullItem)
        (inJar marmite)
        (item marmite nullItem)
        (inJar honey)
        (item honey nullItem)
        (dirty bigKnife)
        (dirty smallKnife)
        (notPluggedIn blueToaster)
        (next fridge side)
        (next side toaster)
        (next toaster side)
        (next side servePlate)
        (next servePlate side)
        )
    (:goal
      (and
        (isMade bread)
        (location bread servePlate)
        (onToast marmite)
        (item marmite bread)
        (isMade pita)
        (location pita servePlate)
        (onToast honey)
        (item honey pita)
       ))
)

