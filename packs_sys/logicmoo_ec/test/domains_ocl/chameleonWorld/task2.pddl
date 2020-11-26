
(define (problem task2)
   (:domain chameleonWorld)
   (:objects
         door1 - door
         flexarium1 - flexarium
         veiledChameleon - chameleon
         box1 box2 - box
         newsPaper1 newsPaper2 - substrate
        )
    (:init
        (doorClosed door1)
        (inFlexarium veiledChameleon)
        (boxClosed box1)
        )
    (:goal
      (and
        (doorOpen door1)
        (inBox veiledChameleon box1)
        (boxClosed box1)
       ))
)

