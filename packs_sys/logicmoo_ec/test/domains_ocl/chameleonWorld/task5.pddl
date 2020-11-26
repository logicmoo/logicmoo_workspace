(define (problem task5)
   (:domain chameleonWorld)
   (:objects
         door1 - door
         flexarium1 - flexarium
         veiledChameleon - chameleon
         box1 box2 - box
         newsPaper1 newsPaper2 - substrate
        )
    (:init
        (doorOpen door1)
        (boxOpen box1)
        (boxOpen box2)
        )
    (:goal
      (and
        (doorClosed door1)
        (boxClosed box1)
        (boxClosed box2)
       ))
)

