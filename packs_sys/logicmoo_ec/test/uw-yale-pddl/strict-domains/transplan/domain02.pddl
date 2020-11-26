(define (domain simple-transplan)
   (:extends transplan)

   (:axiom
      :vars (?p - package)
      :context (and)
      :implies (volume ?p 0))

   (:axiom
       :vars (?x - (either vehicle depot)
	      ?c - compartment)
       :implies (contains ?x ?c 0))

   (:axiom
       :vars (?x - (either vehicle depot)
	      ?c - compartment)
       :implies (anon-volume ?x ?c 0))

   (:axiom
      :vars (?v - vehicle)
      :context (and)
      :implies (mass ?v 0))
   (:axiom
      :vars (?tr - train)
      :context (and)
      :implies (locomotive-mass ?tr 0))
   (:axiom
      :vars (?x - (either vehicle depot) ?c - compartment)
      :context (and)
      :implies (capacity ?x ?c 0))
   (:axiom
      :vars (?v - vehicle ?r - route)
      :context (and)
      :implies (fuel-rate ?v ?r 0))
   (:axiom
      :vars (?v - vehicle ?r - route)
      :context (and)
      :implies (fuel-waste ?v ?r 0))
   (:axiom
      :vars (?p - vehicle)
      :context (and)
      :implies (water-rate ?p 0))
)