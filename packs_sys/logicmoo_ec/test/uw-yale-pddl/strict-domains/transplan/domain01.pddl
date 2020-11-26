(define (domain transplan)
   (:requirements :dag-expansions :typing :domain-axioms :equality
		  :disjunctive-preconditions :existential-preconditions
		  :fluents :conditional-effects)
   (:types package package-shape vehicle-specialty kind-of-stuff
	   - object
	   equipment - package
             chute ramp crane plane-ramp - equipment

           ; The place hierarchy:
	   place - object
             region 
  	     city 
             location - place
                transport-center - location
	          airport train-station - transport-center
	        non-transport-center - location
                  street-address post-office depot - non-transport-center

           route - object
              air-route road-route rail-route - route

	   vehicle - object
             airplane truck train traincar       - vehicle
	   
	   compartment  - object) ; e.g., cargo-area vs. gas-tank

   (:constants
       water fuel items - kind-of-stuff
       bulky liquid granular loose mail livestock auto equip - package-shape
       tanker hopper flatbed livestock-carrier car-carrier none mail-carrier
          - vehicle-specialty
       elapsed-time - (fluent number)
       no-package - package  ; used for liquid-transfers not corresponding to
                            ; a real package.
       cargo-area gas-tank water-tank fuel-storage water-storage storage-tank
       - compartment) 
; Note that compartments are "generic," i.e., not names of actual
; individual objects.  You always have to mention the vehicle or depot
; to pick out a particular compartment.  However, the list above may
; not be exhaustive.

   (:predicates
       (can-carry ?v - vehicle  
		  ?p - package)
       (can-travel ?v - vehicle
		   ?r - route)
       (armored ?v - vehicle)   ; What's this for?
       (refrigerated ?v - vehicle)
       (perishable ?p - package)
       (valuable ?p - package)
       (hazardous ?p - package)
       (local ?r - route)   ; True if not suitable for long-range transport
       (shape ?p - package ?s - package-shape)
       (stuff ?p - package ?s - kind-of-stuff)  ; liquids and granular only
       (specialty ?v - vehicle ?s - vehicle-specialty)

       (volume ?p - package ?v - number)
       (mass ?v - vehicle ?m - number)
       ; A train's fuel capacity etc. are really the fuel capacity etc. of
       ; it's locomotive.  However, for mass we need to separate out the
       ; mass of the locomotive, because the mass of the train is the sum
       ; of the masses of the cars.
       (locomotive-mass ?tr - train ?m - number) ; kg
       ; Total mass is a fluent because for a train it can change
       (total-mass ?v - vehicle ?m - (expression (fluent number)))

       (capacity ?v - (either vehicle depot) ?c - compartment
		 ?n - number) ; cubic meters
       (fuel-rate ?v - vehicle ?r - route ?rate - number)
	    ; liter per km per kg
       (fuel-waste ?v - vehicle ?r - route ?w - number) ; Extra to start 
       (speed ?v - vehicle ?r - route ?s - number)       ; km/hr
       (latency ?v - vehicle ?r - route ?l - number)  ; Extra to start
       (water-rate ?p - vehicle ?rate - number)
                ; liter per hr per cubic meter of animals (!)

       ; These are used several times over, and so are defined as "macros"
       ; ?p-vol is the volume of the package; ?v-cargo is the current
       ; volume of the cargo.
       (load-feasible ?p - package ?v - vehicle ?l - location
		      ?p-vol - number ?v-cargo - (fluent number))
       (unload-feasible ?p - package ?v - vehicle ?l - location
		      ?p-vol - number ?v-cargo - (fluent number))
		     

       (city-allows-hazardous ?c - city)
       (pv-compatible ?pt - package-shape ?vt - vehicle-specialty)

       (in ?l1 ?l2 - place)
       (connects ?r - route ?l1 ?l2 - place ?distance - number)
       (serves ?tc - transport-center ?g - (either region city))
       (hub ?tc - transport-center)
       (same-tcenter-type ?tc1 ?tc2 - transport-center)

       (at ?x - (either equipment package vehicle)
	   ?l - location)  ; you can't be at a region
       (aboard ?p - package ?v - vehicle)
       (holding ?c - crane ?p - package)
       (attached ?tc ?tr - vehicle)
		    ; only traincars and trains 
       (alive ?p - package)

       (contains ?v - (either depot vehicle)
		 ?c - compartment
		 ?f - (fluent number))

       ; container contains a nonzero amount of stuff of kind ?k:
       (contains-kind ?v - (either depot vehicle) ?c - compartment
		      ?k - kind-of-stuff)
       ; Only one kind of stuff at a time.
       (container ?p - package ?c - compartment)  ; for liquids
       ; Volume of liquid not accounted for by named packages
       (anon-volume ?v - (either depot vehicle)
		    ?c - compartment
		    ?f - (fluent number))

       (available ?x - (either transport-center route vehicle equipment))

       (chute-connected ?c - chute ?v - vehicle)
       (ramp-connected ?r - plane-ramp ?a - airplane)
       (ramp-down ?tr - (either truck traincar))


       (door-open ?d - vehicle)
       (valve-open ?d - vehicle)
       (empty ?x - crane))

   ; All roads are 2-way
   (:axiom
       :vars (?r - route ?l1 ?l2 - location ?d - number)
       :context (connects ?r ?l1 ?l2 ?d)
       :implies (connects ?r ?l2 ?l1 ?d))

   (:axiom
       :vars (?p - package ?v - vehicle)
       :context (and (valuable ?p)
		     (armored ?v))
       :implies (can-carry ?v ?p))

   (:axiom
       :vars (?p - package ?v - vehicle)
       :context (and (perishable ?p)
		     (refrigerated ?v))
       :implies (can-carry ?v ?p))

   (:axiom
       :vars (?p - package ?v - vehicle
	      ?sh - package-shape ?vs - vehicle-specialty)
       :context (and (shape ?p ?sh)
		     (specialty ?v ?vs)
		     (pv-compatible ?sh ?vs))
       :implies (can-carry ?v ?p))

   (:axiom
      :vars (?p - package)
      :context (equipment ?p)
      :implies (shape ?p equip))

   (:axiom
      :vars (?x ?y - transport-center)
      :context (and (airport ?x) (airport ?y))
      :implies (same-tcenter-type ?x ?y))
   
   (:axiom
      :vars (?x ?y - transport-center)
      :context (and (train-station ?x) (train-station ?y))
      :implies (same-tcenter-type ?x ?y))

   (:axiom
      :vars (?v - vehicle ?r - route)
      :context (and (road-route ?r) (truck ?v))
      :implies (can-travel ?v ?r))

   (:axiom
      :vars (?v - vehicle ?r - route)
      :context (and (air-route ?r) (airplane ?v))
      :implies (can-travel ?v ?r))

   (:axiom
      :vars (?v - vehicle ?r - route)
      :context (and (rail-route ?r) (train ?v))
      :implies (can-travel ?v ?r))

   (:axiom
       :vars (?p - package ?v - vehicle ?l - location
	      ?p-vol ?v-cap - number ?v-cargo - (fluent number))
       :context (and (at ?p ?l)
		     (at ?v ?l)
		     (capacity ?v cargo-area ?v-cap)
		     (contains ?v cargo-area ?v-cargo)
		     (volume ?p ?p-vol)
		     (fluent-test
		        (>= (- ?v-cap ?v-cargo)
			   ?p-vol)))
       :implies (load-feasible ?p ?v ?l ?p-vol ?v-cargo))

   (:axiom 
       :vars (?p - package ?v - traincar ?tr - train ?l - location
	      ?p-vol ?v-cap - number ?v-cargo - (fluent number))
       :context (and (at ?p ?l)
		     (attached ?v ?tr)
		     (at ?tr ?l)
		     (capacity ?v cargo-area ?v-cap)
		     (contains ?v cargo-area ?v-cargo)
		     (volume ?p ?p-vol)
		     (fluent-test
		        (>= (- ?v-cap ?v-cargo)
			   ?p-vol)))
       :implies (load-feasible ?p ?v ?l ?p-vol ?v-cargo))


   (:axiom
       :vars (?p - package ?v - vehicle ?l - location
	      ?p-vol - number ?v-cargo - (fluent number))
       :context (and (aboard ?p ?v)
		     (at ?v ?l)
		     (volume ?p ?p-vol)
		     (contains ?v cargo-area ?v-cargo))
       :implies (unload-feasible ?p ?v ?l ?p-vol ?v-cargo))

   (:axiom
       :vars (?p - package ?v - traincar ?tr - train ?l - location
	      ?p-vol - number ?v-cargo - (fluent number))
       :context (and (aboard ?p ?v)
		     (attached ?v ?tr)
		     (at ?tr ?l)
		     (volume ?p ?p-vol)
		     (contains ?v cargo-area ?v-cargo))
       :implies (unload-feasible ?p ?v ?l ?p-vol ?v-cargo))

   (:axiom
      :vars (?tr - train ?lm - number)
      :context (locomotive-mass ?tr ?lm)
      :implies (total-mass ?tr 
			   (+ ?lm
			      (sum (?tc - traincar ?tcm - number)
				   (and (attached ?tc ?tr)
					(mass ?tc ?tcm))
				   ?tcm))))

   (:axiom
      :vars (?v - vehicle ?m - number)
      :context (and (not (train ?v))
		    (mass ?v ?m))
      :implies (total-mass ?v ?m))

   (:axiom
      :vars (?tr - train ?tc - traincar ?x - package)
      :context (and (attached ?tc ?tr)
		    (aboard ?x ?tc))
      :implies (aboard ?x ?tr))

   (:axiom
       :vars (?x - package)
       :context (alive ?x)
       :implies (shape ?x livestock))

   (:timeless
       (pv-compatible bulky none)
       (pv-compatible liquid tanker)
       (pv-compatible granular none)
       (pv-compatible loose none)
       (pv-compatible mail mail-carrier)
       (pv-compatible granular hopper)
       (pv-compatible equip flatbed)
       (pv-compatible livestock livestock-carrier)
       (pv-compatible auto car-carrier)
       (pv-compatible bulky flatbed))

   ; *** PRIMITIVE actions ***

   (:action open-door
      :parameters (?v - vehicle)
      :effect (door-open ?v))
   (:action close-door
      :parameters (?v - vehicle)
      :effect (not (door-open ?v)))

; It would be nice if we could attach the preconditions and such to
; load instead of its instances.  But we get hierarchical interference
; problems if we do that.

   (:action load-items
      :parameters (?p - package ?v - vehicle)
      :vars (?l - location ?p-vol - number ?v-cargo - (fluent number))
      :precondition (and (load-feasible ?p ?v ?l ?p-vol ?v-cargo)
			 (or (fluent-test (= ?v-cargo 0))
			     (contains-kind ?v cargo-area items)))
      :effect (and (aboard ?p ?v)
		   (not (at ?p ?l))
		   (change ?v-cargo
			   (+ ?v-cargo ?p-vol))
		   (contains-kind ?v cargo-area items))
      :only-in-expansions t)

   (:action unload-items
      :parameters (?p - package ?v - vehicle)
      :vars (?l - location ?p-vol - number ?v-cargo - (fluent number))
      :precondition (unload-feasible ?p ?v ?l ?p-vol ?v-cargo)
      :effect (and (at ?p ?l)
		   (not (aboard ?p ?v))
		   (change ?v-cargo (- ?v-cargo ?p-vol))
		   (when (fluent-test (= ?v-cargo ?p-vol))
			 (not (contains-kind ?v cargo-area items))))
      :only-in-expansions t)

   (:action pick-up
      :parameters (?p - package ?c - crane)
      :vars (?l - location ?p-vol - number
	     ?v - vehicle ?v-cargo - (fluent number))
      :precondition (and (empty ?c)            
		         (at ?c ?l)
			 (or (at ?p ?l)
			     (unload-feasible ?p ?v ?l
					      ?p-vol ?v-cargo)))
      :effect (and (holding ?c ?p)
		   (not (empty ?c))
		   (when (at ?p ?l)
			 (not (at ?p ?l)))
		   (when (aboard ?p ?v)
			 (and (not (aboard ?p ?v))
			      (change ?v-cargo
				      (- ?v-cargo ?p-vol))
			      (when (= ?v-cargo ?p-vol)
				    (not (contains-kind
					    ?v cargo-area items))))))
      :only-in-expansions t)

   (:action put-down
      :parameters (?p - package ?d - (either vehicle location))
      :vars (?c - crane ?l - location ?p-vol ?d-cap - number
	     ?d-cargo - (fluent number))
      :precondition (and (holding ?c ?p)
			 (at ?c ?l)
			 (or (and (vehicle ?d)
				  (capacity ?d cargo-area ?d-cap)
				  (contains ?d cargo-area ?d-cargo)
				  (fluent-test
				     (> (- ?d-cap ?d-cargo)
					?p-vol))
				  (or (fluent-test (= ?d-cargo 0))
				      (contains-kind ?d cargo-area items)))
			     (and (place ?d)
				  (= ?d ?l))))
      :effect (and (empty ?c)
		   (when (vehicle ?d)
			 (and (aboard ?p ?d)
			      (contains-kind ?d cargo-area items)
			      (change ?d-cargo
				      (+ ?d-cargo ?p-vol))))
		   (when (place ?d)
			 (at ?p ?l)))
      :only-in-expansions t)

   (:action connect-chute
     :parameters (?c - chute ?v - vehicle)
     :vars (?l - location)
     :precondition (and (at ?c ?l)
			(at ?v ?l))
     :effect (and (chute-connected ?c ?v)
		  (not (at ?c ?l))))

   (:action disconnect-chute
      :parameters (?c - chute ?v - vehicle)
      :precondition (chute-connected ?c ?v)
      :effect (and (not (chute-connected ?c ?v))
		   (forall (?l - location)
		      (when (at ?v ?l)
			    (at ?c ?l)))))

   (:action fill-hopper
     :parameters (?p - package ?v - vehicle ?c - chute)
     :vars (?l - location ?p-vol - number ?v-cargo - (fluent number)
	    ?ks - kind-of-stuff)
     :precondition  (and (chute-connected ?c ?v)
			 (load-feasible ?p ?v ?l ?p-vol ?v-cargo)
			 (or (fluent-test (= ?v-cargo 0))
			     (and (stuff ?p ?ks)
				  (contains-kind ?v cargo-area ?ks))))
     :effect (and (aboard ?p ?v)
		  (not (at ?p ?l))
		  (change ?v-cargo
			  (+ ?v-cargo ?p-vol)))
     :only-in-expansions t)

   (:action empty-hopper
     :parameters (?p - package ?v - vehicle ?c - chute)
     :vars (?l - location ?p-vol - number ?v-cargo - (fluent number)
	    ?ks - kind-of-stuff)
     :precondition  (and (chute-connected ?c ?v)
			 (unload-feasible ?p ?v ?l ?p-vol ?v-cargo)
			 (contains-kind ?v cargo-area ?ks))
     :effect (and (at ?p ?l)
		  (not (aboard ?p ?v))
		  (change ?v-cargo
			  (- ?v-cargo ?p-vol))
		  (when (fluent-test (= ?v-cargo ?p-vol))
			(not (contains-kind ?v cargo-area ?ks))))
     :only-in-expansions t)

   (:action raise-ramp
       :parameters (?v - (either truck traincar))
       :effect (not (ramp-down ?v))
       :only-in-expansions t)

   (:action lower-ramp
       :parameters (?v - (either truck traincar))
       :effect (ramp-down ?v)
       :only-in-expansions t)

   (:action load-livestock
       :parameters (?p - package ?v - (either truck traincar))
       :vars (?l - location ?p-vol - number ?v-cargo - (fluent number))
       :precondition (and (load-feasible ?p ?v ?l ?p-vol ?v-cargo)
			  (ramp-down ?v)
			  (or (fluent-test (= ?v-cargo 0))
			      (contains-kind ?v cargo-area items)))
       :effect (and (aboard ?p ?v)
		    (not (at ?p ?l))
		    (change ?v-cargo
			    (+ ?v-cargo ?p-vol))
		    (contains-kind ?v cargo-area items))
       :only-in-expansions t)

   (:action unload-livestock
       :parameters (?p - package ?v - (either truck traincar))
       :vars (?l - location ?p-vol - number ?v-cargo - (fluent number))
       :precondition (and (unload-feasible ?p ?v ?l ?p-vol ?v-cargo)
			  (alive ?p)  ; If dead, can't be unloaded!
			  (ramp-down ?v))
       :effect (and (at ?p ?l)
		    (not (aboard ?p ?v))
		    (change ?v-cargo (- ?v-cargo ?p-vol))
		    (when (fluent-test (= ?v-cargo ?p-vol))
			  (not (contains-kind ?v cargo-area items))))
       :only-in-expansions t)

   (:action load-cars
       :parameters (?p - package ?v - (either truck traincar))
       :vars (?l - location ?p-vol - number ?v-cargo - (fluent number))
       :precondition (and (load-feasible ?p ?v ?l ?p-vol ?v-cargo)
			  (ramp-down ?v)
			  (or (fluent-test (= ?v-cargo 0))
			      (contains-kind ?v cargo-area items)))
       :effect (and (aboard ?p ?v)
		    (not (at ?p ?l))
		    (change ?v-cargo
			    (+ ?v-cargo ?p-vol))
		    (contains-kind ?v cargo-area items))
       :only-in-expansions t)

   (:action unload-cars
       :parameters (?p - package ?v - (either truck traincar))
       :vars (?l - location ?p-vol - number ?v-cargo - (fluent number))
       :precondition (and (unload-feasible ?p ?v ?l ?p-vol ?v-cargo)
			  (ramp-down ?v))
       :effect (and (at ?p ?l)
		    (not (aboard ?p ?v))
		    (change ?v-cargo (- ?v-cargo ?p-vol))
		    (when (fluent-test (= ?v-cargo ?p-vol))
			  (not (contains-kind ?v cargo-area items))))
       :only-in-expansions t)

   (:action move
       :parameters (?v - vehicle ?ol ?dl - location ?r - route)
       :vars (?dist - number ?fuel-needed ?v-fuel - (fluent number)
		             ?v-mass ?vr-waste ?vr-rate
			     ?vr-speed ?vr-latency ?delta-time - number)
       :precondition (and (at ?v ?ol)
			  (can-travel ?v ?r)
			  (or (connects ?r ?ol ?dl ?dist)
			      (and (truck ?v)
				   (exists (?ocity ?dcity - city)
				      (and (in ?ol ?ocity)
					   (in ?dl ?dcity)
					   (not (= ?ocity ?dcity))
					   (connects ?r ?ocity ?dcity ?dist)))))
			  (fuel-waste ?v ?r ?vr-waste)
			  (fuel-rate ?v ?r ?vr-rate)
       			  (total-mass ?v ?v-mass)
			  (= ?fuel-needed
			     (+ ?vr-waste
				(* ?v-mass
				   ?dist
				   ?vr-rate)))
			  (contains ?v gas-tank ?v-fuel)
			  (fluent-test (>= ?fuel-needed
					   (* 1000 ?v-fuel))) ; 1000 liter/m^3
			  (speed ?v ?r ?vr-speed)
			  (latency ?v ?r ?vr-latency)
			  (eval (+ ?vr-latency
				   (* ?dist ?vr-speed))
				?delta-time))
       :effect (and (at ?v ?dl)
		    (not (at ?v ?ol))
		    (when (train ?v)
			  (forall (?tc - traincar)
			     (when (attached ?tc ?v)
				   (and (at ?tc ?dl)
					(not (at ?tc ?ol))))))
		    (change elapsed-time
			    (+ elapsed-time
			       ?delta-time))
		    (change ?v-fuel
			    (- ?v-fuel
			       (/ ?fuel-needed 1000)))
		    (forall (?a - vehicle
			     ?cargo ?water-used ?water
			       - (fluent number)
			     ?water-rate - number)
		       (when (and (specialty ?a livestock-carrier)
				  (contains ?a water-tank ?water)
				  (contains ?a cargo-area ?cargo)
				  (water-rate ?a ?water-rate)
				  (= ?water-used
				     (* ?water-rate ?cargo ?delta-time)))
			     (and (when (fluent-test (> ?water-used
							(* 1000 ?water)))
					(and (change ?water 0)
					     (forall (?x - package)
						(when (aboard ?x ?a)
						      (not (alive ?x))))))
				  (when (fluent-test (<= ?water-used
							 (* 1000 ?water)))
					(change ?water
						(- ?water (/ ?water-used
							     1000))))))))
       :only-in-expansions t)

   (:action attach-train-car
       :parameters (?t - train ?c - traincar)
       :vars (?l - location)
       :precondition (and (at ?c ?l)  
			  (at ?t ?l))
       :effect (and (attached ?c ?t)
		    (not (at ?c ?l))))

   (:action detach-train-car
       :parameters (?t - train ?c - traincar)
       :vars (?l - location)
       :precondition (and (at ?t ?l)  
			  (attached ?c ?t))
       :effect (and (at ?c ?l)
		    (not (attached ?c ?t))))

   (:action attach-conveyor-ramp
       :parameters (?v - airplane ?r - plane-ramp)
       :vars (?l - location)
       :precondition (and (available ?r)   
			  (at ?r ?l)  
			  (at ?v ?l))
       :effect (and (ramp-connected ?r ?v)
		    (not (available ?r))))

   (:action detach-conveyor-ramp
       :parameters (?v - airplane ?r - plane-ramp)
       :vars (?l - location)
       :precondition (and (ramp-connected ?r ?v)  
			  (at ?r ?l)  
			  (at ?v ?l))
       :effect (and (available ?r)
		    (not (ramp-connected ?r ?v))))


   (:action liquid-transfer
       :parameters (?source - (either depot vehicle)
		    ?source-compartment - compartment
		    ?dest - (either depot vehicle)
		    ?dest-compartment - compartment
		    ?amt - number
		    ?p - package)
       :vars (?s-vol ?d-vol - (fluent number)
	      ?d-cap - number
	      ?fluid - kind-of-stuff)
       :precondition (and (or (and (depot ?source)
				   (vehicle ?dest)
				   (at ?dest ?source))
			      (and (vehicle ?source)
				   (depot ?dest)
				   (at ?source ?dest))
			      (and (vehicle ?source)
				   (vehicle ?dest)
				   (exists (?loc - location)
				      (and (at ?source ?loc)
					   (at ?dest ?loc)))))
			  (contains ?source ?source-compartment ?s-vol)
			  (contains ?dest ?dest-compartment ?d-vol)
			  (capacity ?dest ?dest-compartment ?d-cap)
			  (fluent-test
			     (>= (- ?d-cap ?d-vol)
				 ?amt))
			  (fluent-test
			     (>= ?s-vol ?amt))
			  (contains-kind ?source ?source-compartment ?fluid)
			  (or (and (= ?fluid fuel)
				   (not (contains-kind ?dest ?dest-compartment
						       water)))
			      (and (= ?fluid water)
				   (not (contains-kind ?dest ?dest-compartment
						       fuel))))
			  (or (not (= ?p no-package))
			      (exists (?s-a-vol ?d-a-vol - (fluent number))
				 (and (anon-volume ?source
						   ?source-compartment
						   ?s-a-vol)
				      (anon-volume ?dest
						   ?dest-compartment
						   ?d-a-vol)
				      (fluent-test (>= ?s-a-vol ?amt))))))
					
       :effect (and (change ?s-vol (- ?s-vol ?amt))
		    (change ?d-vol (+ ?d-vol ?amt))
		    (when (fluent-test (= ?s-vol ?amt))
			  (not (contains-kind
				  ?source ?source-compartment ?fluid)))
		    (when (fluent-test (> ?amt 0))
			  (contains-kind ?dest ?dest-compartment ?fluid))
		    (when (= ?p no-package)
			  (forall (?s-a-vol ?d-a-vol - (fluent number))
			     (when (and (anon-volume ?source
						     ?source-compartment
						     ?s-a-vol)
					(anon-volume ?dest
						     ?dest-compartment
						     ?d-a-vol))
				   (and (change ?s-a-vol (- ?s-a-vol ?amt))
					(change ?d-a-vol (+ ?d-a-vol ?amt))))))
		    (when (not (= ?p no-package))
			  (and (when (not (= ?source-compartment
					     ?dest-compartment))
				     (and (not (container
						  ?p ?source-compartment))
					  (container ?p ?dest-compartment)))
			       (when (not (= ?source ?dest))
				     (and (when (vehicle ?source)
						(not (aboard ?p ?source)))
					  (when (depot ?source)
						(not (at ?p ?source)))
					  (when (vehicle ?dest)
						(aboard ?p ?dest))
					  (when (depot ?dest)
						(at ?p ?dest))))))))

   ; *** EXPANDED actions ***

  ; The preconditions mentioned here are augmented by further preconditions
  ; specific to each expansion method.

  (:action load 
       :parameters (?p - package ?v - vehicle)
       :precondition (can-carry ?v ?p)
       :expansion :methods)

  (:action unload
      :parameters (?p - package ?v - vehicle)
      :precondition (aboard ?p ?v)
      :expansion :methods)

  (:action transport
      :parameters (?p - package ?orig ?dest - place)
      :expansion :methods)

  (:action TRANSPORT-DIRECT
      :parameters (?package - package ?origin ?destination - location)
      :expansion :methods
      :only-in-expansions t)

  (:action TRANSPORT-VIA-HUB
      :parameters (?package - package ?tcenter1 ?tcenter2 - transport-center)
      :expansion :methods
      :only-in-expansions t)

  (:action TRANSPORT-BETWEEN-TCENTERS
      :parameters (?package - package ?tcenter1 ?tcenter2 - transport-center)
      :expansion :methods
      :only-in-expansions t)

  (:action achieve-vehicle-at
      :parameters (?v - vehicle ?l - location)
      :expansion :methods)

  (:method load
      :name load-items
      :parameters (?p - package ?v - vehicle)
      :vars (?l - location)
      :precondition (and (at ?p ?l)
			 (at ?v ?l)
			 (specialty ?v none))
      :expansion (constrained (series (tag (achieve (door-open ?v))
					   (> e1))
				      (tag (< b2)
					   (load-items ?p ?v)
					   (> e2))
				      (tag (< b3)
					   (achieve (not (door-open ?v)))))
		     (in-context (series e1 b2)
			 :maintain (at ?p ?l))
		     (in-context (series e2 b3)
			 :maintain (aboard ?p ?v))
		     (in-context (series e1 b3)
			 :maintain (and (at ?v ?l)
					(door-open ?v)))))

  (:method unload
      :name unload-items
      :parameters (?p - package ?v - vehicle)
      :vars (?l - location)
      :precondition (and (specialty ?v none)
			 (aboard ?p ?v)
			 (at ?v ?l))
      :expansion (constrained (series (tag (achieve (door-open ?v))
					   (> e1))
				      (in-context (unload-items ?p ?v)
					  :precondition (aboard ?p ?v))
				      (tag (< b3)
					   (achieve (not (door-open ?v)))))
		    (in-context b3
		       :precondition (at ?v ?l))
		    (in-context (series e1 b3)
		       :maintain (door-open  ?v))))

  (:method load
      :name load-equipment
      :parameters (?p - package ?v - vehicle)
      :vars (?l - location)
      :precondition (and (specialty ?v flatbed) 
			 (at ?p ?l)
			 (at ?v ?l))
      :expansion
         (forsome (?c - crane)
	    (constrained (series (tag (< b1)
                                      (pick-up ?p ?c)
				      (> e1))
				 (tag (< b2)
				      (in-context (put-down ?p ?v)
					 :precondition (at   ?v ?l))))
               (in-context b1
                  :precondition 
	             (and (empty ?c)           
			  (at ?c ?l)))
	       (in-context (series e1 b2)
		   :maintain (and (holding ?c ?p)
				  (at ?c ?l))))))

;;; declare-method for unloading FLATBED truck or traincar
  (:method unload
      :name unload-equipment
      :parameters (?p - package ?v - vehicle)
      :vars (?l - location)
      :precondition (and (specialty ?v flatbed)
			 (aboard ?p ?v)  
			 (at ?v ?l))
      :expansion
         (forsome (?c - crane)
           (constrained (series (tag (< b1)
                                     (pick-up ?p ?c)
				     (> e1))
			        (tag (< b2)
				     (put-down ?p ?l)))
              (in-context b1
		 :precondition (and (at ?c ?l)  
			            (empty ?c)))
	      (in-context (series e1 b2)
		 :maintain (and (holding ?c ?p)
				(at ?c ?l))))))

  ;;; declare-method for loading HOPPER truck or traincar
  (:method load
      :name load-granular
      :parameters (?p - package ?v - vehicle)
      :vars (?l - location)
      :precondition (and (specialty ?v hopper)   
			 (at ?p ?l) 
			 (at ?v ?l))
      :expansion
         (forsome (?c - chute)
	    (constrained (series (tag (achieve (chute-connected ?c ?v))
				      (> e1))
				 (tag (< b2)
				      (fill-hopper ?p ?v ?c)
				      (> e2))
				 (tag (< b3)
				      (achieve (not (chute-connected ?c ?v)))))
	       (in-context b2
		  :precondition (at ?p ?l))
	       (in-context (series e2 b3)
		  :maintain (aboard ?p ?v))
	       (in-context b3
		  :precondition (at ?v ?l))
	       (in-context (series e1 b3)
		  :maintain (chute-connected ?c ?v)))))

;;; declare-method for unloading HOPPER truck or traincar
  (:method unload
      :name unload-granular
      :parameters (?p - package ?v - vehicle)
      :vars (?l - location)
      :precondition (and (specialty ?v hopper)
			 (aboard ?p ?v) 
			 (at ?v ?l))
      :expansion
         (forsome (?c - chute)
	    (constrained (series (tag (achieve (chute-connected ?c ?v))
				      (> e1))
				 (tag (< b2)
				      (empty-hopper ?p ?v ?c))
				 (tag (< b3)
				      (achieve (not (chute-connected ?c ?v)))))
	       (in-context b2
		  :precondition (aboard ?p ?v))
	       (in-context b3
		  :precondition (at ?v ?l))
	       (in-context (series e1 b3)
		  :maintain (chute-connected ?c ?v)))))

;;; declare-method for loading TANKER truck or traincar
  (:method load
      :name load-liquid
      :parameters (?p - package ?v - vehicle)
      :vars (?l - depot ?tank - compartment ?p-vol - number)
      :precondition (and (specialty ?v tanker)
			 (at ?p ?l)
			 (at ?v ?l)
			 (container ?p ?tank)
			 (volume ?p ?p-vol))
      :expansion
         (liquid-transfer ?l ?tank ?v cargo-area ?p-vol ?p))

;;; declare-method for unloading TANKER truck or traincar
  (:method unload
      :name unload-liquid
      :parameters (?p - package ?v - vehicle)
      :vars (?l - depot ?p-vol - number)
      :precondition (and (specialty ?v tanker)
			 (aboard ?p ?v)
			 (container ?p cargo-area)
			 (at ?v ?l)
			 (volume ?p ?p-vol))
      :expansion
         (liquid-transfer ?v cargo-area ?l storage-tank ?p-vol ?p))

;;; declare-method for loading LIVESTOCK 
  (:method load
      :name load-livestock
      :parameters (?p - package ?v - (either truck traincar))
      :vars (?l - location)
      :precondition (and (specialty ?v livestock-carrier)
			 (shape ?p livestock)  
			 (at ?p ?l)
			 (at ?v ?l))
      :expansion
         (constrained (series (tag n1 (lower-ramp ?v))
			      (tag n3 (load-livestock ?p ?v))
			      (tag n4 (raise-ramp ?v)))
	    (in-context n3
	       :precondition (at ?p ?l))
	    (in-context (series (> n3) (< n4))
	       :maintain (aboard ?p ?v))
	    (in-context n4
	       :precondition (at ?v ?l))
	    (in-context (series (> n1) (< n3))
	       :maintain (ramp-down   ?v))))

;;; declare-method for unloading LIVESTOCK 
  (:method unload
      :name unload-livestock
      :parameters (?p - package ?v - (either truck traincar))
      :vars (?l - location)
      :precondition (and (specialty ?v livestock-carrier)  
			 (shape ?p livestock)
			 (aboard ?p ?v)
			 (at ?v ?l))
      :expansion
         (constrained (series (tag n1 (lower-ramp ?v))
			      (tag n2 (unload-livestock ?p ?v))
			      (tag n4 (raise-ramp ?v)))
             (in-context n2
		:precondition (aboard ?p ?v))
	     (in-context n4
		:precondition (at ?v ?l))
	     (in-context (series (> n1) (< n2))
		:maintain (ramp-down   ?v))))

;;; declare-method for loading AUTO truck or traincar with CARS
  (:method load
      :name load-cars
      :parameters (?p - package ?v - (either truck traincar))
      :vars (?l - location)
      :precondition (and (specialty ?v car-carrier)
			 (shape ?p auto)
			 (at ?p ?l)
			 (at ?v ?l))
      :expansion
         (constrained (series (tag n1 (lower-ramp ?v))
			      (tag n2 (load-cars ?p ?v))
			      (tag n3 (raise-ramp ?v)))
	    (in-context n2
	       :precondition (at ?p ?l))
	    (in-context (series (> n2) (< n3))
	       :maintain (aboard ?p ?v))
	    (in-context n3
	       :precondition (at ?v ?l))
	    (in-context (series (> n1) (< n3))
	       :maintain (ramp-down  ?v))))

;;; declare-method for unloading AUTO truck or traincar with CARS
  (:method unload
      :name unload-cars
      :parameters (?p - package ?v - (either truck traincar))
      :vars (?l - location)
      :precondition (and (specialty ?v car-carrier)
			 (shape ?p auto)
			 (aboard ?p ?v)
			 (at ?v ?l))
      :expansion
         (constrained (series (tag n1 (lower-ramp ?v))
			      (tag n2 (unload-cars ?p ?v))
			      (tag n3 (raise-ramp ?v)))
	    (in-context n2
	       :precondition (aboard ?p ?v))
	    (in-context n3
	       :precondition (at ?v ?l))
	    (in-context (series (> n1) (< n3))
	       :maintain (ramp-down  ?v ))))

;;; declare-method for loading AIRPLANE 
  (:method load
      :name load-airplane
      :parameters (?p - package ?v - airplane)
      :vars (?l - location)
      :precondition (and (at ?p ?l)
			 (at ?v ?l))
      :expansion
         (forsome (?r - plane-ramp)
	    (constrained (series (tag n1 (achieve (ramp-connected ?r ?v)))
				 (tag n2 (achieve (door-open ?v)))
				 (tag n3 (load-items ?p ?v))
				 (tag n4 (achieve (not (door-open ?v))))
				 (tag n5 (achieve (not (ramp-connected
							  ?r ?v)))))
	       (in-context n1
		  :precondition (and (available ?r)
				     (at ?r ?l)))
	       (in-context n3
		  :precondition (at ?p ?l))
	       (in-context (series (> n3) (< n5))
		  :maintain (aboard ?p ?v))
	       (in-context n5
		  :precondition (at ?v ?l))
	       (in-context (series (> n1) (< n5))
		  :maintain (ramp-connected ?r ?v))
	       (in-context (series (> n2) (< n4))
		  :maintain (door-open ?v)))))

;;; declare-method for unloading AIRPLANE 
  (:method unload
      :name unload-airplane
      :parameters (?p - package ?v - airplane)
      :vars (?l - location)
      :precondition (and (aboard ?p ?v)
			 (at ?v ?l))
      :expansion
         (forsome (?r - plane-ramp)
	    (constrained (series (tag n1 (achieve (ramp-connected ?r ?v)))
				 (tag n2 (achieve (door-open ?v)))
				 (tag n3 (unload-items ?p ?v))
				 (tag n4 (achieve (not (door-open ?v))))
				 (tag n5 (achieve (not (ramp-connected
							  ?r ?v)))))
	       (in-context n1
		  :precondition (and (available ?r)
				     (at ?r ?l)))
	       (in-context n3
		  :precondition (aboard ?p ?v))
	       (in-context n5
		  :precondition (at ?v ?l))
	       (in-context (series (> n1) (< n5))
		  :maintain (ramp-connected ?r ?v))
	       (in-context (series (> n2) (< n4))
		  :maintain (door-open ?v)))))

   
;--------------------------
;  transport DECLARE-METHODS - TOP LEVEL.
;--------------------------

;;; top-level transport declare-method 1
   (:method TRANSPORT
       :name transport-direct
       :parameters (?package - package ?origin ?destination - location)
       :expansion (TRANSPORT-DIRECT ?package ?origin ?destination))

;;; 2 transport: 
;;;?origin (tcenter, not hub) -> ?destination (tcenter, not hub)	
   (:method TRANSPORT
       :name transport-via-hub
       :parameters (?package - package ?origin ?destination - transport-center)
       :vars (?ocity ?dcity - city)
       :precondition (and (IN ?origin      ?ocity)
			  (IN ?destination ?dcity)
			  (not (= ?ocity ?dcity)) 
			  (not (hub ?origin))
			  (not (hub ?destination)))

       :expansion (TRANSPORT-VIA-HUB ?package ?origin ?destination))

;;; 3 transport: 
;;;?origin (not tcenter) -> ?tcenter -> ?destination (tcenter)
   (:method TRANSPORT
       :name to-tcenter-then-go
       :parameters (?package - package 
                    ?origin - location
		    ?destination - transport-center)
       :vars (?ocity ?dcity - city)
       :precondition (and (not (transport-center ?origin))
			  (in ?origin ?ocity)
			  (in ?destination ?dcity)
			  (not (= ?ocity ?dcity))
			  (not (transport-center ?ocity))
			  (available ?destination))

       :expansion (forsome (?tcenter - transport-center)
		    (in-context (series 
				   (TRANSPORT-DIRECT ?package ?origin ?tcenter)
				   (TRANSPORT-BETWEEN-TCENTERS 
				       ?package ?tcenter ?destination))
		       :precondition 
			     (and (not (= ?tcenter ?destination))
				  (same-tcenter-type ?tcenter ?destination)
				  (available ?tcenter)))))


;;; 4 transport: 
;;; ?origin (tcenter) -> ?tcenter -> ?destination (not tcenter)
   (:method TRANSPORT
       :name go-then-to-tcenter
       :parameters (?package - package ?origin - transport-center ?destination - location)  
       :vars (?ocity ?dcity - city)
       :precondition (and (not (transport-center ?destination))
			  (IN ?origin ?ocity)
			  (IN ?destination ?dcity)
			  (not (= ?ocity ?dcity))
			  (available ?origin))
       :expansion
          (forsome (?tcenter - transport-center)
             (in-context (series (TRANSPORT-BETWEEN-TCENTERS
				     ?package  ?origin ?tcenter)
				 (TRANSPORT-DIRECT
				     ?package  ?tcenter ?destination))
		:precondition (and (SERVES ?tcenter ?dcity)
				   (not (= ?tcenter ?origin))
				   (available ?tcenter)))))

;;; transport: 
;; ?origin (not tcenter) -> 
;;       ?tcenter1 -> ?tcenter2 -> ?destination (not tcenter)
   (:method TRANSPORT
       :name use-two-tcenters
       :parameters (?package  - package ?origin ?destination - location)
       :vars (?ocity ?dcity - city)
       :precondition (and (not (transport-center ?origin))
			  (not (transport-center ?destination))
			  (in ?origin ?ocity)
			  (in ?destination ?dcity)
			  (not (= ?ocity ?dcity)))

          :expansion 
	     (forsome (?tcenter1 ?tcenter2 - transport-center)
		(in-context (series (transport-direct ?package  ?origin   ?tcenter1)
				    (transport-between-tcenters 
				        ?package  ?tcenter1 ?tcenter2)
				    (transport-direct 
                                        ?package  ?tcenter2 ?destination))
		   :precondition (and (serves ?tcenter1 ?ocity)
				      (serves ?tcenter2 ?dcity)
				      (same-tcenter-type ?tcenter1
							 ?tcenter2)
				      (available ?tcenter1)
				      (available ?tcenter2)))))
					
;--------------------------
;  TRANSPORT DECLARE-METHODS - BETWEEN TCENTERS. 
;--------------------------

;;; transport: ?tcenter1 -> ?tcenter2 where ?tcenter1 = ?tcenter2
   (:method TRANSPORT-BETWEEN-TCENTERS
       :name no-op
       :parameters (?package - package ?tcenter1 ?tcenter2 - transport-center)
       :precondition (= ?tcenter1 ?tcenter2)
       :expansion (--))

;;; transport: ?tcenter1 -> ?tcenter2
   (:method TRANSPORT-BETWEEN-TCENTERS
       :name direct
       :parameters (?package - package ?tcenter1 ?tcenter2 - transport-center) 
       :precondition (and (same-tcenter-type ?tcenter1 ?tcenter2)
			  (not (= ?tcenter1 ?tcenter2)))
       :expansion 
          (TRANSPORT-DIRECT ?package ?tcenter1 ?tcenter2))

;;; transport: ?tcenter1 (not hub) -> ?tcenter2 (not hub)
   (:method TRANSPORT-BETWEEN-TCENTERS
       :name via-hub
       :parameters (?package - package ?tcenter1 ?tcenter2 - transport-center) 
       :precondition (and  (not (hub ?tcenter1))
			   (not (hub ?tcenter2))
			   (not (= ?tcenter1 ?tcenter2)))
       :expansion (TRANSPORT-VIA-HUB ?package  ?tcenter1 ?tcenter2))


;;; transport: ?tcenter1 -> ?hub -> ?tcenter2.  
   (:method TRANSPORT-VIA-HUB
       :name safe
       :parameters (?package - package ?tcenter1 ?tcenter2 - transport-center)
       :vars (?city1 ?city2 - city ?region1 ?region2 - region)
       :precondition (and (not (hazardous ?package))
			  (in ?tcenter1 ?city1)
			  (IN ?tcenter2 ?city2)
			  (IN ?city1 ?region1)
			  (IN ?city2 ?region2))
       :expansion
	  (forsome (?hub - transport-center)
	      (in-context (series (transport-direct ?package ?tcenter1 ?hub)
				  (transport-direct ?package ?hub ?tcenter2))
	          :precondition (and (hub ?hub)
				     (serves ?hub ?region1)
				     (serves ?hub ?region2)
				     (available ?hub)))))

;;; transport: ?tcenter1 -> ?hub -> ?tcenter2.  
   (:method TRANSPORT-VIA-HUB
       :name hazardous
       :parameters (?package - package ?tcenter1 ?tcenter2 - transport-center)
       :vars (?city1 ?city2 - city ?region1 ?region2 - region)
       :precondition (and (hazardous ?package)
			  (in ?tcenter1 ?city1)
			  (in ?tcenter2 ?city2)
			  (in ?city1 ?region1)
			  (IN ?city2 ?region2))
       :expansion
          (forsome (?hub - transport-center ?cityh - city)
	     (in-context (series (transport-direct ?package ?tcenter1 ?hub)
				 (transport-direct ?package ?hub ?tcenter2))
		:precondition
		    (and (hub ?hub)
			 (in ?hub ?cityh)
			 (city-allows-hazardous ?cityh)
			 (serves ?hub ?region1)
			 (serves ?hub ?region2)
			 (available ?hub)))))

;--------------------------
;  TRANSPORT DECLARE-METHODS - DIRECT. 
;--------------------------

   (:method TRANSPORT-DIRECT
       :name just-do-it
       :parameters (?package - package ?origin ?destination - location) 
       :expansion
	  (forsome (?vehicle - vehicle 
		    ?ocity ?dcity - city)
	     (in-context (constrained
			     (series (tag n1 (achieve
						(at
						   ?vehicle ?origin)))
				     (tag n2 (load ?package ?vehicle))
				     (tag n3 (achieve
						(at
						   ?vehicle ?destination)))
				     (tag n4 (unload ?package ?vehicle)))
			     (in-context (< n2)
				:precondition (at ?package ?origin))
			     (in-context (series (> n1) (< n2))
				:maintain (at ?vehicle  ?origin))
			     (in-context (series (> n2) (< n4))
				:maintain (aboard ?package ?vehicle)))
		 :precondition (and (can-carry ?vehicle ?package)
				    (available ?vehicle)
				    (or (and  ; same city via truck
					   (truck ?vehicle)
					   (in ?origin ?ocity)
					   (in ?destination ?ocity))
					(and ;diff city 
					   (IN ?origin ?ocity)
					   (IN ?destination ?dcity)
					   (not (= ?ocity ?dcity))
					   (exists (?route - route
						    ?dist - number)
					      (and (connects
						      ?route  
						      ?ocity  ?dcity
						      ?dist)
						   (can-travel
						      ?vehicle ?route)
						   (AVAILABLE ?route)))))))))

;--------------------------------
;  MOVE VEHICLE DECLARE-METHODS
;--------------------------------

;;; move non-traincars directly
   (:method achieve-vehicle-at
       :name just-move-it
       :parameters (?vehicle - vehicle ?location - location) 
       :precondition (not (traincar ?vehicle))
       :expansion
          (forsome (?origin - location ?r - route ?dist - number)
	     (in-context (move ?vehicle ?origin ?location ?r)
		:precondition (and (at ?vehicle ?origin)
				   (connects ?r ?origin ?location ?dist)
				   (can-travel ?vehicle ?r)))))

; trucks get to cheat a little
   (:method achieve-vehicle-at
       :name move-truck
       :parameters (?truck - truck ?location - location)
       :expansion
          (forsome (?origin - location ?r - route ?dist - number
		    ?ocity ?dcity - city)
	     (in-context (move ?truck  ?origin ?location ?r)
		:precondition
		   (and (at ?truck ?origin)
			(in ?origin ?ocity)
			(in ?location ?dcity)
			(connects ?r ?ocity ?dcity ?dist)
			(can-travel ?truck ?r)))))
 
;--------------------
;  TRAIN CAR DECLARE-METHODS
;--------------------

;;; send train to pickup car, deliver it to destination, and detach it	    
   (:method ACHIEVE-VEHICLE-AT
       :name move-traincar
       :parameters (?tc - traincar ?destination - location) 
       :expansion
          (forsome (?r - rail-route ?origin - location ?train - train
		    ?dist - number)
	     (constrained (series (tag n1 (achieve (at  ?train ?origin)))
				  (tag n2 (attach-train-car ?train ?tc))
				  (tag n3 (achieve (at ?train ?destination)))
				  (tag n4 (detach-train-car ?train ?tc)))
		(in-context (< n1)
		   :precondition (and (at ?tc ?origin)
				      (connects ?r ?origin ?destination
						?dist)))
		(in-context (series (> n2) (< n4))
		   :maintain (attached ?tc ?train))
		(in-context (series (> n1) (< n2))
		   :maintain (at ?train ?origin))
		(in-context (series (> n3) (< n4))
		   :maintain (at ?train ?destination)))))


)