(define (addendum load-methods)
  (:domain transplan)

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
)

(define (addendum main-methods)
   (:domain transplan)
   
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
