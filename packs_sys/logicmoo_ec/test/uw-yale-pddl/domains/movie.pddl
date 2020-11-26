(define (domain movie-dom) (:requirements :adl :typing)
  (:types chips dip pop cheese crackers - object)
  (:predicates (movie-rewound)
               (counter-at-two-hours)
               (counter-at-zero)
               (have-chips)
               (have-dip)
               (have-pop)
               (have-cheese)
               (have-crackers))
  


  
  (:action rewind-movie
           :parameters ()
           :effect (and (movie-rewound)
                        ;; Let's assume that the movie is 2 hours long
                        (when (not (counter-at-two-hours)) 
                          (not (counter-at-zero)))))
  
  (:action reset-counter
           :parameters ()
           :effect (counter-at-zero))


  ;;; Get the food and snacks for the movie
  (:action get-chips

           :parameters (?x - chips)
           :effect (have-chips))
  
  (:action get-dip
           :parameters (?x - dip)
           :effect (have-dip))

  (:action get-pop
           :parameters (?x - pop)
           :effect (have-pop))
  
  (:action get-cheese
           :parameters (?x - cheese)
           :effect (have-cheese))
  
  (:action get-crackers
           :parameters (?x - crackers)
           :effect (have-crackers)))


(define (problem movie-5) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 - chips
   d1 d2 d3 d4 d5 - dip
   p1 p2 p3 p4 p5 - pop
   z1 z2 z3 z4 z5 - cheese
   k1 k2 k3 k4 k5 - crackers)
  (:init (not (movie-rewound)) 
         ;; Counter is at something like 1 hour
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-6) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 - chips
   d1 d2 d3 d4 d5 d6 - dip
   p1 p2 p3 p4 p5 p6 - pop
   z1 z2 z3 z4 z5 z6 - cheese

   k1 k2 k3 k4 k5 k6 - crackers)
  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))

(define (problem movie-7) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 - chips
   d1 d2 d3 d4 d5 d6 d7 - dip
   p1 p2 p3 p4 p5 p6 p7 - pop
   z1 z2 z3 z4 z5 z6 z7 - cheese

   k1 k2 k3 k4 k5 k6 k7 - crackers)
  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))

(define (problem movie-8) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 - chips

   d1 d2 d3 d4 d5 d6 d7 d8 - dip
   p1 p2 p3 p4 p5 p6 p7 p8 - pop
   z1 z2 z3 z4 z5 z6 z7 z8 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 - crackers)
  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-9) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 c9 - chips
   d1 d2 d3 d4 d5 d6 d7 d8 d9 - dip
   p1 p2 p3 p4 p5 p6 p7 p8 p9 - pop
   z1 z2 z3 z4 z5 z6 z7 z8 z9 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 k9 - crackers)

  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))

  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))

(define (problem movie-10) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 - chips
   d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 - dip
   p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 - pop
   z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 - crackers)
  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))

(define (problem movie-11) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 - chips
   d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 - dip
   p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 - pop
   z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 - crackers)
  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))

(define (problem movie-12) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 - chips
   d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 - dip
   p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 - pop
   z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 - crackers)
  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 

              (have-cheese) (have-crackers))))



(define (problem movie-13) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 - chips
   d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 - dip
   p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 - pop
   z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 - crackers)
  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-14) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 - chips
   d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 - dip
   p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 - pop
   z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 - crackers)
  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-15) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 - chips
   d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 - dip
   p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 - pop
   z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 - crackers)

  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))

  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-16) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 - chips
   d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 - dip
   p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 - pop
   z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 - crackers)
  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)

              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-17) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 - chips
   d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 - dip
   p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 - pop

   z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16 z17 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 k17 - crackers)
  (:init (not (movie-rewound)) 

         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-18) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 - chips
   d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 - dip

   p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 - pop
   z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16 z17 z18 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 k17 k18 - crackers)
  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-19) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 - chips
   d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 - dip
   p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 - pop
   z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16 z17 z18 z19 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 k17 k18 k19 - crackers)
  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-20) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 - chips
   d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 - dip
   p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 - pop
   z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16 z17 z18 z19 z20 - cheese
   k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 k17 k18 k19 k20 - crackers)

  (:init (not (movie-rewound)) 
         (not (counter-at-two-hours)) (not (counter-at-zero)))

  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


