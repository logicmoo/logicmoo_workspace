( define ( problem att-bw-large-a ) 
    (:domain att-bw)
  (:objects A B C D E F G H I)
  (:init (block A) (block B) (block C) (block D) (block E)
	 (block F) (block G) (block H) (block I) (is-table table)
	 (on A table) (on D table) (on F table)
	 (on C B) (on B A) (on E D)
	 (on I H) (on H G) (on G F)
	 (clear C) (clear E) (clear I))
  (:goal (AND (on A E) (on E table)
               (on H I) (on I D) (on D table)
               (on B C) (on C G) (on G F) (on F table)
               (clear A) (clear H) (clear B))))


;;;
;;; bw_large_a
;;; Initial:  3/2/1   5/4	9/8/7/6         "3 on 2 on 1"
;;; Goal:     1/5     8/9/4	2/3/7/6
;;; Length: 6

