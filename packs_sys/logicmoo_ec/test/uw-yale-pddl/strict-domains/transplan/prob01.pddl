(define (problem transplan-prob-1)
   (:domain simple-transplan)
   (:objects region-a region-b region-c region-d region-e region-f
	     region-g region-h region-i
	     - region
	     metropolis smallville anytown berlin delhi paris ithaca syracuse athens
             redville greenville yellowtown blueton lvov krakow curitiba frankfurt
	     gainesville lossesburg london gotham buenos-aires brasilia bombay
	     shanghai beijing jerusalem cairo
	     ; Any resemblance to real places is purely coincidental!
	     - city
	     loc-a1 loc-a2 loc-a3 loc-a4 loc-a5 loc-a6 loc-a7 loc-a8
	     loc-c1 loc-c2 loc-c3 loc-c4 loc-c5
	     loc-d1 loc-d2 loc-d3 loc-d4 loc-d5 loc-d6 loc-d7 loc-d8 loc-d9
	     loc-d10 loc-d11 loc-d12
	     loc-f1 loc-f2 loc-f3 loc-f4 loc-f5 loc-f6 loc-f7 loc-f8 loc-f9
	     loc-f10
	     loc-g1 loc-g2 loc-g3 loc-g4 loc-g5 loc-g6 loc-g7 loc-g8 loc-g9
	     - location
	     london-airport metropolis-airport paris-airport berlin-airport
	     shanghai-airport
	     - airport
	     london-station paris-station berlin-station bombay-station curitiba-station
	     buenos-aires-station smallville-station
	     - train-station
	     delhi-depot - depot

	     street-a1 street-a2 street-a3 street-a4 street-a5
	     street-c1 street-c2 street-c3 street-c4
	     street-f1 street-f2 street-f3 street-f4
	     street-d1 street-d2 street-d3 street-d4 street-d5 street-d6
	     street-d7 street-d8 street-d9 street-d10
	     street-g1 street-g2 street-g3 street-g4 street-g5 street-g6
	     street-g7 street-g8 road-g9
	     street-h1 street-h2 street-h3 street-h4
	     street-i1 street-i2 street-i3 street-i4 street-i5 street-i6 street-i7
	     highway1 - road-route 
	     air-route1 air-route2 air-route3 air-route4 air-route5 air-route6
	     air-route7 air-route8
	     - air-route
	     railroad1 - rail-route
   	     train1 - train
	     tc1 tc2 tc3 tc4 tc5 tc6 tc7 - traincar
	     truck1 - truck
	     lv1 - package)

   (:init     (in london region-a)
	         (in loc-a1 london) (in loc-a2 london) (in loc-a3 london)
		 (in london-airport london) (in london-station london)
		 (in loc-a8 jerusalem) (in loc-a7 jerusalem) (in loc-a6 blueton)
	      (in metropolis region-c)
	         (in loc-c1 metropolis) (in loc-c2 metropolis) (in loc-c3 metropolis)
		 (in metropolis-airport metropolis)
		 (in loc-c4 anytown)  (in loc-c5 anytown)
		 (in loc-c6 smallville)

	      (in delhi region-d)
	         (in loc-d1 delhi)  (in loc-d2 delhi) (in delhi-depot delhi)
		 (in loc-d3 bombay) (in loc-d4 bombay) (in bombay-station bombay)
		 (in loc-d5 shanghai) (in loc-d6 beijing) (in loc-d7 beijing)
		 (in loc-d8 greenville)  (in loc-d9 lvov) (in loc-d10 lvov)
		 (in loc-d11 lvov) (in loc-d12 shanghai)
		 (in shanghai-airport shanghai)

	      (in buenos-aires region-f)
	         (in loc-f1 buenos-aires) (in loc-f2 buenos-aires)
		 (in loc-f3 buenos-aires) (in loc-f4 gainesville)
		 (in loc-f5 gainesville)  (in loc-f8 gotham)
		 (in loc-f6 lossesburg) (in loc-f9 lossesburg) (in loc-f10 redville)
		 
	      (in paris region-g)
	         (in loc-g1 paris) (in loc-g2 paris) (in loc-g3 paris)
		 (in loc-g4 paris) (in loc-g5 athens) (in loc-g6 athens)
		 (in loc-g7 cairo) (in loc-g8 cairo) (in loc-g9 cairo)
		 (in loc-g10 cairo)

	      (in brasilia region-h)
	         (in loc-h1 brasilia) (in loc-h2 syracuse)
		 (in loc-h3 syracuse)  (in loc-h4 curitiba)
		 (in loc-h5 curitiba) (in loc-h6 curitiba)
		 (in curitiba-station curitiba)

	      (in berlin region-i)
	         (in berlin-station berlin)
		 (in loc-i1 berlin) (in loc-i2 berlin) (in loc-i3 berlin)
		 (in loc-i4 berlin) (in loc-i5 berlin)
		 (in loc-i7 ithaca) (in loc-i6 ithaca) (in loc-i8 ithaca)
		 (in loc-i9 frankfurt)
		 
	      (in smallville region-c)
	      (in anytown region-c)
	      (in bombay region-d)
	      (in shanghai region-d)
	      (in beijing region-d)
	      (in gainesville region-f)
	      (in lossesburg region-f)
	      (in london region-f)
	      (in gotham region-i)
	      (in ithaca region-i)
	      (in syracuse region-h)
	      (in athens region-g)
	      (in redville region-f)
	      (in greenville region-d)
	      (in yellowtown region-c)
	      (in blueton region-a)
	      (in lvov region-d)
	      (in curitiba region-h)
	      (in frankfurt region-i)
	      (in jerusalem region-a)
	      (in cairo region-g)

	      (connects street-a1 loc-a1 loc-a2 3)
	      (connects street-a2 loc-a2 loc-a3 2)
	      (connects street-a3 loc-a1 loc-a3 2)
	      (connects street-a4 loc-a3 london-station 4)
	      (connects street-a5 loc-a1 london-airport 1)
	      
	      (connects street-c1 loc-c1 loc-c2 5)
	      (connects street-c3 loc-c2 loc-c5 20)
	      (connects street-c5 loc-c5 loc-c6 20)
	      (connects street-c4 loc-c6 loc-f6 300)
	      (connects street-c6 loc-c6 metropolis-airport 10)
	      (connects street-c7 loc-c2 metropolis-airport 5)

	      (connects street-d1 loc-a8 loc-d1 125)
	      (connects street-d2 loc-d1 loc-d2 3)
	      (connects street-d3 loc-d2 loc-d4 25)
	      (connects street-d4 loc-d2 loc-d3 35)
	      (connects street-d5 loc-d4 loc-d12 50)
	      (connects street-d6 loc-d12 loc-d7 53)
	      (connects street-d7 loc-d12 loc-d6 75)
	      (connects street-d8 loc-d6 loc-d8 80)
	      (connects street-d9 loc-d8 loc-d10 90)
	      (connects street-d10 loc-d9 loc-d10 10)
	      (connects street-d10 loc-d10 loc-d11 11)

	      (connects street-f1 loc-f1 loc-f2 5)
	      (connects street-f1 loc-f2 loc-f3 6)
	      (connects street-f2 loc-f3 loc-f4 50)
	      (connects street-f3 loc-f4 loc-f5 8)
	      (connects street-f4 loc-f4 loc-f8 100)
	      (connects street-f6 loc-f9 loc-f8 3)
	      (connects street-f6 loc-f9 loc-f5 150)

	      (connects street-d10 loc-d10 loc-g3 100)
	      (connects street-g1 loc-d11 loc-g1 130)
	      (connects street-g2 loc-g1 loc-g2 8)
	      (connects street-g2 loc-g2 loc-g3 7)
	      (connects street-g2 loc-g3 loc-g4 6)
	      (connects street-g3 loc-g4 loc-g6 90)
	      (connects street-g4 loc-g7 loc-g6 120)
	      (connects street-g5 loc-g8 loc-g6 110)
	      (connects street-g6 loc-g9 loc-g5 125)
	      (connects street-g7 loc-g10 loc-g5 145)
	      (connects street-g8 loc-g5 loc-g1 86)

	      (connects road-g9 loc-g4 loc-d2 500)
	      (connects highway1 paris berlin 1000)

	      (connects street-h1 loc-h1 loc-h2 25)
	      (connects street-h2 loc-h2 loc-h3 2)
	      (connects street-h2 loc-h2 loc-h4 35)
	      (connects street-h3 loc-h2 loc-h5 45)
	      (connects street-h4 loc-h3 loc-h6 55)

	      (connects street-i1 loc-i1 loc-i2 1)
	      (connects street-i1 loc-i2 loc-i3 1)
	      (connects street-i1 loc-i3 loc-i4 1)
	      (connects street-i1 loc-i4 loc-i5 1)
	      (connects street-i2 loc-i2 loc-i5 4)
	      (connects street-i3 loc-i1 loc-i4 5)
	      (connects street-i4 loc-i1 loc-i6 15)
	      (connects street-i5 loc-i6 loc-i8 25)
	      (connects street-i6 loc-i8 loc-i9 55)
	      (connects street-i7 loc-i9 loc-i7 65)

	      (connects railroad1 london-station bombay-station 500)
	      (connects railroad1 london-station paris-station 1000)
	      (connects railroad1 london-station curitiba-station 1500)
	      (connects railroad1 london-station berlin-station 2000)
	      (connects railroad1 london-station buenos-aires-station 2500)
	      (connects railroad1 london-station smallville-station 3000)
              (connects railroad1 bombay-station paris-station 500)
              (connects railroad1 bombay-station curitiba-station 1000)
              (connects railroad1 bombay-station berlin-station 1500)
              (connects railroad1 bombay-station buenos-aires-station 2000)
              (connects railroad1 bombay-station smallville-station 2500)
	      (connects railroad1 paris-station  curitiba-station 5000)
	      (connects railroad1 paris-station  berlin-station 1000)
	      (connects railroad1 paris-station  buenos-aires-station 1500)
	      (connects railroad1 paris-station  smallville-station 3000)
	      (connects railroad1 curitiba-station berlin-station 500)
	      (connects railroad1 curitiba-station buenos-aires-station 1000)
	      (connects railroad1 curitiba-station smallville-station 1500)
	      (connects railroad1 berlin-station buenos-aires-station 500)
	      (connects railroad1 berlin-station smallville-station 1000)
	      (connects railroad1 buenos-aires-station smallville-station 1500)

	      (connects air-route1 paris-airport berlin-airport 1000)
	      (connects air-route2 paris-airport shanghai-airport 500)
	      (connects air-route3 paris-airport metropolis-airport 1400)
	      (connects air-route4 paris-airport london-airport 1000)
	      (connects air-route5 london-airport metropolis-airport 1000)
	      (connects air-route6 london-airport berlin-airport 1400)
	      (connects air-route7 shanghai-airport berlin-airport 1200)
	      (connects air-route8 metropolis-airport berlin-airport 1000)

	      (hub paris-station)
	      (hub berlin-station)
	      (hub metropolis-airport)
	      (hub london-station)
	      (hub shanghai-airport)
	      (serves london-station region-a)
	      (serves london-station region-d)
	      (serves london-station region-g)
	      (serves metropolis-airport region-c)
	      (serves metropolis-airport region-f)
	      (serves berlin-station region-i)
	      (serves berlin-station region-h)
	      (serves berlin-station region-f)
	      (serves paris-station region-g)
	      (serves shanghai-airport region-d)

	 (current-value elapsed-time 0)
;	  (mass tc1 0) (mass tc2 0) (mass tc3 0) (mass tc4 0)
;	  (mass tc5 0) (mass tc6 0) (mass tc7 0)
	  (specialty tc1 livestock-carrier)
;	  (locomotive-mass train1 0)
	  (contains train1 gas-tank train1-gas-tank-contents)
	  (contains tc1 cargo-area tc1-cargo-area-contents)
	  (current-value tc1-cargo-area-contents 0)
	  (capacity tc1 cargo-area 100)
	  (fuel-waste train1 railroad1 0)
	  (fuel-rate train1 railroad1 0)
	  (speed train1 railroad1 100)
	  (latency train1 railroad1 100)
	  (alive lv1)
	  (shape lv1 livestock)
	  ;(volume lv1 20)
	  (at lv1 london-station) (at train1 london-station)
	  (at tc1 london-station) (at tc2 london-station) (at tc3 london-station)
	  (at tc4 london-station) (at tc5 london-station) (at tc6 london-station))
   (:goal (at lv1 bombay-station))
   (:expansion (transport lv1 london-station bombay-station))
)
