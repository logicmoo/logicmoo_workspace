/* Simple Translog, August, 1999. *****************************************

  Written in: OCLh
  Originator: Donghong Liu      Aug 99
  Updated:    Lee McCLuskey     Sept 99

   Derived from Univ. of Maryland's literal-based specification     

This model captures the object structure and actions in a "transport
logistics" domain where packages have to be transported around 
different locations in different cities, using trucks and trains */

/*********************** sort hierarchy *****************************/

domain_name(translog4).

sorts(non_primitive_sorts, [
       location, city_location,tcentre,not_tcentre, route, 
       physical_obj, vehicle, railv]).
sorts(primitive_sorts, [
        airport, aircraft, train_station, post_office, clocation, city, package, 
        train, traincar, truck, road_route, rail_route, region]).
sorts(physical_obj, [vehicle, package]).
sorts(vehicle, [railv,truck,aircraft]).
sorts(railv, [traincar,train]).
sorts(location, [city_location,city,airport]).
sorts(city_location, [tcentre,not_tcentre]).
sorts(tcentre, [train_station]).
sorts(not_tcentre, [clocation,post_office]).
sorts(route, [road_route, rail_route]).

objects(aircraft,[ac1,ac2,ac3,ac4,ac5,ac6,ac7,ac8,ac9,ac10,ac11,ac12,
     ac13,ac14,ac15,ac16]).
objects(airport, [ap1, ap2, ap3, ap4 ]).

objects(train_station, [ 
                        city1_ts1_x,city2_ts1_x,city3_ts1_x,
                        city1_ts1_y,city2_ts1_y,city3_ts1_y,
                        city1_ts1_z,city2_ts1_z,city3_ts1_z,
                        city1_ts1,city2_ts1,city3_ts1]).
objects(clocation, [ 
                        city1_cl1_x,city1_cl2_x,city2_cl1_x,city3_cl1_x,
                        city1_cl1_y,city1_cl2_y,city2_cl1_y,city3_cl1_y,
                        city1_cl1_z,city1_cl2_z,city2_cl1_z,city(n3cl1z),
                        city1_cl1,city1_cl2,city2_cl1,city3_cl1]).
objects(post_office, [ post_1]).
objects(city, [ 
                        city1_x, city2_x, city3_x,
                        city1_y, city2_y, city3_y,
                        city1_z, city2_z, city3_z,
                        city1, city2, city3]).
objects(train,[ 
                        train1_x,train2_x,
                        train1_z,train2_z,
                        train1_y,train2_y,
                        train1,train2]).
objects(traincar,[ 
                        traincar1_x,
                        traincar1_z,
                        traincar1_y,
                        traincar1]).
objects(road_route, [ 
                        road_route_21_x,road_route_32_x, road_route_31_x ,
                        road_route_21_z,road_route_32_z, road_route_31_z ,
                        road_route_21_y,road_route_32_y, road_route_31_y ,
                        road_route_21,road_route_32, road_route_31 ]).
objects(rail_route,[
                        rail_route(n2x),rail_route(n3x) ,
                        rail_route(n2z),rail_route(n3z) ,
                        rail_route(n2y),rail_route(n3y) ,
                        rail_route_2,rail_route_3 ]).
objects(truck, [
                        truck_1_x, truck(n2x), truck(n3x), truck_11_x, truck_22_x, truck_33_x,
                        truck_1_z, truck(n2z), truck(n3z), truck_11_z, truck_22_z, truck_33_z,
                        truck_1_y, truck(n2y), truck(n3y), truck_11_y, truck_22_y, truck_33_y,
                        truck_1, truck_2, truck_3, truck_11, truck_22, truck_33]).

objects(package,[pk_1, pk_2, pk_3, pk_4, pk_5, pk_6, 
                  pk_1_x, pk(n2x), pk(n3x), pk_4_x, pk_5_x, pk_6_x,
                  pk_1_z, pk(n2z), pk(n3z), pk_4_z, pk_5_z, pk_6_z,
                  pk_1_y, pk(n2y), pk(n3y), pk_4_y, pk_5_y, pk_6_y]).

objects(region,[east,west,south,north]).

/*********************** predcate defns ***********************************/

predicates([
% dynamic 
  at(physical_obj,city_location),
  moveable(vehicle),
  available(vehicle),
  busy(vehicle),
  attached(railv,railv),
  unattached(railv),
  waiting(package),
  certified(package),
  uncertified(package),
  loaded(package,truck),
  loaded(package,traincar),
  loaded(package,aircraft),
  delivered(package),
% static
  rv_compatible(route,vehicle),
  ap_serves(airport,city),
  connects(route,location,location),
  in_city(location, city),
  in_region(location,region),
  serves_region(airport,region),
  route_available(route) ]).

/*********************** invariants ****************************************/

% LHS vars univ. quantified over primitive sorts
% RHS free vars are existentially quantified

implied_invariant([loaded(P,V)], [at(V,L),at(P,L)]).

inconsistent_constraint([certified(P), not_insured(P)]).

atomic_invariants([
      rv_compatible(rail_route,traincar),
      rv_compatible(rail_route,train),
      rv_compatible(road_route,truck),

      serves_region(ap1,east),
      in_city(ap1,city1),in_region(ap1,east),

      in_city(city1_cl1,city1), in_city(city1_ts1,city1),
      in_city(city1_cl2,city1), in_city(city1_ts2,city1),
      in_city(city2_cl1,city2), in_city(city2_ts1,city2),
      in_city(city3_cl1,city3), in_city(city3_ts1,city3),
      serves(city1_ts1,city1), serves(city1_ts2,city1),
      serves(city2_ts1,city2),
      serves(city3_ts1,city3),
      route_available(road_route_31),
      connects(road_route_31,city3,city1),
      connects(road_route_31,city1,city3),
      route_available(road_route_32),
      connects(road_route_32,city3,city2),
      connects(road_route_32,city2,city3),
      route_available(rail_route_1),
      connects(rail_route_1,city1_ts2,city1_ts1),
      connects(rail_route_1,city1_ts1,city1_ts2),
      route_available(rail_route_2),
      connects(rail_route_2,city2_ts1,city1_ts1),
      connects(rail_route_2,city1_ts1,city2_ts1),
      connects(road_route_21,city2,city1),
      route_available(road_route_21),
      connects(road_route_21,city1,city2),

      in_region(city1_ts1,east),in_region(city1,east),
      in_region(city2_ts1,east), in_region(city2,east),
      in_region(city3_ts1,east), in_region(city3,east),
      in_region(city1_ts2,east), in_region(city1_cl1,east),
      in_region(city1_cl2,east), in_region(city2_cl1,east),
      in_region(city3_cl1,east), 

      serves_region(ap2,west),
      in_city(ap2,city1_x),in_region(ap2,west),

      in_region(city1_ts1_x,west),in_region(city1_x,west),
      in_region(city2_ts1_x,west), in_region(city2_x,west),
      in_region(city3_ts1_x,west), in_region(city3_x,west),
      in_region(city1_ts2_x,west), in_region(city1_cl1_x,west),
      in_region(city1_cl2_x,west), in_region(city2_cl1_x,west),
      in_region(city3_cl1_x,west), 

      in_city(city1_cl1_x,city1_x), in_city(city1_ts1_x,city1_x),
      in_city(city1_cl2_x,city1_x), in_city(city1_ts2_x,city1_x),
      in_city(city2_cl1_x,city2_x), in_city(city2_ts1_x,city2_x),
      in_city(city3_cl1_x,city3_x), in_city(city3_ts1_x,city3_x),
      serves(city1_ts1_x,city1_x), serves(city1_ts2_x,city1_x),
      serves(city2_ts1_x,city2_x),
      serves(city3_ts1_x,city3_x),
      route_available(road_route_31_x),
      connects(road_route_31_x,city3_x,city1_x),
      connects(road_route_31_x,city1_x,city3_x),
      route_available(road_route_32_x),
      connects(road_route_32_x,city3_x,city2_x),
      connects(road_route_32_x,city2_x,city3_x),
      route_available(rail_route_1_x),
      connects(rail_route_1_x,city1_ts2_x,city1_ts1_x),
      connects(rail_route_1_x,city1_ts1_x,city1_ts2_x),
      route_available(rail_route(n2x)),
      connects(rail_route(n2x),city2_ts1_x,city1_ts1_x),
      connects(rail_route(n2x),city1_ts1_x,city2_ts1_x),

      connects(road_route_21_x,city1_x,city2_x),
      connects(road_route_21_x,city2_x,city1_x),
      route_available(road_route_21_x),

      serves_region(ap3,south),
      in_city(ap3,city1_y),in_region(ap3,south),

      in_region(city1_ts1_y,south),in_region(city1_y,south),
      in_region(city2_ts1_y,south), in_region(city2_y,south),
      in_region(city3_ts1_y,south), in_region(city3_y,south),
      in_region(city1_ts2_y,south), in_region(city1_cl1_y,south),
      in_region(city1_cl2_y,south), in_region(city2_cl1_y,south),
      in_region(city3_cl1_y,south), 

      in_city(city1_cl1_y,city1_y), in_city(city1_ts1_y,city1_y),
      in_city(city1_cl2_y,city1_y), in_city(city1_ts2_y,city1_y),
      in_city(city2_cl1_y,city2_y), in_city(city2_ts1_y,city2_y),
      in_city(city3_cl1_y,city3_y), in_city(city3_ts1_y,city3_y),
      serves(city1_ts1_y,city1_y), serves(city1_ts2_y,city1_y),
      serves(city2_ts1_y,city2_y),
      serves(city3_ts1_y,city3_y),
      route_available(road_route_31_y),
      connects(road_route_31_y,city3_y,city1_y),
      connects(road_route_31_y,city1_y,city3_y),
      route_available(road_route_32_y),
      connects(road_route_32_y,city3_y,city2_y),
      connects(road_route_32_y,city2_y,city3_y),
      route_available(rail_route_1_y),
      connects(rail_route_1_y,city1_ts2_y,city1_ts1_y),
      connects(rail_route_1_y,city1_ts1_y,city1_ts2_y),
      route_available(rail_route(n2y)),
      connects(rail_route(n2y),city2_ts1_y,city1_ts1_y),
      connects(rail_route(n2y),city1_ts1_y,city2_ts1_y),

      connects(road_route_21_y,city1_y,city2_y),
      connects(road_route_21_y,city2_y,city1_y),
      route_available(road_route_21_y),

      serves_region(ap4,north),
      in_city(ap4,city1_z),in_region(ap4,north),

      in_region(city1_ts1_z,north),in_region(city1_z,north),
      in_region(city2_ts1_z,north), in_region(city2_z,north),
      in_region(city3_ts1_z,north), in_region(city3_z,north),
      in_region(city1_ts2_z,north), in_region(city1_cl1_z,north),
      in_region(city1_cl2_z,north), in_region(city2_cl1_z,north),
      in_region(city(n3cl1z),north), 

      in_city(city1_cl1_z,city1_z), in_city(city1_ts1_z,city1_z),
      in_city(city1_cl2_z,city1_z), in_city(city1_ts2_z,city1_z),
      in_city(city2_cl1_z,city2_z), in_city(city2_ts1_z,city2_z),
      in_city(city(n3cl1z),city3_z), in_city(city3_ts1_z,city3_z),
      serves(city1_ts1_z,city1_z), serves(city1_ts2_z,city1_z),
      serves(city2_ts1_z,city2_z),
      serves(city3_ts1_z,city3_z),
      route_available(road_route_31_z),
      connects(road_route_31_z,city3_z,city1_z),
      connects(road_route_31_z,city1_z,city3_z),
      route_available(road_route_32_z),
      connects(road_route_32_z,city3_z,city2_z),
      connects(road_route_32_z,city2_z,city3_z),
      route_available(rail_route_1_z),
      connects(rail_route_1_z,city1_ts2_z,city1_ts1_z),
      connects(rail_route_1_z,city1_ts1_z,city1_ts2_z),
      route_available(rail_route(n2z)),
      connects(rail_route(n2z),city2_ts1_z,city1_ts1_z),
      connects(rail_route(n2z),city1_ts1_z,city2_ts1_z),

      connects(road_route_21_z,city1_z,city2_z),
      connects(road_route_21_z,city2_z,city1_z),
      route_available(road_route_21_z)
     ]).

/*********************** ss classes ****************************************/

substate_classes(physical_obj, P,
       [
        [at(P,L)]
       ]).
substate_classes(railv, V,
       [
        [unattached(V)] , [attached(V,V1)]
       ]).
substate_classes(vehicle, T,
       [
        [moveable(T),available(T)], 
        [moveable(T),busy(T)] 
       ]).

substate_classes(package, P,
       [
        [uncertified(P)],
        [waiting(P),certified(P)],
        [loaded(P,V),certified(P)],
        [delivered(P)]
      ]) .





