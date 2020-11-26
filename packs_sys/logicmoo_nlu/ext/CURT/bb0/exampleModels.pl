/*************************************************************************

         name: exampleModels.pl (Chapter 1)
      version: January 15, 2002
  description: Some example models defined over a vocabulary
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(exampleModels,[example/2]).


/*========================================================================
   Example Models
========================================================================*/

example(1,
        model([d1,d2,d3,d4,d5],
              [f(0,jules,d1),
               f(0,vincent,d2),
               f(0,pumpkin,d3),
               f(0,honey_bunny,d4),
               f(0,yolanda,d5),
               f(1,customer,[d1,d2]),
               f(1,robber,[d3,d4]),
               f(2,love,[(d3,d4)])])).


example(2,
        model([d1,d2,d3,d4,d5,d6],
              [f(0,jules,d1),
               f(0,vincent,d2),
               f(0,pumpkin,d3),
               f(0,honey_bunny,d4),
               f(0,yolanda,d4),
               f(1,customer,[d1,d2,d5,d6]),
               f(1,robber,[d3,d4]),
               f(2,love,[])])).


example(3,
        model([d1,d2,d3,d4,d5,d6,d7,d8],
              [f(0,mia,d1),
               f(0,jody,d2),
               f(0,jules,d3),
               f(0,vincent,d4),
               f(1,woman,[d1,d2]),
               f(1,man,[d3,d4]),
               f(1,joke,[d5,d6]),
               f(1,episode,[d7,d8]),
               f(2,in,[(d5,d7),(d5,d8)]),
               f(2,tell,[(d1,d5),(d2,d6)])])).
