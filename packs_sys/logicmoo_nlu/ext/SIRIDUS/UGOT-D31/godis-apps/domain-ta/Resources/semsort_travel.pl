/*************************************************************************

         name: semsort_travel.pl
      version: May, 2002
  description: IBiS1 semantic sorts for TA domain
 
*************************************************************************/

:- discontiguous sem_sort/2.

/*----------------------------------------------------------------------
     Conceptual knowledge in terms of semantic sorts
----------------------------------------------------------------------*/

sem_sort( C, top ):-
	sem_sort( C, city );
	sem_sort( C, means_of_transport );
	sem_sort( C, month );
	sem_sort( C, day );
	sem_sort( C, class );
	sem_sort( C, country ).



sem_sort( malmoe, city ).
sem_sort( paris, city ).
sem_sort( london, city ).
sem_sort( gothenburg, city ).
sem_sort( washington, city ).
sem_sort( seattle, city ).
sem_sort( hongkong, city ).


sem_sort( sweden, country ).
sem_sort( britain, country ).
sem_sort( usa, country ).
sem_sort( china, country ).
sem_sort( france, country ).


sem_sort( plane, means_of_transport ).
sem_sort( boat, means_of_transport ).
sem_sort( train, means_of_transport ).

sem_sort( january, month).
sem_sort( february, month ).
sem_sort( march, month ).
sem_sort( april, month ).
sem_sort( may, month ).
sem_sort( june, month ).
sem_sort( july, month ).
sem_sort( august, month ).
sem_sort( september, month ).
sem_sort( october, month ).
sem_sort( november, month ).
sem_sort( december, month ).

sem_sort( D, day ) :-dates(Ds), member(D,Ds).

dates([ first, second, third, fourth, fifth, sixth, seventh, eighth, ninth, tenth, eleventh, twelfth, thirteenth, fourteenth, fifteenth, sixteenth, eeventeenth, eighteenth, nineteenth, twentieth, twentyfirst, twentysecond, twentythird, twentyfourth, twentyfifth, twentysixth, twentyseventh, twentyeigth, twentyninth, thirtieth, thirtyfirst]).


sem_sort( economy, class ).
sem_sort( business, class ).

sem_sort( N, price ):- integer( N ).

