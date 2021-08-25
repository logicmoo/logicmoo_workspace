/* PD624 code file DEMO2.PL
Example taken from Appendix B, section 3.3.3 ('seeding working memory').
Note in particular that these rules will not lead to any conclusions
unless you 'seed' working memory by hand, as described in the
aforementioned section of Appendix B.
*/

rule demo forward
 if
   start &
   deduce 'it is going to rain today'
 then
   announce ['I am not going out today'] &
   halt.

rule conclude_rain backward
 if
  'the barometric pressure is rising' &
  'the western sky is cloudy'
 then
  'it is going to rain today'.

