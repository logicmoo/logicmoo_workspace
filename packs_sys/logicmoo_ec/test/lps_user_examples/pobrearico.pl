:- expects_dialect(lps).

% iendo de A a B
% Un problema de búsqueda

maxTime(10). 

fluents salud/1, ahorros/1, suerte/1, salario/1,
        pobreza/1, empleo/1, emprendimiento/1, riqueza/1. 
events  ir/1, miseria, hambre, enfermedad, posible/1.  
actions paso_a/1.  

initially pobreza(yo), salud(yo), suerte(yo). 

observe miseria from 1 to 2.
% observe enfermedad from 2 to 3. 

ir([A]) if posible(A), paso_a(A).
ir([A|B]) if posible(A), paso_a(A), ir(B). 

plan(A,B, [B]) :- conecta(A,B). 
plan(A,B, [A,C|R]) :- conecta(A,C), not(C=B), plan(C,B, R). 

conecta(pobreza, empleo).
conecta(pobreza, emprendimiento).
conecta(empleo, riqueza).  
conecta(emprendimiento, riqueza). 

ahorros(yo) if salario(yo). 

paso_a(pobreza) terminates empleo(yo) if empleo(yo).
paso_a(pobreza) terminates emprendimiento(yo) if 
  emprendimiento(yo).
paso_a(empleo) terminates pobreza(yo) if pobreza(yo).
paso_a(emprendimiento) terminates pobreza(yo) if pobreza(yo). 

paso_a(empleo) initiates empleo(yo) if salud(yo).
paso_a(emprendimiento) initiates emprendimiento(yo) if 
  salud(yo), suerte(yo). 

posible(pobreza) if pobreza(yo).
posible(empleo) if salud(yo).
posible(emprendimiento) if salud(yo), suerte(yo).
posible(riqueza) if ahorros(yo).
posible(riqueza) if suerte(yo). 

paso_a(empleo) initiates salario(yo) if salud(yo).
paso_a(emprendimiento) initiates ahorros(yo) if 
  salud(yo), suerte(yo).  
paso_a(riqueza) initiates riqueza(yo) if ahorros(yo). 
paso_a(riqueza) initiates riqueza(yo) if suerte(yo). 

% enfermedad terminates salud(yo). 
enfermedad terminates suerte(yo).

if miseria from T1 to T2 
then plan(pobreza, riqueza, Plan), ir(Plan) from T2 to T3.  
if hambre from T1 to T2 
then plan(pobreza, riqueza, Plan), ir(Plan) from T2 to T3.

/** <examples>
?- go(T).
?- go.
?- plan(pobreza, riqueza, Plan). 
*/
