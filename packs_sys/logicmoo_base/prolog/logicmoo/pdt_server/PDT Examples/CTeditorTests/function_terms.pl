
a(f).
a(f()).                 % Illegal before SWI-Prolog 7.x
a(f(1)).
a(f(_)).
a(f(X,X,Y,Y,1,2,3,4)).
a('blub'(1)).
a(_blub(1)).            % Illegal

a([]).
a([1,2,3]).
a([1|[2,3]]).
a([1|2,3]).             % Illegal


