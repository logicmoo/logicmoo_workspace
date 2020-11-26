% -*-Prolog-*-
%% a simple Knowledge Representation Language:
%%   class(Class)
%%   instance(Individual,Class)
%%   subclass(SuperClass,SubClass)
%%   role(Class,Role)
%%   type(Class,Role,Type)
%%   range(Class,Role,Range)


% roles are inherited.
role(Super,R), subclass(Super,Sub) ==> role(Sub,R).

% types are inherited.
type(Super,Role,Type), subclass(Super,Sub) ==> type(Sub,Role,Type).

% classification rule
subclass(Super,Sub),
      subclass(Super,SubSub),
      {Sub \== SubSub},
      \+ ( ~subsumes(Sub,SubSub)),
      \+ ( ~primitive(SubSub))
      ==>
      subclass(Sub,SubSub).

disjoint(C1,C2) ==> disjoint(C2,C1).

 ~subsumes(C1,C2) <- subclass(C2,C1).

 ~subsumes(C1,C2) <- disjoint(C1,C2).

 ~subsumes(C1,C2) <-
  % we can't infer that C1 subsumes C2 if C1 has a role that C2 doen't.
  role(C1,R),
  \+ role(C2,R).

 ~subsumes(C1,C2) <-
  % we can't infer that C1 subsumes C2 if C1 has a role a type that...
  type(C1,R,T1),
  type(C2,R,T2),
  ~subsume(T1,T2).

