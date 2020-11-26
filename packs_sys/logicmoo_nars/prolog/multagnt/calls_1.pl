?-['teacher'].
?-['learner1'].

?-demo(teacher:gilchrist_family,parent(warren,david),L).
?-demo(teacher:gilchrist_family,parent(charles,julia),L).
?-demo(teacher:gilchrist_family,parent(warren,variable(a)),L).
?-demo(teacher:gilchrist_family,parent(variable(parent),variable(child)),L).

?-demo(teacher:entropy,entropy_increases(kitchen,fridge),L).
?-demo(teacher:entropy,entropy_increases(kitchen,living_room),L).
?-demo(teacher:entropy,entropy_increases(variable(a),variable(b)),L).

?-demo(teacher:t_member,member(a,[a,b,c]),L).
?-demo(teacher:t_member,member(b,[a,b,c]),L).
?-demo(teacher:t_member,member(e,[q,w,e,r,t,z,t,r,e,w,q]),L).
?-demo(teacher:t_member,member(o,[q,w,e,r,t,z,t,r,e,w,q]),L).
?-demo(teacher:t_member,member(variable(a),[a,b,c]),L).
?-demo(teacher:t_member,member(variable(a),variable(list)),L).
?-demo(teacher:t_member,member(a,variable(list)),L).

?-demo(teacher:t_reverse,reverse([a,b,c],[c,b,a]),L).
?-demo(teacher:t_reverse,reverse([b,a,c],[c,b,a]),L).
?-demo(teacher:t_reverse,reverse(variable(a),[c,b,a]),L).

?-can_do(Tl,Tt,parent(warren,david),C).
?-can_do(Tl,Tt,parent(charles,julia),C).
?-can_do(Tl,Tt,parent(variable(parent),variable(child)),C).

?-demo(Tl,Tt,entropy_increases(kitchen,fridge),C).
?-can_do(Tl,Tt,entropy_increases(kitchen,fridge),C).
?-can_do(Tl,Tt,entropy_increases(variable(a),variable(b)),C).

?-can_do(Tl,Tt,member(a,[g,f,d,s,a]),C).
?-can_do(Tl,Tt,member(e,[q,w,e,r,t,z,t,r,e,w,q]),C).
?-can_do(Tl,Tt,member(variable(a),[g,f,d,s,a]),C).
?-can_do(Tl,Tt,member(variable(a),variable(list)),C).

?-can_do(Tl,Tt,reverse([a,b,c],[c,b,a]),C).
?-can_do(Tl,Tt,reverse([b,a,c],[c,b,a]),C).
?-can_do(Tl,Tt,reverse(variable(c),[c,b,a]),C).
