contract([X, Y | UncontractedTail], [W | ContractedTail]) :-
	contracted_form(X, Y, Z), !,
	contract(UncontractedTail, ContractedTail).
contract([A | UncontractedTail], [A | ContractedTail]) :-
	contract(UncontractedTail, ContractedTail).

contracted_form(do, not, 'don''t').
contracted_form(does, not, 'doesn''t').
contracted_form(will, not, 'won''t').
contracted_form(have, not, 'haven''t').
contracted_form(has, not, 'hasn''t').
contracted_form(had, not, 'hadn''t').
contracted_form(is, not, 'isn''t').
contracted_form(are, not, 'aren''t').

contracted_form('I', am, 'I''m').
contracted_form(you, are, 'you''re').
contracted_form(he, is, 'he''s').
contracted_form(she, is, 'she''s').
contracted_form(we, are, 'we''re').
contracted_form(they, are, 'they''re').