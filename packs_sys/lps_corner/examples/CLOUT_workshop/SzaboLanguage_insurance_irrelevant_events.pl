
:- expects_dialect(lps).

:- include(example('SzaboLanguage_insurance_base.pl')).

observe pledge(allGoods(counterparty),"Some key to all Counterparty goods") at 2018/5/20.
observe to(counterParty,getTitle(usd(600))) at "2018-05-20".

% irrelevant events happened, but our truth prevails so we succeed...
observe safeArrival("10 John D. tractors") at "2018-06-21T15:00".
observe choiceOf("TractoR'Us") at 2018/6/25.
% observe to("TractoR'Us",usd(120000)) at 2018/6/25.
observe to(holder,foreclose("Some key to all Counterparty goods", usd(5000))) at 2018/8/2.
