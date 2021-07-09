
:- expects_dialect(lps).

:- include(example('SzaboLanguage_insurance_base.pl')).

% can't claim when it arrives
observe safeArrival("10 John D. tractors") at "2018-06-21T15:30".
observe choiceOf("TractoR'Us") at "2018-06-21T15:00". 

% observe to("TractoR'Us",usd(120000)) at 2018/6/25.
% observe to(holder,foreclose("Some key to all Counterparty goods", usd(5000))) at 2018/8/2.
