
:- expects_dialect(lps).

:- include(example('loanAgreementPostConditionsRTbase.pl')).
% The borrower defaults, by paying but not notifying the payment,
% the payment of 0 does not become due, and the contract terminates "correctly".
% Change '2016-06-07' to '2016-06-06' or '2016-06-08' and see what happens.
initially exceed_assets_liabilities.
observe request(borrower, 1000) at '2014-06-01T10:00'. % request on time at 10:00.
observe advance(lender, 1000) at '2014-06-01T15:00'. % advance on time at 15:00.
observe pay(borrower, lender, 550) at '2015-06-01'. % pay on time.
observe notify(lender, default(pay(borrower, lender, 525))) at '2016-06-04'.
observe pay(borrower, lender, 525) at '2016-06-06'.% borrower pays one day after default.
