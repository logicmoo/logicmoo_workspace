
:- expects_dialect(lps).

:- include(example('loanAgreementPostConditionsRTbase.pl')).
% Borrower pays early, in violation of the contract.
% The lender complains. The borower tries a trick.
% The lender sticks to the rules.
% 
initially exceed_assets_liabilities.
observe request(borrower, 1000) at '2014-06-01T10:00'. % request on time at 10:00.
observe advance(lender, 1000) at '2014-06-01T15:00'. % advance on time at 15:00.
observe pay(borrower, lender, 550) at '2015-05-31'. % pay a day early, not allowed.
observe notify(lender, default(pay(borrower, lender, 550))) at '2015-06-02'.
observe notify(borrower, remedy(pay(borrower, lender, 550))) at '2015-06-03T15:00'.
observe pay(borrower, lender, 525) at '2016-06-01'. % pay on time.

