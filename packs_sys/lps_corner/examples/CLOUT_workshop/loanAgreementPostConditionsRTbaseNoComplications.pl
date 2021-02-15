
:- expects_dialect(lps).

:- include(example('loanAgreementPostConditionsRTbase.pl')).
% The simplest case with no complications:
%
initially exceed_assets_liabilities.
observe request(borrower, 1000) at '2014-06-01T10:00'. % request on time at 10:00.
observe advance(lender, 1000) at '2014-06-01T15:00'. % advance on time at 15:00.
observe pay(borrower, lender, 550) at '2015-06-01'. % pay on time.
observe pay(borrower, lender, 525) at '2016-06-01'. % pay on time.
