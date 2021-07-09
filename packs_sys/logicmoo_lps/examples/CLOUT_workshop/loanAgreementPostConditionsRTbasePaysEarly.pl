
:- expects_dialect(lps).

:- include(example('loanAgreementPostConditionsRTbase.pl')).
%NOTE: this example is taking too long to run for the web (it takes about 90 seconds on barebones lps.swi), likely due to unconstrained times
%   the example worked prior to the Feb 3 2021 'untimed_are_relaxed' change; TODO: revise loanAgreementPostConditionsRTbase.pl
% Borrower pays early. Against the contract.
% Fortunately, the lender does not complain.
initially exceed_assets_liabilities.
observe request(borrower, 1000) at '2014-06-01T10:00'. % request on time at 10:00.
observe advance(lender, 1000) at '2014-06-01T15:00'. % advance on time at 15:00.
observe pay(borrower, lender, 550) at '2015-05-31'. % pay a day early, not allowed.
observe pay(borrower, lender, 525) at '2016-06-01'. % pay on time.

