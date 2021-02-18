
:- expects_dialect(lps).

:- include(example('loanAgreementPostConditionsRTbase.pl')).
% Borrower cures a violation of covenant
%
initially exceed_assets_liabilities.
initially tax_due(uncle_sam, 10).
observe request(borrower, 1000) at '2014-06-01T10:00'. % request on time at 10:00.
observe advance(lender, 1000) at '2014-06-02T15:00'. % advance on time on day 2.
observe notify(lender, default(covenant(pay(borrower, uncle_sam, 10)))) at '2014-06-05'.
observe pay(borrower, uncle_sam, 10) at '2014-06-06'.
observe notify(borrower, remedy(covenant(pay(borrower, uncle_sam, 10)))) at '2014-06-05'.
observe pay(borrower, lender, 550) at '2015-06-01'. % pay on time.
observe pay(borrower, lender, 525) at '2016-06-01'. % pay on time.

