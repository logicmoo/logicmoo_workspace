
:- expects_dialect(lps).

% contract for derivative of UCITS ETF FTSE MIB
% https://www.bloomberg.com/quote/ETFMIB:IM
% INCOMPLETE, missing receiving of investment

etf_url('https://www.bloomberg.com/quote/ETFMIB:IM').
maturity(MY/M/D) :- investment_received(_,Y/M/D), MY is Y+2. % Two years
investment_received(100,'0x3c52edeb19f9f12d64b56e88498113cc2397a077',2017/12/26). 
investor_email('mc@logicalcontracts.com').
	% Wei, account, date; preconditiom to force ammount > minimum ammount ?
initial_quote(21.0,2017/12/26).

:- include(system('date_utils.pl')).

if 	end_of_day(Today) to T1, maturity(Maturity), Maturity @>=Today
then sample_quote(Quote) from T1 to T2, finalize(Quote) from T2, lps_terminate(maturity).

sample_quote(Quote) from T1 to T2 if 
	etf_url(URL),
	get_symbol_quote(URL,Quote) from T1 to T2. % TBD

finalize(Quote) from T1 to T3 if 
	compute_payment(Quote,Payment),
	lps_my_account(Us), investment_received(_,Sender,_),
	e_sendTransaction(Us,Sender,Payment,PaymentTx) from T1 to T2,
	investor_email(Email),
	lps_send_email(Email,'Maturity reached for investment',
    	'Lucky you, we transferred now ~w to your Ethereum account ~w (transaction ~w)'-[Payment,Sender,PaymentTx]) 
		from T2 to T3.

compute_payment(Quote,Payment) :- 
	initial_quote(IQ,_), Quote>IQ, investment_received(X,_Sender,_), 
	Payment is X*max(1.05, 1+(Quote-IQ)/IQ).
compute_payment(Quote,Payment) :- 
	initial_quote(IQ,_), \+ Quote>IQ, investment_received(X,_Sender,_), 
	Payment is X*max(0.9, 1+(Quote-IQ)/IQ).

:- use_module(library(http/http_open)).

% TBD:
get_symbol_quote(Quote) :- etf_url(URL),
    load_html(URL,Data,[]), find_subtree(Data,element()).

% Untested DOM searcher, to use in conjunction with the output of load_html
% Bloomberg changes HTML for "unsupported browsers"
find_subtree(Element,Element) :- !.
find_subtree([A|Cn],E) :- atom(A), !, find_subtree(Cn,E).
find_subtree([C1|_Cn],E) :- find_subtree(C1,E), ground(E), !.
find_subtree([_C1|Cn],E) :- find_subtree(Cn,E).
find_subtree(element(Tag,Attr,Children),E) :- find_subtree(Children,E).

/** <examples> 
?- go(Timeline).
*/
