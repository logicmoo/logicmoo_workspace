:- module(abstract_domain, [abstract_domain/1,
			    top/2,
			    bot/2,
			    eval/3,
			    trusted_result/4]).

user:file_search_path(domains, library(audit/domains)).

%% abstract_domain(?Domain).
:- multifile abstract_domain:abstract_domain/1.

%% top(+Domain:abstract_domain,-Top).
:- multifile abstract_domain:top/2.

%% bot(+Domain:abstract_domain,-Bot).
:- multifile abstract_domain:bot/2.

%% eval(+Domain:abstract_domain,+Expr,-Value).
:- multifile abstract_domain:eval/3.

%% trusted_result(+Domain:abstract_domain,:Goal,+Module,-Result).
:- multifile abstract_domain:trusted_result/4.
