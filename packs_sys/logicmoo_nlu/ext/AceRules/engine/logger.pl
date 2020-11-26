% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(logger, [
		log/1,              % +Text
		last_log_message/1  % -Text
	]).

/** <module> Logger module

Writes log messages onto the user error device.

@author Tobias Kuhn
@version 2007-02-06
*/


%% last_log_message(-Message)
%
% Returns the last log message. This can be used in the case of errors.

:- dynamic(last_log_message/1).


%% log(-Text)
%
% Writes the text onto the standard error device, together with a timestamp.

log(Text) :-
	retractall(last_log_message(_)),
	assert(last_log_message(Text)),
	get_time(X),
	format(user_error, '~4f ~w~n', [X, Text]).
