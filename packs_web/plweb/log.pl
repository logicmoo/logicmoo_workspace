log :-
	catch(delete_file(log), _, true),
	debug_message_context(+time),
	debug(http(request)),
%	debug(http(_)>log),
        tmon.
