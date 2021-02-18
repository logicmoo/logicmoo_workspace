#!/usr/bin/env swipl

:- module(logicmoo_plweb,[]).

end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dmsg("Ensure PLWEB").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% So we dont get stuck in Console color snooping test!
%:- user:use_module(library(theme/auto)).
%:- initialization(user:use_module(library(theme/auto)),restore_state).

:- multifile(predicate_options:predicate_options/3).
:- dynamic(predicate_options:predicate_options/3).

%:- attach_packs('/opt/logicmoo_workspace/packs_web/plweb/packs').

%:- user:['/opt/logicmoo_workspace/packs_web/plweb/plweb.pl'].
%:- plweb:with_mutex(plweb_init, server_init).
%:- doc_enable(true).

maybe:- logicmoo_base_port(Base), X is Base+20,
   plweb:server([port(X)]).

                           