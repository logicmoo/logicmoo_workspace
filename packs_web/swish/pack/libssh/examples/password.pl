% Assumes this pack is installed as pack
:- use_module(library(ssh_server)).

% This demo shows using password  based   authentication.  The server is
% simply started using
%
%    ?- ssh_server.
%
% If you want to disable public key login, use
%
%    ?- ssh_server([auth_methods([password])]).
%
% and may be accessed using  ``ssh   -p  2020  bob@localhost`` using the
% password _secret_. Of course, normally the  passwords are stored on an
% external file.

:- multifile
    ssh_server:verify_password/3.

ssh_server:verify_password(_ServerName, User, Password) :-
    granted(User, Hash),
    crypto_password_hash(Password, Hash).

granted(bob, '$pbkdf2-sha512$t=131072$OadWSEr5pvETQby5t1YkaQ$uRxQ5VDtu3xfArPAEeFztKx5xo+KDK+kfDBViCYyhBl3aqgdsq6gsxig4upckuh7LXYebCPTDw4V9U7I3UGVrQ').
