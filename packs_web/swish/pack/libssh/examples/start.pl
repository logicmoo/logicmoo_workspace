% A very simply script that allows starting Prolog as server using
%
%     % swipl start.pl
%
% After which you should be able to  connect to Prolog using the command
% below,  provided  you  have  your  local    public  SSH  key  in  your
% ``~/.ssh/authorized_keys`` file and thus  can   do  ``ssh  localhost``
% without password.
%
%     % ssh -p 2020 localhost

:- use_module(library(ssh_server)).

:- initialization ssh_server.

