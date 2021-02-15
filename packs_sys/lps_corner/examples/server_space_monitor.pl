
:- expects_dialect(lps).

% Notice that this background server needs relaunching after SWI Prolog restarts.
% TODO: improve LPS engine to avoid the above problem.
maxRealTime(604800). % 86400 * 7 days
minCycleTime(600). % 10 minutes

if sudo(disk_space_used(P)) at T, P>40 then 
	lps_send_email(
         'mc@logicalcontracts.com', 'Demo server disk is full!',
        'The server is using ~w% disk capacity! Clean it up!'-[P]) from T,
	lps_terminate(disk_space_alert_sent)  from _.

/** <examples> 
?- serve(ID).
*/
