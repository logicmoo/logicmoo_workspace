:- module(pdt_common_reload_hook, []).

:- multifile(pdt_reload:reload_message/2).

pdt_reload:reload_message(automatic_reconsult(all), '~~~ Reconsulting previously loaded files\n~~~ For turning reconsulting off or for reconsulting just entry point files\n~~~ use another option from the drop-down menu of the Restart button\n~~~ or change the corresponding preference').
pdt_reload:reload_message(automatic_reconsult(entry_points), '~~~ Reconsulting PDT entry point files\n~~~ For turning reconsulting off or for reconsulting all consulted files\n~~~ use another option from the drop-down menu of the Restart button\n~~~ or change the corresponding preference').