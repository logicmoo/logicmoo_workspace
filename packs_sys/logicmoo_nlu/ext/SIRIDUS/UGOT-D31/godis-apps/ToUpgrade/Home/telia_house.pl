:- module( telia_house, [ house/1 ] ).

% Format: house(Rooms) where Rooms is a list of rooms.
% Each room is a list of devices on the form
% GodisID = ( DeviceType, DeviceResource, DeviceID )

house([ tv_room     = [ lamp         = (lamp,         device_rel1,  'REL1'  ),
		        light_sensor = (light_sensor, device_lux,   'LUX'   ),
		        temp_sensor  = (temp_sensor,  device_temp3, 'TEMP3' ) ],
	kitchen     = [ lamp         = (lamp,         device_rel2,  'REL2'  ) ],
	hall        = [ lamp         = (lamp,         device_rel3,  'REL3'  ) ],
	living_room = [ dimmer       = (dimmer,       device_dim10, 'DIM10' ) ],
	study       = [ lamp         = (lamp,         device_rel4,  'REL4'  ) ] ]).


/*
Why not...

house([ tv_room = [ rel1  : lamp,
		    lyx   : light_sensor,
		    temp3 : temp_sensor ],
	kitchen = [ rel2  : lamp ] ]).

SL: because it's not a record!

house([ tv_room = [ lamp = rel1,
		    light_sensor = lux
		    temp_sensor = temp3 ],
	kitchen = [ lamp = rel2 ]).


*/
