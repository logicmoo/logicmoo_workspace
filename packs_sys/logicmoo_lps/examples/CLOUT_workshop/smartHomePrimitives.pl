:- use_module(lps_utils(redisclient)).



getSmartHomeInfo(User,Password) :-
    restapi_login('https://ictlab.ap.ngrok.io/restapi/',User,Password),
    getSmartHomeInfo.

getSmartHomeInfo :-
    (redisclient:restapi_token(_)->true; 
        (print_message(error, "Please login first"-[]), fail) ),
    restapi_request('tag/devices/',ID,(
        print_message(information,"Fetching..."-[]),
        restapi_request_result(ID,json(R)), 
        memberchk(results=Devices,R),
        forall(member(json(Device),Devices),(
            memberchk(name=Name,Device),
            memberchk(device_tag=Tags_,Device), 
            findall(tag(TID,Tname,Fullname,Type),(
                member(json(Tag),Tags_),
                memberchk(name=Tname,Tag), memberchk(full_name=Fullname,Tag), 
                memberchk(type=Type,Tag), memberchk(id=TID,Tag)
                ),
            Tags),
            assert(device(Name,Tags))
            )),
        print_message(informational,"..loaded smart home information"-[])
        )),
    restapi_server_base(Base),
    print_message(informational,"Info was requested to server ~a, hold on..."-[Base]). 

/*
?- redisclient:restapi_request_result(restapi5,R), print_term(R,[]).
json([ count = 1,
       next = @(null),
       previous = @(null),
       results = [ json([ (id = 1),
		(name = 'Turn on the lights on 3 pm'),
		(conditions = [ json([ (day = [1,0,2,3,4,5,6]),
			 (time = '15:00:00'),
			 (type = schedule_weekday),
			 (datetime = '2021-01-26T13:23:56'),
			 (yearly_repeat = @(true))
		       ])
		]),
		(actions = [ json([ (tag = 'ict.HueLight01.onoff'),
			 (type = set_tag),
			 (value = '1'),
			 (minMax = json([max='1',min='0']))
		       ]),
		  json([ (tag = 'ict.HueLight02.onoff'),
			 (type = set_tag),
			 (value = '1'),
			 (minMax = json([max='1',min='0']))
		       ])
		]),
		(active = @(true)),
		(schedule = @(true)),
		(owner = 1)
	      ])
       ]
     ])
*/