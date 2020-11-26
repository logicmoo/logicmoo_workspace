
judged_dialogue_processing([['(no words)'],[[utterance_type,command],[action,switch],[onoff,on],[device,fan]],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],[[command,dev(fan,'$VAR'(0),on,100)]],im,good).

judged_dialogue_processing([[command,dev(fan,'$VAR'(0),on,100)],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],[say(dev(fan,kitchen,on,100)),[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,on,100)]],dm,good).

judged_dialogue_processing([say(dev(fan,kitchen,on,100))],[tts([116,104,101,32,102,97,110,32,105,110,32,116,104,101,32,107,105,116,99,104,101,110,32,105,115,32,111,110])],om,good).

judged_dialogue_processing([[is,the,fan,switched,on],[[utterance_type,query],[state,be],[onoff,on],[device,fan]],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,on,100)]],[[query,dev(fan,'$VAR'(0),on,'$VAR'(1))]],im,good).

judged_dialogue_processing([[query,dev(fan,'$VAR'(0),on,'$VAR'(1))],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,on,100)]],[say(dev(fan,kitchen,on,100)),[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,on,100)]],dm,good).

judged_dialogue_processing([say(dev(fan,kitchen,on,100))],[tts([116,104,101,32,102,97,110,32,105,110,32,116,104,101,32,107,105,116,99,104,101,110,32,105,115,32,111,110])],om,good).

judged_dialogue_processing([[switch,on,the,light,in,the,kitchen],[[utterance_type,command],[action,switch],[onoff,on],[device,light],[location,kitchen]],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,on,100)]],[[command,dev(light,kitchen,on,100)]],im,good).

judged_dialogue_processing([[command,dev(light,kitchen,on,100)],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,on,100)]],[say(dev(light,kitchen,on,100)),[dev(light,kitchen,on,100),dev(light,living_room,off,0),dev(fan,kitchen,on,100)]],dm,good).

judged_dialogue_processing([say(dev(light,kitchen,on,100))],[tts([116,104,101,32,108,105,103,104,116,32,105,110,32,116,104,101,32,107,105,116,99,104,101,110,32,105,115,32,111,110])],om,good).

judged_dialogue_processing([[is,the,light,in,the,kitchen,switched,on],[[utterance_type,query],[state,be],[onoff,on],[device,light],[location,kitchen]],[dev(light,kitchen,on,100),dev(light,living_room,off,0),dev(fan,kitchen,on,100)]],[[query,dev(light,kitchen,on,'$VAR'(0))]],im,good).

judged_dialogue_processing([[query,dev(light,kitchen,on,'$VAR'(0))],[dev(light,kitchen,on,100),dev(light,living_room,off,0),dev(fan,kitchen,on,100)]],[say(dev(light,kitchen,on,100)),[dev(light,kitchen,on,100),dev(light,living_room,off,0),dev(fan,kitchen,on,100)]],dm,good).

judged_dialogue_processing([say(dev(light,kitchen,on,100))],[tts([116,104,101,32,108,105,103,104,116,32,105,110,32,116,104,101,32,107,105,116,99,104,101,110,32,105,115,32,111,110])],om,good).

judged_dialogue_processing([[switch,on,the,light],[[utterance_type,command],[action,switch],[onoff,on],[device,light]],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],[[command,dev(light,'$VAR'(0),on,100)]],im,good).

judged_dialogue_processing([[command,dev(light,'$VAR'(0),on,100)],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],[say(ambiguous),[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],dm,good).

judged_dialogue_processing([say(ambiguous)],[tts([115,111,114,114,121,44,32,116,104,97,116,39,115,32,97,109,98,105,103,117,111,117,115])],om,good).

%Judgements added 2008-02-22_11-22-33

judged_dialogue_processing([[switch,on,the,light,in,the,kitchen],[[utterance_type,command],[action,switch],[onoff,on],[device,light],[location,kitchen]],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],[[command,dev(light,kitchen,on,100)]],im,good).

judged_dialogue_processing([[command,dev(light,kitchen,on,100)],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],[say(dev(light,kitchen,on,100)),[dev(light,kitchen,on,100),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],dm,good).

judged_dialogue_processing([[switch,off,the,light],[[utterance_type,command],[action,switch],[onoff,off],[device,light]],[dev(light,kitchen,on,100),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],[[command,dev(light,'$VAR'(0),off,0)]],im,good).

judged_dialogue_processing([[command,dev(light,'$VAR'(0),off,0)],[dev(light,kitchen,on,100),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],[say(dev(light,kitchen,off,0)),[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],dm,good).

judged_dialogue_processing([say(dev(light,kitchen,off,0))],[tts([116,104,101,32,108,105,103,104,116,32,105,110,32,116,104,101,32,107,105,116,99,104,101,110,32,105,115,32,111,102,102])],om,good).

judged_dialogue_processing([[is,the,light,switched,on,in,the,living,room],[[utterance_type,query],[state,be],[onoff,on],[device,light],[location,living_room]],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],[[query,dev(light,living_room,on,'$VAR'(0))]],im,good).

judged_dialogue_processing([[query,dev(light,living_room,on,'$VAR'(0))],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],[say(no),[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],dm,good).

judged_dialogue_processing([say(no)],[tts([110,111])],om,good).

judged_dialogue_processing([[dim,the,light],[[utterance_type,command],[action,dim],[device,light]],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],[[command,dev(light,'$VAR'(0),on,50)]],im,good).

judged_dialogue_processing([[command,dev(light,'$VAR'(0),on,50)],[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],[say(ambiguous),[dev(light,kitchen,off,0),dev(light,living_room,off,0),dev(fan,kitchen,off,0)]],dm,good).
