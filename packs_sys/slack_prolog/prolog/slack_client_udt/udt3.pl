(base) root@gitlab:/opt/logicmoo_workspace/packs_sys# swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.0.3)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- use_module(library(slack_client)).
% dot_cfg:using_dot_type(core,slack_client)
Installed packages (23):

i aleph@5                   - Aleph Inductive Logic Programming system
i body_reordering@1.4.111   - Clause expansion Utils for deciding which order to run Goals in a body
i dictoo@1.4.111            - Dict-like OO Syntax
i eggdrop@1.4.111           - Hook up to an existing IRC Client called an Eggdrop
i flux@1.1.118              - FLUX: A logic programming method for reasoning agents and ALPprologis a Prolog implementation of an action programming language. With ALPprolog you can program strategies for autonomous agents in dynamic domains like e.g. the Wumpus world.
i gvar_syntax@1.4.111       - Global Variable Syntax
i instant_prolog_docs@1.4.111 - Magically document prolog source files based on predicate and variable naming conventions
i logicmoo_base@1.4.111     - LogicMOO - Extends Prolog Programming to support Dynamic Epistemic Logic (DEL) with Constraints
i logicmoo_nlu@1.4.111      - Various English to Logic Converters - warning: HUGE amount of lexical and test data
i logicmoo_utils@1.4.111    - Common predicates that are used throughout LogicMOO Software
i logtalk@3.37.0            - Logtalk - Object-Oriented Logic Programming Language
i multimodal_dcg@1.4.111    - Reduce floundering of DCGs by constraining and narrowing search
i pfc@1.4.111               - Pfc -- a package for forward chaining in Prolog
i planner_api@1.1.118       - planner_api -- A SWI-Prolog Pack that lets Prolog code seamlessly use planners speaking PDDLish and OCLh
i predicate_streams@1.4.111 - Implement your own Abstract Predicate Streams
i programk@1.4.111          - AIML 2.0 - Because an AIML Interpreter/Chatbot needed to exist in Prolog to!
i prologmud@1.4.111         - Online text adventure game - MUD Server
i prologmud_samples@1.4.111 - Online text adventure game - Sample
i rdet@1.0.1                - Runtime determinacy checker
i slack_prolog@1.1.118      - Prolog interface to Slack http://www.slack.com
i tabling_dra@1.4.111       - SWI-Prolog interface to Table-handling procedures for the "dra" interpreter. Written by Feliks Kluzniak at UTD (March 2009)
i tauchain_prolog@1.4.111   - Tauchain implemented in Prolog - See IDNI.org Tauchain
i wam_common_lisp@1.4.111   - ANSI Common Lisp implemented in Prolog
% HI there
% tcp_connect(<socket>(0x7fc4cc023680),gitlab:3334).
% ÿû
% PrologMUD  (Eggdrop v1.6.21 (C) 1997 Robey Pointer (C) 2011 Eggheads)
% Please enter your nickname.
% (If you are new, enter 'NEW' here.)
% Enter your password.
% Connected to PrologMUD, running eggdrop v1.6.21
%      ____                __
%     / __/___ _ ___ _ ___/ /____ ___   ___
%    / _/ / _ `// _ `// _  // __// _ \ / _ \
%   /___/ \_, / \_, / \_,_//_/   \___// .__/
%        /___/ /___/                 /_/
PuTTY% Hey swipl!  My name is PrologMUD and I am running eggdrop v1.6.21, on Linux 5.0.0-13-generic.
% Local time is now 22:58
% You are an owner of this bot. Only +n users can see this! For more info,
% see .help set motd. Please edit the motd file in your bot's 'text'
% Use .help for basic help.
% Use .help <command> for help on a specific command.
% Use .help all to get a full command list.
% Use .help *somestring* to list any help texts containing "somestring".
% Have fun.
% Commands start with '.' (like '.quit' or '.help')
% Everything else goes out to the party line.
% maybe_call(get2react([chon,"swipl","10"])).
% You have no messages.
% *** swipl joined the party line.
% Echo turned off.
% Set your console to ##prolog: - (none).
true.

?- PuTTY% Set your console to #logicmoo: - (none).
% add_slack_info1(var,accept_tos_url,"https://prologclassworkspace.slack.com/account/tos")
% add_slack_info1(var,instance,'B011BQ6E60P')
% add_slack_info1('B011BQ6E60P',app_id,"A6NL8MJ6Q")
% add_slack_info1('B011BQ6E60P',deleted,false)
% add_slack_info1('B011BQ6E60P',icons,_213484{image_36:"https://a.slack-edge.com/80588/img/services/gdrive_36.png",image_48:"https://a.slack-edge.com/80588/img/plugins/gdrive/service_48.png",image_72:"https://a.slack-edge.com/80588/img/plugins/gdrive/service_72.png"})
% add_slack_info1('B011BQ6E60P',id,"B011BQ6E60P")
% add_slack_info1('B011BQ6E60P',name,"Google Drive")
% add_slack_info1('B011BQ6E60P',updated,1586327475)
% add_slack_info1(var,instance,'B011CBBP8P9')
% add_slack_info1('B011CBBP8P9',app_id,"A02")
% add_slack_info1('B011CBBP8P9',deleted,false)
% add_slack_info1('B011CBBP8P9',icons,_213642{image_36:"https://a.slack-edge.com/80588/img/plugins/app/bot_36.png",image_48:"https://a.slack-edge.com/80588/img/plugins/app/bot_48.png",image_72:"https://a.slack-edge.com/80588/img/plugins/app/service_72.png"})
% add_slack_info1('B011CBBP8P9',id,"B011CBBP8P9")
% add_slack_info1('B011CBBP8P9',name,"Slack API Tester")
% add_slack_info1('B011CBBP8P9',updated,1586485210)
% add_slack_info1(var,instance,'B011CKJMJQP')
% add_slack_info1('B011CKJMJQP',app_id,"A0F7VRG6Q")
% add_slack_info1('B011CKJMJQP',deleted,false)
% add_slack_info1('B011CKJMJQP',icons,_213800{image_36:"https://a.slack-edge.com/80588/img/services/outgoing-webhook_36.png",image_48:"https://a.slack-edge.com/80588/img/services/outgoing-webhook_48.png",image_72:"https://a.slack-edge.com/80588/img/services/outgoing-webhook_72.png"})
% add_slack_info1('B011CKJMJQP',id,"B011CKJMJQP")
% add_slack_info1('B011CKJMJQP',name,"outgoing-webhook")
% add_slack_info1('B011CKJMJQP',updated,1586664716)
% add_slack_info1(var,instance,'B011LFELPJM')
% add_slack_info1('B011LFELPJM',app_id,"A6NL8MJ6Q")
% add_slack_info1('B011LFELPJM',deleted,false)
% add_slack_info1('B011LFELPJM',icons,_213958{image_36:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_36.png",image_48:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_48.png",image_72:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_72.png"})
% add_slack_info1('B011LFELPJM',id,"B011LFELPJM")
% add_slack_info1('B011LFELPJM',name,"Google Drive")
% add_slack_info1('B011LFELPJM',updated,1586327493)
% add_slack_info1(var,instance,'B011LFELQKB')
% add_slack_info1('B011LFELQKB',app_id,"A6NL8MJ6Q")
% add_slack_info1('B011LFELQKB',deleted,false)
% add_slack_info1('B011LFELQKB',icons,_214116{image_36:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_36.png",image_48:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_48.png",image_72:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_72.png"})
% add_slack_info1('B011LFELQKB',id,"B011LFELQKB")
% add_slack_info1('B011LFELQKB',name,"Google Drive")
% add_slack_info1('B011LFELQKB',updated,1586327493)
% add_slack_info1(var,instance,'B011P80BDQD')
% add_slack_info1('B011P80BDQD',app_id,"A6NL8MJ6Q")
% add_slack_info1('B011P80BDQD',deleted,false)
% add_slack_info1('B011P80BDQD',icons,_214274{image_36:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_36.png",image_48:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_48.png",image_72:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_72.png"})
% add_slack_info1('B011P80BDQD',id,"B011P80BDQD")
% add_slack_info1('B011P80BDQD',name,"Google Drive")
% add_slack_info1('B011P80BDQD',updated,1586598678)
% add_slack_info1(var,instance,'B011PGDHZPF')
% add_slack_info1('B011PGDHZPF',app_id,"A011Z01HK0R")
% add_slack_info1('B011PGDHZPF',deleted,false)
% add_slack_info1('B011PGDHZPF',icons,_214432{image_36:"https://a.slack-edge.com/80588/img/plugins/app/bot_36.png",image_48:"https://a.slack-edge.com/80588/img/plugins/app/bot_48.png",image_72:"https://a.slack-edge.com/80588/img/plugins/app/service_72.png"})
% add_slack_info1('B011PGDHZPF',id,"B011PGDHZPF")
% add_slack_info1('B011PGDHZPF',name,"OwlieBot")
% add_slack_info1('B011PGDHZPF',updated,1586659323)
% add_slack_info1(var,instance,'B011PGDHZS9')
% add_slack_info1('B011PGDHZS9',app_id,"A011Z01HK0R")
% add_slack_info1('B011PGDHZS9',deleted,false)
% add_slack_info1('B011PGDHZS9',icons,_214590{image_36:"https://a.slack-edge.com/80588/img/plugins/app/bot_36.png",image_48:"https://a.slack-edge.com/80588/img/plugins/app/bot_48.png",image_72:"https://a.slack-edge.com/80588/img/plugins/app/service_72.png"})
% add_slack_info1('B011PGDHZS9',id,"B011PGDHZS9")
% add_slack_info1('B011PGDHZS9',name,"OwlieBot")
% add_slack_info1('B011PGDHZS9',updated,1586659323)
% add_slack_info1(var,instance,'B011PGDJ00M')
% add_slack_info1('B011PGDJ00M',app_id,"A011Z01HK0R")
% add_slack_info1('B011PGDJ00M',deleted,false)
% add_slack_info1('B011PGDJ00M',icons,_214748{image_36:"https://a.slack-edge.com/80588/img/plugins/app/bot_36.png",image_48:"https://a.slack-edge.com/80588/img/plugins/app/bot_48.png",image_72:"https://a.slack-edge.com/80588/img/plugins/app/service_72.png"})
% add_slack_info1('B011PGDJ00M',id,"B011PGDJ00M")
% add_slack_info1('B011PGDJ00M',name,"OwlieBot")
% add_slack_info1('B011PGDJ00M',updated,1586659323)
% add_slack_info1(var,instance,'B0129NYJ5NC')
% add_slack_info1('B0129NYJ5NC',app_id,"A0F7XDUAZ")
% add_slack_info1('B0129NYJ5NC',deleted,false)
% add_slack_info1('B0129NYJ5NC',icons,_214906{image_36:"https://a.slack-edge.com/80588/img/services/outgoing-webhook_36.png",image_48:"https://a.slack-edge.com/80588/img/services/outgoing-webhook_48.png",image_72:"https://a.slack-edge.com/80588/img/services/outgoing-webhook_72.png"})
% add_slack_info1('B0129NYJ5NC',id,"B0129NYJ5NC")
% add_slack_info1('B0129NYJ5NC',name,"incoming-webhook")
% add_slack_info1('B0129NYJ5NC',updated,1586664339)
% add_slack_info1(var,instance,'B0129P82UTS')
% add_slack_info1('B0129P82UTS',app_id,"A0F7YS25R")
% add_slack_info1('B0129P82UTS',deleted,false)
% add_slack_info1('B0129P82UTS',icons,_215064{image_36:"https://a.slack-edge.com/80588/img/services/bots_36.png",image_48:"https://a.slack-edge.com/80588/img/plugins/bot/service_48.png",image_72:"https://a.slack-edge.com/80588/img/services/bots_72.png"})
% add_slack_info1('B0129P82UTS',id,"B0129P82UTS")
% add_slack_info1('B0129P82UTS',name,"bot")
% add_slack_info1('B0129P82UTS',updated,1586665440)
% add_slack_info1(var,cache_ts,1586671107)
% add_slack_info1(var,cache_ts_version,"v2-bunny")
% add_slack_info1(var,cache_version,"v21-nomad")
% add_slack_info1(var,can_manage_shared_channels,false)
% add_slack_info1(var,instance,'C010LM69Q9X')
% add_slack_info1('C010LM69Q9X',created,1585303371)
% add_slack_info1('C010LM69Q9X',creator,"U010LM68Y65")
% add_slack_info1('C010LM69Q9X',has_pins,false)
% add_slack_info1('C010LM69Q9X',id,"C010LM69Q9X")
% add_slack_info1('C010LM69Q9X',is_archived,false)
% add_slack_info1('C010LM69Q9X',is_channel,true)
% add_slack_info1('C010LM69Q9X',is_general,false)
% add_slack_info1('C010LM69Q9X',is_member,false)
% add_slack_info1('C010LM69Q9X',is_mpim,false)
% add_slack_info1('C010LM69Q9X',is_org_shared,false)
% add_slack_info1('C010LM69Q9X',is_private,false)
% add_slack_info1('C010LM69Q9X',is_shared,false)
% add_slack_info1('C010LM69Q9X',name,"random")
% add_slack_info1('C010LM69Q9X',name_normalized,"random")
% add_slack_info1('C010LM69Q9X',previous_names,[])
% add_slack_info1('C010LM69Q9X',priority,0)
% add_slack_info1('C010LM69Q9X',unlinked,0)
% add_slack_info1(var,instance,'C010WLMHEJH')
% add_slack_info1('C010WLMHEJH',created,1585303371)
% add_slack_info1('C010WLMHEJH',creator,"U010LM68Y65")
% add_slack_info1('C010WLMHEJH',has_pins,true)
% add_slack_info1('C010WLMHEJH',id,"C010WLMHEJH")
% add_slack_info1('C010WLMHEJH',is_archived,false)
% add_slack_info1('C010WLMHEJH',is_channel,true)
% add_slack_info1('C010WLMHEJH',is_general,true)
% add_slack_info1('C010WLMHEJH',is_member,false)
% add_slack_info1('C010WLMHEJH',is_mpim,false)
% add_slack_info1('C010WLMHEJH',is_org_shared,false)
% add_slack_info1('C010WLMHEJH',is_private,false)
% add_slack_info1('C010WLMHEJH',is_shared,false)
% add_slack_info1('C010WLMHEJH',name,"general")
% add_slack_info1('C010WLMHEJH',name_normalized,"general")
% add_slack_info1('C010WLMHEJH',previous_names,[])
% add_slack_info1('C010WLMHEJH',priority,0)
% add_slack_info1('C010WLMHEJH',unlinked,0)
% add_slack_info1(var,instance,'C010WLMHTPF')
% add_slack_info1('C010WLMHTPF',created,1585303371)
% add_slack_info1('C010WLMHTPF',creator,"U010LM68Y65")
% add_slack_info1('C010WLMHTPF',has_pins,false)
% add_slack_info1('C010WLMHTPF',id,"C010WLMHTPF")
% add_slack_info1('C010WLMHTPF',is_archived,false)
% add_slack_info1('C010WLMHTPF',is_channel,true)
% add_slack_info1('C010WLMHTPF',is_general,false)
% add_slack_info1('C010WLMHTPF',is_member,false)
% add_slack_info1('C010WLMHTPF',is_mpim,false)
% add_slack_info1('C010WLMHTPF',is_org_shared,false)
% add_slack_info1('C010WLMHTPF',is_private,false)
% add_slack_info1('C010WLMHTPF',is_shared,false)
% add_slack_info1('C010WLMHTPF',name,"announcements")
% add_slack_info1('C010WLMHTPF',name_normalized,"announcements")
% add_slack_info1('C010WLMHTPF',previous_names,[])
% add_slack_info1('C010WLMHTPF',priority,0)
% add_slack_info1('C010WLMHTPF',unlinked,0)
% add_slack_info1(var,instance,'C011BE1SZGF')
% add_slack_info1('C011BE1SZGF',created,1586206871)
% add_slack_info1('C011BE1SZGF',creator,"U0115Q96815")
% add_slack_info1('C011BE1SZGF',has_pins,false)
% add_slack_info1('C011BE1SZGF',id,"C011BE1SZGF")
% add_slack_info1('C011BE1SZGF',is_archived,false)
% add_slack_info1('C011BE1SZGF',is_channel,true)
% add_slack_info1('C011BE1SZGF',is_general,false)
% add_slack_info1('C011BE1SZGF',is_member,false)
% add_slack_info1('C011BE1SZGF',is_mpim,false)
% add_slack_info1('C011BE1SZGF',is_org_shared,false)
% add_slack_info1('C011BE1SZGF',is_private,false)
% add_slack_info1('C011BE1SZGF',is_shared,false)
% add_slack_info1('C011BE1SZGF',name,"annie-private")
% add_slack_info1('C011BE1SZGF',name_normalized,"annie-private")
% add_slack_info1('C011BE1SZGF',previous_names,[])
% add_slack_info1('C011BE1SZGF',priority,0)
% add_slack_info1('C011BE1SZGF',unlinked,0)
% add_slack_info1(var,instance,'C011CJSP6P9')
% add_slack_info1('C011CJSP6P9',created,1586631019)
% add_slack_info1('C011CJSP6P9',creator,"U011KMTBGN8")
% add_slack_info1('C011CJSP6P9',has_pins,false)
% add_slack_info1('C011CJSP6P9',id,"C011CJSP6P9")
% add_slack_info1('C011CJSP6P9',is_archived,false)
% add_slack_info1('C011CJSP6P9',is_channel,true)
% add_slack_info1('C011CJSP6P9',is_general,false)
% add_slack_info1('C011CJSP6P9',is_member,false)
% add_slack_info1('C011CJSP6P9',is_mpim,false)
% add_slack_info1('C011CJSP6P9',is_org_shared,false)
% add_slack_info1('C011CJSP6P9',is_private,false)
% add_slack_info1('C011CJSP6P9',is_shared,false)
% add_slack_info1('C011CJSP6P9',name,"pet_talk")
% add_slack_info1('C011CJSP6P9',name_normalized,"pet_talk")
% add_slack_info1('C011CJSP6P9',previous_names,[])
% add_slack_info1('C011CJSP6P9',priority,0)
% add_slack_info1('C011CJSP6P9',unlinked,0)
% add_slack_info1(var,instance,'C011HV9FHQT')
% add_slack_info1('C011HV9FHQT',created,1586440307)
% add_slack_info1('C011HV9FHQT',creator,"U010LM68Y65")
% add_slack_info1('C011HV9FHQT',has_pins,false)
% add_slack_info1('C011HV9FHQT',id,"C011HV9FHQT")
% add_slack_info1('C011HV9FHQT',is_archived,false)
% add_slack_info1('C011HV9FHQT',is_channel,true)
% add_slack_info1('C011HV9FHQT',is_general,false)
% add_slack_info1('C011HV9FHQT',is_member,false)
% add_slack_info1('C011HV9FHQT',is_mpim,false)
% add_slack_info1('C011HV9FHQT',is_org_shared,false)
% add_slack_info1('C011HV9FHQT',is_private,false)
% add_slack_info1('C011HV9FHQT',is_shared,false)
% add_slack_info1('C011HV9FHQT',name,"studygroup")
% add_slack_info1('C011HV9FHQT',name_normalized,"studygroup")
% add_slack_info1('C011HV9FHQT',previous_names,[])
% add_slack_info1('C011HV9FHQT',priority,0)
% add_slack_info1('C011HV9FHQT',unlinked,0)
% add_slack_info1(var,instance,'C011LNF6F9P')
% add_slack_info1('C011LNF6F9P',created,1586332652)
% add_slack_info1('C011LNF6F9P',creator,"U010LM68Y65")
% add_slack_info1('C011LNF6F9P',has_pins,true)
% add_slack_info1('C011LNF6F9P',id,"C011LNF6F9P")
% add_slack_info1('C011LNF6F9P',is_archived,false)
% add_slack_info1('C011LNF6F9P',is_channel,true)
% add_slack_info1('C011LNF6F9P',is_general,false)
% add_slack_info1('C011LNF6F9P',is_member,false)
% add_slack_info1('C011LNF6F9P',is_mpim,false)
% add_slack_info1('C011LNF6F9P',is_org_shared,false)
% add_slack_info1('C011LNF6F9P',is_private,false)
% add_slack_info1('C011LNF6F9P',is_shared,false)
% add_slack_info1('C011LNF6F9P',name,"instructorannouncements")
% add_slack_info1('C011LNF6F9P',name_normalized,"instructorannouncements")
% add_slack_info1('C011LNF6F9P',previous_names,[])
% add_slack_info1('C011LNF6F9P',priority,0)
% add_slack_info1('C011LNF6F9P',unlinked,0)
% add_slack_info1(var,instance,'C011M65UV8R')
% add_slack_info1('C011M65UV8R',created,1586340605)
% add_slack_info1('C011M65UV8R',creator,"U010LM68Y65")
% add_slack_info1('C011M65UV8R',has_pins,false)
% add_slack_info1('C011M65UV8R',id,"C011M65UV8R")
% add_slack_info1('C011M65UV8R',is_archived,false)
% add_slack_info1('C011M65UV8R',is_channel,true)
% add_slack_info1('C011M65UV8R',is_general,false)
% add_slack_info1('C011M65UV8R',is_member,false)
% add_slack_info1('C011M65UV8R',is_mpim,false)
% add_slack_info1('C011M65UV8R',is_org_shared,false)
% add_slack_info1('C011M65UV8R',is_private,false)
% add_slack_info1('C011M65UV8R',is_shared,false)
% add_slack_info1('C011M65UV8R',name,"help")
% add_slack_info1('C011M65UV8R',name_normalized,"help")
% add_slack_info1('C011M65UV8R',previous_names,[])
% add_slack_info1('C011M65UV8R',priority,0)
% add_slack_info1('C011M65UV8R',unlinked,0)
% add_slack_info1(var,instance,'C011VR42F6Y')
% add_slack_info1('C011VR42F6Y',created,1586663738)
% add_slack_info1('C011VR42F6Y',creator,"U010LM68Y65")
% add_slack_info1('C011VR42F6Y',has_pins,false)
% add_slack_info1('C011VR42F6Y',id,"C011VR42F6Y")
% add_slack_info1('C011VR42F6Y',is_archived,false)
% add_slack_info1('C011VR42F6Y',is_channel,true)
% add_slack_info1('C011VR42F6Y',is_general,false)
% add_slack_info1('C011VR42F6Y',is_member,true)
% add_slack_info1('C011VR42F6Y',is_mpim,false)
% add_slack_info1('C011VR42F6Y',is_org_shared,false)
% add_slack_info1('C011VR42F6Y',is_private,false)
% add_slack_info1('C011VR42F6Y',is_shared,false)
% add_slack_info1('C011VR42F6Y',last_read,"1586667243.004400")
% add_slack_info1('C011VR42F6Y',members,["U0129P82V6U","U011Z6B31UH","U011PGDJ07P","U0115Q96815","U010LM68Y65"])
% add_slack_info1('C011VR42F6Y',name,"bottestchannel")
% add_slack_info1('C011VR42F6Y',name_normalized,"bottestchannel")
% add_slack_info1('C011VR42F6Y',previous_names,[])
% add_slack_info1('C011VR42F6Y',priority,0)
% add_slack_info1('C011VR42F6Y',purpose,_151850{creator:"U010LM68Y65",last_set:1586663739,value:"channel for testing the bot"})
% add_slack_info1('C011VR42F6Y',topic,_151786{creator:"",last_set:0,value:""})
% add_slack_info1('C011VR42F6Y',unlinked,0)
% add_slack_info1(var,instance,'C011XJ80QQG')
% add_slack_info1('C011XJ80QQG',created,1586340699)
% add_slack_info1('C011XJ80QQG',creator,"U010LM68Y65")
% add_slack_info1('C011XJ80QQG',has_pins,false)
% add_slack_info1('C011XJ80QQG',id,"C011XJ80QQG")
% add_slack_info1('C011XJ80QQG',is_archived,false)
% add_slack_info1('C011XJ80QQG',is_channel,true)
% add_slack_info1('C011XJ80QQG',is_general,false)
% add_slack_info1('C011XJ80QQG',is_member,false)
% add_slack_info1('C011XJ80QQG',is_mpim,false)
% add_slack_info1('C011XJ80QQG',is_org_shared,false)
% add_slack_info1('C011XJ80QQG',is_private,false)
% add_slack_info1('C011XJ80QQG',is_shared,false)
% add_slack_info1('C011XJ80QQG',name,"help-remote-learning")
% add_slack_info1('C011XJ80QQG',name_normalized,"help-remote-learning")
% add_slack_info1('C011XJ80QQG',previous_names,[])
% add_slack_info1('C011XJ80QQG',priority,0)
% add_slack_info1('C011XJ80QQG',unlinked,0)
% add_slack_info1(var,dnd_enabled,false)
% add_slack_info1(var,next_dnd_end_ts,1)
% add_slack_info1(var,next_dnd_start_ts,1)
% add_slack_info1(var,snooze_enabled,false)
% add_slack_info1(var,groups,[])
% add_slack_info1(var,instance,'D011N5555MY')
% add_slack_info1('D011N5555MY',created,1586667065)
% add_slack_info1('D011N5555MY',has_pins,false)
% add_slack_info1('D011N5555MY',id,"D011N5555MY")
% add_slack_info1('D011N5555MY',is_archived,false)
% add_slack_info1('D011N5555MY',is_im,true)
% add_slack_info1('D011N5555MY',is_open,false)
% add_slack_info1('D011N5555MY',is_org_shared,false)
% add_slack_info1('D011N5555MY',last_read,"0000000000.000000")
% add_slack_info1('D011N5555MY',priority,0)
% add_slack_info1('D011N5555MY',user,"U011Z6B31UH")
% add_slack_info1(var,instance,'D0129P82VMW')
% add_slack_info1('D0129P82VMW',created,1586665440)
% add_slack_info1('D0129P82VMW',has_pins,false)
% add_slack_info1('D0129P82VMW',id,"D0129P82VMW")
% add_slack_info1('D0129P82VMW',is_archived,false)
% add_slack_info1('D0129P82VMW',is_im,true)
% add_slack_info1('D0129P82VMW',is_open,true)
% add_slack_info1('D0129P82VMW',is_org_shared,false)
% add_slack_info1('D0129P82VMW',last_read,"0000000000.000000")
% add_slack_info1('D0129P82VMW',priority,0)
% add_slack_info1('D0129P82VMW',user,"USLACKBOT")
% add_slack_info1(var,is_europe,false)
% add_slack_info1(var,latest_event_ts,"1586670507.000000")
% add_slack_info1(var,non_threadable_channels,[])
% add_slack_info1(var,ok,true)
% add_slack_info1(var,read_only_channels,[])
% add_slack_info1(var,created,1586665440)
% add_slack_info1(var,id,"U0129P82V6U")
% add_slack_info1(var,manual_presence,"active")
% add_slack_info1(var,name,"prolog_bot")
% add_slack_info1(var,a11y_animations,true)
% add_slack_info1(var,activity_view,"page")
% add_slack_info1(var,add_apps_prompt_dismissed,false)
% add_slack_info1(var,add_channel_prompt_dismissed,false)
% add_slack_info1(var,all_channels_loud,false)
% add_slack_info1(var,all_notifications_prefs,"{\"global\":{\"global_desktop\":\"mentions_dms\",\"global_mpdm_desktop\":\"everything\",\"global_mobile\":\"mentions_dms\",\"global_mpdm_mobile\":\"everything\",\"mobile_sound\":\"b2.mp3\",\"desktop_sound\":\"knock_brush.mp3\",\"global_keywords\":\"\",\"push_idle_wait\":0,\"no_text_in_notifications\":false,\"push_show_preview\":true,\"threads_everything\":true},\"channels\":[]}")
% add_slack_info1(var,all_unreads_sort_order,"")
% add_slack_info1(var,allow_calls_to_set_current_status,true)
% add_slack_info1(var,allow_cmd_tab_iss,false)
% add_slack_info1(var,analytics_upsell_coachmark_seen,false)
% add_slack_info1(var,app_subdomain_check_completed,0)
% add_slack_info1(var,arrow_history,false)
% add_slack_info1(var,at_channel_suppressed_channels,"")
% add_slack_info1(var,box_enabled,false)
% add_slack_info1(var,browsers_dismissed_channels_low_results_education,false)
% add_slack_info1(var,browsers_dismissed_files_low_results_education,false)
% add_slack_info1(var,browsers_dismissed_initial_activity_education,false)
% add_slack_info1(var,browsers_dismissed_initial_drafts_education,false)
% add_slack_info1(var,browsers_dismissed_initial_saved_education,false)
% add_slack_info1(var,browsers_dismissed_people_low_results_education,false)
% add_slack_info1(var,browsers_dismissed_user_groups_low_results_education,false)
% add_slack_info1(var,browsers_seen_initial_activity_education,false)
% add_slack_info1(var,browsers_seen_initial_channels_education,false)
% add_slack_info1(var,browsers_seen_initial_drafts_education,false)
% add_slack_info1(var,browsers_seen_initial_files_education,false)
% add_slack_info1(var,browsers_seen_initial_people_education,false)
% add_slack_info1(var,browsers_seen_initial_saved_education,false)
% add_slack_info1(var,browsers_seen_initial_user_groups_education,false)
% add_slack_info1(var,channel_sections,"")
% add_slack_info1(var,channel_sidebar_hide_invite,false)
% add_slack_info1(var,channel_sort,"default")
% add_slack_info1(var,client_logs_pri,"")
% add_slack_info1(var,color_names_in_list,true)
% add_slack_info1(var,confirm_clear_all_unreads,true)
% add_slack_info1(var,confirm_sh_call_start,true)
% add_slack_info1(var,confirm_user_marked_away,true)
% add_slack_info1(var,contextual_message_shortcuts_modal_was_seen,true)
% add_slack_info1(var,convert_emoticons,true)
% add_slack_info1(var,deprecation_modal_last_seen,0)
% add_slack_info1(var,deprecation_toast_last_seen,0)
% add_slack_info1(var,dismissed_app_launcher_limit,false)
% add_slack_info1(var,dismissed_app_launcher_welcome,false)
% add_slack_info1(var,dismissed_installed_app_dm_suggestions,"")
% add_slack_info1(var,dismissed_scroll_search_tooltip_count,0)
% add_slack_info1(var,display_display_names,true)
% add_slack_info1(var,display_real_names_override,0)
% add_slack_info1(var,dnd_after_friday,"22:00")
% add_slack_info1(var,dnd_after_monday,"22:00")
% add_slack_info1(var,dnd_after_saturday,"22:00")
% add_slack_info1(var,dnd_after_sunday,"22:00")
% add_slack_info1(var,dnd_after_thursday,"22:00")
% add_slack_info1(var,dnd_after_tuesday,"22:00")
% add_slack_info1(var,dnd_after_wednesday,"22:00")
% add_slack_info1(var,dnd_before_friday,"8:00")
% add_slack_info1(var,dnd_before_monday,"8:00")
% add_slack_info1(var,dnd_before_saturday,"8:00")
% add_slack_info1(var,dnd_before_sunday,"8:00")
% add_slack_info1(var,dnd_before_thursday,"8:00")
% add_slack_info1(var,dnd_before_tuesday,"8:00")
% add_slack_info1(var,dnd_before_wednesday,"8:00")
% add_slack_info1(var,dnd_custom_new_badge_seen,false)
% add_slack_info1(var,dnd_days,"every_day")
% add_slack_info1(var,dnd_enabled,true)
% add_slack_info1(var,dnd_enabled_friday,"partial")
% add_slack_info1(var,dnd_enabled_monday,"partial")
% add_slack_info1(var,dnd_enabled_saturday,"partial")
% add_slack_info1(var,dnd_enabled_sunday,"partial")
% add_slack_info1(var,dnd_enabled_thursday,"partial")
% add_slack_info1(var,dnd_enabled_tuesday,"partial")
% add_slack_info1(var,dnd_enabled_wednesday,"partial")
% add_slack_info1(var,dnd_end_hour,"8:00")
% add_slack_info1(var,dnd_notification_schedule_new_badge_seen,false)
% add_slack_info1(var,dnd_start_hour,"22:00")
% add_slack_info1(var,dnd_weekdays_off_allday,false)
% add_slack_info1(var,dropbox_enabled,false)
% add_slack_info1(var,edge_upload_proxy_check_completed,0)
% add_slack_info1(var,email_alerts,"instant")
% add_slack_info1(var,email_alerts_sleep_until,0)
% add_slack_info1(var,email_developer,true)
% add_slack_info1(var,email_offers,true)
% add_slack_info1(var,email_research,true)
% add_slack_info1(var,email_tips,true)
% add_slack_info1(var,email_weekly,true)
% add_slack_info1(var,emoji_autocomplete_big,false)
% add_slack_info1(var,emoji_mode,"default")
% add_slack_info1(var,emoji_use,"")
% add_slack_info1(var,enable_react_emoji_picker,true)
% add_slack_info1(var,enable_unread_view,false)
% add_slack_info1(var,ent_org_wide_channels_sidebar,true)
% add_slack_info1(var,enter_is_special_in_tbt,false)
% add_slack_info1(var,enterprise_excluded_app_teams,null)
% add_slack_info1(var,enterprise_mdm_custom_msg,"")
% add_slack_info1(var,enterprise_migration_seen,true)
% add_slack_info1(var,expand_inline_imgs,true)
% add_slack_info1(var,expand_internal_inline_imgs,true)
% add_slack_info1(var,expand_non_media_attachments,true)
% add_slack_info1(var,expand_snippets,false)
% add_slack_info1(var,f_key_search,false)
% add_slack_info1(var,failover_proxy_check_completed,0)
% add_slack_info1(var,flannel_server_pool,"random")
% add_slack_info1(var,folder_data,"[]")
% add_slack_info1(var,folders_enabled,false)
% add_slack_info1(var,frecency_ent_jumper,"")
% add_slack_info1(var,frecency_ent_jumper_backup,"")
% add_slack_info1(var,frecency_jumper,"")
% add_slack_info1(var,fuller_timestamps,false)
% add_slack_info1(var,graphic_emoticons,false)
% add_slack_info1(var,growls_enabled,true)
% add_slack_info1(var,growth_all_banners_prefs,"")
% add_slack_info1(var,growth_msg_limit_approaching_cta_count,0)
% add_slack_info1(var,growth_msg_limit_approaching_cta_ts,0)
% add_slack_info1(var,growth_msg_limit_long_reached_cta_count,0)
% add_slack_info1(var,growth_msg_limit_long_reached_cta_last_ts,0)
% add_slack_info1(var,growth_msg_limit_reached_cta_count,0)
% add_slack_info1(var,growth_msg_limit_reached_cta_last_ts,0)
% add_slack_info1(var,growth_msg_limit_sixty_day_banner_cta_count,0)
% add_slack_info1(var,growth_msg_limit_sixty_day_banner_cta_last_ts,0)
% add_slack_info1(var,has_acknowledged_shortcut_speedbump,false)
% add_slack_info1(var,has_created_channel,false)
% add_slack_info1(var,has_drafted_message,false)
% add_slack_info1(var,has_installed_apps,null)
% add_slack_info1(var,has_invited,false)
% add_slack_info1(var,has_received_mention_or_reaction,true)
% add_slack_info1(var,has_received_threaded_message,false)
% add_slack_info1(var,has_recently_shared_a_channel,false)
% add_slack_info1(var,has_searched,false)
% add_slack_info1(var,has_starred_item,false)
% add_slack_info1(var,has_uploaded,false)
% add_slack_info1(var,has_used_quickswitcher_shortcut,false)
% add_slack_info1(var,hide_hex_swatch,false)
% add_slack_info1(var,hide_user_group_info_pane,false)
% add_slack_info1(var,highlight_words,"")
% add_slack_info1(var,ia_platform_actions_lab,1)
% add_slack_info1(var,ia_slackbot_survey_timestamp_48h,0)
% add_slack_info1(var,ia_slackbot_survey_timestamp_7d,0)
% add_slack_info1(var,ia_top_nav_theme,"")
% add_slack_info1(var,iap1_lab,0)
% add_slack_info1(var,in_interactive_mas_migration_flow,false)
% add_slack_info1(var,in_prod_surveys_enabled,true)
% add_slack_info1(var,jumbomoji,true)
% add_slack_info1(var,k_key_omnibox,true)
% add_slack_info1(var,k_key_omnibox_auto_hide_count,0)
% add_slack_info1(var,keyboard,null)
% add_slack_info1(var,last_dismissed_scroll_search_tooltip_timestamp,0)
% add_slack_info1(var,last_seen_at_channel_warning,0)
% add_slack_info1(var,last_snippet_type,"")
% add_slack_info1(var,last_tos_acknowledged,null)
% add_slack_info1(var,lessons_enabled,false)
% add_slack_info1(var,load_lato_2,false)
% add_slack_info1(var,locale,"en-US")
% add_slack_info1(var,'de-DE',"Deutsch (Deutschland)")
% add_slack_info1(var,'en-GB',"English (UK)")
% add_slack_info1(var,'en-US',"English (US)")
% add_slack_info1(var,'es-ES',"Español (España)")
% add_slack_info1(var,'es-LA',"Español (Latinoamérica)")
% add_slack_info1(var,'fr-FR',"Français (France)")
% add_slack_info1(var,'ja-JP',"???")
% add_slack_info1(var,'pt-BR',"Português (Brasil)")
% add_slack_info1(var,loud_channels,"")
% add_slack_info1(var,loud_channels_set,"")
% add_slack_info1(var,ls_disabled,false)
% add_slack_info1(var,mac_ssb_bounce,"")
% add_slack_info1(var,mac_ssb_bullet,true)
% add_slack_info1(var,mark_msgs_read_immediately,true)
% add_slack_info1(var,mentions_exclude_at_channels,true)
% add_slack_info1(var,mentions_exclude_at_user_groups,false)
% add_slack_info1(var,mentions_exclude_reactions,false)
% add_slack_info1(var,message_navigation_toast_was_seen,false)
% add_slack_info1(var,messages_theme,"default")
% add_slack_info1(var,msg_input_send_btn,false)
% add_slack_info1(var,msg_input_send_btn_auto_set,false)
% add_slack_info1(var,msg_input_sticky_composer,true)
% add_slack_info1(var,mute_sounds,false)
% add_slack_info1(var,muted_channels,"")
% add_slack_info1(var,needs_initial_password_set,false)
% add_slack_info1(var,never_channels,"")
% add_slack_info1(var,new_msg_snd,"knock_brush.mp3")
% add_slack_info1(var,newxp_seen_last_message,0)
% add_slack_info1(var,newxp_suggested_channels,"C010LM69Q9X,C010WLMHEJH,C010WLMHTPF,C011BE1SZGF,C011CJSP6P9,C011HV9FHQT,C011LNF6F9P,C011M65UV8R,C011XJ80QQG")
% add_slack_info1(var,no_created_overlays,false)
% add_slack_info1(var,no_invites_widget_in_sidebar,false)
% add_slack_info1(var,no_joined_overlays,false)
% add_slack_info1(var,no_macelectron_banner,false)
% add_slack_info1(var,no_macssb1_banner,false)
% add_slack_info1(var,no_macssb2_banner,false)
% add_slack_info1(var,no_omnibox_in_channels,false)
% add_slack_info1(var,no_text_in_notifications,false)
% add_slack_info1(var,no_winssb1_banner,false)
% add_slack_info1(var,obey_inline_img_limit,true)
% add_slack_info1(var,onboarding_cancelled,false)
% add_slack_info1(var,onboarding_complete,false)
% add_slack_info1(var,onboarding_role_apps,null)
% add_slack_info1(var,onboarding_slackbot_conversation_step,0)
% add_slack_info1(var,onboarding_state,0)
% add_slack_info1(var,opened_slackbot_dm,false)
% add_slack_info1(var,overloaded_message_enabled,true)
% add_slack_info1(var,pagekeys_handled,true)
% add_slack_info1(var,plain_text_mode,false)
% add_slack_info1(var,posts_formatting_guide,true)
% add_slack_info1(var,preferred_skin_tone,"")
% add_slack_info1(var,privacy_policy_seen,true)
% add_slack_info1(var,prompted_for_email_disabling,false)
% add_slack_info1(var,purchaser,false)
% add_slack_info1(var,push_at_channel_suppressed_channels,"")
% add_slack_info1(var,push_dm_alert,true)
% add_slack_info1(var,push_everything,false)
% add_slack_info1(var,push_idle_wait,0)
% add_slack_info1(var,push_loud_channels,"")
% add_slack_info1(var,push_loud_channels_set,"")
% add_slack_info1(var,push_mention_alert,true)
% add_slack_info1(var,push_mention_channels,"")
% add_slack_info1(var,push_show_preview,true)
% add_slack_info1(var,push_sound,"b2.mp3")
% add_slack_info1(var,require_at,true)
% add_slack_info1(var,saved_view,"page")
% add_slack_info1(var,search_channel_sort,"relevant")
% add_slack_info1(var,search_exclude_bots,false)
% add_slack_info1(var,search_exclude_channels,"")
% add_slack_info1(var,search_file_sort,"score")
% add_slack_info1(var,search_hide_deactivated_users,false)
% add_slack_info1(var,search_hide_my_channels,false)
% add_slack_info1(var,search_only_current_team,false)
% add_slack_info1(var,search_only_my_channels,false)
% add_slack_info1(var,search_only_show_online,false)
% add_slack_info1(var,search_people_sort,"relevant")
% add_slack_info1(var,search_sort,"not_set")
% add_slack_info1(var,seen_administration_menu,false)
% add_slack_info1(var,seen_app_space_coachmark,false)
% add_slack_info1(var,seen_app_space_tutorial,false)
% add_slack_info1(var,seen_calls_interactive_coachmark,false)
% add_slack_info1(var,seen_channel_browser_admin_coachmark,false)
% add_slack_info1(var,seen_channel_search,false)
% add_slack_info1(var,seen_contextual_message_shortcuts_modal,false)
% add_slack_info1(var,seen_corporate_export_alert,false)
% add_slack_info1(var,seen_custom_status_badge,false)
% add_slack_info1(var,seen_custom_status_callout,false)
% add_slack_info1(var,seen_custom_status_expiration_badge,false)
% add_slack_info1(var,seen_domain_invite_reminder,false)
% add_slack_info1(var,seen_drafts_section_coachmark,false)
% add_slack_info1(var,seen_emoji_update_overlay_coachmark,false)
% add_slack_info1(var,seen_gdrive_coachmark,false)
% add_slack_info1(var,seen_guest_admin_slackbot_announcement,false)
% add_slack_info1(var,seen_highlights_arrows_coachmark,false)
% add_slack_info1(var,seen_highlights_coachmark,false)
% add_slack_info1(var,seen_highlights_warm_welcome,false)
% add_slack_info1(var,seen_ia_education,false)
% add_slack_info1(var,seen_intl_channel_names_coachmark,false)
% add_slack_info1(var,seen_japanese_locale_change_message,false)
% add_slack_info1(var,seen_keyboard_shortcuts_coachmark,false)
% add_slack_info1(var,seen_locale_change_message,0)
% add_slack_info1(var,seen_markdown_paste_shortcut,0)
% add_slack_info1(var,seen_markdown_paste_toast,0)
% add_slack_info1(var,seen_member_invite_reminder,false)
% add_slack_info1(var,seen_message_navigation_educational_toast,false)
% add_slack_info1(var,seen_name_tagging_coachmark,false)
% add_slack_info1(var,seen_new_search_ui,false)
% add_slack_info1(var,seen_onboarding_banner,false)
% add_slack_info1(var,seen_onboarding_channels,false)
% add_slack_info1(var,seen_onboarding_direct_messages,false)
% add_slack_info1(var,seen_onboarding_invites,false)
% add_slack_info1(var,seen_onboarding_private_groups,false)
% add_slack_info1(var,seen_onboarding_recent_mentions,false)
% add_slack_info1(var,seen_onboarding_search,false)
% add_slack_info1(var,seen_onboarding_slackbot_conversation,false)
% add_slack_info1(var,seen_onboarding_starred_items,false)
% add_slack_info1(var,seen_onboarding_start,false)
% add_slack_info1(var,seen_p2_locale_change_message,0)
% add_slack_info1(var,seen_people_search,true)
% add_slack_info1(var,seen_people_search_count,0)
% add_slack_info1(var,seen_quickswitcher_shortcut_tip_count,0)
% add_slack_info1(var,seen_shared_channels_coachmark,false)
% add_slack_info1(var,seen_shared_channels_opt_in_change_message,false)
% add_slack_info1(var,seen_shdep_slackbot_message,false)
% add_slack_info1(var,seen_single_emoji_msg,false)
% add_slack_info1(var,seen_sonic_deluxe_toast,0)
% add_slack_info1(var,seen_ssb_prompt,false)
% add_slack_info1(var,seen_threads_notification_banner,false)
% add_slack_info1(var,seen_unread_view_coachmark,false)
% add_slack_info1(var,seen_welcome_2,false)
% add_slack_info1(var,seen_workflow_builder_deluxe_toast,false)
% add_slack_info1(var,seen_wysiwyg_deluxe_toast,false)
% add_slack_info1(var,send_your_first_message_banner_enabled,null)
% add_slack_info1(var,separate_private_channels,false)
% add_slack_info1(var,separate_shared_channels,true)
% add_slack_info1(var,set_tz_automatically,true)
% add_slack_info1(var,shdep_promo_code_submitted,false)
% add_slack_info1(var,show_all_skin_tones,false)
% add_slack_info1(var,show_autocomplete_help,1)
% add_slack_info1(var,show_ent_onboarding,true)
% add_slack_info1(var,show_ia_tour_relaunch,0)
% add_slack_info1(var,show_jumper_scores,false)
% add_slack_info1(var,show_memory_instrument,false)
% add_slack_info1(var,show_shared_channels_education_banner,true)
% add_slack_info1(var,show_sidebar_quickswitcher_button,false)
% add_slack_info1(var,show_typing,true)
% add_slack_info1(var,sidebar_behavior,"")
% add_slack_info1(var,sidebar_theme,"default")
% add_slack_info1(var,sidebar_theme_custom_values,"")
% add_slack_info1(var,snippet_editor_wrap_long_lines,false)
% add_slack_info1(var,spaces_new_xp_banner_dismissed,false)
% add_slack_info1(var,ss_emojis,true)
% add_slack_info1(var,ssb_space_window,"")
% add_slack_info1(var,start_scroll_at_oldest,true)
% add_slack_info1(var,sunset_interactive_message_views,0)
% add_slack_info1(var,suppress_link_warning,false)
% add_slack_info1(var,tab_ui_return_selects,true)
% add_slack_info1(var,threads_everything,true)
% add_slack_info1(var,time24,false)
% add_slack_info1(var,tractor_enabled,false)
% add_slack_info1(var,tractor_experiment_group,"")
% add_slack_info1(var,two_factor_auth_enabled,false)
% add_slack_info1(var,two_factor_backup_type,null)
% add_slack_info1(var,two_factor_type,null)
% add_slack_info1(var,tz,"America/Los_Angeles")
% add_slack_info1(var,unread_collapsed_channels,null)
% add_slack_info1(var,up_to_browse_kb_shortcut,true)
% add_slack_info1(var,used_custom_status_kb_shortcut,false)
% add_slack_info1(var,user_colors,"")
% add_slack_info1(var,webapp_spellcheck,true)
% add_slack_info1(var,welcome_message_hidden,false)
% add_slack_info1(var,welcome_place_state,"none")
% add_slack_info1(var,whats_new_read,1586665440)
% add_slack_info1(var,whocanseethis_dm_mpdm_badge,true)
% add_slack_info1(var,workflow_builder_coachmarks,"{}")
% add_slack_info1(var,workflow_builder_intro_modal_clicked_through,false)
% add_slack_info1(var,all,[])
% add_slack_info1(var,self,[])
% add_slack_info1(var,avatar_base_url,"https://ca.slack-edge.com/")
% add_slack_info1(var,date_create,1585303370)
% add_slack_info1(var,domain,"prologclassworkspace")
% add_slack_info1(var,email_domain,"")
% add_slack_info1(var,image_102,"https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-102.png")
% add_slack_info1(var,image_132,"https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-132.png")
% add_slack_info1(var,image_230,"https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-230.png")
% add_slack_info1(var,image_34,"https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-34.png")
% add_slack_info1(var,image_44,"https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-44.png")
% add_slack_info1(var,image_68,"https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-68.png")
% add_slack_info1(var,image_88,"https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-88.png")
% add_slack_info1(var,image_default,true)
% add_slack_info1(var,id,"T010WMWBAGY")
% add_slack_info1(var,limit_ts,0)
% add_slack_info1(var,messages_count,753)
% add_slack_info1(var,msg_edit_window_mins,-1)
% add_slack_info1(var,name,"PrologClass")
% add_slack_info1(var,onboarding_channel_id,"C010WLMHTPF")
% add_slack_info1(var,over_storage_limit,false)
% add_slack_info1(var,plan,"")
% add_slack_info1(var,all_users_can_purchase,true)
% add_slack_info1(var,allow_calls,true)
% add_slack_info1(var,allow_calls_interactive_screen_sharing,true)
% add_slack_info1(var,allow_message_deletion,true)
% add_slack_info1(var,allow_retention_override,false)
% add_slack_info1(var,app_whitelist_enabled,false)
% add_slack_info1(var,auth_mode,"normal")
% add_slack_info1(var,block_file_download,false)
% add_slack_info1(var,box_app_installed,false)
% add_slack_info1(var,calling_app_name,"Slack")
% add_slack_info1(var,audio,[])
% add_slack_info1(var,profile_field_options,[])
% add_slack_info1(var,instance,'A00')
% add_slack_info1('A00',id,"A00")
% add_slack_info1('A00',image,"img/slack_hash_128.png")
% add_slack_info1('A00',name,"Slack")
% add_slack_info1(var,can_receive_shared_channels_invites,true)
% add_slack_info1(var,channel_email_addresses_enabled,true)
% add_slack_info1(var,compliance_export_start,0)
% add_slack_info1(var,custom_contact_email,null)
% add_slack_info1(var,custom_status_default_emoji,":speech_balloon:")
% unknown(slack_receive(var,custom_status_presets-[[":spiral_calendar_pad:","In a meeting","In a meeting","1_hour"],[":bus:","Commuting","Commuting","30_minutes"],[":face_with_thermometer:","Out sick","Out sick","all_day"],[":palm_tree:","Vacationing","Vacationing","no_expiration"],[":house_with_garden:","Working remotely","Working remotely","all_day"]]))
% add_slack_info1(var,default_channel_creation_enabled,true)
% unknown(slack_receive(var,default_channels-["C010WLMHEJH","C010LM69Q9X","C010WLMHTPF","C011LNF6F9P","C011M65UV8R","C011XJ80QQG","C011HV9FHQT","C011CJSP6P9"]))
% unknown(slack_receive(var,default_rxns-["simple_smile","thumbsup","white_check_mark","heart","eyes"]))
% add_slack_info1(var,dev_company_segment,null)
% add_slack_info1(var,disable_email_ingestion,false)
% add_slack_info1(var,disable_file_deleting,false)
% add_slack_info1(var,disable_file_editing,false)
% add_slack_info1(var,disable_file_uploads,"allow_all")
% add_slack_info1(var,disable_sidebar_connect_prompts,[])
% add_slack_info1(var,disable_sidebar_install_prompts,[])
% add_slack_info1(var,disallow_public_file_urls,false)
% add_slack_info1(var,discoverable,"unlisted")
% add_slack_info1(var,display_email_addresses,true)
% add_slack_info1(var,display_real_names,false)
% add_slack_info1(var,dm_retention_duration,0)
% add_slack_info1(var,dm_retention_type,0)
% add_slack_info1(var,dnd_after_friday,"22:00")
% add_slack_info1(var,dnd_after_monday,"22:00")
% add_slack_info1(var,dnd_after_saturday,"22:00")
% add_slack_info1(var,dnd_after_sunday,"22:00")
% add_slack_info1(var,dnd_after_thursday,"22:00")
% add_slack_info1(var,dnd_after_tuesday,"22:00")
% add_slack_info1(var,dnd_after_wednesday,"22:00")
% add_slack_info1(var,dnd_before_friday,"08:00")
% add_slack_info1(var,dnd_before_monday,"08:00")
% add_slack_info1(var,dnd_before_saturday,"08:00")
% add_slack_info1(var,dnd_before_sunday,"08:00")
% add_slack_info1(var,dnd_before_thursday,"08:00")
% add_slack_info1(var,dnd_before_tuesday,"08:00")
% add_slack_info1(var,dnd_before_wednesday,"08:00")
% add_slack_info1(var,dnd_days,"every_day")
% add_slack_info1(var,dnd_enabled,true)
% add_slack_info1(var,dnd_enabled_friday,"partial")
% add_slack_info1(var,dnd_enabled_monday,"partial")
% add_slack_info1(var,dnd_enabled_saturday,"partial")
% add_slack_info1(var,dnd_enabled_sunday,"partial")
% add_slack_info1(var,dnd_enabled_thursday,"partial")
% add_slack_info1(var,dnd_enabled_tuesday,"partial")
% add_slack_info1(var,dnd_enabled_wednesday,"partial")
% add_slack_info1(var,dnd_end_hour,"08:00")
% add_slack_info1(var,dnd_start_hour,"22:00")
% add_slack_info1(var,dnd_weekdays_off_allday,false)
% add_slack_info1(var,dropbox_legacy_picker,false)
% add_slack_info1(var,enable_shared_channels,2)
% add_slack_info1(var,ent_required_browser,null)
% add_slack_info1(var,enterprise_default_channels,[])
% add_slack_info1(var,enterprise_mandatory_channels,[])
% add_slack_info1(var,enterprise_mdm_disable_file_download,false)
% add_slack_info1(var,enterprise_mobile_device_check,false)
% add_slack_info1(var,external_shared_channel_requests_approval_channel,null)
% add_slack_info1(var,file_limit_whitelisted,false)
% add_slack_info1(var,file_retention_duration,0)
% add_slack_info1(var,file_retention_type,0)
% add_slack_info1(var,gdrive_enabled_team,false)
% add_slack_info1(var,gg_enabled,false)
% add_slack_info1(var,group_retention_duration,0)
% add_slack_info1(var,group_retention_type,0)
% add_slack_info1(var,has_hipaa_compliance,false)
% add_slack_info1(var,has_seen_partner_promo,false)
% add_slack_info1(var,hide_referers,true)
% add_slack_info1(var,invite_requests_enabled,true)
% add_slack_info1(var,invites_limit,true)
% add_slack_info1(var,invites_only_admins,false)
% add_slack_info1(var,locale,"en-US")
% add_slack_info1(var,loud_channel_mentions_limit,10000)
% add_slack_info1(var,mobile_passcode_timeout_in_seconds,-1)
% add_slack_info1(var,msg_edit_window_mins,-1)
% add_slack_info1(var,onedrive_app_installed,false)
% add_slack_info1(var,onedrive_enabled_team,false)
% add_slack_info1(var,org_calls_apps,null)
% add_slack_info1(var,received_esc_route_to_channel_awareness_message,false)
% add_slack_info1(var,required_minimum_mobile_version,null)
% add_slack_info1(var,retention_duration,0)
% add_slack_info1(var,retention_type,0)
% add_slack_info1(var,self_serve_select,false)
% add_slack_info1(var,show_join_leave,false)
% add_slack_info1(var,single_user_exports,false)
% add_slack_info1(var,slackbot_responses_disabled,false)
% add_slack_info1(var,subteams_auto_create_admin,false)
% add_slack_info1(var,subteams_auto_create_owner,false)
% add_slack_info1(var,uses_customized_custom_status_presets,false)
% add_slack_info1(var,warn_before_at_channel,"always")
% add_slack_info1(var,welcome_place_enabled,true)
% unknown(slack_receive(var,type-["owners_and_admins"]))
% add_slack_info1(var,who_can_archive_channels,"regular")
% add_slack_info1(var,who_can_at_channel,"ra")
% add_slack_info1(var,who_can_at_everyone,"regular")
% add_slack_info1(var,who_can_change_team_profile,"admin")
% unknown(slack_receive(var,type-["regular"]))
% add_slack_info1(var,who_can_create_channels,"regular")
% add_slack_info1(var,who_can_create_delete_user_groups,"admin")
% unknown(slack_receive(var,type-["anyone"]))
% add_slack_info1(var,who_can_create_groups,"ra")
% add_slack_info1(var,who_can_create_shared_channels,"admin")
% unknown(slack_receive(var,type-["regular"]))
% unknown(slack_receive(var,type-["NO_ONE"]))
% add_slack_info1(var,who_can_edit_user_groups,"admin")
% add_slack_info1(var,who_can_kick_channels,"admin")
% add_slack_info1(var,who_can_kick_groups,"regular")
% add_slack_info1(var,who_can_manage_channel_posting_prefs,"ra")
% unknown(slack_receive(var,type-["ORG_ADMINS_AND_OWNERS"]))
% unknown(slack_receive(var,type-["admin"]))
% unknown(slack_receive(var,type-["regular"]))
% add_slack_info1(var,type,[])
% add_slack_info1(var,user,[])
% add_slack_info1(var,type,[])
% add_slack_info1(var,user,[])
% unknown(slack_receive(var,type-["admin"]))
% add_slack_info1(var,who_can_post_general,"ra")
% unknown(slack_receive(var,type-["ra"]))
% unknown(slack_receive(var,type-["TOPLEVEL_ADMINS_AND_OWNERS"]))
% add_slack_info1(var,workflow_builder_enabled,true)
% add_slack_info1(var,workflows_export_csv_enabled,true)
% add_slack_info1(var,workflows_webhook_trigger_enabled,true)
% add_slack_info1(var,thread_only_channels,[])
% add_slack_info1(var,url,"wss://cerberus-xxxx.lb.slack-msgs.com/websocket/yMVca33O9XnBPOnjyGJ_0xPxRW4qAZ6wHXOAjShQ1G71T63zCTDTGZsAL8L9WKMZZiEsKC8UmP8TxEv9aj0HJ5NRAz3c1gBUyFCCwvgDMms=")
% add_slack_info1(var,instance,'U010LM68Y65')
% add_slack_info1('U010LM68Y65',color,"9f69e7")
% add_slack_info1('U010LM68Y65',deleted,false)
% add_slack_info1('U010LM68Y65',id,"U010LM68Y65")
% add_slack_info1('U010LM68Y65',is_admin,true)
% add_slack_info1('U010LM68Y65',is_app_user,false)
% add_slack_info1('U010LM68Y65',is_bot,false)
% add_slack_info1('U010LM68Y65',is_owner,true)
% add_slack_info1('U010LM68Y65',is_primary_owner,true)
% add_slack_info1('U010LM68Y65',is_restricted,false)
% add_slack_info1('U010LM68Y65',is_ultra_restricted,false)
% add_slack_info1('U010LM68Y65',name,"annie")
% add_slack_info1('U010LM68Y65',presence,"away")
% add_slack_info1('U010LM68Y65',profile,_42580{avatar_hash:"95bd769325fe",display_name:"Anne Ogborn (Instructor)",display_name_normalized:"Anne Ogborn (Instructor)",email:"annie@theelginworks.com",fields:[],first_name:"Anne",image_1024:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_original.jpg",is_custom_image:true,last_name:"Ogborn",phone:"",real_name:"Anne Ogborn",real_name_normalized:"Anne Ogborn",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:"Teach Prolog!"})
% add_slack_info1('U010LM68Y65',real_name,"Anne Ogborn")
% add_slack_info1('U010LM68Y65',team_id,"T010WMWBAGY")
% add_slack_info1('U010LM68Y65',tz,"Europe/Amsterdam")
% add_slack_info1('U010LM68Y65',tz_label,"Central European Summer Time")
% add_slack_info1('U010LM68Y65',tz_offset,7200)
% add_slack_info1('U010LM68Y65',updated,1586333062)
% add_slack_info1(var,instance,'U010W7LKG84')
% add_slack_info1('U010W7LKG84',color,"4bbe2e")
% add_slack_info1('U010W7LKG84',deleted,false)
% add_slack_info1('U010W7LKG84',id,"U010W7LKG84")
% add_slack_info1('U010W7LKG84',is_admin,false)
% add_slack_info1('U010W7LKG84',is_app_user,false)
% add_slack_info1('U010W7LKG84',is_bot,false)
% add_slack_info1('U010W7LKG84',is_invited_user,true)
% add_slack_info1('U010W7LKG84',is_owner,false)
% add_slack_info1('U010W7LKG84',is_primary_owner,false)
% add_slack_info1('U010W7LKG84',is_restricted,false)
% add_slack_info1('U010W7LKG84',is_ultra_restricted,false)
% add_slack_info1('U010W7LKG84',name,"anne")
% add_slack_info1('U010W7LKG84',presence,"away")
% add_slack_info1('U010W7LKG84',profile,_42774{avatar_hash:"g18794bbfdc1",display_name:"",display_name_normalized:"",email:"anne@swi-prolog.org",fields:null,image_192:"https://secure.gravatar.com/avatar/18794bbfdc1916c36b7eb27c4352ef06.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-192.png",image_24:"https://secure.gravatar.com/avatar/18794bbfdc1916c36b7eb27c4352ef06.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-24.png",image_32:"https://secure.gravatar.com/avatar/18794bbfdc1916c36b7eb27c4352ef06.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-32.png",image_48:"https://secure.gravatar.com/avatar/18794bbfdc1916c36b7eb27c4352ef06.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-48.png",image_512:"https://secure.gravatar.com/avatar/18794bbfdc1916c36b7eb27c4352ef06.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-512.png",image_72:"https://secure.gravatar.com/avatar/18794bbfdc1916c36b7eb27c4352ef06.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-72.png",phone:"",real_name:"anne",real_name_normalized:"anne",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U010W7LKG84',real_name,"anne")
% add_slack_info1('U010W7LKG84',team_id,"T010WMWBAGY")
% add_slack_info1('U010W7LKG84',tz,"Europe/Amsterdam")
% add_slack_info1('U010W7LKG84',tz_label,"Central European Summer Time")
% add_slack_info1('U010W7LKG84',tz_offset,7200)
% add_slack_info1('U010W7LKG84',updated,1585303396)
% add_slack_info1(var,instance,'U0115Q96815')
% add_slack_info1('U0115Q96815',color,"e7392d")
% add_slack_info1('U0115Q96815',deleted,false)
% add_slack_info1('U0115Q96815',id,"U0115Q96815")
% add_slack_info1('U0115Q96815',is_admin,true)
% add_slack_info1('U0115Q96815',is_app_user,false)
% add_slack_info1('U0115Q96815',is_bot,false)
% add_slack_info1('U0115Q96815',is_owner,false)
% add_slack_info1('U0115Q96815',is_primary_owner,false)
% add_slack_info1('U0115Q96815',is_restricted,false)
% add_slack_info1('U0115Q96815',is_ultra_restricted,false)
% add_slack_info1('U0115Q96815',name,"iandrich87")
% add_slack_info1('U0115Q96815',presence,"away")
% add_slack_info1('U0115Q96815',profile,_42952{avatar_hash:"e58fb09dc2b0",display_name:"Ian Andrich",display_name_normalized:"Ian Andrich",email:"iandrich87@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_192.jpg",image_24:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_24.jpg",image_32:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_32.jpg",image_48:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_48.jpg",image_512:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_512.jpg",image_72:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_72.jpg",image_original:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_original.jpg",is_custom_image:true,phone:"",real_name:"Ian Andrich",real_name_normalized:"Ian Andrich",skype:"",status_emoji:":speech_balloon:",status_expiration:1586671200,status_text:"On Break",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U0115Q96815',real_name,"Ian Andrich")
% add_slack_info1('U0115Q96815',team_id,"T010WMWBAGY")
% add_slack_info1('U0115Q96815',tz,"America/Los_Angeles")
% add_slack_info1('U0115Q96815',tz_label,"Pacific Daylight Time")
% add_slack_info1('U0115Q96815',tz_offset,-25200)
% add_slack_info1('U0115Q96815',updated,1586661520)
% add_slack_info1(var,instance,'U0119NXLRR8')
% add_slack_info1('U0119NXLRR8',color,"674b1b")
% add_slack_info1('U0119NXLRR8',deleted,false)
% add_slack_info1('U0119NXLRR8',id,"U0119NXLRR8")
% add_slack_info1('U0119NXLRR8',is_admin,false)
% add_slack_info1('U0119NXLRR8',is_app_user,false)
% add_slack_info1('U0119NXLRR8',is_bot,false)
% add_slack_info1('U0119NXLRR8',is_owner,false)
% add_slack_info1('U0119NXLRR8',is_primary_owner,false)
% add_slack_info1('U0119NXLRR8',is_restricted,false)
% add_slack_info1('U0119NXLRR8',is_ultra_restricted,false)
% add_slack_info1('U0119NXLRR8',name,"jens")
% add_slack_info1('U0119NXLRR8',presence,"away")
% add_slack_info1('U0119NXLRR8',profile,_43138{avatar_hash:"g7de1bd2ce2d",display_name:"Jens Schauder",display_name_normalized:"Jens Schauder",email:"jens@schauderhaft.de",fields:null,image_192:"https://secure.gravatar.com/avatar/7de1bd2ce2d7b597f0e7f54de638e59c.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-192.png",image_24:"https://secure.gravatar.com/avatar/7de1bd2ce2d7b597f0e7f54de638e59c.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-24.png",image_32:"https://secure.gravatar.com/avatar/7de1bd2ce2d7b597f0e7f54de638e59c.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-32.png",image_48:"https://secure.gravatar.com/avatar/7de1bd2ce2d7b597f0e7f54de638e59c.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-48.png",image_512:"https://secure.gravatar.com/avatar/7de1bd2ce2d7b597f0e7f54de638e59c.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-512.png",image_72:"https://secure.gravatar.com/avatar/7de1bd2ce2d7b597f0e7f54de638e59c.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-72.png",phone:"",real_name:"Jens Schauder",real_name_normalized:"Jens Schauder",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U0119NXLRR8',real_name,"Jens Schauder")
% add_slack_info1('U0119NXLRR8',team_id,"T010WMWBAGY")
% add_slack_info1('U0119NXLRR8',tz,"Europe/Amsterdam")
% add_slack_info1('U0119NXLRR8',tz_label,"Central European Summer Time")
% add_slack_info1('U0119NXLRR8',tz_offset,7200)
% add_slack_info1('U0119NXLRR8',updated,1586261481)
% add_slack_info1(var,instance,'U0119TB8NGJ')
% add_slack_info1('U0119TB8NGJ',color,"53b759")
% add_slack_info1('U0119TB8NGJ',deleted,false)
% add_slack_info1('U0119TB8NGJ',id,"U0119TB8NGJ")
% add_slack_info1('U0119TB8NGJ',is_admin,false)
% add_slack_info1('U0119TB8NGJ',is_app_user,false)
% add_slack_info1('U0119TB8NGJ',is_bot,false)
% add_slack_info1('U0119TB8NGJ',is_owner,false)
% add_slack_info1('U0119TB8NGJ',is_primary_owner,false)
% add_slack_info1('U0119TB8NGJ',is_restricted,false)
% add_slack_info1('U0119TB8NGJ',is_ultra_restricted,false)
% add_slack_info1('U0119TB8NGJ',name,"mikemps")
% add_slack_info1('U0119TB8NGJ',presence,"away")
% add_slack_info1('U0119TB8NGJ',profile,_43312{avatar_hash:"c337ddec90e5",display_name:"msuarz",display_name_normalized:"msuarz",email:"mikemps@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_original.jpg",is_custom_image:true,phone:"",real_name:"msuarz",real_name_normalized:"msuarz",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U0119TB8NGJ',real_name,"msuarz")
% add_slack_info1('U0119TB8NGJ',team_id,"T010WMWBAGY")
% add_slack_info1('U0119TB8NGJ',tz,"America/New_York")
% add_slack_info1('U0119TB8NGJ',tz_label,"Eastern Daylight Time")
% add_slack_info1('U0119TB8NGJ',tz_offset,-14400)
% add_slack_info1('U0119TB8NGJ',updated,1586265641)
% add_slack_info1(var,instance,'U0119UJNZJN')
% add_slack_info1('U0119UJNZJN',color,"385a86")
% add_slack_info1('U0119UJNZJN',deleted,false)
% add_slack_info1('U0119UJNZJN',id,"U0119UJNZJN")
% add_slack_info1('U0119UJNZJN',is_admin,false)
% add_slack_info1('U0119UJNZJN',is_app_user,false)
% add_slack_info1('U0119UJNZJN',is_bot,false)
% add_slack_info1('U0119UJNZJN',is_owner,false)
% add_slack_info1('U0119UJNZJN',is_primary_owner,false)
% add_slack_info1('U0119UJNZJN',is_restricted,false)
% add_slack_info1('U0119UJNZJN',is_ultra_restricted,false)
% add_slack_info1('U0119UJNZJN',name,"bcobb")
% add_slack_info1('U0119UJNZJN',presence,"away")
% add_slack_info1('U0119UJNZJN',profile,_43498{avatar_hash:"g269b40217df",display_name:"bcobb",display_name_normalized:"bcobb",email:"bcobb@uwalumni.com",fields:null,first_name:"Brian",image_192:"https://secure.gravatar.com/avatar/269b40217dfecccd37c71650992dd2cc.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-192.png",image_24:"https://secure.gravatar.com/avatar/269b40217dfecccd37c71650992dd2cc.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-24.png",image_32:"https://secure.gravatar.com/avatar/269b40217dfecccd37c71650992dd2cc.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-32.png",image_48:"https://secure.gravatar.com/avatar/269b40217dfecccd37c71650992dd2cc.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-48.png",image_512:"https://secure.gravatar.com/avatar/269b40217dfecccd37c71650992dd2cc.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-512.png",image_72:"https://secure.gravatar.com/avatar/269b40217dfecccd37c71650992dd2cc.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-72.png",last_name:"Cobb",phone:"",real_name:"Brian Cobb",real_name_normalized:"Brian Cobb",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:"Software Engineer @ Harry’s"})
% add_slack_info1('U0119UJNZJN',real_name,"Brian Cobb")
% add_slack_info1('U0119UJNZJN',team_id,"T010WMWBAGY")
% add_slack_info1('U0119UJNZJN',tz,"America/New_York")
% add_slack_info1('U0119UJNZJN',tz_label,"Eastern Daylight Time")
% add_slack_info1('U0119UJNZJN',tz_offset,-14400)
% add_slack_info1('U0119UJNZJN',updated,1586265837)
% add_slack_info1(var,instance,'U011A1XQ5JN')
% add_slack_info1('U011A1XQ5JN',color,"d55aef")
% add_slack_info1('U011A1XQ5JN',deleted,false)
% add_slack_info1('U011A1XQ5JN',id,"U011A1XQ5JN")
% add_slack_info1('U011A1XQ5JN',is_admin,false)
% add_slack_info1('U011A1XQ5JN',is_app_user,false)
% add_slack_info1('U011A1XQ5JN',is_bot,false)
% add_slack_info1('U011A1XQ5JN',is_owner,false)
% add_slack_info1('U011A1XQ5JN',is_primary_owner,false)
% add_slack_info1('U011A1XQ5JN',is_restricted,false)
% add_slack_info1('U011A1XQ5JN',is_ultra_restricted,false)
% add_slack_info1('U011A1XQ5JN',name,"heiko.angermann")
% add_slack_info1('U011A1XQ5JN',presence,"away")
% add_slack_info1('U011A1XQ5JN',profile,_43680{avatar_hash:"094e2a661ac4",display_name:"Heiko Angermann",display_name_normalized:"Heiko Angermann",email:"heiko.angermann@dhbw-heidenheim.de",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_1024.png",image_192:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_192.png",image_24:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_24.png",image_32:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_32.png",image_48:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_48.png",image_512:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_512.png",image_72:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_72.png",image_original:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_original.png",is_custom_image:true,phone:"",real_name:"Heiko Angermann",real_name_normalized:"Heiko Angermann",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011A1XQ5JN',real_name,"Heiko Angermann")
% add_slack_info1('U011A1XQ5JN',team_id,"T010WMWBAGY")
% add_slack_info1('U011A1XQ5JN',tz,"Europe/Amsterdam")
% add_slack_info1('U011A1XQ5JN',tz_label,"Central European Summer Time")
% add_slack_info1('U011A1XQ5JN',tz_offset,7200)
% add_slack_info1('U011A1XQ5JN',updated,1586333804)
% add_slack_info1(var,instance,'U011A2HJEHY')
% add_slack_info1('U011A2HJEHY',color,"e06b56")
% add_slack_info1('U011A2HJEHY',deleted,false)
% add_slack_info1('U011A2HJEHY',id,"U011A2HJEHY")
% add_slack_info1('U011A2HJEHY',is_admin,false)
% add_slack_info1('U011A2HJEHY',is_app_user,false)
% add_slack_info1('U011A2HJEHY',is_bot,false)
% add_slack_info1('U011A2HJEHY',is_owner,false)
% add_slack_info1('U011A2HJEHY',is_primary_owner,false)
% add_slack_info1('U011A2HJEHY',is_restricted,false)
% add_slack_info1('U011A2HJEHY',is_ultra_restricted,false)
% add_slack_info1('U011A2HJEHY',name,"yasinovskyy")
% add_slack_info1('U011A2HJEHY',presence,"away")
% add_slack_info1('U011A2HJEHY',profile,_43866{avatar_hash:"gf2651671335",display_name:"Roman",display_name_normalized:"Roman",email:"yasinovskyy@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/f2651671335b57d18f5d3faee2baec5f.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0021-192.png",image_24:"https://secure.gravatar.com/avatar/f2651671335b57d18f5d3faee2baec5f.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0021-24.png",image_32:"https://secure.gravatar.com/avatar/f2651671335b57d18f5d3faee2baec5f.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0021-32.png",image_48:"https://secure.gravatar.com/avatar/f2651671335b57d18f5d3faee2baec5f.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0021-48.png",image_512:"https://secure.gravatar.com/avatar/f2651671335b57d18f5d3faee2baec5f.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0021-512.png",image_72:"https://secure.gravatar.com/avatar/f2651671335b57d18f5d3faee2baec5f.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0021-72.png",phone:"",real_name:"Roman",real_name_normalized:"Roman",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011A2HJEHY',real_name,"Roman")
% add_slack_info1('U011A2HJEHY',team_id,"T010WMWBAGY")
% add_slack_info1('U011A2HJEHY',tz,"America/Chicago")
% add_slack_info1('U011A2HJEHY',tz_label,"Central Daylight Time")
% add_slack_info1('U011A2HJEHY',tz_offset,-18000)
% add_slack_info1('U011A2HJEHY',updated,1586267544)
% add_slack_info1(var,instance,'U011A8S1MCJ')
% add_slack_info1('U011A8S1MCJ',color,"e23f99")
% add_slack_info1('U011A8S1MCJ',deleted,false)
% add_slack_info1('U011A8S1MCJ',id,"U011A8S1MCJ")
% add_slack_info1('U011A8S1MCJ',is_admin,false)
% add_slack_info1('U011A8S1MCJ',is_app_user,false)
% add_slack_info1('U011A8S1MCJ',is_bot,false)
% add_slack_info1('U011A8S1MCJ',is_owner,false)
% add_slack_info1('U011A8S1MCJ',is_primary_owner,false)
% add_slack_info1('U011A8S1MCJ',is_restricted,false)
% add_slack_info1('U011A8S1MCJ',is_ultra_restricted,false)
% add_slack_info1('U011A8S1MCJ',name,"koeng101")
% add_slack_info1('U011A8S1MCJ',presence,"away")
% add_slack_info1('U011A8S1MCJ',profile,_44040{avatar_hash:"gdbd3cc24b09",display_name:"Keoni Gandall",display_name_normalized:"Keoni Gandall",email:"koeng101@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/dbd3cc24b092b17d53c09de6bab7969c.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-192.png",image_24:"https://secure.gravatar.com/avatar/dbd3cc24b092b17d53c09de6bab7969c.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-24.png",image_32:"https://secure.gravatar.com/avatar/dbd3cc24b092b17d53c09de6bab7969c.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-32.png",image_48:"https://secure.gravatar.com/avatar/dbd3cc24b092b17d53c09de6bab7969c.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-48.png",image_512:"https://secure.gravatar.com/avatar/dbd3cc24b092b17d53c09de6bab7969c.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-512.png",image_72:"https://secure.gravatar.com/avatar/dbd3cc24b092b17d53c09de6bab7969c.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-72.png",phone:"",real_name:"Keoni Gandall",real_name_normalized:"Keoni Gandall",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011A8S1MCJ',real_name,"Keoni Gandall")
% add_slack_info1('U011A8S1MCJ',team_id,"T010WMWBAGY")
% add_slack_info1('U011A8S1MCJ',tz,"America/Los_Angeles")
% add_slack_info1('U011A8S1MCJ',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011A8S1MCJ',tz_offset,-25200)
% add_slack_info1('U011A8S1MCJ',updated,1586270914)
% add_slack_info1(var,instance,'U011A97HCQ2')
% add_slack_info1('U011A97HCQ2',color,"e475df")
% add_slack_info1('U011A97HCQ2',deleted,false)
% add_slack_info1('U011A97HCQ2',id,"U011A97HCQ2")
% add_slack_info1('U011A97HCQ2',is_admin,false)
% add_slack_info1('U011A97HCQ2',is_app_user,false)
% add_slack_info1('U011A97HCQ2',is_bot,false)
% add_slack_info1('U011A97HCQ2',is_owner,false)
% add_slack_info1('U011A97HCQ2',is_primary_owner,false)
% add_slack_info1('U011A97HCQ2',is_restricted,false)
% add_slack_info1('U011A97HCQ2',is_ultra_restricted,false)
% add_slack_info1('U011A97HCQ2',name,"daan.v.berkel.1980_pr")
% add_slack_info1('U011A97HCQ2',presence,"away")
% add_slack_info1('U011A97HCQ2',profile,_44214{avatar_hash:"391d326e2fde",display_name:"dvberkel",display_name_normalized:"dvberkel",email:"daan.v.berkel.1980+prolog@gmail.com",fields:[],first_name:"Daan",image_1024:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_original.jpg",is_custom_image:true,last_name:"van Berkel",phone:"",real_name:"Daan van Berkel",real_name_normalized:"Daan van Berkel",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:"Inspired by life, the universe and everything"})
% add_slack_info1('U011A97HCQ2',real_name,"Daan van Berkel")
% add_slack_info1('U011A97HCQ2',team_id,"T010WMWBAGY")
% add_slack_info1('U011A97HCQ2',tz,"Europe/Amsterdam")
% add_slack_info1('U011A97HCQ2',tz_label,"Central European Summer Time")
% add_slack_info1('U011A97HCQ2',tz_offset,7200)
% add_slack_info1('U011A97HCQ2',updated,1586271591)
% add_slack_info1(var,instance,'U011AMX30FL')
% add_slack_info1('U011AMX30FL',color,"e85d72")
% add_slack_info1('U011AMX30FL',deleted,false)
% add_slack_info1('U011AMX30FL',id,"U011AMX30FL")
% add_slack_info1('U011AMX30FL',is_admin,false)
% add_slack_info1('U011AMX30FL',is_app_user,false)
% add_slack_info1('U011AMX30FL',is_bot,false)
% add_slack_info1('U011AMX30FL',is_owner,false)
% add_slack_info1('U011AMX30FL',is_primary_owner,false)
% add_slack_info1('U011AMX30FL',is_restricted,false)
% add_slack_info1('U011AMX30FL',is_ultra_restricted,false)
% add_slack_info1('U011AMX30FL',name,"fgiraffe")
% add_slack_info1('U011AMX30FL',presence,"away")
% add_slack_info1('U011AMX30FL',profile,_44408{avatar_hash:"6939a209b8d9",display_name:"Frank Giraffe",display_name_normalized:"Frank Giraffe",email:"fgiraffe@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_1024.png",image_192:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_192.png",image_24:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_24.png",image_32:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_32.png",image_48:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_48.png",image_512:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_512.png",image_72:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_72.png",image_original:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_original.png",is_custom_image:true,phone:"",real_name:"Frank Giraffe",real_name_normalized:"Frank Giraffe",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011AMX30FL',real_name,"Frank Giraffe")
% add_slack_info1('U011AMX30FL',team_id,"T010WMWBAGY")
% add_slack_info1('U011AMX30FL',tz,"America/Los_Angeles")
% add_slack_info1('U011AMX30FL',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011AMX30FL',tz_offset,-25200)
% add_slack_info1('U011AMX30FL',updated,1586278290)
% add_slack_info1(var,instance,'U011BHGS69M')
% add_slack_info1('U011BHGS69M',color,"e96699")
% add_slack_info1('U011BHGS69M',deleted,false)
% add_slack_info1('U011BHGS69M',id,"U011BHGS69M")
% add_slack_info1('U011BHGS69M',is_admin,false)
% add_slack_info1('U011BHGS69M',is_app_user,false)
% add_slack_info1('U011BHGS69M',is_bot,false)
% add_slack_info1('U011BHGS69M',is_owner,false)
% add_slack_info1('U011BHGS69M',is_primary_owner,false)
% add_slack_info1('U011BHGS69M',is_restricted,false)
% add_slack_info1('U011BHGS69M',is_ultra_restricted,false)
% add_slack_info1('U011BHGS69M',name,"-")
% add_slack_info1('U011BHGS69M',presence,"away")
% add_slack_info1('U011BHGS69M',profile,_44594{avatar_hash:"58e98c991c54",display_name:"Abe",display_name_normalized:"Abe",email:"_@abevoelker.com",fields:[],first_name:"Abe",image_1024:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_original.jpg",is_custom_image:true,last_name:"Voelker",phone:"",real_name:"Abe Voelker",real_name_normalized:"Abe Voelker",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011BHGS69M',real_name,"Abe Voelker")
% add_slack_info1('U011BHGS69M',team_id,"T010WMWBAGY")
% add_slack_info1('U011BHGS69M',tz,"America/Chicago")
% add_slack_info1('U011BHGS69M',tz_label,"Central Daylight Time")
% add_slack_info1('U011BHGS69M',tz_offset,-18000)
% add_slack_info1('U011BHGS69M',updated,1586263057)
% add_slack_info1(var,instance,'U011BHHPHST')
% add_slack_info1('U011BHHPHST',color,"2b6836")
% add_slack_info1('U011BHHPHST',deleted,false)
% add_slack_info1('U011BHHPHST',id,"U011BHHPHST")
% add_slack_info1('U011BHHPHST',is_admin,false)
% add_slack_info1('U011BHHPHST',is_app_user,false)
% add_slack_info1('U011BHHPHST',is_bot,false)
% add_slack_info1('U011BHHPHST',is_owner,false)
% add_slack_info1('U011BHHPHST',is_primary_owner,false)
% add_slack_info1('U011BHHPHST',is_restricted,false)
% add_slack_info1('U011BHHPHST',is_ultra_restricted,false)
% add_slack_info1('U011BHHPHST',name,"alvinng15")
% add_slack_info1('U011BHHPHST',presence,"away")
% add_slack_info1('U011BHHPHST',profile,_44788{avatar_hash:"gff5a6c73495",display_name:"",display_name_normalized:"",email:"alvinng15@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/ff5a6c73495eaebee9593b5082425858.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-192.png",image_24:"https://secure.gravatar.com/avatar/ff5a6c73495eaebee9593b5082425858.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-24.png",image_32:"https://secure.gravatar.com/avatar/ff5a6c73495eaebee9593b5082425858.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-32.png",image_48:"https://secure.gravatar.com/avatar/ff5a6c73495eaebee9593b5082425858.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-48.png",image_512:"https://secure.gravatar.com/avatar/ff5a6c73495eaebee9593b5082425858.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-512.png",image_72:"https://secure.gravatar.com/avatar/ff5a6c73495eaebee9593b5082425858.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-72.png",phone:"",real_name:"Alvin",real_name_normalized:"Alvin",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011BHHPHST',real_name,"Alvin")
% add_slack_info1('U011BHHPHST',team_id,"T010WMWBAGY")
% add_slack_info1('U011BHHPHST',tz,"Asia/Kuala_Lumpur")
% add_slack_info1('U011BHHPHST',tz_label,"Singapore Standard Time")
% add_slack_info1('U011BHHPHST',tz_offset,28800)
% add_slack_info1('U011BHHPHST',updated,1586262066)
% add_slack_info1(var,instance,'U011BHN1971')
% add_slack_info1('U011BHN1971',color,"5a4592")
% add_slack_info1('U011BHN1971',deleted,false)
% add_slack_info1('U011BHN1971',id,"U011BHN1971")
% add_slack_info1('U011BHN1971',is_admin,false)
% add_slack_info1('U011BHN1971',is_app_user,false)
% add_slack_info1('U011BHN1971',is_bot,false)
% add_slack_info1('U011BHN1971',is_owner,false)
% add_slack_info1('U011BHN1971',is_primary_owner,false)
% add_slack_info1('U011BHN1971',is_restricted,false)
% add_slack_info1('U011BHN1971',is_ultra_restricted,false)
% add_slack_info1('U011BHN1971',name,"stephynb")
% add_slack_info1('U011BHN1971',presence,"away")
% add_slack_info1('U011BHN1971',profile,_44962{avatar_hash:"49dd028f9955",display_name:"Stephyn Butcher",display_name_normalized:"Stephyn Butcher",email:"stephynb@gmail.com",fields:[],first_name:"Stephyn",image_1024:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_1024.png",image_192:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_192.png",image_24:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_24.png",image_32:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_32.png",image_48:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_48.png",image_512:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_512.png",image_72:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_72.png",image_original:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_original.png",is_custom_image:true,last_name:"Butcher",phone:"",real_name:"Stephyn Butcher",real_name_normalized:"Stephyn Butcher",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:"Data Chef @ PXY Data"})
% add_slack_info1('U011BHN1971',real_name,"Stephyn Butcher")
% add_slack_info1('U011BHN1971',team_id,"T010WMWBAGY")
% add_slack_info1('U011BHN1971',tz,"America/New_York")
% add_slack_info1('U011BHN1971',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011BHN1971',tz_offset,-14400)
% add_slack_info1('U011BHN1971',updated,1586282414)
% add_slack_info1(var,instance,'U011BHT4Y31')
% add_slack_info1('U011BHT4Y31',color,"9e3997")
% add_slack_info1('U011BHT4Y31',deleted,false)
% add_slack_info1('U011BHT4Y31',id,"U011BHT4Y31")
% add_slack_info1('U011BHT4Y31',is_admin,false)
% add_slack_info1('U011BHT4Y31',is_app_user,false)
% add_slack_info1('U011BHT4Y31',is_bot,false)
% add_slack_info1('U011BHT4Y31',is_owner,false)
% add_slack_info1('U011BHT4Y31',is_primary_owner,false)
% add_slack_info1('U011BHT4Y31',is_restricted,false)
% add_slack_info1('U011BHT4Y31',is_ultra_restricted,false)
% add_slack_info1('U011BHT4Y31',name,"lucid9")
% add_slack_info1('U011BHT4Y31',presence,"away")
% add_slack_info1('U011BHT4Y31',profile,_45156{avatar_hash:"b5b18b9ca75c",display_name:"Lucian Dragus",display_name_normalized:"Lucian Dragus",email:"lucid9@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_1024.png",image_192:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_192.png",image_24:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_24.png",image_32:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_32.png",image_48:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_48.png",image_512:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_512.png",image_72:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_72.png",image_original:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_original.png",is_custom_image:true,phone:"",real_name:"Lucian Dragus",real_name_normalized:"Lucian Dragus",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011BHT4Y31',real_name,"Lucian Dragus")
% add_slack_info1('U011BHT4Y31',team_id,"T010WMWBAGY")
% add_slack_info1('U011BHT4Y31',tz,"America/Bogota")
% add_slack_info1('U011BHT4Y31',tz_label,"South America Pacific Standard Time")
% add_slack_info1('U011BHT4Y31',tz_offset,-18000)
% add_slack_info1('U011BHT4Y31',updated,1586265325)
% add_slack_info1(var,instance,'U011BJCDXDM')
% add_slack_info1('U011BJCDXDM',color,"3c8c69")
% add_slack_info1('U011BJCDXDM',deleted,false)
% add_slack_info1('U011BJCDXDM',id,"U011BJCDXDM")
% add_slack_info1('U011BJCDXDM',is_admin,false)
% add_slack_info1('U011BJCDXDM',is_app_user,false)
% add_slack_info1('U011BJCDXDM',is_bot,false)
% add_slack_info1('U011BJCDXDM',is_owner,false)
% add_slack_info1('U011BJCDXDM',is_primary_owner,false)
% add_slack_info1('U011BJCDXDM',is_restricted,false)
% add_slack_info1('U011BJCDXDM',is_ultra_restricted,false)
% add_slack_info1('U011BJCDXDM',name,"rand.fitzpatrick")
% add_slack_info1('U011BJCDXDM',presence,"away")
% add_slack_info1('U011BJCDXDM',profile,_45342{avatar_hash:"g6a912025c15",display_name:"Rand Fitzpatrick",display_name_normalized:"Rand Fitzpatrick",email:"rand.fitzpatrick@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/6a912025c15dbd97306c6efcc4cc5f46.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-192.png",image_24:"https://secure.gravatar.com/avatar/6a912025c15dbd97306c6efcc4cc5f46.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-24.png",image_32:"https://secure.gravatar.com/avatar/6a912025c15dbd97306c6efcc4cc5f46.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-32.png",image_48:"https://secure.gravatar.com/avatar/6a912025c15dbd97306c6efcc4cc5f46.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-48.png",image_512:"https://secure.gravatar.com/avatar/6a912025c15dbd97306c6efcc4cc5f46.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-512.png",image_72:"https://secure.gravatar.com/avatar/6a912025c15dbd97306c6efcc4cc5f46.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-72.png",phone:"",real_name:"Rand Fitzpatrick",real_name_normalized:"Rand Fitzpatrick",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011BJCDXDM',real_name,"Rand Fitzpatrick")
% add_slack_info1('U011BJCDXDM',team_id,"T010WMWBAGY")
% add_slack_info1('U011BJCDXDM',tz,"America/Denver")
% add_slack_info1('U011BJCDXDM',tz_label,"Mountain Daylight Time")
% add_slack_info1('U011BJCDXDM',tz_offset,-21600)
% add_slack_info1('U011BJCDXDM',updated,1586269075)
% add_slack_info1(var,instance,'U011BL67NT1')
% add_slack_info1('U011BL67NT1',color,"9d8eee")
% add_slack_info1('U011BL67NT1',deleted,false)
% add_slack_info1('U011BL67NT1',id,"U011BL67NT1")
% add_slack_info1('U011BL67NT1',is_admin,false)
% add_slack_info1('U011BL67NT1',is_app_user,false)
% add_slack_info1('U011BL67NT1',is_bot,false)
% add_slack_info1('U011BL67NT1',is_owner,false)
% add_slack_info1('U011BL67NT1',is_primary_owner,false)
% add_slack_info1('U011BL67NT1',is_restricted,false)
% add_slack_info1('U011BL67NT1',is_ultra_restricted,false)
% add_slack_info1('U011BL67NT1',name,"eike.spang")
% add_slack_info1('U011BL67NT1',presence,"away")
% add_slack_info1('U011BL67NT1',profile,_45516{avatar_hash:"g8533db67b21",display_name:"",display_name_normalized:"",email:"eike.spang@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/8533db67b21527af773b6df7947c2c71.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-192.png",image_24:"https://secure.gravatar.com/avatar/8533db67b21527af773b6df7947c2c71.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-24.png",image_32:"https://secure.gravatar.com/avatar/8533db67b21527af773b6df7947c2c71.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-32.png",image_48:"https://secure.gravatar.com/avatar/8533db67b21527af773b6df7947c2c71.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-48.png",image_512:"https://secure.gravatar.com/avatar/8533db67b21527af773b6df7947c2c71.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-512.png",image_72:"https://secure.gravatar.com/avatar/8533db67b21527af773b6df7947c2c71.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-72.png",phone:"",real_name:"Eike Spang",real_name_normalized:"Eike Spang",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011BL67NT1',real_name,"Eike Spang")
% add_slack_info1('U011BL67NT1',team_id,"T010WMWBAGY")
% add_slack_info1('U011BL67NT1',tz,"Europe/London")
% add_slack_info1('U011BL67NT1',tz_label,"British Summer Time")
% add_slack_info1('U011BL67NT1',tz_offset,3600)
% add_slack_info1('U011BL67NT1',updated,1586284344)
% add_slack_info1(var,instance,'U011BP5EN4X')
% add_slack_info1('U011BP5EN4X',color,"e96699")
% add_slack_info1('U011BP5EN4X',deleted,false)
% add_slack_info1('U011BP5EN4X',id,"U011BP5EN4X")
% add_slack_info1('U011BP5EN4X',is_admin,false)
% add_slack_info1('U011BP5EN4X',is_app_user,false)
% add_slack_info1('U011BP5EN4X',is_bot,false)
% add_slack_info1('U011BP5EN4X',is_owner,false)
% add_slack_info1('U011BP5EN4X',is_primary_owner,false)
% add_slack_info1('U011BP5EN4X',is_restricted,false)
% add_slack_info1('U011BP5EN4X',is_ultra_restricted,false)
% add_slack_info1('U011BP5EN4X',name,"contact")
% add_slack_info1('U011BP5EN4X',presence,"away")
% add_slack_info1('U011BP5EN4X',profile,_45690{avatar_hash:"gd63157fafa1",display_name:"David",display_name_normalized:"David",email:"contact@davidporter.id.au",fields:null,image_192:"https://secure.gravatar.com/avatar/d63157fafa10faa7e96e05f4de8b6d9c.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-192.png",image_24:"https://secure.gravatar.com/avatar/d63157fafa10faa7e96e05f4de8b6d9c.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-24.png",image_32:"https://secure.gravatar.com/avatar/d63157fafa10faa7e96e05f4de8b6d9c.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-32.png",image_48:"https://secure.gravatar.com/avatar/d63157fafa10faa7e96e05f4de8b6d9c.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-48.png",image_512:"https://secure.gravatar.com/avatar/d63157fafa10faa7e96e05f4de8b6d9c.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-512.png",image_72:"https://secure.gravatar.com/avatar/d63157fafa10faa7e96e05f4de8b6d9c.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-72.png",phone:"",real_name:"David",real_name_normalized:"David",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011BP5EN4X',real_name,"David")
% add_slack_info1('U011BP5EN4X',team_id,"T010WMWBAGY")
% add_slack_info1('U011BP5EN4X',tz,"America/Los_Angeles")
% add_slack_info1('U011BP5EN4X',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011BP5EN4X',tz_offset,-25200)
% add_slack_info1('U011BP5EN4X',updated,1586311052)
% add_slack_info1(var,instance,'U011BQ5L0RM')
% add_slack_info1('U011BQ5L0RM',color,"684b6c")
% add_slack_info1('U011BQ5L0RM',deleted,false)
% add_slack_info1('U011BQ5L0RM',id,"U011BQ5L0RM")
% add_slack_info1('U011BQ5L0RM',is_admin,false)
% add_slack_info1('U011BQ5L0RM',is_app_user,false)
% add_slack_info1('U011BQ5L0RM',is_bot,false)
% add_slack_info1('U011BQ5L0RM',is_owner,false)
% add_slack_info1('U011BQ5L0RM',is_primary_owner,false)
% add_slack_info1('U011BQ5L0RM',is_restricted,false)
% add_slack_info1('U011BQ5L0RM',is_ultra_restricted,false)
% add_slack_info1('U011BQ5L0RM',name,"kscraja")
% add_slack_info1('U011BQ5L0RM',presence,"away")
% add_slack_info1('U011BQ5L0RM',profile,_45864{avatar_hash:"g957dcae7974",display_name:"Chandra Koduru",display_name_normalized:"Chandra Koduru",email:"kscraja@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/957dcae7974cf60bebd964894ca2ac79.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-192.png",image_24:"https://secure.gravatar.com/avatar/957dcae7974cf60bebd964894ca2ac79.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-24.png",image_32:"https://secure.gravatar.com/avatar/957dcae7974cf60bebd964894ca2ac79.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-32.png",image_48:"https://secure.gravatar.com/avatar/957dcae7974cf60bebd964894ca2ac79.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-48.png",image_512:"https://secure.gravatar.com/avatar/957dcae7974cf60bebd964894ca2ac79.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-512.png",image_72:"https://secure.gravatar.com/avatar/957dcae7974cf60bebd964894ca2ac79.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-72.png",phone:"",real_name:"Chandra Koduru",real_name_normalized:"Chandra Koduru",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011BQ5L0RM',real_name,"Chandra Koduru")
% add_slack_info1('U011BQ5L0RM',team_id,"T010WMWBAGY")
% add_slack_info1('U011BQ5L0RM',tz,"Asia/Kolkata")
% add_slack_info1('U011BQ5L0RM',tz_label,"India Standard Time")
% add_slack_info1('U011BQ5L0RM',tz_offset,19800)
% add_slack_info1('U011BQ5L0RM',updated,1586327148)
% add_slack_info1(var,instance,'U011CAED9LP')
% add_slack_info1('U011CAED9LP',color,"bb86b7")
% add_slack_info1('U011CAED9LP',deleted,false)
% add_slack_info1('U011CAED9LP',id,"U011CAED9LP")
% add_slack_info1('U011CAED9LP',is_admin,false)
% add_slack_info1('U011CAED9LP',is_app_user,false)
% add_slack_info1('U011CAED9LP',is_bot,false)
% add_slack_info1('U011CAED9LP',is_owner,false)
% add_slack_info1('U011CAED9LP',is_primary_owner,false)
% add_slack_info1('U011CAED9LP',is_restricted,false)
% add_slack_info1('U011CAED9LP',is_ultra_restricted,false)
% add_slack_info1('U011CAED9LP',name,"perplexedcoding")
% add_slack_info1('U011CAED9LP',presence,"away")
% add_slack_info1('U011CAED9LP',profile,_46038{avatar_hash:"a152cb76da74",display_name:"Pilne",display_name_normalized:"Pilne",email:"perplexedcoding@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_1024.png",image_192:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_192.png",image_24:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_24.png",image_32:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_32.png",image_48:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_48.png",image_512:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_512.png",image_72:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_72.png",image_original:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_original.png",is_custom_image:true,phone:"",real_name:"Pilne",real_name_normalized:"Pilne",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011CAED9LP',real_name,"Pilne")
% add_slack_info1('U011CAED9LP',team_id,"T010WMWBAGY")
% add_slack_info1('U011CAED9LP',tz,"America/Chicago")
% add_slack_info1('U011CAED9LP',tz_label,"Central Daylight Time")
% add_slack_info1('U011CAED9LP',tz_offset,-18000)
% add_slack_info1('U011CAED9LP',updated,1586469956)
% add_slack_info1(var,instance,'U011CAHLMGF')
% add_slack_info1('U011CAHLMGF',color,"5a4592")
% add_slack_info1('U011CAHLMGF',deleted,false)
% add_slack_info1('U011CAHLMGF',id,"U011CAHLMGF")
% add_slack_info1('U011CAHLMGF',is_admin,false)
% add_slack_info1('U011CAHLMGF',is_app_user,false)
% add_slack_info1('U011CAHLMGF',is_bot,false)
% add_slack_info1('U011CAHLMGF',is_owner,false)
% add_slack_info1('U011CAHLMGF',is_primary_owner,false)
% add_slack_info1('U011CAHLMGF',is_restricted,false)
% add_slack_info1('U011CAHLMGF',is_ultra_restricted,false)
% add_slack_info1('U011CAHLMGF',name,"dedgrant")
% add_slack_info1('U011CAHLMGF',presence,"away")
% add_slack_info1('U011CAHLMGF',profile,_46224{avatar_hash:"gfe2441e0be5",display_name:"Darren Grant",display_name_normalized:"Darren Grant",email:"dedgrant@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/fe2441e0be5232fdf5a435ed04b10c67.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-192.png",image_24:"https://secure.gravatar.com/avatar/fe2441e0be5232fdf5a435ed04b10c67.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-24.png",image_32:"https://secure.gravatar.com/avatar/fe2441e0be5232fdf5a435ed04b10c67.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-32.png",image_48:"https://secure.gravatar.com/avatar/fe2441e0be5232fdf5a435ed04b10c67.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-48.png",image_512:"https://secure.gravatar.com/avatar/fe2441e0be5232fdf5a435ed04b10c67.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-512.png",image_72:"https://secure.gravatar.com/avatar/fe2441e0be5232fdf5a435ed04b10c67.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-72.png",phone:"",real_name:"Darren Grant",real_name_normalized:"Darren Grant",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011CAHLMGF',real_name,"Darren Grant")
% add_slack_info1('U011CAHLMGF',team_id,"T010WMWBAGY")
% add_slack_info1('U011CAHLMGF',tz,"America/Los_Angeles")
% add_slack_info1('U011CAHLMGF',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011CAHLMGF',tz_offset,-25200)
% add_slack_info1('U011CAHLMGF',updated,1586470311)
% add_slack_info1(var,instance,'U011CEGJRT4')
% add_slack_info1('U011CEGJRT4',color,"2b6836")
% add_slack_info1('U011CEGJRT4',deleted,false)
% add_slack_info1('U011CEGJRT4',id,"U011CEGJRT4")
% add_slack_info1('U011CEGJRT4',is_admin,false)
% add_slack_info1('U011CEGJRT4',is_app_user,false)
% add_slack_info1('U011CEGJRT4',is_bot,false)
% add_slack_info1('U011CEGJRT4',is_owner,false)
% add_slack_info1('U011CEGJRT4',is_primary_owner,false)
% add_slack_info1('U011CEGJRT4',is_restricted,false)
% add_slack_info1('U011CEGJRT4',is_ultra_restricted,false)
% add_slack_info1('U011CEGJRT4',name,"mathijs.claassen")
% add_slack_info1('U011CEGJRT4',presence,"away")
% add_slack_info1('U011CEGJRT4',profile,_46398{avatar_hash:"g57786800758",display_name:"Mathijs C",display_name_normalized:"Mathijs C",email:"mathijs.claassen@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/57786800758fa14d4d3e4dd86f011bd8.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-192.png",image_24:"https://secure.gravatar.com/avatar/57786800758fa14d4d3e4dd86f011bd8.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-24.png",image_32:"https://secure.gravatar.com/avatar/57786800758fa14d4d3e4dd86f011bd8.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-32.png",image_48:"https://secure.gravatar.com/avatar/57786800758fa14d4d3e4dd86f011bd8.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-48.png",image_512:"https://secure.gravatar.com/avatar/57786800758fa14d4d3e4dd86f011bd8.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-512.png",image_72:"https://secure.gravatar.com/avatar/57786800758fa14d4d3e4dd86f011bd8.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-72.png",phone:"",real_name:"Mathijs C",real_name_normalized:"Mathijs C",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011CEGJRT4',real_name,"Mathijs C")
% add_slack_info1('U011CEGJRT4',team_id,"T010WMWBAGY")
% add_slack_info1('U011CEGJRT4',tz,"Europe/Amsterdam")
% add_slack_info1('U011CEGJRT4',tz_label,"Central European Summer Time")
% add_slack_info1('U011CEGJRT4',tz_offset,7200)
% add_slack_info1('U011CEGJRT4',updated,1586329501)
% add_slack_info1(var,instance,'U011CEPL7GT')
% add_slack_info1('U011CEPL7GT',color,"73769d")
% add_slack_info1('U011CEPL7GT',deleted,false)
% add_slack_info1('U011CEPL7GT',id,"U011CEPL7GT")
% add_slack_info1('U011CEPL7GT',is_admin,false)
% add_slack_info1('U011CEPL7GT',is_app_user,false)
% add_slack_info1('U011CEPL7GT',is_bot,false)
% add_slack_info1('U011CEPL7GT',is_owner,false)
% add_slack_info1('U011CEPL7GT',is_primary_owner,false)
% add_slack_info1('U011CEPL7GT',is_restricted,false)
% add_slack_info1('U011CEPL7GT',is_ultra_restricted,false)
% add_slack_info1('U011CEPL7GT',name,"dijkstra.arjen")
% add_slack_info1('U011CEPL7GT',presence,"away")
% add_slack_info1('U011CEPL7GT',profile,_46572{avatar_hash:"932acf64c769",display_name:"Arjen Dijkstra",display_name_normalized:"Arjen Dijkstra",email:"dijkstra.arjen@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_original.jpg",is_custom_image:true,phone:"",real_name:"Arjen Dijkstra",real_name_normalized:"Arjen Dijkstra",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011CEPL7GT',real_name,"Arjen Dijkstra")
% add_slack_info1('U011CEPL7GT',team_id,"T010WMWBAGY")
% add_slack_info1('U011CEPL7GT',tz,"Europe/Amsterdam")
% add_slack_info1('U011CEPL7GT',tz_label,"Central European Summer Time")
% add_slack_info1('U011CEPL7GT',tz_offset,7200)
% add_slack_info1('U011CEPL7GT',updated,1586450013)
% add_slack_info1(var,instance,'U011CF6QXT4')
% add_slack_info1('U011CF6QXT4',color,"99a949")
% add_slack_info1('U011CF6QXT4',deleted,false)
% add_slack_info1('U011CF6QXT4',id,"U011CF6QXT4")
% add_slack_info1('U011CF6QXT4',is_admin,false)
% add_slack_info1('U011CF6QXT4',is_app_user,false)
% add_slack_info1('U011CF6QXT4',is_bot,false)
% add_slack_info1('U011CF6QXT4',is_owner,false)
% add_slack_info1('U011CF6QXT4',is_primary_owner,false)
% add_slack_info1('U011CF6QXT4',is_restricted,false)
% add_slack_info1('U011CF6QXT4',is_ultra_restricted,false)
% add_slack_info1('U011CF6QXT4',name,"nimurphy")
% add_slack_info1('U011CF6QXT4',presence,"away")
% add_slack_info1('U011CF6QXT4',profile,_46758{avatar_hash:"gcac3b9e4941",display_name:"",display_name_normalized:"",email:"nimurphy@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/cac3b9e49415330fe95cde52d15cbd49.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0014-192.png",image_24:"https://secure.gravatar.com/avatar/cac3b9e49415330fe95cde52d15cbd49.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0014-24.png",image_32:"https://secure.gravatar.com/avatar/cac3b9e49415330fe95cde52d15cbd49.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0014-32.png",image_48:"https://secure.gravatar.com/avatar/cac3b9e49415330fe95cde52d15cbd49.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0014-48.png",image_512:"https://secure.gravatar.com/avatar/cac3b9e49415330fe95cde52d15cbd49.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0014-512.png",image_72:"https://secure.gravatar.com/avatar/cac3b9e49415330fe95cde52d15cbd49.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0014-72.png",phone:"",real_name:"Niall Murphy",real_name_normalized:"Niall Murphy",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011CF6QXT4',real_name,"Niall Murphy")
% add_slack_info1('U011CF6QXT4',team_id,"T010WMWBAGY")
% add_slack_info1('U011CF6QXT4',tz,"Europe/London")
% add_slack_info1('U011CF6QXT4',tz_label,"British Summer Time")
% add_slack_info1('U011CF6QXT4',tz_offset,3600)
% add_slack_info1('U011CF6QXT4',updated,1586330183)
% add_slack_info1(var,instance,'U011D15H1T5')
% add_slack_info1('U011D15H1T5',color,"674b1b")
% add_slack_info1('U011D15H1T5',deleted,false)
% add_slack_info1('U011D15H1T5',id,"U011D15H1T5")
% add_slack_info1('U011D15H1T5',is_admin,false)
% add_slack_info1('U011D15H1T5',is_app_user,false)
% add_slack_info1('U011D15H1T5',is_bot,false)
% add_slack_info1('U011D15H1T5',is_owner,false)
% add_slack_info1('U011D15H1T5',is_primary_owner,false)
% add_slack_info1('U011D15H1T5',is_restricted,false)
% add_slack_info1('U011D15H1T5',is_ultra_restricted,false)
% add_slack_info1('U011D15H1T5',name,"anthony")
% add_slack_info1('U011D15H1T5',presence,"away")
% add_slack_info1('U011D15H1T5',profile,_46932{avatar_hash:"16d2b3d3ae9f",display_name:"Tony",display_name_normalized:"Tony",email:"anthony@anthonymiller.tech",fields:null,first_name:"Tony",image_1024:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_original.jpg",is_custom_image:true,last_name:"Miller",phone:"",real_name:"Tony Miller",real_name_normalized:"Tony Miller",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:"Software Engineer (Node/React) "})
% add_slack_info1('U011D15H1T5',real_name,"Tony Miller")
% add_slack_info1('U011D15H1T5',team_id,"T010WMWBAGY")
% add_slack_info1('U011D15H1T5',tz,"America/New_York")
% add_slack_info1('U011D15H1T5',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011D15H1T5',tz_offset,-14400)
% add_slack_info1('U011D15H1T5',updated,1586306287)
% add_slack_info1(var,instance,'U011DD9K459')
% add_slack_info1('U011DD9K459',color,"e0a729")
% add_slack_info1('U011DD9K459',deleted,false)
% add_slack_info1('U011DD9K459',id,"U011DD9K459")
% add_slack_info1('U011DD9K459',is_admin,false)
% add_slack_info1('U011DD9K459',is_app_user,false)
% add_slack_info1('U011DD9K459',is_bot,false)
% add_slack_info1('U011DD9K459',is_owner,false)
% add_slack_info1('U011DD9K459',is_primary_owner,false)
% add_slack_info1('U011DD9K459',is_restricted,false)
% add_slack_info1('U011DD9K459',is_ultra_restricted,false)
% add_slack_info1('U011DD9K459',name,"mgondan77")
% add_slack_info1('U011DD9K459',presence,"away")
% add_slack_info1('U011DD9K459',profile,_47126{avatar_hash:"g112dbc3412e",display_name:"Matthias",display_name_normalized:"Matthias",email:"mgondan77@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/112dbc3412ec0d602d06bfb3cd366cb1.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-192.png",image_24:"https://secure.gravatar.com/avatar/112dbc3412ec0d602d06bfb3cd366cb1.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-24.png",image_32:"https://secure.gravatar.com/avatar/112dbc3412ec0d602d06bfb3cd366cb1.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-32.png",image_48:"https://secure.gravatar.com/avatar/112dbc3412ec0d602d06bfb3cd366cb1.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-48.png",image_512:"https://secure.gravatar.com/avatar/112dbc3412ec0d602d06bfb3cd366cb1.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-512.png",image_72:"https://secure.gravatar.com/avatar/112dbc3412ec0d602d06bfb3cd366cb1.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-72.png",phone:"",real_name:"Matthias",real_name_normalized:"Matthias",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011DD9K459',real_name,"Matthias")
% add_slack_info1('U011DD9K459',team_id,"T010WMWBAGY")
% add_slack_info1('U011DD9K459',tz,"Europe/Amsterdam")
% add_slack_info1('U011DD9K459',tz_label,"Central European Summer Time")
% add_slack_info1('U011DD9K459',tz_offset,7200)
% add_slack_info1('U011DD9K459',updated,1586325132)
% add_slack_info1(var,instance,'U011E5YEVD0')
% add_slack_info1('U011E5YEVD0',color,"9b3b45")
% add_slack_info1('U011E5YEVD0',deleted,false)
% add_slack_info1('U011E5YEVD0',id,"U011E5YEVD0")
% add_slack_info1('U011E5YEVD0',is_admin,false)
% add_slack_info1('U011E5YEVD0',is_app_user,false)
% add_slack_info1('U011E5YEVD0',is_bot,false)
% add_slack_info1('U011E5YEVD0',is_owner,false)
% add_slack_info1('U011E5YEVD0',is_primary_owner,false)
% add_slack_info1('U011E5YEVD0',is_restricted,false)
% add_slack_info1('U011E5YEVD0',is_ultra_restricted,false)
% add_slack_info1('U011E5YEVD0',name,"gilbertbgarza")
% add_slack_info1('U011E5YEVD0',presence,"away")
% add_slack_info1('U011E5YEVD0',profile,_47300{avatar_hash:"g44d05cf6268",display_name:"Gilbert",display_name_normalized:"Gilbert",email:"gilbertbgarza@gmail.com",fields:[],first_name:"Gilbert",image_192:"https://secure.gravatar.com/avatar/44d05cf62687c3489b863e4677414ca6.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-192.png",image_24:"https://secure.gravatar.com/avatar/44d05cf62687c3489b863e4677414ca6.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-24.png",image_32:"https://secure.gravatar.com/avatar/44d05cf62687c3489b863e4677414ca6.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-32.png",image_48:"https://secure.gravatar.com/avatar/44d05cf62687c3489b863e4677414ca6.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-48.png",image_512:"https://secure.gravatar.com/avatar/44d05cf62687c3489b863e4677414ca6.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-512.png",image_72:"https://secure.gravatar.com/avatar/44d05cf62687c3489b863e4677414ca6.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-72.png",last_name:"",phone:"",real_name:"Gilbert",real_name_normalized:"Gilbert",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:"https://pubb.substack.com"})
% add_slack_info1('U011E5YEVD0',real_name,"Gilbert")
% add_slack_info1('U011E5YEVD0',team_id,"T010WMWBAGY")
% add_slack_info1('U011E5YEVD0',tz,"America/Los_Angeles")
% add_slack_info1('U011E5YEVD0',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011E5YEVD0',tz_offset,-25200)
% add_slack_info1('U011E5YEVD0',updated,1586495820)
% add_slack_info1(var,instance,'U011F5LT91C')
% add_slack_info1('U011F5LT91C',color,"d58247")
% add_slack_info1('U011F5LT91C',deleted,false)
% add_slack_info1('U011F5LT91C',id,"U011F5LT91C")
% add_slack_info1('U011F5LT91C',is_admin,false)
% add_slack_info1('U011F5LT91C',is_app_user,false)
% add_slack_info1('U011F5LT91C',is_bot,false)
% add_slack_info1('U011F5LT91C',is_owner,false)
% add_slack_info1('U011F5LT91C',is_primary_owner,false)
% add_slack_info1('U011F5LT91C',is_restricted,false)
% add_slack_info1('U011F5LT91C',is_ultra_restricted,false)
% add_slack_info1('U011F5LT91C',name,"tamir.bahar")
% add_slack_info1('U011F5LT91C',presence,"away")
% add_slack_info1('U011F5LT91C',profile,_47482{avatar_hash:"13ce838e7e15",display_name:"Tamir Bahar",display_name_normalized:"Tamir Bahar",email:"tamir.bahar@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_original.jpg",is_custom_image:true,phone:"",real_name:"Tamir Bahar",real_name_normalized:"Tamir Bahar",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011F5LT91C',real_name,"Tamir Bahar")
% add_slack_info1('U011F5LT91C',team_id,"T010WMWBAGY")
% add_slack_info1('U011F5LT91C',tz,"Asia/Jerusalem")
% add_slack_info1('U011F5LT91C',tz_label,"Israel Daylight Time")
% add_slack_info1('U011F5LT91C',tz_offset,10800)
% add_slack_info1('U011F5LT91C',updated,1586273775)
% add_slack_info1(var,instance,'U011F5NMK1C')
% add_slack_info1('U011F5NMK1C',color,"bb86b7")
% add_slack_info1('U011F5NMK1C',deleted,false)
% add_slack_info1('U011F5NMK1C',id,"U011F5NMK1C")
% add_slack_info1('U011F5NMK1C',is_admin,false)
% add_slack_info1('U011F5NMK1C',is_app_user,false)
% add_slack_info1('U011F5NMK1C',is_bot,false)
% add_slack_info1('U011F5NMK1C',is_owner,false)
% add_slack_info1('U011F5NMK1C',is_primary_owner,false)
% add_slack_info1('U011F5NMK1C',is_restricted,false)
% add_slack_info1('U011F5NMK1C',is_ultra_restricted,false)
% add_slack_info1('U011F5NMK1C',name,"lee.yi.jie.joel")
% add_slack_info1('U011F5NMK1C',presence,"away")
% add_slack_info1('U011F5NMK1C',profile,_47668{avatar_hash:"gfa549aca537",display_name:"Joel Lee",display_name_normalized:"Joel Lee",email:"lee.yi.jie.joel@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/fa549aca53739ab4e97bde3cd6a6bed5.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-192.png",image_24:"https://secure.gravatar.com/avatar/fa549aca53739ab4e97bde3cd6a6bed5.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-24.png",image_32:"https://secure.gravatar.com/avatar/fa549aca53739ab4e97bde3cd6a6bed5.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-32.png",image_48:"https://secure.gravatar.com/avatar/fa549aca53739ab4e97bde3cd6a6bed5.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-48.png",image_512:"https://secure.gravatar.com/avatar/fa549aca53739ab4e97bde3cd6a6bed5.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-512.png",image_72:"https://secure.gravatar.com/avatar/fa549aca53739ab4e97bde3cd6a6bed5.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-72.png",phone:"",real_name:"Joel Lee",real_name_normalized:"Joel Lee",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011F5NMK1C',real_name,"Joel Lee")
% add_slack_info1('U011F5NMK1C',team_id,"T010WMWBAGY")
% add_slack_info1('U011F5NMK1C',tz,"Asia/Kuala_Lumpur")
% add_slack_info1('U011F5NMK1C',tz_label,"Singapore Standard Time")
% add_slack_info1('U011F5NMK1C',tz_offset,28800)
% add_slack_info1('U011F5NMK1C',updated,1586263196)
% add_slack_info1(var,instance,'U011F63HYFQ')
% add_slack_info1('U011F63HYFQ',color,"db3150")
% add_slack_info1('U011F63HYFQ',deleted,false)
% add_slack_info1('U011F63HYFQ',id,"U011F63HYFQ")
% add_slack_info1('U011F63HYFQ',is_admin,false)
% add_slack_info1('U011F63HYFQ',is_app_user,false)
% add_slack_info1('U011F63HYFQ',is_bot,false)
% add_slack_info1('U011F63HYFQ',is_owner,false)
% add_slack_info1('U011F63HYFQ',is_primary_owner,false)
% add_slack_info1('U011F63HYFQ',is_restricted,false)
% add_slack_info1('U011F63HYFQ',is_ultra_restricted,false)
% add_slack_info1('U011F63HYFQ',name,"g")
% add_slack_info1('U011F63HYFQ',presence,"away")
% add_slack_info1('U011F63HYFQ',profile,_47842{avatar_hash:"b29c8c21e4ee",display_name:"Gustav",display_name_normalized:"Gustav",email:"g@gu.pe",fields:null,first_name:"Gustav",image_1024:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_original.jpg",is_custom_image:true,last_name:"Melck",phone:"",real_name:"Gustav Melck",real_name_normalized:"Gustav Melck",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011F63HYFQ',real_name,"Gustav Melck")
% add_slack_info1('U011F63HYFQ',team_id,"T010WMWBAGY")
% add_slack_info1('U011F63HYFQ',tz,"Africa/Harare")
% add_slack_info1('U011F63HYFQ',tz_label,"Central Africa Time")
% add_slack_info1('U011F63HYFQ',tz_offset,7200)
% add_slack_info1('U011F63HYFQ',updated,1586268238)
% add_slack_info1(var,instance,'U011F6CKX46')
% add_slack_info1('U011F6CKX46',color,"235e5b")
% add_slack_info1('U011F6CKX46',deleted,false)
% add_slack_info1('U011F6CKX46',id,"U011F6CKX46")
% add_slack_info1('U011F6CKX46',is_admin,false)
% add_slack_info1('U011F6CKX46',is_app_user,false)
% add_slack_info1('U011F6CKX46',is_bot,false)
% add_slack_info1('U011F6CKX46',is_owner,false)
% add_slack_info1('U011F6CKX46',is_primary_owner,false)
% add_slack_info1('U011F6CKX46',is_restricted,false)
% add_slack_info1('U011F6CKX46',is_ultra_restricted,false)
% add_slack_info1('U011F6CKX46',name,"minhnhdo")
% add_slack_info1('U011F6CKX46',presence,"away")
% add_slack_info1('U011F6CKX46',profile,_48036{avatar_hash:"g36eb3ff17c5",display_name:"",display_name_normalized:"",email:"minhnhdo@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/36eb3ff17c5f928c0a177f401badb386.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-192.png",image_24:"https://secure.gravatar.com/avatar/36eb3ff17c5f928c0a177f401badb386.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-24.png",image_32:"https://secure.gravatar.com/avatar/36eb3ff17c5f928c0a177f401badb386.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-32.png",image_48:"https://secure.gravatar.com/avatar/36eb3ff17c5f928c0a177f401badb386.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-48.png",image_512:"https://secure.gravatar.com/avatar/36eb3ff17c5f928c0a177f401badb386.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-512.png",image_72:"https://secure.gravatar.com/avatar/36eb3ff17c5f928c0a177f401badb386.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-72.png",phone:"",real_name:"Matthew",real_name_normalized:"Matthew",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011F6CKX46',real_name,"Matthew")
% add_slack_info1('U011F6CKX46',team_id,"T010WMWBAGY")
% add_slack_info1('U011F6CKX46',tz,"America/New_York")
% add_slack_info1('U011F6CKX46',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011F6CKX46',tz_offset,-14400)
% add_slack_info1('U011F6CKX46',updated,1586264618)
% add_slack_info1(var,instance,'U011F6R5ZML')
% add_slack_info1('U011F6R5ZML',color,"c386df")
% add_slack_info1('U011F6R5ZML',deleted,false)
% add_slack_info1('U011F6R5ZML',id,"U011F6R5ZML")
% add_slack_info1('U011F6R5ZML',is_admin,false)
% add_slack_info1('U011F6R5ZML',is_app_user,false)
% add_slack_info1('U011F6R5ZML',is_bot,false)
% add_slack_info1('U011F6R5ZML',is_owner,false)
% add_slack_info1('U011F6R5ZML',is_primary_owner,false)
% add_slack_info1('U011F6R5ZML',is_restricted,false)
% add_slack_info1('U011F6R5ZML',is_ultra_restricted,false)
% add_slack_info1('U011F6R5ZML',name,"yolaine.bourda")
% add_slack_info1('U011F6R5ZML',presence,"away")
% add_slack_info1('U011F6R5ZML',profile,_48210{avatar_hash:"g1d510b7c106",display_name:"Yolaine",display_name_normalized:"Yolaine",email:"yolaine.bourda@wanadoo.fr",fields:null,image_192:"https://secure.gravatar.com/avatar/1d510b7c1065e0b2e95075a4f0207063.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-192.png",image_24:"https://secure.gravatar.com/avatar/1d510b7c1065e0b2e95075a4f0207063.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-24.png",image_32:"https://secure.gravatar.com/avatar/1d510b7c1065e0b2e95075a4f0207063.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-32.png",image_48:"https://secure.gravatar.com/avatar/1d510b7c1065e0b2e95075a4f0207063.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-48.png",image_512:"https://secure.gravatar.com/avatar/1d510b7c1065e0b2e95075a4f0207063.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-512.png",image_72:"https://secure.gravatar.com/avatar/1d510b7c1065e0b2e95075a4f0207063.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-72.png",phone:"",real_name:"Yolaine",real_name_normalized:"Yolaine",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011F6R5ZML',real_name,"Yolaine")
% add_slack_info1('U011F6R5ZML',team_id,"T010WMWBAGY")
% add_slack_info1('U011F6R5ZML',tz,"Europe/Brussels")
% add_slack_info1('U011F6R5ZML',tz_label,"Central European Summer Time")
% add_slack_info1('U011F6R5ZML',tz_offset,7200)
% add_slack_info1('U011F6R5ZML',updated,1586265314)
% add_slack_info1(var,instance,'U011F6ZP8S2')
% add_slack_info1('U011F6ZP8S2',color,"a63024")
% add_slack_info1('U011F6ZP8S2',deleted,false)
% add_slack_info1('U011F6ZP8S2',id,"U011F6ZP8S2")
% add_slack_info1('U011F6ZP8S2',is_admin,false)
% add_slack_info1('U011F6ZP8S2',is_app_user,false)
% add_slack_info1('U011F6ZP8S2',is_bot,false)
% add_slack_info1('U011F6ZP8S2',is_owner,false)
% add_slack_info1('U011F6ZP8S2',is_primary_owner,false)
% add_slack_info1('U011F6ZP8S2',is_restricted,false)
% add_slack_info1('U011F6ZP8S2',is_ultra_restricted,false)
% add_slack_info1('U011F6ZP8S2',name,"brendan")
% add_slack_info1('U011F6ZP8S2',presence,"away")
% add_slack_info1('U011F6ZP8S2',profile,_48384{avatar_hash:"2d76eb68e694",display_name:"Brendan Molloy",display_name_normalized:"Brendan Molloy",email:"brendan@bbqsrc.net",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_original.jpg",is_custom_image:true,phone:"",real_name:"Brendan Molloy",real_name_normalized:"Brendan Molloy",skype:"",status_emoji:":flag-se:",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011F6ZP8S2',real_name,"Brendan Molloy")
% add_slack_info1('U011F6ZP8S2',team_id,"T010WMWBAGY")
% add_slack_info1('U011F6ZP8S2',tz,"Europe/Amsterdam")
% add_slack_info1('U011F6ZP8S2',tz_label,"Central European Summer Time")
% add_slack_info1('U011F6ZP8S2',tz_offset,7200)
% add_slack_info1('U011F6ZP8S2',updated,1586272992)
% add_slack_info1(var,instance,'U011F7VBCCE')
% add_slack_info1('U011F7VBCCE',color,"43761b")
% add_slack_info1('U011F7VBCCE',deleted,false)
% add_slack_info1('U011F7VBCCE',id,"U011F7VBCCE")
% add_slack_info1('U011F7VBCCE',is_admin,false)
% add_slack_info1('U011F7VBCCE',is_app_user,false)
% add_slack_info1('U011F7VBCCE',is_bot,false)
% add_slack_info1('U011F7VBCCE',is_owner,false)
% add_slack_info1('U011F7VBCCE',is_primary_owner,false)
% add_slack_info1('U011F7VBCCE',is_restricted,false)
% add_slack_info1('U011F7VBCCE',is_ultra_restricted,false)
% add_slack_info1('U011F7VBCCE',name,"jesse")
% add_slack_info1('U011F7VBCCE',presence,"away")
% add_slack_info1('U011F7VBCCE',profile,_48570{avatar_hash:"041f88fb82ec",display_name:"esbe",display_name_normalized:"esbe",email:"jesse@toomanybees.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_original.jpg",is_custom_image:true,phone:"",real_name:"esbe",real_name_normalized:"esbe",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011F7VBCCE',real_name,"esbe")
% add_slack_info1('U011F7VBCCE',team_id,"T010WMWBAGY")
% add_slack_info1('U011F7VBCCE',tz,"America/New_York")
% add_slack_info1('U011F7VBCCE',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011F7VBCCE',tz_offset,-14400)
% add_slack_info1('U011F7VBCCE',updated,1586268088)
% add_slack_info1(var,instance,'U011FBA5FPG')
% add_slack_info1('U011FBA5FPG',color,"7d414c")
% add_slack_info1('U011FBA5FPG',deleted,false)
% add_slack_info1('U011FBA5FPG',id,"U011FBA5FPG")
% add_slack_info1('U011FBA5FPG',is_admin,false)
% add_slack_info1('U011FBA5FPG',is_app_user,false)
% add_slack_info1('U011FBA5FPG',is_bot,false)
% add_slack_info1('U011FBA5FPG',is_owner,false)
% add_slack_info1('U011FBA5FPG',is_primary_owner,false)
% add_slack_info1('U011FBA5FPG',is_restricted,false)
% add_slack_info1('U011FBA5FPG',is_ultra_restricted,false)
% add_slack_info1('U011FBA5FPG',name,"p.meled.in")
% add_slack_info1('U011FBA5FPG',presence,"away")
% add_slack_info1('U011FBA5FPG',profile,_48756{avatar_hash:"1a5ae3ec46e3",display_name:"Pavel Meledin",display_name_normalized:"Pavel Meledin",email:"p.meled.in@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_1024.png",image_192:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_192.png",image_24:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_24.png",image_32:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_32.png",image_48:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_48.png",image_512:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_512.png",image_72:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_72.png",image_original:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_original.png",is_custom_image:true,phone:"",real_name:"Pavel Meledin",real_name_normalized:"Pavel Meledin",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011FBA5FPG',real_name,"Pavel Meledin")
% add_slack_info1('U011FBA5FPG',team_id,"T010WMWBAGY")
% add_slack_info1('U011FBA5FPG',tz,"Asia/Jerusalem")
% add_slack_info1('U011FBA5FPG',tz_label,"Israel Daylight Time")
% add_slack_info1('U011FBA5FPG',tz_offset,10800)
% add_slack_info1('U011FBA5FPG',updated,1586273037)
% add_slack_info1(var,instance,'U011FCJFGB0')
% add_slack_info1('U011FCJFGB0',color,"4d5e26")
% add_slack_info1('U011FCJFGB0',deleted,false)
% add_slack_info1('U011FCJFGB0',id,"U011FCJFGB0")
% add_slack_info1('U011FCJFGB0',is_admin,false)
% add_slack_info1('U011FCJFGB0',is_app_user,false)
% add_slack_info1('U011FCJFGB0',is_bot,false)
% add_slack_info1('U011FCJFGB0',is_owner,false)
% add_slack_info1('U011FCJFGB0',is_primary_owner,false)
% add_slack_info1('U011FCJFGB0',is_restricted,false)
% add_slack_info1('U011FCJFGB0',is_ultra_restricted,false)
% add_slack_info1('U011FCJFGB0',name,"haasis_noah")
% add_slack_info1('U011FCJFGB0',presence,"away")
% add_slack_info1('U011FCJFGB0',profile,_48942{avatar_hash:"gd7f8d0a8465",display_name:"Noah Haasis",display_name_normalized:"Noah Haasis",email:"haasis_noah@yahoo.de",fields:null,image_192:"https://secure.gravatar.com/avatar/d7f8d0a84659a0531b808a41a47a35a8.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-192.png",image_24:"https://secure.gravatar.com/avatar/d7f8d0a84659a0531b808a41a47a35a8.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-24.png",image_32:"https://secure.gravatar.com/avatar/d7f8d0a84659a0531b808a41a47a35a8.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-32.png",image_48:"https://secure.gravatar.com/avatar/d7f8d0a84659a0531b808a41a47a35a8.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-48.png",image_512:"https://secure.gravatar.com/avatar/d7f8d0a84659a0531b808a41a47a35a8.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-512.png",image_72:"https://secure.gravatar.com/avatar/d7f8d0a84659a0531b808a41a47a35a8.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-72.png",phone:"",real_name:"Noah Haasis",real_name_normalized:"Noah Haasis",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011FCJFGB0',real_name,"Noah Haasis")
% add_slack_info1('U011FCJFGB0',team_id,"T010WMWBAGY")
% add_slack_info1('U011FCJFGB0',tz,"Europe/Amsterdam")
% add_slack_info1('U011FCJFGB0',tz_label,"Central European Summer Time")
% add_slack_info1('U011FCJFGB0',tz_offset,7200)
% add_slack_info1('U011FCJFGB0',updated,1586274913)
% add_slack_info1(var,instance,'U011FT66V38')
% add_slack_info1('U011FT66V38',color,"3c989f")
% add_slack_info1('U011FT66V38',deleted,false)
% add_slack_info1('U011FT66V38',id,"U011FT66V38")
% add_slack_info1('U011FT66V38',is_admin,false)
% add_slack_info1('U011FT66V38',is_app_user,false)
% add_slack_info1('U011FT66V38',is_bot,false)
% add_slack_info1('U011FT66V38',is_owner,false)
% add_slack_info1('U011FT66V38',is_primary_owner,false)
% add_slack_info1('U011FT66V38',is_restricted,false)
% add_slack_info1('U011FT66V38',is_ultra_restricted,false)
% add_slack_info1('U011FT66V38',name,"kai")
% add_slack_info1('U011FT66V38',presence,"away")
% add_slack_info1('U011FT66V38',profile,_49116{avatar_hash:"g30966c6e1a5",display_name:"Kai Koenig",display_name_normalized:"Kai Koenig",email:"kai@ventego-creative.co.nz",fields:null,image_192:"https://secure.gravatar.com/avatar/30966c6e1a52faf6b287b59a01195938.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-192.png",image_24:"https://secure.gravatar.com/avatar/30966c6e1a52faf6b287b59a01195938.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-24.png",image_32:"https://secure.gravatar.com/avatar/30966c6e1a52faf6b287b59a01195938.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-32.png",image_48:"https://secure.gravatar.com/avatar/30966c6e1a52faf6b287b59a01195938.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-48.png",image_512:"https://secure.gravatar.com/avatar/30966c6e1a52faf6b287b59a01195938.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-512.png",image_72:"https://secure.gravatar.com/avatar/30966c6e1a52faf6b287b59a01195938.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-72.png",phone:"",real_name:"Kai Koenig",real_name_normalized:"Kai Koenig",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011FT66V38',real_name,"Kai Koenig")
% add_slack_info1('U011FT66V38',team_id,"T010WMWBAGY")
% add_slack_info1('U011FT66V38',tz,"Pacific/Auckland")
% add_slack_info1('U011FT66V38',tz_label,"New Zealand Standard Time")
% add_slack_info1('U011FT66V38',tz_offset,43200)
% add_slack_info1('U011FT66V38',updated,1586305801)
% add_slack_info1(var,instance,'U011GEQMWJZ')
% add_slack_info1('U011GEQMWJZ',color,"5b89d5")
% add_slack_info1('U011GEQMWJZ',deleted,false)
% add_slack_info1('U011GEQMWJZ',id,"U011GEQMWJZ")
% add_slack_info1('U011GEQMWJZ',is_admin,false)
% add_slack_info1('U011GEQMWJZ',is_app_user,false)
% add_slack_info1('U011GEQMWJZ',is_bot,false)
% add_slack_info1('U011GEQMWJZ',is_owner,false)
% add_slack_info1('U011GEQMWJZ',is_primary_owner,false)
% add_slack_info1('U011GEQMWJZ',is_restricted,false)
% add_slack_info1('U011GEQMWJZ',is_ultra_restricted,false)
% add_slack_info1('U011GEQMWJZ',name,"dbryan.green")
% add_slack_info1('U011GEQMWJZ',presence,"away")
% add_slack_info1('U011GEQMWJZ',profile,_49290{avatar_hash:"gd50aca5de10",display_name:"Bryan Green",display_name_normalized:"Bryan Green",email:"dbryan.green@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/d50aca5de10df94a1a772ae616f8a6c6.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-192.png",image_24:"https://secure.gravatar.com/avatar/d50aca5de10df94a1a772ae616f8a6c6.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-24.png",image_32:"https://secure.gravatar.com/avatar/d50aca5de10df94a1a772ae616f8a6c6.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-32.png",image_48:"https://secure.gravatar.com/avatar/d50aca5de10df94a1a772ae616f8a6c6.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-48.png",image_512:"https://secure.gravatar.com/avatar/d50aca5de10df94a1a772ae616f8a6c6.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-512.png",image_72:"https://secure.gravatar.com/avatar/d50aca5de10df94a1a772ae616f8a6c6.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-72.png",phone:"",real_name:"Bryan Green",real_name_normalized:"Bryan Green",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011GEQMWJZ',real_name,"Bryan Green")
% add_slack_info1('U011GEQMWJZ',team_id,"T010WMWBAGY")
% add_slack_info1('U011GEQMWJZ',tz,"America/Chicago")
% add_slack_info1('U011GEQMWJZ',tz_label,"Central Daylight Time")
% add_slack_info1('U011GEQMWJZ',tz_offset,-18000)
% add_slack_info1('U011GEQMWJZ',updated,1586262056)
% add_slack_info1(var,instance,'U011GFBHBE1')
% add_slack_info1('U011GFBHBE1',color,"9b3b45")
% add_slack_info1('U011GFBHBE1',deleted,false)
% add_slack_info1('U011GFBHBE1',id,"U011GFBHBE1")
% add_slack_info1('U011GFBHBE1',is_admin,false)
% add_slack_info1('U011GFBHBE1',is_app_user,false)
% add_slack_info1('U011GFBHBE1',is_bot,false)
% add_slack_info1('U011GFBHBE1',is_owner,false)
% add_slack_info1('U011GFBHBE1',is_primary_owner,false)
% add_slack_info1('U011GFBHBE1',is_restricted,false)
% add_slack_info1('U011GFBHBE1',is_ultra_restricted,false)
% add_slack_info1('U011GFBHBE1',name,"me1")
% add_slack_info1('U011GFBHBE1',presence,"away")
% add_slack_info1('U011GFBHBE1',profile,_49464{avatar_hash:"7bf4bc6ab9a8",display_name:"",display_name_normalized:"",email:"me@thedanabrams.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_original.jpg",is_custom_image:true,phone:"",real_name:"Dan Abrams",real_name_normalized:"Dan Abrams",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011GFBHBE1',real_name,"Dan Abrams")
% add_slack_info1('U011GFBHBE1',team_id,"T010WMWBAGY")
% add_slack_info1('U011GFBHBE1',tz,"America/New_York")
% add_slack_info1('U011GFBHBE1',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011GFBHBE1',tz_offset,-14400)
% add_slack_info1('U011GFBHBE1',updated,1586641521)
% add_slack_info1(var,instance,'U011GJME5C5')
% add_slack_info1('U011GJME5C5',color,"50a0cf")
% add_slack_info1('U011GJME5C5',deleted,false)
% add_slack_info1('U011GJME5C5',id,"U011GJME5C5")
% add_slack_info1('U011GJME5C5',is_admin,false)
% add_slack_info1('U011GJME5C5',is_app_user,false)
% add_slack_info1('U011GJME5C5',is_bot,false)
% add_slack_info1('U011GJME5C5',is_owner,false)
% add_slack_info1('U011GJME5C5',is_primary_owner,false)
% add_slack_info1('U011GJME5C5',is_restricted,false)
% add_slack_info1('U011GJME5C5',is_ultra_restricted,false)
% add_slack_info1('U011GJME5C5',name,"coreyhaines")
% add_slack_info1('U011GJME5C5',presence,"away")
% add_slack_info1('U011GJME5C5',profile,_49650{avatar_hash:"g3d7807bb66e",display_name:"corey haines",display_name_normalized:"corey haines",email:"coreyhaines@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/3d7807bb66e1a0c68c73ab2daaa77d8f.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0003-192.png",image_24:"https://secure.gravatar.com/avatar/3d7807bb66e1a0c68c73ab2daaa77d8f.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0003-24.png",image_32:"https://secure.gravatar.com/avatar/3d7807bb66e1a0c68c73ab2daaa77d8f.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0003-32.png",image_48:"https://secure.gravatar.com/avatar/3d7807bb66e1a0c68c73ab2daaa77d8f.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0003-48.png",image_512:"https://secure.gravatar.com/avatar/3d7807bb66e1a0c68c73ab2daaa77d8f.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0003-512.png",image_72:"https://secure.gravatar.com/avatar/3d7807bb66e1a0c68c73ab2daaa77d8f.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0003-72.png",phone:"",real_name:"corey haines",real_name_normalized:"corey haines",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011GJME5C5',real_name,"corey haines")
% add_slack_info1('U011GJME5C5',team_id,"T010WMWBAGY")
% add_slack_info1('U011GJME5C5',tz,"America/Chicago")
% add_slack_info1('U011GJME5C5',tz_label,"Central Daylight Time")
% add_slack_info1('U011GJME5C5',tz_offset,-18000)
% add_slack_info1('U011GJME5C5',updated,1586266536)
% add_slack_info1(var,instance,'U011GKNKBNH')
% add_slack_info1('U011GKNKBNH',color,"8f4a2b")
% add_slack_info1('U011GKNKBNH',deleted,false)
% add_slack_info1('U011GKNKBNH',id,"U011GKNKBNH")
% add_slack_info1('U011GKNKBNH',is_admin,false)
% add_slack_info1('U011GKNKBNH',is_app_user,false)
% add_slack_info1('U011GKNKBNH',is_bot,false)
% add_slack_info1('U011GKNKBNH',is_owner,false)
% add_slack_info1('U011GKNKBNH',is_primary_owner,false)
% add_slack_info1('U011GKNKBNH',is_restricted,false)
% add_slack_info1('U011GKNKBNH',is_ultra_restricted,false)
% add_slack_info1('U011GKNKBNH',name,"n_she")
% add_slack_info1('U011GKNKBNH',presence,"away")
% add_slack_info1('U011GKNKBNH',profile,_49824{avatar_hash:"gbd80d814a4a",display_name:"nadia sheikh",display_name_normalized:"nadia sheikh",email:"n_she@encs.concordia.ca",fields:null,image_192:"https://secure.gravatar.com/avatar/bd80d814a4a185e167dbc60e44f72e12.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-192.png",image_24:"https://secure.gravatar.com/avatar/bd80d814a4a185e167dbc60e44f72e12.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-24.png",image_32:"https://secure.gravatar.com/avatar/bd80d814a4a185e167dbc60e44f72e12.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-32.png",image_48:"https://secure.gravatar.com/avatar/bd80d814a4a185e167dbc60e44f72e12.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-48.png",image_512:"https://secure.gravatar.com/avatar/bd80d814a4a185e167dbc60e44f72e12.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-512.png",image_72:"https://secure.gravatar.com/avatar/bd80d814a4a185e167dbc60e44f72e12.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-72.png",phone:"",real_name:"nadia sheikh",real_name_normalized:"nadia sheikh",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011GKNKBNH',real_name,"nadia sheikh")
% add_slack_info1('U011GKNKBNH',team_id,"T010WMWBAGY")
% add_slack_info1('U011GKNKBNH',tz,"America/New_York")
% add_slack_info1('U011GKNKBNH',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011GKNKBNH',tz_offset,-14400)
% add_slack_info1('U011GKNKBNH',updated,1586267581)
% add_slack_info1(var,instance,'U011GN7T96Z')
% add_slack_info1('U011GN7T96Z',color,"84b22f")
% add_slack_info1('U011GN7T96Z',deleted,false)
% add_slack_info1('U011GN7T96Z',id,"U011GN7T96Z")
% add_slack_info1('U011GN7T96Z',is_admin,false)
% add_slack_info1('U011GN7T96Z',is_app_user,false)
% add_slack_info1('U011GN7T96Z',is_bot,false)
% add_slack_info1('U011GN7T96Z',is_owner,false)
% add_slack_info1('U011GN7T96Z',is_primary_owner,false)
% add_slack_info1('U011GN7T96Z',is_restricted,false)
% add_slack_info1('U011GN7T96Z',is_ultra_restricted,false)
% add_slack_info1('U011GN7T96Z',name,"charleslee592")
% add_slack_info1('U011GN7T96Z',presence,"away")
% add_slack_info1('U011GN7T96Z',profile,_49998{avatar_hash:"84ed3433263f",display_name:"Charles",display_name_normalized:"Charles",email:"charleslee592@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_original.jpg",is_custom_image:true,phone:"",real_name:"Charles",real_name_normalized:"Charles",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011GN7T96Z',real_name,"Charles")
% add_slack_info1('U011GN7T96Z',team_id,"T010WMWBAGY")
% add_slack_info1('U011GN7T96Z',tz,"America/New_York")
% add_slack_info1('U011GN7T96Z',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011GN7T96Z',tz_offset,-14400)
% add_slack_info1('U011GN7T96Z',updated,1586270117)
% add_slack_info1(var,instance,'U011GQW1T3P')
% add_slack_info1('U011GQW1T3P',color,"619a4f")
% add_slack_info1('U011GQW1T3P',deleted,false)
% add_slack_info1('U011GQW1T3P',id,"U011GQW1T3P")
% add_slack_info1('U011GQW1T3P',is_admin,false)
% add_slack_info1('U011GQW1T3P',is_app_user,false)
% add_slack_info1('U011GQW1T3P',is_bot,false)
% add_slack_info1('U011GQW1T3P',is_owner,false)
% add_slack_info1('U011GQW1T3P',is_primary_owner,false)
% add_slack_info1('U011GQW1T3P',is_restricted,false)
% add_slack_info1('U011GQW1T3P',is_ultra_restricted,false)
% add_slack_info1('U011GQW1T3P',name,"dogenpunk")
% add_slack_info1('U011GQW1T3P',presence,"away")
% add_slack_info1('U011GQW1T3P',profile,_50184{avatar_hash:"gbeea7193413",display_name:"Matthew M. Nelson",display_name_normalized:"Matthew M. Nelson",email:"dogenpunk@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/beea7193413a677625692f93e6e5b222.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-192.png",image_24:"https://secure.gravatar.com/avatar/beea7193413a677625692f93e6e5b222.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-24.png",image_32:"https://secure.gravatar.com/avatar/beea7193413a677625692f93e6e5b222.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-32.png",image_48:"https://secure.gravatar.com/avatar/beea7193413a677625692f93e6e5b222.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-48.png",image_512:"https://secure.gravatar.com/avatar/beea7193413a677625692f93e6e5b222.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-512.png",image_72:"https://secure.gravatar.com/avatar/beea7193413a677625692f93e6e5b222.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-72.png",phone:"",real_name:"Matthew M. Nelson",real_name_normalized:"Matthew M. Nelson",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011GQW1T3P',real_name,"Matthew M. Nelson")
% add_slack_info1('U011GQW1T3P',team_id,"T010WMWBAGY")
% add_slack_info1('U011GQW1T3P',tz,"America/Chicago")
% add_slack_info1('U011GQW1T3P',tz_label,"Central Daylight Time")
% add_slack_info1('U011GQW1T3P',tz_offset,-18000)
% add_slack_info1('U011GQW1T3P',updated,1586272290)
% add_slack_info1(var,instance,'U011GR47SUV')
% add_slack_info1('U011GR47SUV',color,"a72f79")
% add_slack_info1('U011GR47SUV',deleted,false)
% add_slack_info1('U011GR47SUV',id,"U011GR47SUV")
% add_slack_info1('U011GR47SUV',is_admin,false)
% add_slack_info1('U011GR47SUV',is_app_user,false)
% add_slack_info1('U011GR47SUV',is_bot,false)
% add_slack_info1('U011GR47SUV',is_owner,false)
% add_slack_info1('U011GR47SUV',is_primary_owner,false)
% add_slack_info1('U011GR47SUV',is_restricted,false)
% add_slack_info1('U011GR47SUV',is_ultra_restricted,false)
% add_slack_info1('U011GR47SUV',name,"jamie")
% add_slack_info1('U011GR47SUV',presence,"away")
% add_slack_info1('U011GR47SUV',profile,_50358{avatar_hash:"098bc76f6326",display_name:"Jamie",display_name_normalized:"Jamie",email:"jamie@lowerarchy.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_original.jpg",is_custom_image:true,phone:"",real_name:"Jamie",real_name_normalized:"Jamie",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011GR47SUV',real_name,"Jamie")
% add_slack_info1('U011GR47SUV',team_id,"T010WMWBAGY")
% add_slack_info1('U011GR47SUV',tz,"America/Los_Angeles")
% add_slack_info1('U011GR47SUV',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011GR47SUV',tz_offset,-25200)
% add_slack_info1('U011GR47SUV',updated,1586272594)
% add_slack_info1(var,instance,'U011H0V8DQR')
% add_slack_info1('U011H0V8DQR',color,"99a949")
% add_slack_info1('U011H0V8DQR',deleted,false)
% add_slack_info1('U011H0V8DQR',id,"U011H0V8DQR")
% add_slack_info1('U011H0V8DQR',is_admin,false)
% add_slack_info1('U011H0V8DQR',is_app_user,false)
% add_slack_info1('U011H0V8DQR',is_bot,false)
% add_slack_info1('U011H0V8DQR',is_owner,false)
% add_slack_info1('U011H0V8DQR',is_primary_owner,false)
% add_slack_info1('U011H0V8DQR',is_restricted,false)
% add_slack_info1('U011H0V8DQR',is_ultra_restricted,false)
% add_slack_info1('U011H0V8DQR',name,"mail")
% add_slack_info1('U011H0V8DQR',presence,"away")
% add_slack_info1('U011H0V8DQR',profile,_50544{avatar_hash:"g8bdd4fab8a1",display_name:"Kien",display_name_normalized:"Kien",email:"mail@kien.ai",fields:null,image_192:"https://secure.gravatar.com/avatar/8bdd4fab8a1978914c4f36cb23d01d76.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-192.png",image_24:"https://secure.gravatar.com/avatar/8bdd4fab8a1978914c4f36cb23d01d76.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-24.png",image_32:"https://secure.gravatar.com/avatar/8bdd4fab8a1978914c4f36cb23d01d76.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-32.png",image_48:"https://secure.gravatar.com/avatar/8bdd4fab8a1978914c4f36cb23d01d76.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-48.png",image_512:"https://secure.gravatar.com/avatar/8bdd4fab8a1978914c4f36cb23d01d76.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-512.png",image_72:"https://secure.gravatar.com/avatar/8bdd4fab8a1978914c4f36cb23d01d76.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-72.png",phone:"",real_name:"Kien",real_name_normalized:"Kien",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011H0V8DQR',real_name,"Kien")
% add_slack_info1('U011H0V8DQR',team_id,"T010WMWBAGY")
% add_slack_info1('U011H0V8DQR',tz,"Asia/Kuala_Lumpur")
% add_slack_info1('U011H0V8DQR',tz_label,"Singapore Standard Time")
% add_slack_info1('U011H0V8DQR',tz_offset,28800)
% add_slack_info1('U011H0V8DQR',updated,1586262184)
% add_slack_info1(var,instance,'U011H1UFX6H')
% add_slack_info1('U011H1UFX6H',color,"dc7dbb")
% add_slack_info1('U011H1UFX6H',deleted,false)
% add_slack_info1('U011H1UFX6H',id,"U011H1UFX6H")
% add_slack_info1('U011H1UFX6H',is_admin,false)
% add_slack_info1('U011H1UFX6H',is_app_user,false)
% add_slack_info1('U011H1UFX6H',is_bot,false)
% add_slack_info1('U011H1UFX6H',is_owner,false)
% add_slack_info1('U011H1UFX6H',is_primary_owner,false)
% add_slack_info1('U011H1UFX6H',is_restricted,false)
% add_slack_info1('U011H1UFX6H',is_ultra_restricted,false)
% add_slack_info1('U011H1UFX6H',name,"akash")
% add_slack_info1('U011H1UFX6H',presence,"away")
% add_slack_info1('U011H1UFX6H',profile,_50718{avatar_hash:"g654dd3aa731",display_name:"Akash",display_name_normalized:"Akash",email:"akash@akash.im",fields:null,image_192:"https://secure.gravatar.com/avatar/654dd3aa73158b8aabbab50f719038bb.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0006-192.png",image_24:"https://secure.gravatar.com/avatar/654dd3aa73158b8aabbab50f719038bb.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0006-24.png",image_32:"https://secure.gravatar.com/avatar/654dd3aa73158b8aabbab50f719038bb.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0006-32.png",image_48:"https://secure.gravatar.com/avatar/654dd3aa73158b8aabbab50f719038bb.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0006-48.png",image_512:"https://secure.gravatar.com/avatar/654dd3aa73158b8aabbab50f719038bb.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0006-512.png",image_72:"https://secure.gravatar.com/avatar/654dd3aa73158b8aabbab50f719038bb.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0006-72.png",phone:"",real_name:"Akash",real_name_normalized:"Akash",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011H1UFX6H',real_name,"Akash")
% add_slack_info1('U011H1UFX6H',team_id,"T010WMWBAGY")
% add_slack_info1('U011H1UFX6H',tz,"Asia/Kolkata")
% add_slack_info1('U011H1UFX6H',tz_label,"India Standard Time")
% add_slack_info1('U011H1UFX6H',tz_offset,19800)
% add_slack_info1('U011H1UFX6H',updated,1586278377)
% add_slack_info1(var,instance,'U011HB1ED17')
% add_slack_info1('U011HB1ED17',color,"d1707d")
% add_slack_info1('U011HB1ED17',deleted,false)
% add_slack_info1('U011HB1ED17',id,"U011HB1ED17")
% add_slack_info1('U011HB1ED17',is_admin,false)
% add_slack_info1('U011HB1ED17',is_app_user,false)
% add_slack_info1('U011HB1ED17',is_bot,false)
% add_slack_info1('U011HB1ED17',is_owner,false)
% add_slack_info1('U011HB1ED17',is_primary_owner,false)
% add_slack_info1('U011HB1ED17',is_restricted,false)
% add_slack_info1('U011HB1ED17',is_ultra_restricted,false)
% add_slack_info1('U011HB1ED17',name,"simonjameskelly")
% add_slack_info1('U011HB1ED17',presence,"away")
% add_slack_info1('U011HB1ED17',profile,_50892{avatar_hash:"c2a374ea5b67",display_name:"Simon Kelly",display_name_normalized:"Simon Kelly",email:"simonjameskelly@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_original.jpg",is_custom_image:true,phone:"",real_name:"Simon Kelly",real_name_normalized:"Simon Kelly",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011HB1ED17',real_name,"Simon Kelly")
% add_slack_info1('U011HB1ED17',team_id,"T010WMWBAGY")
% add_slack_info1('U011HB1ED17',tz,"Europe/London")
% add_slack_info1('U011HB1ED17',tz_label,"British Summer Time")
% add_slack_info1('U011HB1ED17',tz_offset,3600)
% add_slack_info1('U011HB1ED17',updated,1586358276)
% add_slack_info1(var,instance,'U011HDG1KLM')
% add_slack_info1('U011HDG1KLM',color,"b14cbc")
% add_slack_info1('U011HDG1KLM',deleted,false)
% add_slack_info1('U011HDG1KLM',id,"U011HDG1KLM")
% add_slack_info1('U011HDG1KLM',is_admin,false)
% add_slack_info1('U011HDG1KLM',is_app_user,false)
% add_slack_info1('U011HDG1KLM',is_bot,false)
% add_slack_info1('U011HDG1KLM',is_owner,false)
% add_slack_info1('U011HDG1KLM',is_primary_owner,false)
% add_slack_info1('U011HDG1KLM',is_restricted,false)
% add_slack_info1('U011HDG1KLM',is_ultra_restricted,false)
% add_slack_info1('U011HDG1KLM',name,"thijs")
% add_slack_info1('U011HDG1KLM',presence,"away")
% add_slack_info1('U011HDG1KLM',profile,_51078{avatar_hash:"g942f6a63d4a",display_name:"Thijs van Dien",display_name_normalized:"Thijs van Dien",email:"thijs@vandien.net",fields:null,image_192:"https://secure.gravatar.com/avatar/942f6a63d4a85a1fc660ea6662e85832.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-192.png",image_24:"https://secure.gravatar.com/avatar/942f6a63d4a85a1fc660ea6662e85832.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-24.png",image_32:"https://secure.gravatar.com/avatar/942f6a63d4a85a1fc660ea6662e85832.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-32.png",image_48:"https://secure.gravatar.com/avatar/942f6a63d4a85a1fc660ea6662e85832.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-48.png",image_512:"https://secure.gravatar.com/avatar/942f6a63d4a85a1fc660ea6662e85832.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-512.png",image_72:"https://secure.gravatar.com/avatar/942f6a63d4a85a1fc660ea6662e85832.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-72.png",phone:"",real_name:"Thijs van Dien",real_name_normalized:"Thijs van Dien",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011HDG1KLM',real_name,"Thijs van Dien")
% add_slack_info1('U011HDG1KLM',team_id,"T010WMWBAGY")
% add_slack_info1('U011HDG1KLM',tz,"Europe/Warsaw")
% add_slack_info1('U011HDG1KLM',tz_label,"Central European Summer Time")
% add_slack_info1('U011HDG1KLM',tz_offset,7200)
% add_slack_info1('U011HDG1KLM',updated,1586291227)
% add_slack_info1(var,instance,'U011HDRGSQ1')
% add_slack_info1('U011HDRGSQ1',color,"a2a5dc")
% add_slack_info1('U011HDRGSQ1',deleted,false)
% add_slack_info1('U011HDRGSQ1',id,"U011HDRGSQ1")
% add_slack_info1('U011HDRGSQ1',is_admin,false)
% add_slack_info1('U011HDRGSQ1',is_app_user,false)
% add_slack_info1('U011HDRGSQ1',is_bot,false)
% add_slack_info1('U011HDRGSQ1',is_owner,false)
% add_slack_info1('U011HDRGSQ1',is_primary_owner,false)
% add_slack_info1('U011HDRGSQ1',is_restricted,false)
% add_slack_info1('U011HDRGSQ1',is_ultra_restricted,false)
% add_slack_info1('U011HDRGSQ1',name,"bmschwar")
% add_slack_info1('U011HDRGSQ1',presence,"away")
% add_slack_info1('U011HDRGSQ1',profile,_51252{avatar_hash:"g04ec229feb1",display_name:"",display_name_normalized:"",email:"bmschwar@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/04ec229feb1755d8319db1b89179f823.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-192.png",image_24:"https://secure.gravatar.com/avatar/04ec229feb1755d8319db1b89179f823.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-24.png",image_32:"https://secure.gravatar.com/avatar/04ec229feb1755d8319db1b89179f823.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-32.png",image_48:"https://secure.gravatar.com/avatar/04ec229feb1755d8319db1b89179f823.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-48.png",image_512:"https://secure.gravatar.com/avatar/04ec229feb1755d8319db1b89179f823.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-512.png",image_72:"https://secure.gravatar.com/avatar/04ec229feb1755d8319db1b89179f823.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-72.png",phone:"",real_name:"Ben Schwartz",real_name_normalized:"Ben Schwartz",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011HDRGSQ1',real_name,"Ben Schwartz")
% add_slack_info1('U011HDRGSQ1',team_id,"T010WMWBAGY")
% add_slack_info1('U011HDRGSQ1',tz,"America/Los_Angeles")
% add_slack_info1('U011HDRGSQ1',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011HDRGSQ1',tz_offset,-25200)
% add_slack_info1('U011HDRGSQ1',updated,1586268306)
% add_slack_info1(var,instance,'U011HDST18D')
% add_slack_info1('U011HDST18D',color,"9f69e7")
% add_slack_info1('U011HDST18D',deleted,false)
% add_slack_info1('U011HDST18D',id,"U011HDST18D")
% add_slack_info1('U011HDST18D',is_admin,false)
% add_slack_info1('U011HDST18D',is_app_user,false)
% add_slack_info1('U011HDST18D',is_bot,false)
% add_slack_info1('U011HDST18D',is_owner,false)
% add_slack_info1('U011HDST18D',is_primary_owner,false)
% add_slack_info1('U011HDST18D',is_restricted,false)
% add_slack_info1('U011HDST18D',is_ultra_restricted,false)
% add_slack_info1('U011HDST18D',name,"norbert")
% add_slack_info1('U011HDST18D',presence,"away")
% add_slack_info1('U011HDST18D',profile,_51426{avatar_hash:"613b726a79a0",display_name:"Norbert Hartl",display_name_normalized:"Norbert Hartl",email:"norbert@hartl.name",fields:null,first_name:"Norbert",image_1024:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_original.jpg",is_custom_image:true,last_name:"Hartl",phone:"",real_name:"Norbert Hartl",real_name_normalized:"Norbert Hartl",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:"Smalltalk mostly. @NorbertHartl on twitter"})
% add_slack_info1('U011HDST18D',real_name,"Norbert Hartl")
% add_slack_info1('U011HDST18D',team_id,"T010WMWBAGY")
% add_slack_info1('U011HDST18D',tz,"Europe/Amsterdam")
% add_slack_info1('U011HDST18D',tz_label,"Central European Summer Time")
% add_slack_info1('U011HDST18D',tz_offset,7200)
% add_slack_info1('U011HDST18D',updated,1586442777)
% add_slack_info1(var,instance,'U011HJ34T89')
% add_slack_info1('U011HJ34T89',color,"4ec0d6")
% add_slack_info1('U011HJ34T89',deleted,false)
% add_slack_info1('U011HJ34T89',id,"U011HJ34T89")
% add_slack_info1('U011HJ34T89',is_admin,false)
% add_slack_info1('U011HJ34T89',is_app_user,false)
% add_slack_info1('U011HJ34T89',is_bot,false)
% add_slack_info1('U011HJ34T89',is_owner,false)
% add_slack_info1('U011HJ34T89',is_primary_owner,false)
% add_slack_info1('U011HJ34T89',is_restricted,false)
% add_slack_info1('U011HJ34T89',is_ultra_restricted,false)
% add_slack_info1('U011HJ34T89',name,"ara")
% add_slack_info1('U011HJ34T89',presence,"away")
% add_slack_info1('U011HJ34T89',profile,_51620{avatar_hash:"gc62d1931b65",display_name:"Ara Hacopian",display_name_normalized:"Ara Hacopian",email:"ara@tehanu.net",fields:null,image_192:"https://secure.gravatar.com/avatar/c62d1931b6571728464c9a7ca7a99075.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-192.png",image_24:"https://secure.gravatar.com/avatar/c62d1931b6571728464c9a7ca7a99075.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-24.png",image_32:"https://secure.gravatar.com/avatar/c62d1931b6571728464c9a7ca7a99075.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-32.png",image_48:"https://secure.gravatar.com/avatar/c62d1931b6571728464c9a7ca7a99075.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-48.png",image_512:"https://secure.gravatar.com/avatar/c62d1931b6571728464c9a7ca7a99075.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-512.png",image_72:"https://secure.gravatar.com/avatar/c62d1931b6571728464c9a7ca7a99075.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-72.png",phone:"",real_name:"Ara Hacopian",real_name_normalized:"Ara Hacopian",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011HJ34T89',real_name,"Ara Hacopian")
% add_slack_info1('U011HJ34T89',team_id,"T010WMWBAGY")
% add_slack_info1('U011HJ34T89',tz,"Europe/Amsterdam")
% add_slack_info1('U011HJ34T89',tz_label,"Central European Summer Time")
% add_slack_info1('U011HJ34T89',tz_offset,7200)
% add_slack_info1('U011HJ34T89',updated,1586270077)
% add_slack_info1(var,instance,'U011HUGFX8R')
% add_slack_info1('U011HUGFX8R',color,"aba727")
% add_slack_info1('U011HUGFX8R',deleted,false)
% add_slack_info1('U011HUGFX8R',id,"U011HUGFX8R")
% add_slack_info1('U011HUGFX8R',is_admin,false)
% add_slack_info1('U011HUGFX8R',is_app_user,false)
% add_slack_info1('U011HUGFX8R',is_bot,false)
% add_slack_info1('U011HUGFX8R',is_owner,false)
% add_slack_info1('U011HUGFX8R',is_primary_owner,false)
% add_slack_info1('U011HUGFX8R',is_restricted,false)
% add_slack_info1('U011HUGFX8R',is_ultra_restricted,false)
% add_slack_info1('U011HUGFX8R',name,"adrian.arroyocalle")
% add_slack_info1('U011HUGFX8R',presence,"away")
% add_slack_info1('U011HUGFX8R',profile,_51794{avatar_hash:"b83c9855c884",display_name:"Adrián Arroyo Calle",display_name_normalized:"Adrian Arroyo Calle",email:"adrian.arroyocalle@gmail.com",fields:[],first_name:"Adrián",image_1024:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_original.jpg",is_custom_image:true,last_name:"Arroyo Calle",phone:"",real_name:"Adrián Arroyo Calle",real_name_normalized:"Adrian Arroyo Calle",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011HUGFX8R',real_name,"Adrián Arroyo Calle")
% add_slack_info1('U011HUGFX8R',team_id,"T010WMWBAGY")
% add_slack_info1('U011HUGFX8R',tz,"Europe/Amsterdam")
% add_slack_info1('U011HUGFX8R',tz_label,"Central European Summer Time")
% add_slack_info1('U011HUGFX8R',tz_offset,7200)
% add_slack_info1('U011HUGFX8R',updated,1586360287)
% add_slack_info1(var,instance,'U011J6E2TCM')
% add_slack_info1('U011J6E2TCM',color,"df3dc0")
% add_slack_info1('U011J6E2TCM',deleted,false)
% add_slack_info1('U011J6E2TCM',id,"U011J6E2TCM")
% add_slack_info1('U011J6E2TCM',is_admin,false)
% add_slack_info1('U011J6E2TCM',is_app_user,false)
% add_slack_info1('U011J6E2TCM',is_bot,false)
% add_slack_info1('U011J6E2TCM',is_owner,false)
% add_slack_info1('U011J6E2TCM',is_primary_owner,false)
% add_slack_info1('U011J6E2TCM',is_restricted,false)
% add_slack_info1('U011J6E2TCM',is_ultra_restricted,false)
% add_slack_info1('U011J6E2TCM',name,"ghoen49")
% add_slack_info1('U011J6E2TCM',presence,"away")
% add_slack_info1('U011J6E2TCM',profile,_51988{avatar_hash:"g0378c6e734c",display_name:"Ghoen",display_name_normalized:"Ghoen",email:"ghoen49@hotmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/0378c6e734ca66a0478066777870de3f.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-192.png",image_24:"https://secure.gravatar.com/avatar/0378c6e734ca66a0478066777870de3f.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-24.png",image_32:"https://secure.gravatar.com/avatar/0378c6e734ca66a0478066777870de3f.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-32.png",image_48:"https://secure.gravatar.com/avatar/0378c6e734ca66a0478066777870de3f.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-48.png",image_512:"https://secure.gravatar.com/avatar/0378c6e734ca66a0478066777870de3f.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-512.png",image_72:"https://secure.gravatar.com/avatar/0378c6e734ca66a0478066777870de3f.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-72.png",phone:"",real_name:"Ghoen",real_name_normalized:"Ghoen",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011J6E2TCM',real_name,"Ghoen")
% add_slack_info1('U011J6E2TCM',team_id,"T010WMWBAGY")
% add_slack_info1('U011J6E2TCM',tz,"Europe/Amsterdam")
% add_slack_info1('U011J6E2TCM',tz_label,"Central European Summer Time")
% add_slack_info1('U011J6E2TCM',tz_offset,7200)
% add_slack_info1('U011J6E2TCM',updated,1586335429)
% add_slack_info1(var,instance,'U011JBDQK1U')
% add_slack_info1('U011JBDQK1U',color,"235e5b")
% add_slack_info1('U011JBDQK1U',deleted,false)
% add_slack_info1('U011JBDQK1U',id,"U011JBDQK1U")
% add_slack_info1('U011JBDQK1U',is_admin,false)
% add_slack_info1('U011JBDQK1U',is_app_user,false)
% add_slack_info1('U011JBDQK1U',is_bot,false)
% add_slack_info1('U011JBDQK1U',is_owner,false)
% add_slack_info1('U011JBDQK1U',is_primary_owner,false)
% add_slack_info1('U011JBDQK1U',is_restricted,false)
% add_slack_info1('U011JBDQK1U',is_ultra_restricted,false)
% add_slack_info1('U011JBDQK1U',name,"jace42")
% add_slack_info1('U011JBDQK1U',presence,"away")
% add_slack_info1('U011JBDQK1U',profile,_52162{avatar_hash:"74118c3e8b0b",display_name:"jacob",display_name_normalized:"jacob",email:"jace42@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_original.jpg",is_custom_image:true,phone:"",real_name:"jacob",real_name_normalized:"jacob",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011JBDQK1U',real_name,"jacob")
% add_slack_info1('U011JBDQK1U',team_id,"T010WMWBAGY")
% add_slack_info1('U011JBDQK1U',tz,"America/Los_Angeles")
% add_slack_info1('U011JBDQK1U',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011JBDQK1U',tz_offset,-25200)
% add_slack_info1('U011JBDQK1U',updated,1586494257)
% add_slack_info1(var,instance,'U011JQEHLM7')
% add_slack_info1('U011JQEHLM7',color,"8469bc")
% add_slack_info1('U011JQEHLM7',deleted,false)
% add_slack_info1('U011JQEHLM7',id,"U011JQEHLM7")
% add_slack_info1('U011JQEHLM7',is_admin,false)
% add_slack_info1('U011JQEHLM7',is_app_user,false)
% add_slack_info1('U011JQEHLM7',is_bot,false)
% add_slack_info1('U011JQEHLM7',is_owner,false)
% add_slack_info1('U011JQEHLM7',is_primary_owner,false)
% add_slack_info1('U011JQEHLM7',is_restricted,false)
% add_slack_info1('U011JQEHLM7',is_ultra_restricted,false)
% add_slack_info1('U011JQEHLM7',name,"konjevic")
% add_slack_info1('U011JQEHLM7',presence,"away")
% add_slack_info1('U011JQEHLM7',profile,_52348{avatar_hash:"g9976060bfb2",display_name:"Mihael Konjevic",display_name_normalized:"Mihael Konjevic",email:"konjevic@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/9976060bfb26220dd042340394d06b31.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-192.png",image_24:"https://secure.gravatar.com/avatar/9976060bfb26220dd042340394d06b31.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-24.png",image_32:"https://secure.gravatar.com/avatar/9976060bfb26220dd042340394d06b31.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-32.png",image_48:"https://secure.gravatar.com/avatar/9976060bfb26220dd042340394d06b31.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-48.png",image_512:"https://secure.gravatar.com/avatar/9976060bfb26220dd042340394d06b31.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-512.png",image_72:"https://secure.gravatar.com/avatar/9976060bfb26220dd042340394d06b31.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-72.png",phone:"",real_name:"Mihael Konjevic",real_name_normalized:"Mihael Konjevic",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011JQEHLM7',real_name,"Mihael Konjevic")
% add_slack_info1('U011JQEHLM7',team_id,"T010WMWBAGY")
% add_slack_info1('U011JQEHLM7',tz,"Europe/Amsterdam")
% add_slack_info1('U011JQEHLM7',tz_label,"Central European Summer Time")
% add_slack_info1('U011JQEHLM7',tz_offset,7200)
% add_slack_info1('U011JQEHLM7',updated,1586285633)
% add_slack_info1(var,instance,'U011KM298TW')
% add_slack_info1('U011KM298TW',color,"3c989f")
% add_slack_info1('U011KM298TW',deleted,false)
% add_slack_info1('U011KM298TW',id,"U011KM298TW")
% add_slack_info1('U011KM298TW',is_admin,false)
% add_slack_info1('U011KM298TW',is_app_user,false)
% add_slack_info1('U011KM298TW',is_bot,false)
% add_slack_info1('U011KM298TW',is_owner,false)
% add_slack_info1('U011KM298TW',is_primary_owner,false)
% add_slack_info1('U011KM298TW',is_restricted,false)
% add_slack_info1('U011KM298TW',is_ultra_restricted,false)
% add_slack_info1('U011KM298TW',name,"pbh101")
% add_slack_info1('U011KM298TW',presence,"away")
% add_slack_info1('U011KM298TW',profile,_52522{avatar_hash:"109c92769476",display_name:"Patrick",display_name_normalized:"Patrick",email:"pbh101@gmail.com",fields:null,first_name:"Patrick",image_1024:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_1024.png",image_192:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_192.png",image_24:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_24.png",image_32:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_32.png",image_48:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_48.png",image_512:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_512.png",image_72:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_72.png",image_original:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_original.png",is_custom_image:true,last_name:"Braga-Henebry",phone:"",real_name:"Patrick Braga-Henebry",real_name_normalized:"Patrick Braga-Henebry",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011KM298TW',real_name,"Patrick Braga-Henebry")
% add_slack_info1('U011KM298TW',team_id,"T010WMWBAGY")
% add_slack_info1('U011KM298TW',tz,"America/Chicago")
% add_slack_info1('U011KM298TW',tz_label,"Central Daylight Time")
% add_slack_info1('U011KM298TW',tz_offset,-18000)
% add_slack_info1('U011KM298TW',updated,1586263046)
% add_slack_info1(var,instance,'U011KMR024U')
% add_slack_info1('U011KMR024U',color,"e0a729")
% add_slack_info1('U011KMR024U',deleted,false)
% add_slack_info1('U011KMR024U',id,"U011KMR024U")
% add_slack_info1('U011KMR024U',is_admin,false)
% add_slack_info1('U011KMR024U',is_app_user,false)
% add_slack_info1('U011KMR024U',is_bot,false)
% add_slack_info1('U011KMR024U',is_owner,false)
% add_slack_info1('U011KMR024U',is_primary_owner,false)
% add_slack_info1('U011KMR024U',is_restricted,false)
% add_slack_info1('U011KMR024U',is_ultra_restricted,false)
% add_slack_info1('U011KMR024U',name,"n.gagliani")
% add_slack_info1('U011KMR024U',presence,"away")
% add_slack_info1('U011KMR024U',profile,_52716{avatar_hash:"40680a4173a5",display_name:"",display_name_normalized:"",email:"n.gagliani@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_original.jpg",is_custom_image:true,phone:"",real_name:"Nicolas",real_name_normalized:"Nicolas",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011KMR024U',real_name,"Nicolas")
% add_slack_info1('U011KMR024U',team_id,"T010WMWBAGY")
% add_slack_info1('U011KMR024U',tz,"Europe/Amsterdam")
% add_slack_info1('U011KMR024U',tz_label,"Central European Summer Time")
% add_slack_info1('U011KMR024U',tz_offset,7200)
% add_slack_info1('U011KMR024U',updated,1586348986)
% add_slack_info1(var,instance,'U011KMTBGN8')
% add_slack_info1('U011KMTBGN8',color,"684b6c")
% add_slack_info1('U011KMTBGN8',deleted,false)
% add_slack_info1('U011KMTBGN8',id,"U011KMTBGN8")
% add_slack_info1('U011KMTBGN8',is_admin,false)
% add_slack_info1('U011KMTBGN8',is_app_user,false)
% add_slack_info1('U011KMTBGN8',is_bot,false)
% add_slack_info1('U011KMTBGN8',is_owner,false)
% add_slack_info1('U011KMTBGN8',is_primary_owner,false)
% add_slack_info1('U011KMTBGN8',is_restricted,false)
% add_slack_info1('U011KMTBGN8',is_ultra_restricted,false)
% add_slack_info1('U011KMTBGN8',name,"wernher.behrendt")
% add_slack_info1('U011KMTBGN8',presence,"away")
% add_slack_info1('U011KMTBGN8',profile,_52902{avatar_hash:"e36a7342e518",display_name:"Wernher",display_name_normalized:"Wernher",email:"wernher.behrendt@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_original.jpg",is_custom_image:true,phone:"",real_name:"Wernher",real_name_normalized:"Wernher",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011KMTBGN8',real_name,"Wernher")
% add_slack_info1('U011KMTBGN8',team_id,"T010WMWBAGY")
% add_slack_info1('U011KMTBGN8',tz,"Europe/Amsterdam")
% add_slack_info1('U011KMTBGN8',tz_label,"Central European Summer Time")
% add_slack_info1('U011KMTBGN8',tz_offset,7200)
% add_slack_info1('U011KMTBGN8',updated,1586267877)
% add_slack_info1(var,instance,'U011KT0C952')
% add_slack_info1('U011KT0C952',deleted,true)
% add_slack_info1('U011KT0C952',id,"U011KT0C952")
% add_slack_info1('U011KT0C952',is_app_user,false)
% add_slack_info1('U011KT0C952',is_bot,false)
% add_slack_info1('U011KT0C952',name,"alexander.shendi")
% add_slack_info1('U011KT0C952',presence,"away")
% add_slack_info1('U011KT0C952',profile,_53088{avatar_hash:"g0dda380b62b",display_name:"alexshendi",display_name_normalized:"alexshendi",email:"alexander.shendi@web.de",fields:null,first_name:"Alexander",image_192:"https://secure.gravatar.com/avatar/0dda380b62b9a6900bd8ffa56c6a1710.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-192.png",image_24:"https://secure.gravatar.com/avatar/0dda380b62b9a6900bd8ffa56c6a1710.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-24.png",image_32:"https://secure.gravatar.com/avatar/0dda380b62b9a6900bd8ffa56c6a1710.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-32.png",image_48:"https://secure.gravatar.com/avatar/0dda380b62b9a6900bd8ffa56c6a1710.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-48.png",image_512:"https://secure.gravatar.com/avatar/0dda380b62b9a6900bd8ffa56c6a1710.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-512.png",image_72:"https://secure.gravatar.com/avatar/0dda380b62b9a6900bd8ffa56c6a1710.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-72.png",last_name:"Shendi",phone:"",real_name:"Alexander Shendi",real_name_normalized:"Alexander Shendi",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011KT0C952',team_id,"T010WMWBAGY")
% add_slack_info1('U011KT0C952',updated,1586641780)
% add_slack_info1(var,instance,'U011KUZT45S')
% add_slack_info1('U011KUZT45S',color,"902d59")
% add_slack_info1('U011KUZT45S',deleted,false)
% add_slack_info1('U011KUZT45S',id,"U011KUZT45S")
% add_slack_info1('U011KUZT45S',is_admin,false)
% add_slack_info1('U011KUZT45S',is_app_user,false)
% add_slack_info1('U011KUZT45S',is_bot,false)
% add_slack_info1('U011KUZT45S',is_owner,false)
% add_slack_info1('U011KUZT45S',is_primary_owner,false)
% add_slack_info1('U011KUZT45S',is_restricted,false)
% add_slack_info1('U011KUZT45S',is_ultra_restricted,false)
% add_slack_info1('U011KUZT45S',name,"alex.kelley")
% add_slack_info1('U011KUZT45S',presence,"away")
% add_slack_info1('U011KUZT45S',profile,_53230{avatar_hash:"g6bce4d4273b",display_name:"",display_name_normalized:"",email:"alex.kelley@adkpartners.com",fields:null,image_192:"https://secure.gravatar.com/avatar/6bce4d4273be9134f2367e62f629265c.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-192.png",image_24:"https://secure.gravatar.com/avatar/6bce4d4273be9134f2367e62f629265c.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-24.png",image_32:"https://secure.gravatar.com/avatar/6bce4d4273be9134f2367e62f629265c.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-32.png",image_48:"https://secure.gravatar.com/avatar/6bce4d4273be9134f2367e62f629265c.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-48.png",image_512:"https://secure.gravatar.com/avatar/6bce4d4273be9134f2367e62f629265c.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-512.png",image_72:"https://secure.gravatar.com/avatar/6bce4d4273be9134f2367e62f629265c.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-72.png",phone:"",real_name:"Alex Kelley",real_name_normalized:"Alex Kelley",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011KUZT45S',real_name,"Alex Kelley")
% add_slack_info1('U011KUZT45S',team_id,"T010WMWBAGY")
% add_slack_info1('U011KUZT45S',tz,"America/Los_Angeles")
% add_slack_info1('U011KUZT45S',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011KUZT45S',tz_offset,-25200)
% add_slack_info1('U011KUZT45S',updated,1586267786)
% add_slack_info1(var,instance,'U011L2C91N0')
% add_slack_info1('U011L2C91N0',color,"8d4b84")
% add_slack_info1('U011L2C91N0',deleted,false)
% add_slack_info1('U011L2C91N0',id,"U011L2C91N0")
% add_slack_info1('U011L2C91N0',is_admin,false)
% add_slack_info1('U011L2C91N0',is_app_user,false)
% add_slack_info1('U011L2C91N0',is_bot,false)
% add_slack_info1('U011L2C91N0',is_owner,false)
% add_slack_info1('U011L2C91N0',is_primary_owner,false)
% add_slack_info1('U011L2C91N0',is_restricted,false)
% add_slack_info1('U011L2C91N0',is_ultra_restricted,false)
% add_slack_info1('U011L2C91N0',name,"olivier.mathieu314")
% add_slack_info1('U011L2C91N0',presence,"away")
% add_slack_info1('U011L2C91N0',profile,_53404{avatar_hash:"g9da71f110ae",display_name:"o314",display_name_normalized:"o314",email:"olivier.mathieu314@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/9da71f110ae187230e855a9a56c3d077.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-192.png",image_24:"https://secure.gravatar.com/avatar/9da71f110ae187230e855a9a56c3d077.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-24.png",image_32:"https://secure.gravatar.com/avatar/9da71f110ae187230e855a9a56c3d077.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-32.png",image_48:"https://secure.gravatar.com/avatar/9da71f110ae187230e855a9a56c3d077.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-48.png",image_512:"https://secure.gravatar.com/avatar/9da71f110ae187230e855a9a56c3d077.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-512.png",image_72:"https://secure.gravatar.com/avatar/9da71f110ae187230e855a9a56c3d077.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-72.png",phone:"",real_name:"o314",real_name_normalized:"o314",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011L2C91N0',real_name,"o314")
% add_slack_info1('U011L2C91N0',team_id,"T010WMWBAGY")
% add_slack_info1('U011L2C91N0',tz,"Europe/Brussels")
% add_slack_info1('U011L2C91N0',tz_label,"Central European Summer Time")
% add_slack_info1('U011L2C91N0',tz_offset,7200)
% add_slack_info1('U011L2C91N0',updated,1586269599)
% add_slack_info1(var,instance,'U011LBM8A7N')
% add_slack_info1('U011LBM8A7N',color,"dd8527")
% add_slack_info1('U011LBM8A7N',deleted,false)
% add_slack_info1('U011LBM8A7N',id,"U011LBM8A7N")
% add_slack_info1('U011LBM8A7N',is_admin,false)
% add_slack_info1('U011LBM8A7N',is_app_user,false)
% add_slack_info1('U011LBM8A7N',is_bot,false)
% add_slack_info1('U011LBM8A7N',is_owner,false)
% add_slack_info1('U011LBM8A7N',is_primary_owner,false)
% add_slack_info1('U011LBM8A7N',is_restricted,false)
% add_slack_info1('U011LBM8A7N',is_ultra_restricted,false)
% add_slack_info1('U011LBM8A7N',name,"evanwhackett")
% add_slack_info1('U011LBM8A7N',presence,"away")
% add_slack_info1('U011LBM8A7N',profile,_53578{avatar_hash:"gb79bcc93f12",display_name:"Evan Hackett",display_name_normalized:"Evan Hackett",email:"evanwhackett@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/b79bcc93f1242a3ef916f2ab0fcd1fc5.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-192.png",image_24:"https://secure.gravatar.com/avatar/b79bcc93f1242a3ef916f2ab0fcd1fc5.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-24.png",image_32:"https://secure.gravatar.com/avatar/b79bcc93f1242a3ef916f2ab0fcd1fc5.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-32.png",image_48:"https://secure.gravatar.com/avatar/b79bcc93f1242a3ef916f2ab0fcd1fc5.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-48.png",image_512:"https://secure.gravatar.com/avatar/b79bcc93f1242a3ef916f2ab0fcd1fc5.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-512.png",image_72:"https://secure.gravatar.com/avatar/b79bcc93f1242a3ef916f2ab0fcd1fc5.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-72.png",phone:"",real_name:"Evan Hackett",real_name_normalized:"Evan Hackett",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011LBM8A7N',real_name,"Evan Hackett")
% add_slack_info1('U011LBM8A7N',team_id,"T010WMWBAGY")
% add_slack_info1('U011LBM8A7N',tz,"America/Los_Angeles")
% add_slack_info1('U011LBM8A7N',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011LBM8A7N',tz_offset,-25200)
% add_slack_info1('U011LBM8A7N',updated,1586275601)
% add_slack_info1(var,instance,'U011LD9GRHA')
% add_slack_info1('U011LD9GRHA',color,"bd9336")
% add_slack_info1('U011LD9GRHA',deleted,false)
% add_slack_info1('U011LD9GRHA',id,"U011LD9GRHA")
% add_slack_info1('U011LD9GRHA',is_admin,false)
% add_slack_info1('U011LD9GRHA',is_app_user,false)
% add_slack_info1('U011LD9GRHA',is_bot,false)
% add_slack_info1('U011LD9GRHA',is_owner,false)
% add_slack_info1('U011LD9GRHA',is_primary_owner,false)
% add_slack_info1('U011LD9GRHA',is_restricted,false)
% add_slack_info1('U011LD9GRHA',is_ultra_restricted,false)
% add_slack_info1('U011LD9GRHA',name,"jason.rothfuss")
% add_slack_info1('U011LD9GRHA',presence,"away")
% add_slack_info1('U011LD9GRHA',profile,_53752{avatar_hash:"358cce1eb3b5",display_name:"Jason Rothfuss",display_name_normalized:"Jason Rothfuss",email:"jason.rothfuss@icloud.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_original.jpg",is_custom_image:true,phone:"",real_name:"Jason Rothfuss",real_name_normalized:"Jason Rothfuss",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011LD9GRHA',real_name,"Jason Rothfuss")
% add_slack_info1('U011LD9GRHA',team_id,"T010WMWBAGY")
% add_slack_info1('U011LD9GRHA',tz,"America/New_York")
% add_slack_info1('U011LD9GRHA',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011LD9GRHA',tz_offset,-14400)
% add_slack_info1('U011LD9GRHA',updated,1586281244)
% add_slack_info1(var,instance,'U011LFELS4R')
% add_slack_info1('U011LFELS4R',color,"5b89d5")
% add_slack_info1('U011LFELS4R',deleted,false)
% add_slack_info1('U011LFELS4R',id,"U011LFELS4R")
% add_slack_info1('U011LFELS4R',is_admin,false)
% add_slack_info1('U011LFELS4R',is_app_user,false)
% add_slack_info1('U011LFELS4R',is_bot,true)
% add_slack_info1('U011LFELS4R',is_owner,false)
% add_slack_info1('U011LFELS4R',is_primary_owner,false)
% add_slack_info1('U011LFELS4R',is_restricted,false)
% add_slack_info1('U011LFELS4R',is_ultra_restricted,false)
% add_slack_info1('U011LFELS4R',name,"googledrive")
% add_slack_info1('U011LFELS4R',presence,"away")
% add_slack_info1('U011LFELS4R',profile,_53938{always_active:true,api_app_id:"A6NL8MJ6Q",avatar_hash:"8ffce3229e29",bot_id:"B011LFELQKB",display_name:"",display_name_normalized:"",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_1024.png",image_192:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_192.png",image_24:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_24.png",image_32:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_32.png",image_48:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_48.png",image_512:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_512.png",image_72:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_72.png",image_original:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_original.png",is_custom_image:true,phone:"",real_name:"Google Drive",real_name_normalized:"Google Drive",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011LFELS4R',real_name,"Google Drive")
% add_slack_info1('U011LFELS4R',team_id,"T010WMWBAGY")
% add_slack_info1('U011LFELS4R',tz,"America/Los_Angeles")
% add_slack_info1('U011LFELS4R',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011LFELS4R',tz_offset,-25200)
% add_slack_info1('U011LFELS4R',updated,1586327495)
% add_slack_info1(var,instance,'U011M3W2WRJ')
% add_slack_info1('U011M3W2WRJ',color,"e7392d")
% add_slack_info1('U011M3W2WRJ',deleted,false)
% add_slack_info1('U011M3W2WRJ',id,"U011M3W2WRJ")
% add_slack_info1('U011M3W2WRJ',is_admin,false)
% add_slack_info1('U011M3W2WRJ',is_app_user,false)
% add_slack_info1('U011M3W2WRJ',is_bot,false)
% add_slack_info1('U011M3W2WRJ',is_owner,false)
% add_slack_info1('U011M3W2WRJ',is_primary_owner,false)
% add_slack_info1('U011M3W2WRJ',is_restricted,false)
% add_slack_info1('U011M3W2WRJ',is_ultra_restricted,false)
% add_slack_info1('U011M3W2WRJ',name,"terri")
% add_slack_info1('U011M3W2WRJ',presence,"away")
% add_slack_info1('U011M3W2WRJ',profile,_54132{avatar_hash:"eb3d07dc3d5c",display_name:"Terri Yu",display_name_normalized:"Terri Yu",email:"terri@terriyu.info",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_original.jpg",is_custom_image:true,phone:"",real_name:"Terri Yu",real_name_normalized:"Terri Yu",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011M3W2WRJ',real_name,"Terri Yu")
% add_slack_info1('U011M3W2WRJ',team_id,"T010WMWBAGY")
% add_slack_info1('U011M3W2WRJ',tz,"America/New_York")
% add_slack_info1('U011M3W2WRJ',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011M3W2WRJ',tz_offset,-14400)
% add_slack_info1('U011M3W2WRJ',updated,1586655163)
% add_slack_info1(var,instance,'U011MJN89SR')
% add_slack_info1('U011MJN89SR',color,"db3150")
% add_slack_info1('U011MJN89SR',deleted,false)
% add_slack_info1('U011MJN89SR',id,"U011MJN89SR")
% add_slack_info1('U011MJN89SR',is_admin,false)
% add_slack_info1('U011MJN89SR',is_app_user,false)
% add_slack_info1('U011MJN89SR',is_bot,false)
% add_slack_info1('U011MJN89SR',is_owner,false)
% add_slack_info1('U011MJN89SR',is_primary_owner,false)
% add_slack_info1('U011MJN89SR',is_restricted,false)
% add_slack_info1('U011MJN89SR',is_ultra_restricted,false)
% add_slack_info1('U011MJN89SR',name,"gfleetwood")
% add_slack_info1('U011MJN89SR',presence,"away")
% add_slack_info1('U011MJN89SR',profile,_54318{avatar_hash:"g91c06ec38a8",display_name:"gordonf",display_name_normalized:"gordonf",email:"gfleetwood@protonmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/91c06ec38a8978d595ccc3797ae090f2.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-192.png",image_24:"https://secure.gravatar.com/avatar/91c06ec38a8978d595ccc3797ae090f2.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-24.png",image_32:"https://secure.gravatar.com/avatar/91c06ec38a8978d595ccc3797ae090f2.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-32.png",image_48:"https://secure.gravatar.com/avatar/91c06ec38a8978d595ccc3797ae090f2.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-48.png",image_512:"https://secure.gravatar.com/avatar/91c06ec38a8978d595ccc3797ae090f2.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-512.png",image_72:"https://secure.gravatar.com/avatar/91c06ec38a8978d595ccc3797ae090f2.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-72.png",phone:"",real_name:"gordonf",real_name_normalized:"gordonf",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011MJN89SR',real_name,"gordonf")
% add_slack_info1('U011MJN89SR',team_id,"T010WMWBAGY")
% add_slack_info1('U011MJN89SR',tz,"America/New_York")
% add_slack_info1('U011MJN89SR',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011MJN89SR',tz_offset,-14400)
% add_slack_info1('U011MJN89SR',updated,1586471612)
% add_slack_info1(var,instance,'U011MMUG8M8')
% add_slack_info1('U011MMUG8M8',color,"5870dd")
% add_slack_info1('U011MMUG8M8',deleted,false)
% add_slack_info1('U011MMUG8M8',id,"U011MMUG8M8")
% add_slack_info1('U011MMUG8M8',is_admin,false)
% add_slack_info1('U011MMUG8M8',is_app_user,false)
% add_slack_info1('U011MMUG8M8',is_bot,false)
% add_slack_info1('U011MMUG8M8',is_owner,false)
% add_slack_info1('U011MMUG8M8',is_primary_owner,false)
% add_slack_info1('U011MMUG8M8',is_restricted,false)
% add_slack_info1('U011MMUG8M8',is_ultra_restricted,false)
% add_slack_info1('U011MMUG8M8',name,"prologclass.slack.apr")
% add_slack_info1('U011MMUG8M8',presence,"away")
% add_slack_info1('U011MMUG8M8',profile,_54492{avatar_hash:"g92e5a895938",display_name:"Eoin Groat",display_name_normalized:"Eoin Groat",email:"prologclass.slack.apr.2020@normal-gaussian.com",fields:null,image_192:"https://secure.gravatar.com/avatar/92e5a89593845e9615daebce7df45683.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-192.png",image_24:"https://secure.gravatar.com/avatar/92e5a89593845e9615daebce7df45683.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-24.png",image_32:"https://secure.gravatar.com/avatar/92e5a89593845e9615daebce7df45683.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-32.png",image_48:"https://secure.gravatar.com/avatar/92e5a89593845e9615daebce7df45683.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-48.png",image_512:"https://secure.gravatar.com/avatar/92e5a89593845e9615daebce7df45683.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-512.png",image_72:"https://secure.gravatar.com/avatar/92e5a89593845e9615daebce7df45683.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-72.png",phone:"",real_name:"Eoin Groat",real_name_normalized:"Eoin Groat",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011MMUG8M8',real_name,"Eoin Groat")
% add_slack_info1('U011MMUG8M8',team_id,"T010WMWBAGY")
% add_slack_info1('U011MMUG8M8',tz,"Europe/London")
% add_slack_info1('U011MMUG8M8',tz_label,"British Summer Time")
% add_slack_info1('U011MMUG8M8',tz_offset,3600)
% add_slack_info1('U011MMUG8M8',updated,1586620024)
% add_slack_info1(var,instance,'U011MQ9GB9R')
% add_slack_info1('U011MQ9GB9R',color,"c386df")
% add_slack_info1('U011MQ9GB9R',deleted,false)
% add_slack_info1('U011MQ9GB9R',id,"U011MQ9GB9R")
% add_slack_info1('U011MQ9GB9R',is_admin,false)
% add_slack_info1('U011MQ9GB9R',is_app_user,false)
% add_slack_info1('U011MQ9GB9R',is_bot,false)
% add_slack_info1('U011MQ9GB9R',is_owner,false)
% add_slack_info1('U011MQ9GB9R',is_primary_owner,false)
% add_slack_info1('U011MQ9GB9R',is_restricted,false)
% add_slack_info1('U011MQ9GB9R',is_ultra_restricted,false)
% add_slack_info1('U011MQ9GB9R',name,"ac.russell")
% add_slack_info1('U011MQ9GB9R',presence,"away")
% add_slack_info1('U011MQ9GB9R',profile,_54666{avatar_hash:"g8d1532e30b8",display_name:"Adam Russell",display_name_normalized:"Adam Russell",email:"ac.russell@live.com",fields:null,image_192:"https://secure.gravatar.com/avatar/8d1532e30b8e1aea9d9eb57d61eb4c55.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-192.png",image_24:"https://secure.gravatar.com/avatar/8d1532e30b8e1aea9d9eb57d61eb4c55.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-24.png",image_32:"https://secure.gravatar.com/avatar/8d1532e30b8e1aea9d9eb57d61eb4c55.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-32.png",image_48:"https://secure.gravatar.com/avatar/8d1532e30b8e1aea9d9eb57d61eb4c55.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-48.png",image_512:"https://secure.gravatar.com/avatar/8d1532e30b8e1aea9d9eb57d61eb4c55.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-512.png",image_72:"https://secure.gravatar.com/avatar/8d1532e30b8e1aea9d9eb57d61eb4c55.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-72.png",phone:"",real_name:"Adam Russell",real_name_normalized:"Adam Russell",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011MQ9GB9R',real_name,"Adam Russell")
% add_slack_info1('U011MQ9GB9R',team_id,"T010WMWBAGY")
% add_slack_info1('U011MQ9GB9R',tz,"America/New_York")
% add_slack_info1('U011MQ9GB9R',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011MQ9GB9R',tz_offset,-14400)
% add_slack_info1('U011MQ9GB9R',updated,1586554308)
% add_slack_info1(var,instance,'U011NFAKS77')
% add_slack_info1('U011NFAKS77',color,"9e3997")
% add_slack_info1('U011NFAKS77',deleted,false)
% add_slack_info1('U011NFAKS77',id,"U011NFAKS77")
% add_slack_info1('U011NFAKS77',is_admin,false)
% add_slack_info1('U011NFAKS77',is_app_user,false)
% add_slack_info1('U011NFAKS77',is_bot,false)
% add_slack_info1('U011NFAKS77',is_owner,false)
% add_slack_info1('U011NFAKS77',is_primary_owner,false)
% add_slack_info1('U011NFAKS77',is_restricted,false)
% add_slack_info1('U011NFAKS77',is_ultra_restricted,false)
% add_slack_info1('U011NFAKS77',name,"xavier.rs")
% add_slack_info1('U011NFAKS77',presence,"away")
% add_slack_info1('U011NFAKS77',profile,_54840{avatar_hash:"77adeb64a92d",display_name:"Xavier",display_name_normalized:"Xavier",email:"xavier.rs@fastmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_original.jpg",is_custom_image:true,phone:"",real_name:"Xavier",real_name_normalized:"Xavier",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011NFAKS77',real_name,"Xavier")
% add_slack_info1('U011NFAKS77',team_id,"T010WMWBAGY")
% add_slack_info1('U011NFAKS77',tz,"Europe/Amsterdam")
% add_slack_info1('U011NFAKS77',tz_label,"Central European Summer Time")
% add_slack_info1('U011NFAKS77',tz_offset,7200)
% add_slack_info1('U011NFAKS77',updated,1586535245)
% add_slack_info1(var,instance,'U011NGDBDLL')
% add_slack_info1('U011NGDBDLL',color,"4cc091")
% add_slack_info1('U011NGDBDLL',deleted,false)
% add_slack_info1('U011NGDBDLL',id,"U011NGDBDLL")
% add_slack_info1('U011NGDBDLL',is_admin,false)
% add_slack_info1('U011NGDBDLL',is_app_user,false)
% add_slack_info1('U011NGDBDLL',is_bot,false)
% add_slack_info1('U011NGDBDLL',is_owner,false)
% add_slack_info1('U011NGDBDLL',is_primary_owner,false)
% add_slack_info1('U011NGDBDLL',is_restricted,false)
% add_slack_info1('U011NGDBDLL',is_ultra_restricted,false)
% add_slack_info1('U011NGDBDLL',name,"gueletina.ksenia")
% add_slack_info1('U011NGDBDLL',presence,"away")
% add_slack_info1('U011NGDBDLL',profile,_55026{avatar_hash:"fe7767282f64",display_name:"",display_name_normalized:"",email:"gueletina.ksenia@gmail.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_1024.png",image_192:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_192.png",image_24:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_24.png",image_32:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_32.png",image_48:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_48.png",image_512:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_512.png",image_72:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_72.png",image_original:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_original.png",is_custom_image:true,phone:"",real_name:"Ksenia",real_name_normalized:"Ksenia",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011NGDBDLL',real_name,"Ksenia")
% add_slack_info1('U011NGDBDLL',team_id,"T010WMWBAGY")
% add_slack_info1('U011NGDBDLL',tz,"America/New_York")
% add_slack_info1('U011NGDBDLL',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011NGDBDLL',tz_offset,-14400)
% add_slack_info1('U011NGDBDLL',updated,1586369947)
% add_slack_info1(var,instance,'U011NN3462X')
% add_slack_info1('U011NN3462X',color,"50a0cf")
% add_slack_info1('U011NN3462X',deleted,false)
% add_slack_info1('U011NN3462X',id,"U011NN3462X")
% add_slack_info1('U011NN3462X',is_admin,false)
% add_slack_info1('U011NN3462X',is_app_user,false)
% add_slack_info1('U011NN3462X',is_bot,false)
% add_slack_info1('U011NN3462X',is_owner,false)
% add_slack_info1('U011NN3462X',is_primary_owner,false)
% add_slack_info1('U011NN3462X',is_restricted,false)
% add_slack_info1('U011NN3462X',is_ultra_restricted,false)
% add_slack_info1('U011NN3462X',name,"thorsten.blum")
% add_slack_info1('U011NN3462X',presence,"away")
% add_slack_info1('U011NN3462X',profile,_55212{avatar_hash:"gc757cd5e7f8",display_name:"Thorsten",display_name_normalized:"Thorsten",email:"thorsten.blum@toblux.com",fields:null,image_192:"https://secure.gravatar.com/avatar/c757cd5e7f83c3be0dd187f234efa8e6.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-192.png",image_24:"https://secure.gravatar.com/avatar/c757cd5e7f83c3be0dd187f234efa8e6.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-24.png",image_32:"https://secure.gravatar.com/avatar/c757cd5e7f83c3be0dd187f234efa8e6.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-32.png",image_48:"https://secure.gravatar.com/avatar/c757cd5e7f83c3be0dd187f234efa8e6.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-48.png",image_512:"https://secure.gravatar.com/avatar/c757cd5e7f83c3be0dd187f234efa8e6.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-512.png",image_72:"https://secure.gravatar.com/avatar/c757cd5e7f83c3be0dd187f234efa8e6.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-72.png",phone:"",real_name:"Thorsten",real_name_normalized:"Thorsten",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011NN3462X',real_name,"Thorsten")
% add_slack_info1('U011NN3462X',team_id,"T010WMWBAGY")
% add_slack_info1('U011NN3462X',tz,"Europe/Amsterdam")
% add_slack_info1('U011NN3462X',tz_label,"Central European Summer Time")
% add_slack_info1('U011NN3462X',tz_offset,7200)
% add_slack_info1('U011NN3462X',updated,1586635045)
% add_slack_info1(var,instance,'U011PF30S0M')
% add_slack_info1('U011PF30S0M',color,"ea2977")
% add_slack_info1('U011PF30S0M',deleted,false)
% add_slack_info1('U011PF30S0M',id,"U011PF30S0M")
% add_slack_info1('U011PF30S0M',is_admin,false)
% add_slack_info1('U011PF30S0M',is_app_user,false)
% add_slack_info1('U011PF30S0M',is_bot,false)
% add_slack_info1('U011PF30S0M',is_owner,false)
% add_slack_info1('U011PF30S0M',is_primary_owner,false)
% add_slack_info1('U011PF30S0M',is_restricted,false)
% add_slack_info1('U011PF30S0M',is_ultra_restricted,false)
% add_slack_info1('U011PF30S0M',name,"charlie.mac")
% add_slack_info1('U011PF30S0M',presence,"away")
% add_slack_info1('U011PF30S0M',profile,_55386{avatar_hash:"g9689c8e676c",display_name:"Charlie McMackin",display_name_normalized:"Charlie McMackin",email:"charlie.mac@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/9689c8e676c10e2b83062aa945c373e1.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-192.png",image_24:"https://secure.gravatar.com/avatar/9689c8e676c10e2b83062aa945c373e1.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-24.png",image_32:"https://secure.gravatar.com/avatar/9689c8e676c10e2b83062aa945c373e1.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-32.png",image_48:"https://secure.gravatar.com/avatar/9689c8e676c10e2b83062aa945c373e1.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-48.png",image_512:"https://secure.gravatar.com/avatar/9689c8e676c10e2b83062aa945c373e1.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-512.png",image_72:"https://secure.gravatar.com/avatar/9689c8e676c10e2b83062aa945c373e1.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-72.png",phone:"",real_name:"Charlie McMackin",real_name_normalized:"Charlie McMackin",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011PF30S0M',real_name,"Charlie McMackin")
% add_slack_info1('U011PF30S0M',team_id,"T010WMWBAGY")
% add_slack_info1('U011PF30S0M',tz,"America/Chicago")
% add_slack_info1('U011PF30S0M',tz_label,"Central Daylight Time")
% add_slack_info1('U011PF30S0M',tz_offset,-18000)
% add_slack_info1('U011PF30S0M',updated,1586632541)
% add_slack_info1(var,instance,'U011PGDJ07P')
% add_slack_info1('U011PGDJ07P',color,"d55aef")
% add_slack_info1('U011PGDJ07P',deleted,false)
% add_slack_info1('U011PGDJ07P',id,"U011PGDJ07P")
% add_slack_info1('U011PGDJ07P',is_admin,false)
% add_slack_info1('U011PGDJ07P',is_app_user,false)
% add_slack_info1('U011PGDJ07P',is_bot,true)
% add_slack_info1('U011PGDJ07P',is_owner,false)
% add_slack_info1('U011PGDJ07P',is_primary_owner,false)
% add_slack_info1('U011PGDJ07P',is_restricted,false)
% add_slack_info1('U011PGDJ07P',is_ultra_restricted,false)
% add_slack_info1('U011PGDJ07P',name,"prologclassbot")
% add_slack_info1('U011PGDJ07P',presence,"away")
% add_slack_info1('U011PGDJ07P',profile,_55560{always_active:false,api_app_id:"A011Z01HK0R",avatar_hash:"g019ffc4f191",bot_id:"B011PGDJ00M",display_name:"",display_name_normalized:"",fields:null,first_name:"OwlieBot",image_192:"https://secure.gravatar.com/avatar/019ffc4f191910bcd794f4546c14907c.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-192.png",image_24:"https://secure.gravatar.com/avatar/019ffc4f191910bcd794f4546c14907c.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-24.png",image_32:"https://secure.gravatar.com/avatar/019ffc4f191910bcd794f4546c14907c.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-32.png",image_48:"https://secure.gravatar.com/avatar/019ffc4f191910bcd794f4546c14907c.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-48.png",image_512:"https://secure.gravatar.com/avatar/019ffc4f191910bcd794f4546c14907c.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-512.png",image_72:"https://secure.gravatar.com/avatar/019ffc4f191910bcd794f4546c14907c.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-72.png",last_name:"",phone:"",real_name:"OwlieBot",real_name_normalized:"OwlieBot",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011PGDJ07P',real_name,"OwlieBot")
% add_slack_info1('U011PGDJ07P',team_id,"T010WMWBAGY")
% add_slack_info1('U011PGDJ07P',tz,"America/Los_Angeles")
% add_slack_info1('U011PGDJ07P',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011PGDJ07P',tz_offset,-25200)
% add_slack_info1('U011PGDJ07P',updated,1586659324)
% add_slack_info1(var,instance,'U011TBU3DA4')
% add_slack_info1('U011TBU3DA4',color,"df3dc0")
% add_slack_info1('U011TBU3DA4',deleted,false)
% add_slack_info1('U011TBU3DA4',id,"U011TBU3DA4")
% add_slack_info1('U011TBU3DA4',is_admin,false)
% add_slack_info1('U011TBU3DA4',is_app_user,false)
% add_slack_info1('U011TBU3DA4',is_bot,false)
% add_slack_info1('U011TBU3DA4',is_owner,false)
% add_slack_info1('U011TBU3DA4',is_primary_owner,false)
% add_slack_info1('U011TBU3DA4',is_restricted,false)
% add_slack_info1('U011TBU3DA4',is_ultra_restricted,false)
% add_slack_info1('U011TBU3DA4',name,"danielkrueger")
% add_slack_info1('U011TBU3DA4',presence,"away")
% add_slack_info1('U011TBU3DA4',profile,_55750{avatar_hash:"g4f499cd8654",display_name:"Daniel Krueger",display_name_normalized:"Daniel Krueger",email:"danielkrueger@protonmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/4f499cd8654e9f924a0c2804844aee32.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-192.png",image_24:"https://secure.gravatar.com/avatar/4f499cd8654e9f924a0c2804844aee32.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-24.png",image_32:"https://secure.gravatar.com/avatar/4f499cd8654e9f924a0c2804844aee32.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-32.png",image_48:"https://secure.gravatar.com/avatar/4f499cd8654e9f924a0c2804844aee32.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-48.png",image_512:"https://secure.gravatar.com/avatar/4f499cd8654e9f924a0c2804844aee32.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-512.png",image_72:"https://secure.gravatar.com/avatar/4f499cd8654e9f924a0c2804844aee32.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-72.png",phone:"",real_name:"Daniel Krueger",real_name_normalized:"Daniel Krueger",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011TBU3DA4',real_name,"Daniel Krueger")
% add_slack_info1('U011TBU3DA4',team_id,"T010WMWBAGY")
% add_slack_info1('U011TBU3DA4',tz,"Europe/Amsterdam")
% add_slack_info1('U011TBU3DA4',tz_label,"Central European Summer Time")
% add_slack_info1('U011TBU3DA4',tz_offset,7200)
% add_slack_info1('U011TBU3DA4',updated,1586262305)
% add_slack_info1(var,instance,'U011TBYESUQ')
% add_slack_info1('U011TBYESUQ',color,"4cc091")
% add_slack_info1('U011TBYESUQ',deleted,false)
% add_slack_info1('U011TBYESUQ',id,"U011TBYESUQ")
% add_slack_info1('U011TBYESUQ',is_admin,false)
% add_slack_info1('U011TBYESUQ',is_app_user,false)
% add_slack_info1('U011TBYESUQ',is_bot,false)
% add_slack_info1('U011TBYESUQ',is_owner,false)
% add_slack_info1('U011TBYESUQ',is_primary_owner,false)
% add_slack_info1('U011TBYESUQ',is_restricted,false)
% add_slack_info1('U011TBYESUQ',is_ultra_restricted,false)
% add_slack_info1('U011TBYESUQ',name,"vdamjanovic")
% add_slack_info1('U011TBYESUQ',presence,"away")
% add_slack_info1('U011TBYESUQ',profile,_55924{avatar_hash:"g0dbc3897b9a",display_name:"Violeta Damjanovic-Behrendt",display_name_normalized:"Violeta Damjanovic-Behrendt",email:"vdamjanovic@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/0dbc3897b9ad7a917fb68872e75661e3.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-192.png",image_24:"https://secure.gravatar.com/avatar/0dbc3897b9ad7a917fb68872e75661e3.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-24.png",image_32:"https://secure.gravatar.com/avatar/0dbc3897b9ad7a917fb68872e75661e3.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-32.png",image_48:"https://secure.gravatar.com/avatar/0dbc3897b9ad7a917fb68872e75661e3.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-48.png",image_512:"https://secure.gravatar.com/avatar/0dbc3897b9ad7a917fb68872e75661e3.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-512.png",image_72:"https://secure.gravatar.com/avatar/0dbc3897b9ad7a917fb68872e75661e3.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-72.png",phone:"",real_name:"Violeta Damjanovic-Behrendt",real_name_normalized:"Violeta Damjanovic-Behrendt",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011TBYESUQ',real_name,"Violeta Damjanovic-Behrendt")
% add_slack_info1('U011TBYESUQ',team_id,"T010WMWBAGY")
% add_slack_info1('U011TBYESUQ',tz,"Europe/Amsterdam")
% add_slack_info1('U011TBYESUQ',tz_label,"Central European Summer Time")
% add_slack_info1('U011TBYESUQ',tz_offset,7200)
% add_slack_info1('U011TBYESUQ',updated,1586262387)
% add_slack_info1(var,instance,'U011TKT3WJC')
% add_slack_info1('U011TKT3WJC',color,"5870dd")
% add_slack_info1('U011TKT3WJC',deleted,false)
% add_slack_info1('U011TKT3WJC',id,"U011TKT3WJC")
% add_slack_info1('U011TKT3WJC',is_admin,false)
% add_slack_info1('U011TKT3WJC',is_app_user,false)
% add_slack_info1('U011TKT3WJC',is_bot,false)
% add_slack_info1('U011TKT3WJC',is_owner,false)
% add_slack_info1('U011TKT3WJC',is_primary_owner,false)
% add_slack_info1('U011TKT3WJC',is_restricted,false)
% add_slack_info1('U011TKT3WJC',is_ultra_restricted,false)
% add_slack_info1('U011TKT3WJC',name,"ben.eyal.89")
% add_slack_info1('U011TKT3WJC',presence,"away")
% add_slack_info1('U011TKT3WJC',profile,_56098{avatar_hash:"g27acb697cc7",display_name:"Ben Eyal",display_name_normalized:"Ben Eyal",email:"ben.eyal.89@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/27acb697cc7dfdeb1c3dedd2b145609e.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-192.png",image_24:"https://secure.gravatar.com/avatar/27acb697cc7dfdeb1c3dedd2b145609e.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-24.png",image_32:"https://secure.gravatar.com/avatar/27acb697cc7dfdeb1c3dedd2b145609e.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-32.png",image_48:"https://secure.gravatar.com/avatar/27acb697cc7dfdeb1c3dedd2b145609e.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-48.png",image_512:"https://secure.gravatar.com/avatar/27acb697cc7dfdeb1c3dedd2b145609e.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-512.png",image_72:"https://secure.gravatar.com/avatar/27acb697cc7dfdeb1c3dedd2b145609e.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-72.png",phone:"",real_name:"Ben Eyal",real_name_normalized:"Ben Eyal",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011TKT3WJC',real_name,"Ben Eyal")
% add_slack_info1('U011TKT3WJC',team_id,"T010WMWBAGY")
% add_slack_info1('U011TKT3WJC',tz,"Asia/Jerusalem")
% add_slack_info1('U011TKT3WJC',tz_label,"Israel Daylight Time")
% add_slack_info1('U011TKT3WJC',tz_offset,10800)
% add_slack_info1('U011TKT3WJC',updated,1586266280)
% add_slack_info1(var,instance,'U011TQPPFJL')
% add_slack_info1('U011TQPPFJL',color,"de5f24")
% add_slack_info1('U011TQPPFJL',deleted,false)
% add_slack_info1('U011TQPPFJL',id,"U011TQPPFJL")
% add_slack_info1('U011TQPPFJL',is_admin,false)
% add_slack_info1('U011TQPPFJL',is_app_user,false)
% add_slack_info1('U011TQPPFJL',is_bot,false)
% add_slack_info1('U011TQPPFJL',is_owner,false)
% add_slack_info1('U011TQPPFJL',is_primary_owner,false)
% add_slack_info1('U011TQPPFJL',is_restricted,false)
% add_slack_info1('U011TQPPFJL',is_ultra_restricted,false)
% add_slack_info1('U011TQPPFJL',name,"evan")
% add_slack_info1('U011TQPPFJL',presence,"away")
% add_slack_info1('U011TQPPFJL',profile,_56272{avatar_hash:"g223daca478d",display_name:"evan",display_name_normalized:"evan",email:"evan@10000coins.com",fields:null,first_name:"Evan",image_192:"https://secure.gravatar.com/avatar/223daca478dc985e7d641e7bca08cfc3.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-192.png",image_24:"https://secure.gravatar.com/avatar/223daca478dc985e7d641e7bca08cfc3.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-24.png",image_32:"https://secure.gravatar.com/avatar/223daca478dc985e7d641e7bca08cfc3.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-32.png",image_48:"https://secure.gravatar.com/avatar/223daca478dc985e7d641e7bca08cfc3.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-48.png",image_512:"https://secure.gravatar.com/avatar/223daca478dc985e7d641e7bca08cfc3.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-512.png",image_72:"https://secure.gravatar.com/avatar/223daca478dc985e7d641e7bca08cfc3.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-72.png",last_name:"Pipho",phone:"",real_name:"Evan Pipho",real_name_normalized:"Evan Pipho",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011TQPPFJL',real_name,"Evan Pipho")
% add_slack_info1('U011TQPPFJL',team_id,"T010WMWBAGY")
% add_slack_info1('U011TQPPFJL',tz,"America/Los_Angeles")
% add_slack_info1('U011TQPPFJL',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011TQPPFJL',tz_offset,-25200)
% add_slack_info1('U011TQPPFJL',updated,1586268333)
% add_slack_info1(var,instance,'U011TS37NMN')
% add_slack_info1('U011TS37NMN',color,"827327")
% add_slack_info1('U011TS37NMN',deleted,false)
% add_slack_info1('U011TS37NMN',id,"U011TS37NMN")
% add_slack_info1('U011TS37NMN',is_admin,false)
% add_slack_info1('U011TS37NMN',is_app_user,false)
% add_slack_info1('U011TS37NMN',is_bot,false)
% add_slack_info1('U011TS37NMN',is_owner,false)
% add_slack_info1('U011TS37NMN',is_primary_owner,false)
% add_slack_info1('U011TS37NMN',is_restricted,false)
% add_slack_info1('U011TS37NMN',is_ultra_restricted,false)
% add_slack_info1('U011TS37NMN',name,"xjoke")
% add_slack_info1('U011TS37NMN',presence,"away")
% add_slack_info1('U011TS37NMN',profile,_56454{avatar_hash:"2f0564d1fb2a",display_name:"Jonas",display_name_normalized:"Jonas",email:"xjoke@me.com",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_original.jpg",is_custom_image:true,phone:"",real_name:"Jonas",real_name_normalized:"Jonas",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011TS37NMN',real_name,"Jonas")
% add_slack_info1('U011TS37NMN',team_id,"T010WMWBAGY")
% add_slack_info1('U011TS37NMN',tz,"Europe/Amsterdam")
% add_slack_info1('U011TS37NMN',tz_label,"Central European Summer Time")
% add_slack_info1('U011TS37NMN',tz_offset,7200)
% add_slack_info1('U011TS37NMN',updated,1586269352)
% add_slack_info1(var,instance,'U011UA961R6')
% add_slack_info1('U011UA961R6',color,"965d1b")
% add_slack_info1('U011UA961R6',deleted,false)
% add_slack_info1('U011UA961R6',id,"U011UA961R6")
% add_slack_info1('U011UA961R6',is_admin,false)
% add_slack_info1('U011UA961R6',is_app_user,false)
% add_slack_info1('U011UA961R6',is_bot,false)
% add_slack_info1('U011UA961R6',is_owner,false)
% add_slack_info1('U011UA961R6',is_primary_owner,false)
% add_slack_info1('U011UA961R6',is_restricted,false)
% add_slack_info1('U011UA961R6',is_ultra_restricted,false)
% add_slack_info1('U011UA961R6',name,"andrekw")
% add_slack_info1('U011UA961R6',presence,"away")
% add_slack_info1('U011UA961R6',profile,_56640{avatar_hash:"g0839f3eb2f6",display_name:"Andre",display_name_normalized:"Andre",email:"andrekw@gmail.com",fields:null,first_name:"Andre",image_192:"https://secure.gravatar.com/avatar/0839f3eb2f61f264c57a1626766eaa8a.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0008-192.png",image_24:"https://secure.gravatar.com/avatar/0839f3eb2f61f264c57a1626766eaa8a.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0008-24.png",image_32:"https://secure.gravatar.com/avatar/0839f3eb2f61f264c57a1626766eaa8a.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0008-32.png",image_48:"https://secure.gravatar.com/avatar/0839f3eb2f61f264c57a1626766eaa8a.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0008-48.png",image_512:"https://secure.gravatar.com/avatar/0839f3eb2f61f264c57a1626766eaa8a.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0008-512.png",image_72:"https://secure.gravatar.com/avatar/0839f3eb2f61f264c57a1626766eaa8a.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0008-72.png",last_name:"KW",phone:"",real_name:"Andre KW",real_name_normalized:"Andre KW",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:"Machine Learning"})
% add_slack_info1('U011UA961R6',real_name,"Andre KW")
% add_slack_info1('U011UA961R6',team_id,"T010WMWBAGY")
% add_slack_info1('U011UA961R6',tz,"Europe/Amsterdam")
% add_slack_info1('U011UA961R6',tz_label,"Central European Summer Time")
% add_slack_info1('U011UA961R6',tz_offset,7200)
% add_slack_info1('U011UA961R6',updated,1586274514)
% add_slack_info1(var,instance,'U011V30M664')
% add_slack_info1('U011V30M664',color,"bc3663")
% add_slack_info1('U011V30M664',deleted,false)
% add_slack_info1('U011V30M664',id,"U011V30M664")
% add_slack_info1('U011V30M664',is_admin,false)
% add_slack_info1('U011V30M664',is_app_user,false)
% add_slack_info1('U011V30M664',is_bot,false)
% add_slack_info1('U011V30M664',is_owner,false)
% add_slack_info1('U011V30M664',is_primary_owner,false)
% add_slack_info1('U011V30M664',is_restricted,false)
% add_slack_info1('U011V30M664',is_ultra_restricted,false)
% add_slack_info1('U011V30M664',name,"blutokyo")
% add_slack_info1('U011V30M664',presence,"away")
% add_slack_info1('U011V30M664',profile,_56822{avatar_hash:"geabb3a498ac",display_name:"curtosis",display_name_normalized:"curtosis",email:"blutokyo@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/eabb3a498acf7f72f7faff63a5be1029.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-192.png",image_24:"https://secure.gravatar.com/avatar/eabb3a498acf7f72f7faff63a5be1029.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-24.png",image_32:"https://secure.gravatar.com/avatar/eabb3a498acf7f72f7faff63a5be1029.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-32.png",image_48:"https://secure.gravatar.com/avatar/eabb3a498acf7f72f7faff63a5be1029.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-48.png",image_512:"https://secure.gravatar.com/avatar/eabb3a498acf7f72f7faff63a5be1029.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-512.png",image_72:"https://secure.gravatar.com/avatar/eabb3a498acf7f72f7faff63a5be1029.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-72.png",phone:"",real_name:"curtosis",real_name_normalized:"curtosis",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011V30M664',real_name,"curtosis")
% add_slack_info1('U011V30M664',team_id,"T010WMWBAGY")
% add_slack_info1('U011V30M664',tz,"America/New_York")
% add_slack_info1('U011V30M664',tz_label,"Eastern Daylight Time")
% add_slack_info1('U011V30M664',tz_offset,-14400)
% add_slack_info1('U011V30M664',updated,1586283942)
% add_slack_info1(var,instance,'U011VK86RS4')
% add_slack_info1('U011VK86RS4',color,"4bbe2e")
% add_slack_info1('U011VK86RS4',deleted,false)
% add_slack_info1('U011VK86RS4',id,"U011VK86RS4")
% add_slack_info1('U011VK86RS4',is_admin,false)
% add_slack_info1('U011VK86RS4',is_app_user,false)
% add_slack_info1('U011VK86RS4',is_bot,false)
% add_slack_info1('U011VK86RS4',is_owner,false)
% add_slack_info1('U011VK86RS4',is_primary_owner,false)
% add_slack_info1('U011VK86RS4',is_restricted,false)
% add_slack_info1('U011VK86RS4',is_ultra_restricted,false)
% add_slack_info1('U011VK86RS4',name,"andrzej")
% add_slack_info1('U011VK86RS4',presence,"away")
% add_slack_info1('U011VK86RS4',profile,_56996{avatar_hash:"6d89630a4e25",display_name:"Andrzej P",display_name_normalized:"Andrzej P",email:"andrzej@prochyra.name",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_original.jpg",is_custom_image:true,phone:"",real_name:"Andrzej P",real_name_normalized:"Andrzej P",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011VK86RS4',real_name,"Andrzej P")
% add_slack_info1('U011VK86RS4',team_id,"T010WMWBAGY")
% add_slack_info1('U011VK86RS4',tz,"Europe/London")
% add_slack_info1('U011VK86RS4',tz_label,"British Summer Time")
% add_slack_info1('U011VK86RS4',tz_offset,3600)
% add_slack_info1('U011VK86RS4',updated,1586292231)
% add_slack_info1(var,instance,'U011VNH0SQ4')
% add_slack_info1('U011VNH0SQ4',color,"d1707d")
% add_slack_info1('U011VNH0SQ4',deleted,false)
% add_slack_info1('U011VNH0SQ4',id,"U011VNH0SQ4")
% add_slack_info1('U011VNH0SQ4',is_admin,false)
% add_slack_info1('U011VNH0SQ4',is_app_user,false)
% add_slack_info1('U011VNH0SQ4',is_bot,false)
% add_slack_info1('U011VNH0SQ4',is_owner,false)
% add_slack_info1('U011VNH0SQ4',is_primary_owner,false)
% add_slack_info1('U011VNH0SQ4',is_restricted,false)
% add_slack_info1('U011VNH0SQ4',is_ultra_restricted,false)
% add_slack_info1('U011VNH0SQ4',name,"adamjwolf")
% add_slack_info1('U011VNH0SQ4',presence,"away")
% add_slack_info1('U011VNH0SQ4',profile,_57182{avatar_hash:"09b72d9c4dbb",display_name:"Adam J Wolf",display_name_normalized:"Adam J Wolf",email:"adamjwolf@me.com",fields:[],first_name:"Adam",image_1024:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_original.jpg",is_custom_image:true,last_name:"J Wolf",phone:"",real_name:"Adam J Wolf",real_name_normalized:"Adam J Wolf",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:"Principle Engineer @Emailage"})
% add_slack_info1('U011VNH0SQ4',real_name,"Adam J Wolf")
% add_slack_info1('U011VNH0SQ4',team_id,"T010WMWBAGY")
% add_slack_info1('U011VNH0SQ4',tz,"America/Chicago")
% add_slack_info1('U011VNH0SQ4',tz_label,"Central Daylight Time")
% add_slack_info1('U011VNH0SQ4',tz_offset,-18000)
% add_slack_info1('U011VNH0SQ4',updated,1586649360)
% add_slack_info1(var,instance,'U011Y5Z45FB')
% add_slack_info1('U011Y5Z45FB',color,"a63024")
% add_slack_info1('U011Y5Z45FB',deleted,false)
% add_slack_info1('U011Y5Z45FB',id,"U011Y5Z45FB")
% add_slack_info1('U011Y5Z45FB',is_admin,false)
% add_slack_info1('U011Y5Z45FB',is_app_user,false)
% add_slack_info1('U011Y5Z45FB',is_bot,false)
% add_slack_info1('U011Y5Z45FB',is_owner,false)
% add_slack_info1('U011Y5Z45FB',is_primary_owner,false)
% add_slack_info1('U011Y5Z45FB',is_restricted,false)
% add_slack_info1('U011Y5Z45FB',is_ultra_restricted,false)
% add_slack_info1('U011Y5Z45FB',name,"swiclass.frijas")
% add_slack_info1('U011Y5Z45FB',presence,"away")
% add_slack_info1('U011Y5Z45FB',profile,_57376{avatar_hash:"1b23c8c1402b",display_name:"Gerardo Sanchez",display_name_normalized:"Gerardo Sanchez",email:"swiclass.frijas@xoxy.net",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_1024.jpg",image_192:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_192.jpg",image_24:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_24.jpg",image_32:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_32.jpg",image_48:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_48.jpg",image_512:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_512.jpg",image_72:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_72.jpg",image_original:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_original.jpg",is_custom_image:true,phone:"",real_name:"Gerardo Sanchez",real_name_normalized:"Gerardo Sanchez",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011Y5Z45FB',real_name,"Gerardo Sanchez")
% add_slack_info1('U011Y5Z45FB',team_id,"T010WMWBAGY")
% add_slack_info1('U011Y5Z45FB',tz,"America/Los_Angeles")
% add_slack_info1('U011Y5Z45FB',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011Y5Z45FB',tz_offset,-25200)
% add_slack_info1('U011Y5Z45FB',updated,1586576517)
% add_slack_info1(var,instance,'U011Z6B31UH')
% add_slack_info1('U011Z6B31UH',color,"43761b")
% add_slack_info1('U011Z6B31UH',deleted,false)
% add_slack_info1('U011Z6B31UH',id,"U011Z6B31UH")
% add_slack_info1('U011Z6B31UH',is_admin,false)
% add_slack_info1('U011Z6B31UH',is_app_user,false)
% add_slack_info1('U011Z6B31UH',is_bot,false)
% add_slack_info1('U011Z6B31UH',is_owner,false)
% add_slack_info1('U011Z6B31UH',is_primary_owner,false)
% add_slack_info1('U011Z6B31UH',is_restricted,false)
% add_slack_info1('U011Z6B31UH',is_ultra_restricted,false)
% add_slack_info1('U011Z6B31UH',name,"logicmoo")
% add_slack_info1('U011Z6B31UH',presence,"away")
% add_slack_info1('U011Z6B31UH',profile,_57562{avatar_hash:"g7d3b9ee1332",display_name:"DouglasRMiles",display_name_normalized:"DouglasRMiles",email:"logicmoo@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/7d3b9ee133249361afc2b73eb4639038.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-192.png",image_24:"https://secure.gravatar.com/avatar/7d3b9ee133249361afc2b73eb4639038.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-24.png",image_32:"https://secure.gravatar.com/avatar/7d3b9ee133249361afc2b73eb4639038.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-32.png",image_48:"https://secure.gravatar.com/avatar/7d3b9ee133249361afc2b73eb4639038.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-48.png",image_512:"https://secure.gravatar.com/avatar/7d3b9ee133249361afc2b73eb4639038.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-512.png",image_72:"https://secure.gravatar.com/avatar/7d3b9ee133249361afc2b73eb4639038.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-72.png",phone:"",real_name:"DouglasRMiles",real_name_normalized:"DouglasRMiles",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U011Z6B31UH',real_name,"DouglasRMiles")
% add_slack_info1('U011Z6B31UH',team_id,"T010WMWBAGY")
% add_slack_info1('U011Z6B31UH',tz,"America/Los_Angeles")
% add_slack_info1('U011Z6B31UH',tz_label,"Pacific Daylight Time")
% add_slack_info1('U011Z6B31UH',tz_offset,-25200)
% add_slack_info1('U011Z6B31UH',updated,1586663823)
% add_slack_info1(var,instance,'U01201RSD8Q')
% add_slack_info1('U01201RSD8Q',color,"d58247")
% add_slack_info1('U01201RSD8Q',deleted,false)
% add_slack_info1('U01201RSD8Q',id,"U01201RSD8Q")
% add_slack_info1('U01201RSD8Q',is_admin,false)
% add_slack_info1('U01201RSD8Q',is_app_user,false)
% add_slack_info1('U01201RSD8Q',is_bot,false)
% add_slack_info1('U01201RSD8Q',is_owner,false)
% add_slack_info1('U01201RSD8Q',is_primary_owner,false)
% add_slack_info1('U01201RSD8Q',is_restricted,false)
% add_slack_info1('U01201RSD8Q',is_ultra_restricted,false)
% add_slack_info1('U01201RSD8Q',name,"steven.b.stone")
% add_slack_info1('U01201RSD8Q',presence,"away")
% add_slack_info1('U01201RSD8Q',profile,_57736{avatar_hash:"g07e32717609",display_name:"Steve Stone",display_name_normalized:"Steve Stone",email:"steven.b.stone@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/07e32717609b977581d0fe71682f61c3.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-192.png",image_24:"https://secure.gravatar.com/avatar/07e32717609b977581d0fe71682f61c3.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-24.png",image_32:"https://secure.gravatar.com/avatar/07e32717609b977581d0fe71682f61c3.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-32.png",image_48:"https://secure.gravatar.com/avatar/07e32717609b977581d0fe71682f61c3.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-48.png",image_512:"https://secure.gravatar.com/avatar/07e32717609b977581d0fe71682f61c3.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-512.png",image_72:"https://secure.gravatar.com/avatar/07e32717609b977581d0fe71682f61c3.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-72.png",phone:"",real_name:"Steve Stone",real_name_normalized:"Steve Stone",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U01201RSD8Q',real_name,"Steve Stone")
% add_slack_info1('U01201RSD8Q',team_id,"T010WMWBAGY")
% add_slack_info1('U01201RSD8Q',tz,"America/New_York")
% add_slack_info1('U01201RSD8Q',tz_label,"Eastern Daylight Time")
% add_slack_info1('U01201RSD8Q',tz_offset,-14400)
% add_slack_info1('U01201RSD8Q',updated,1586374264)
% add_slack_info1(var,instance,'U0127DHKV4G')
% add_slack_info1('U0127DHKV4G',color,"53b759")
% add_slack_info1('U0127DHKV4G',deleted,false)
% add_slack_info1('U0127DHKV4G',id,"U0127DHKV4G")
% add_slack_info1('U0127DHKV4G',is_admin,false)
% add_slack_info1('U0127DHKV4G',is_app_user,false)
% add_slack_info1('U0127DHKV4G',is_bot,false)
% add_slack_info1('U0127DHKV4G',is_owner,false)
% add_slack_info1('U0127DHKV4G',is_primary_owner,false)
% add_slack_info1('U0127DHKV4G',is_restricted,false)
% add_slack_info1('U0127DHKV4G',is_ultra_restricted,false)
% add_slack_info1('U0127DHKV4G',name,"josephtaber")
% add_slack_info1('U0127DHKV4G',presence,"away")
% add_slack_info1('U0127DHKV4G',profile,_57910{avatar_hash:"g01407b47e57",display_name:"Joe Taber",display_name_normalized:"Joe Taber",email:"josephtaber@gmail.com",fields:null,image_192:"https://secure.gravatar.com/avatar/01407b47e57ece60bf0220d6ba0b521c.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-192.png",image_24:"https://secure.gravatar.com/avatar/01407b47e57ece60bf0220d6ba0b521c.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-24.png",image_32:"https://secure.gravatar.com/avatar/01407b47e57ece60bf0220d6ba0b521c.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-32.png",image_48:"https://secure.gravatar.com/avatar/01407b47e57ece60bf0220d6ba0b521c.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-48.png",image_512:"https://secure.gravatar.com/avatar/01407b47e57ece60bf0220d6ba0b521c.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-512.png",image_72:"https://secure.gravatar.com/avatar/01407b47e57ece60bf0220d6ba0b521c.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-72.png",phone:"",real_name:"Joe Taber",real_name_normalized:"Joe Taber",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U0127DHKV4G',real_name,"Joe Taber")
% add_slack_info1('U0127DHKV4G',team_id,"T010WMWBAGY")
% add_slack_info1('U0127DHKV4G',tz,"America/Chicago")
% add_slack_info1('U0127DHKV4G',tz_label,"Central Daylight Time")
% add_slack_info1('U0127DHKV4G',tz_offset,-18000)
% add_slack_info1('U0127DHKV4G',updated,1586537358)
% add_slack_info1(var,instance,'U0128BERRAL')
% add_slack_info1('U0128BERRAL',color,"385a86")
% add_slack_info1('U0128BERRAL',deleted,false)
% add_slack_info1('U0128BERRAL',id,"U0128BERRAL")
% add_slack_info1('U0128BERRAL',is_admin,false)
% add_slack_info1('U0128BERRAL',is_app_user,false)
% add_slack_info1('U0128BERRAL',is_bot,false)
% add_slack_info1('U0128BERRAL',is_owner,false)
% add_slack_info1('U0128BERRAL',is_primary_owner,false)
% add_slack_info1('U0128BERRAL',is_restricted,false)
% add_slack_info1('U0128BERRAL',is_ultra_restricted,false)
% add_slack_info1('U0128BERRAL',name,"anvilforwork")
% add_slack_info1('U0128BERRAL',presence,"away")
% add_slack_info1('U0128BERRAL',profile,_58084{avatar_hash:"g90b5fa0ea32",display_name:"Josh Lefley",display_name_normalized:"Josh Lefley",email:"anvilforwork@gmail.com",fields:[],first_name:"Josh",image_192:"https://secure.gravatar.com/avatar/90b5fa0ea32490444e215b0b535c1a7d.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-192.png",image_24:"https://secure.gravatar.com/avatar/90b5fa0ea32490444e215b0b535c1a7d.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-24.png",image_32:"https://secure.gravatar.com/avatar/90b5fa0ea32490444e215b0b535c1a7d.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-32.png",image_48:"https://secure.gravatar.com/avatar/90b5fa0ea32490444e215b0b535c1a7d.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-48.png",image_512:"https://secure.gravatar.com/avatar/90b5fa0ea32490444e215b0b535c1a7d.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-512.png",image_72:"https://secure.gravatar.com/avatar/90b5fa0ea32490444e215b0b535c1a7d.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-72.png",last_name:"Lefley",phone:"",real_name:"Josh Lefley",real_name_normalized:"Josh Lefley",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('U0128BERRAL',real_name,"Josh Lefley")
% add_slack_info1('U0128BERRAL',team_id,"T010WMWBAGY")
% add_slack_info1('U0128BERRAL',tz,"America/Chicago")
% add_slack_info1('U0128BERRAL',tz_label,"Central Daylight Time")
% add_slack_info1('U0128BERRAL',tz_offset,-18000)
% add_slack_info1('U0128BERRAL',updated,1586556704)
% add_slack_info1(var,instance,'U0129P82V6U')
% add_slack_info1('U0129P82V6U',color,"e06b56")
% add_slack_info1('U0129P82V6U',deleted,false)
% add_slack_info1('U0129P82V6U',id,"U0129P82V6U")
% add_slack_info1('U0129P82V6U',is_admin,false)
% add_slack_info1('U0129P82V6U',is_app_user,false)
% add_slack_info1('U0129P82V6U',is_bot,true)
% add_slack_info1('U0129P82V6U',is_owner,false)
% add_slack_info1('U0129P82V6U',is_primary_owner,false)
% add_slack_info1('U0129P82V6U',is_restricted,false)
% add_slack_info1('U0129P82V6U',is_ultra_restricted,false)
% add_slack_info1('U0129P82V6U',name,"prolog_bot")
% add_slack_info1('U0129P82V6U',presence,"away")
% add_slack_info1('U0129P82V6U',profile,_58266{always_active:false,api_app_id:"A00",avatar_hash:"6919abfbc0fe",bot_id:"B0129P82UTS",display_name:"",display_name_normalized:"",fields:null,image_1024:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_1024.gif",image_192:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_192.gif",image_24:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_24.gif",image_32:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_32.gif",image_48:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_48.gif",image_512:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_512.gif",image_72:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_72.gif",image_original:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_original.gif",is_custom_image:true,phone:"",real_name:"prolog_bot",real_name_normalized:"prolog_bot",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:"This bot handles messages to and from slack such as evaluation of prolog code"})
% add_slack_info1('U0129P82V6U',real_name,"prolog_bot")
% add_slack_info1('U0129P82V6U',team_id,"T010WMWBAGY")
% add_slack_info1('U0129P82V6U',tz,"America/Los_Angeles")
% add_slack_info1('U0129P82V6U',tz_label,"Pacific Daylight Time")
% add_slack_info1('U0129P82V6U',tz_offset,-25200)
% add_slack_info1('U0129P82V6U',updated,1586665957)
% add_slack_info1(var,instance,'USLACKBOT')
% add_slack_info1('USLACKBOT',color,"757575")
% add_slack_info1('USLACKBOT',deleted,false)
% add_slack_info1('USLACKBOT',id,"USLACKBOT")
% add_slack_info1('USLACKBOT',is_admin,false)
% add_slack_info1('USLACKBOT',is_app_user,false)
% add_slack_info1('USLACKBOT',is_bot,false)
% add_slack_info1('USLACKBOT',is_owner,false)
% add_slack_info1('USLACKBOT',is_primary_owner,false)
% add_slack_info1('USLACKBOT',is_restricted,false)
% add_slack_info1('USLACKBOT',is_ultra_restricted,false)
% add_slack_info1('USLACKBOT',name,"slackbot")
% add_slack_info1('USLACKBOT',presence,"active")
% add_slack_info1('USLACKBOT',profile,_58460{always_active:true,avatar_hash:"sv41d8cd98f0",display_name:"Slackbot",display_name_normalized:"Slackbot",fields:null,first_name:"slackbot",image_192:"https://a.slack-edge.com/80588/marketing/img/avatars/slackbot/avatar-slackbot.png",image_24:"https://a.slack-edge.com/80588/img/slackbot_24.png",image_32:"https://a.slack-edge.com/80588/img/slackbot_32.png",image_48:"https://a.slack-edge.com/80588/img/slackbot_48.png",image_512:"https://a.slack-edge.com/80588/img/slackbot_512.png",image_72:"https://a.slack-edge.com/80588/img/slackbot_72.png",last_name:"",phone:"",real_name:"Slackbot",real_name_normalized:"Slackbot",skype:"",status_emoji:"",status_expiration:0,status_text:"",status_text_canonical:"",team:"T010WMWBAGY",title:""})
% add_slack_info1('USLACKBOT',real_name,"Slackbot")
% add_slack_info1('USLACKBOT',team_id,"T010WMWBAGY")
% add_slack_info1('USLACKBOT',tz,null)
% add_slack_info1('USLACKBOT',tz_label,"Pacific Daylight Time")
% add_slack_info1('USLACKBOT',tz_offset,-25200)
% add_slack_info1('USLACKBOT',updated,0)
:- dynamic slack_info/3.

slack_info(var, accept_tos_url, "https://prologclassworkspace.slack.com/account/tos").
slack_info(var, instance, 'B011BQ6E60P').
slack_info('B011BQ6E60P', app_id, "A6NL8MJ6Q").
slack_info('B011BQ6E60P', deleted, false).
slack_info('B011BQ6E60P', icons, _{image_36:"https://a.slack-edge.com/80588/img/services/gdrive_36.png", image_48:"https://a.slack-edge.com/80588/img/plugins/gdrive/service_48.png", image_72:"https://a.slack-edge.com/80588/img/plugins/gdrive/service_72.png"}).
slack_info('B011BQ6E60P', id, "B011BQ6E60P").
slack_info('B011BQ6E60P', name, "Google Drive").
slack_info('B011BQ6E60P', updated, 1586327475).
slack_info(var, instance, 'B011CBBP8P9').
slack_info('B011CBBP8P9', app_id, "A02").
slack_info('B011CBBP8P9', deleted, false).
slack_info('B011CBBP8P9', icons, _{image_36:"https://a.slack-edge.com/80588/img/plugins/app/bot_36.png", image_48:"https://a.slack-edge.com/80588/img/plugins/app/bot_48.png", image_72:"https://a.slack-edge.com/80588/img/plugins/app/service_72.png"}).
slack_info('B011CBBP8P9', id, "B011CBBP8P9").
slack_info('B011CBBP8P9', name, "Slack API Tester").
slack_info('B011CBBP8P9', updated, 1586485210).
slack_info(var, instance, 'B011CKJMJQP').
slack_info('B011CKJMJQP', app_id, "A0F7VRG6Q").
slack_info('B011CKJMJQP', deleted, false).
slack_info('B011CKJMJQP', icons, _{image_36:"https://a.slack-edge.com/80588/img/services/outgoing-webhook_36.png", image_48:"https://a.slack-edge.com/80588/img/services/outgoing-webhook_48.png", image_72:"https://a.slack-edge.com/80588/img/services/outgoing-webhook_72.png"}).
slack_info('B011CKJMJQP', id, "B011CKJMJQP").
slack_info('B011CKJMJQP', name, "outgoing-webhook").
slack_info('B011CKJMJQP', updated, 1586664716).
slack_info(var, instance, 'B011LFELPJM').
slack_info('B011LFELPJM', app_id, "A6NL8MJ6Q").
slack_info('B011LFELPJM', deleted, false).
slack_info('B011LFELPJM', icons, _{image_36:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_36.png", image_48:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_48.png", image_72:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_72.png"}).
slack_info('B011LFELPJM', id, "B011LFELPJM").
slack_info('B011LFELPJM', name, "Google Drive").
slack_info('B011LFELPJM', updated, 1586327493).
slack_info(var, instance, 'B011LFELQKB').
slack_info('B011LFELQKB', app_id, "A6NL8MJ6Q").
slack_info('B011LFELQKB', deleted, false).
slack_info('B011LFELQKB', icons, _{image_36:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_36.png", image_48:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_48.png", image_72:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_72.png"}).
slack_info('B011LFELQKB', id, "B011LFELQKB").
slack_info('B011LFELQKB', name, "Google Drive").
slack_info('B011LFELQKB', updated, 1586327493).
slack_info(var, instance, 'B011P80BDQD').
slack_info('B011P80BDQD', app_id, "A6NL8MJ6Q").
slack_info('B011P80BDQD', deleted, false).
slack_info('B011P80BDQD', icons, _{image_36:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_36.png", image_48:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_48.png", image_72:"https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-08-28/232381175025_31793c43d684e5a7c75a_72.png"}).
slack_info('B011P80BDQD', id, "B011P80BDQD").
slack_info('B011P80BDQD', name, "Google Drive").
slack_info('B011P80BDQD', updated, 1586598678).
slack_info(var, instance, 'B011PGDHZPF').
slack_info('B011PGDHZPF', app_id, "A011Z01HK0R").
slack_info('B011PGDHZPF', deleted, false).
slack_info('B011PGDHZPF', icons, _{image_36:"https://a.slack-edge.com/80588/img/plugins/app/bot_36.png", image_48:"https://a.slack-edge.com/80588/img/plugins/app/bot_48.png", image_72:"https://a.slack-edge.com/80588/img/plugins/app/service_72.png"}).
slack_info('B011PGDHZPF', id, "B011PGDHZPF").
slack_info('B011PGDHZPF', name, "OwlieBot").
slack_info('B011PGDHZPF', updated, 1586659323).
slack_info(var, instance, 'B011PGDHZS9').
slack_info('B011PGDHZS9', app_id, "A011Z01HK0R").
slack_info('B011PGDHZS9', deleted, false).
slack_info('B011PGDHZS9', icons, _{image_36:"https://a.slack-edge.com/80588/img/plugins/app/bot_36.png", image_48:"https://a.slack-edge.com/80588/img/plugins/app/bot_48.png", image_72:"https://a.slack-edge.com/80588/img/plugins/app/service_72.png"}).
slack_info('B011PGDHZS9', id, "B011PGDHZS9").
slack_info('B011PGDHZS9', name, "OwlieBot").
slack_info('B011PGDHZS9', updated, 1586659323).
slack_info(var, instance, 'B011PGDJ00M').
slack_info('B011PGDJ00M', app_id, "A011Z01HK0R").
slack_info('B011PGDJ00M', deleted, false).
slack_info('B011PGDJ00M', icons, _{image_36:"https://a.slack-edge.com/80588/img/plugins/app/bot_36.png", image_48:"https://a.slack-edge.com/80588/img/plugins/app/bot_48.png", image_72:"https://a.slack-edge.com/80588/img/plugins/app/service_72.png"}).
slack_info('B011PGDJ00M', id, "B011PGDJ00M").
slack_info('B011PGDJ00M', name, "OwlieBot").
slack_info('B011PGDJ00M', updated, 1586659323).
slack_info(var, instance, 'B0129NYJ5NC').
slack_info('B0129NYJ5NC', app_id, "A0F7XDUAZ").
slack_info('B0129NYJ5NC', deleted, false).
slack_info('B0129NYJ5NC', icons, _{image_36:"https://a.slack-edge.com/80588/img/services/outgoing-webhook_36.png", image_48:"https://a.slack-edge.com/80588/img/services/outgoing-webhook_48.png", image_72:"https://a.slack-edge.com/80588/img/services/outgoing-webhook_72.png"}).
slack_info('B0129NYJ5NC', id, "B0129NYJ5NC").
slack_info('B0129NYJ5NC', name, "incoming-webhook").
slack_info('B0129NYJ5NC', updated, 1586664339).
slack_info(var, instance, 'B0129P82UTS').
slack_info('B0129P82UTS', app_id, "A0F7YS25R").
slack_info('B0129P82UTS', deleted, false).
slack_info('B0129P82UTS', icons, _{image_36:"https://a.slack-edge.com/80588/img/services/bots_36.png", image_48:"https://a.slack-edge.com/80588/img/plugins/bot/service_48.png", image_72:"https://a.slack-edge.com/80588/img/services/bots_72.png"}).
slack_info('B0129P82UTS', id, "B0129P82UTS").
slack_info('B0129P82UTS', name, "bot").
slack_info('B0129P82UTS', updated, 1586665440).
slack_info(var, cache_ts, 1586671107).
slack_info(var, cache_ts_version, "v2-bunny").
slack_info(var, cache_version, "v21-nomad").
slack_info(var, can_manage_shared_channels, false).
slack_info(var, instance, 'C010LM69Q9X').
slack_info('C010LM69Q9X', created, 1585303371).
slack_info('C010LM69Q9X', creator, "U010LM68Y65").
slack_info('C010LM69Q9X', has_pins, false).
slack_info('C010LM69Q9X', id, "C010LM69Q9X").
slack_info('C010LM69Q9X', is_archived, false).
slack_info('C010LM69Q9X', is_channel, true).
slack_info('C010LM69Q9X', is_general, false).
slack_info('C010LM69Q9X', is_member, false).
slack_info('C010LM69Q9X', is_mpim, false).
slack_info('C010LM69Q9X', is_org_shared, false).
slack_info('C010LM69Q9X', is_private, false).
slack_info('C010LM69Q9X', is_shared, false).
slack_info('C010LM69Q9X', name, "random").
slack_info('C010LM69Q9X', name_normalized, "random").
slack_info('C010LM69Q9X', previous_names, []).
slack_info('C010LM69Q9X', priority, 0).
slack_info('C010LM69Q9X', unlinked, 0).
slack_info(var, instance, 'C010WLMHEJH').
slack_info('C010WLMHEJH', created, 1585303371).
slack_info('C010WLMHEJH', creator, "U010LM68Y65").
slack_info('C010WLMHEJH', has_pins, true).
slack_info('C010WLMHEJH', id, "C010WLMHEJH").
slack_info('C010WLMHEJH', is_archived, false).
slack_info('C010WLMHEJH', is_channel, true).
slack_info('C010WLMHEJH', is_general, true).
slack_info('C010WLMHEJH', is_member, false).
slack_info('C010WLMHEJH', is_mpim, false).
slack_info('C010WLMHEJH', is_org_shared, false).
slack_info('C010WLMHEJH', is_private, false).
slack_info('C010WLMHEJH', is_shared, false).
slack_info('C010WLMHEJH', name, "general").
slack_info('C010WLMHEJH', name_normalized, "general").
slack_info('C010WLMHEJH', previous_names, []).
slack_info('C010WLMHEJH', priority, 0).
slack_info('C010WLMHEJH', unlinked, 0).
slack_info(var, instance, 'C010WLMHTPF').
slack_info('C010WLMHTPF', created, 1585303371).
slack_info('C010WLMHTPF', creator, "U010LM68Y65").
slack_info('C010WLMHTPF', has_pins, false).
slack_info('C010WLMHTPF', id, "C010WLMHTPF").
slack_info('C010WLMHTPF', is_archived, false).
slack_info('C010WLMHTPF', is_channel, true).
slack_info('C010WLMHTPF', is_general, false).
slack_info('C010WLMHTPF', is_member, false).
slack_info('C010WLMHTPF', is_mpim, false).
slack_info('C010WLMHTPF', is_org_shared, false).
slack_info('C010WLMHTPF', is_private, false).
slack_info('C010WLMHTPF', is_shared, false).
slack_info('C010WLMHTPF', name, "announcements").
slack_info('C010WLMHTPF', name_normalized, "announcements").
slack_info('C010WLMHTPF', previous_names, []).
slack_info('C010WLMHTPF', priority, 0).
slack_info('C010WLMHTPF', unlinked, 0).
slack_info(var, instance, 'C011BE1SZGF').
slack_info('C011BE1SZGF', created, 1586206871).
slack_info('C011BE1SZGF', creator, "U0115Q96815").
slack_info('C011BE1SZGF', has_pins, false).
slack_info('C011BE1SZGF', id, "C011BE1SZGF").
slack_info('C011BE1SZGF', is_archived, false).
slack_info('C011BE1SZGF', is_channel, true).
slack_info('C011BE1SZGF', is_general, false).
slack_info('C011BE1SZGF', is_member, false).
slack_info('C011BE1SZGF', is_mpim, false).
slack_info('C011BE1SZGF', is_org_shared, false).
slack_info('C011BE1SZGF', is_private, false).
slack_info('C011BE1SZGF', is_shared, false).
slack_info('C011BE1SZGF', name, "annie-private").
slack_info('C011BE1SZGF', name_normalized, "annie-private").
slack_info('C011BE1SZGF', previous_names, []).
slack_info('C011BE1SZGF', priority, 0).
slack_info('C011BE1SZGF', unlinked, 0).
slack_info(var, instance, 'C011CJSP6P9').
slack_info('C011CJSP6P9', created, 1586631019).
slack_info('C011CJSP6P9', creator, "U011KMTBGN8").
slack_info('C011CJSP6P9', has_pins, false).
slack_info('C011CJSP6P9', id, "C011CJSP6P9").
slack_info('C011CJSP6P9', is_archived, false).
slack_info('C011CJSP6P9', is_channel, true).
slack_info('C011CJSP6P9', is_general, false).
slack_info('C011CJSP6P9', is_member, false).
slack_info('C011CJSP6P9', is_mpim, false).
slack_info('C011CJSP6P9', is_org_shared, false).
slack_info('C011CJSP6P9', is_private, false).
slack_info('C011CJSP6P9', is_shared, false).
slack_info('C011CJSP6P9', name, "pet_talk").
slack_info('C011CJSP6P9', name_normalized, "pet_talk").
slack_info('C011CJSP6P9', previous_names, []).
slack_info('C011CJSP6P9', priority, 0).
slack_info('C011CJSP6P9', unlinked, 0).
slack_info(var, instance, 'C011HV9FHQT').
slack_info('C011HV9FHQT', created, 1586440307).
slack_info('C011HV9FHQT', creator, "U010LM68Y65").
slack_info('C011HV9FHQT', has_pins, false).
slack_info('C011HV9FHQT', id, "C011HV9FHQT").
slack_info('C011HV9FHQT', is_archived, false).
slack_info('C011HV9FHQT', is_channel, true).
slack_info('C011HV9FHQT', is_general, false).
slack_info('C011HV9FHQT', is_member, false).
slack_info('C011HV9FHQT', is_mpim, false).
slack_info('C011HV9FHQT', is_org_shared, false).
slack_info('C011HV9FHQT', is_private, false).
slack_info('C011HV9FHQT', is_shared, false).
slack_info('C011HV9FHQT', name, "studygroup").
slack_info('C011HV9FHQT', name_normalized, "studygroup").
slack_info('C011HV9FHQT', previous_names, []).
slack_info('C011HV9FHQT', priority, 0).
slack_info('C011HV9FHQT', unlinked, 0).
slack_info(var, instance, 'C011LNF6F9P').
slack_info('C011LNF6F9P', created, 1586332652).
slack_info('C011LNF6F9P', creator, "U010LM68Y65").
slack_info('C011LNF6F9P', has_pins, true).
slack_info('C011LNF6F9P', id, "C011LNF6F9P").
slack_info('C011LNF6F9P', is_archived, false).
slack_info('C011LNF6F9P', is_channel, true).
slack_info('C011LNF6F9P', is_general, false).
slack_info('C011LNF6F9P', is_member, false).
slack_info('C011LNF6F9P', is_mpim, false).
slack_info('C011LNF6F9P', is_org_shared, false).
slack_info('C011LNF6F9P', is_private, false).
slack_info('C011LNF6F9P', is_shared, false).
slack_info('C011LNF6F9P', name, "instructorannouncements").
slack_info('C011LNF6F9P', name_normalized, "instructorannouncements").
slack_info('C011LNF6F9P', previous_names, []).
slack_info('C011LNF6F9P', priority, 0).
slack_info('C011LNF6F9P', unlinked, 0).
slack_info(var, instance, 'C011M65UV8R').
slack_info('C011M65UV8R', created, 1586340605).
slack_info('C011M65UV8R', creator, "U010LM68Y65").
slack_info('C011M65UV8R', has_pins, false).
slack_info('C011M65UV8R', id, "C011M65UV8R").
slack_info('C011M65UV8R', is_archived, false).
slack_info('C011M65UV8R', is_channel, true).
slack_info('C011M65UV8R', is_general, false).
slack_info('C011M65UV8R', is_member, false).
slack_info('C011M65UV8R', is_mpim, false).
slack_info('C011M65UV8R', is_org_shared, false).
slack_info('C011M65UV8R', is_private, false).
slack_info('C011M65UV8R', is_shared, false).
slack_info('C011M65UV8R', name, "help").
slack_info('C011M65UV8R', name_normalized, "help").
slack_info('C011M65UV8R', previous_names, []).
slack_info('C011M65UV8R', priority, 0).
slack_info('C011M65UV8R', unlinked, 0).
slack_info(var, instance, 'C011VR42F6Y').
slack_info('C011VR42F6Y', created, 1586663738).
slack_info('C011VR42F6Y', creator, "U010LM68Y65").
slack_info('C011VR42F6Y', has_pins, false).
slack_info('C011VR42F6Y', id, "C011VR42F6Y").
slack_info('C011VR42F6Y', is_archived, false).
slack_info('C011VR42F6Y', is_channel, true).
slack_info('C011VR42F6Y', is_general, false).
slack_info('C011VR42F6Y', is_member, true).
slack_info('C011VR42F6Y', is_mpim, false).
slack_info('C011VR42F6Y', is_org_shared, false).
slack_info('C011VR42F6Y', is_private, false).
slack_info('C011VR42F6Y', is_shared, false).
slack_info('C011VR42F6Y', last_read, "1586667243.004400").
slack_info('C011VR42F6Y', members, ["U0129P82V6U", "U011Z6B31UH", "U011PGDJ07P", "U0115Q96815", "U010LM68Y65"]).
slack_info('C011VR42F6Y', name, "bottestchannel").
slack_info('C011VR42F6Y', name_normalized, "bottestchannel").
slack_info('C011VR42F6Y', previous_names, []).
slack_info('C011VR42F6Y', priority, 0).
slack_info('C011VR42F6Y', purpose, _{creator:"U010LM68Y65", last_set:1586663739, value:"channel for testing the bot"}).
slack_info('C011VR42F6Y', topic, _{creator:"", last_set:0, value:""}).
slack_info('C011VR42F6Y', unlinked, 0).
slack_info(var, instance, 'C011XJ80QQG').
slack_info('C011XJ80QQG', created, 1586340699).
slack_info('C011XJ80QQG', creator, "U010LM68Y65").
slack_info('C011XJ80QQG', has_pins, false).
slack_info('C011XJ80QQG', id, "C011XJ80QQG").
slack_info('C011XJ80QQG', is_archived, false).
slack_info('C011XJ80QQG', is_channel, true).
slack_info('C011XJ80QQG', is_general, false).
slack_info('C011XJ80QQG', is_member, false).
slack_info('C011XJ80QQG', is_mpim, false).
slack_info('C011XJ80QQG', is_org_shared, false).
slack_info('C011XJ80QQG', is_private, false).
slack_info('C011XJ80QQG', is_shared, false).
slack_info('C011XJ80QQG', name, "help-remote-learning").
slack_info('C011XJ80QQG', name_normalized, "help-remote-learning").
slack_info('C011XJ80QQG', previous_names, []).
slack_info('C011XJ80QQG', priority, 0).
slack_info('C011XJ80QQG', unlinked, 0).
slack_info(var, dnd_enabled, false).
slack_info(var, next_dnd_end_ts, 1).
slack_info(var, next_dnd_start_ts, 1).
slack_info(var, snooze_enabled, false).
slack_info(var, groups, []).
slack_info(var, instance, 'D011N5555MY').
slack_info('D011N5555MY', created, 1586667065).
slack_info('D011N5555MY', has_pins, false).
slack_info('D011N5555MY', id, "D011N5555MY").
slack_info('D011N5555MY', is_archived, false).
slack_info('D011N5555MY', is_im, true).
slack_info('D011N5555MY', is_open, false).
slack_info('D011N5555MY', is_org_shared, false).
slack_info('D011N5555MY', last_read, "0000000000.000000").
slack_info('D011N5555MY', priority, 0).
slack_info('D011N5555MY', user, "U011Z6B31UH").
slack_info(var, instance, 'D0129P82VMW').
slack_info('D0129P82VMW', created, 1586665440).
slack_info('D0129P82VMW', has_pins, false).
slack_info('D0129P82VMW', id, "D0129P82VMW").
slack_info('D0129P82VMW', is_archived, false).
slack_info('D0129P82VMW', is_im, true).
slack_info('D0129P82VMW', is_open, true).
slack_info('D0129P82VMW', is_org_shared, false).
slack_info('D0129P82VMW', last_read, "0000000000.000000").
slack_info('D0129P82VMW', priority, 0).
slack_info('D0129P82VMW', user, "USLACKBOT").
slack_info(var, is_europe, false).
slack_info(var, latest_event_ts, "1586670507.000000").
slack_info(var, non_threadable_channels, []).
slack_info(var, ok, true).
slack_info(var, read_only_channels, []).
slack_info(var, created, 1586665440).
slack_info(var, id, "U0129P82V6U").
slack_info(var, manual_presence, "active").
slack_info(var, name, "prolog_bot").
slack_info(var, a11y_animations, true).
slack_info(var, activity_view, "page").
slack_info(var, add_apps_prompt_dismissed, false).
slack_info(var, add_channel_prompt_dismissed, false).
slack_info(var, all_channels_loud, false).
slack_info(var, all_notifications_prefs, "{\"global\":{\"global_desktop\":\"mentions_dms\",\"global_mpdm_desktop\":\"everything\",\"global_mobile\":\"mentions_dms\",\"global_mpdm_mobile\":\"everything\",\"mobile_sound\":\"b2.mp3\",\"desktop_sound\":\"knock_brush.mp3\",\"global_keywords\":\"\",\"push_idle_wait\":0,\"no_text_in_notifications\":false,\"push_show_preview\":true,\"threads_everything\":true},\"channels\":[]}").
slack_info(var, all_unreads_sort_order, "").
slack_info(var, allow_calls_to_set_current_status, true).
slack_info(var, allow_cmd_tab_iss, false).
slack_info(var, analytics_upsell_coachmark_seen, false).
slack_info(var, app_subdomain_check_completed, 0).
slack_info(var, arrow_history, false).
slack_info(var, at_channel_suppressed_channels, "").
slack_info(var, box_enabled, false).
slack_info(var, browsers_dismissed_channels_low_results_education, false).
slack_info(var, browsers_dismissed_files_low_results_education, false).
slack_info(var, browsers_dismissed_initial_activity_education, false).
slack_info(var, browsers_dismissed_initial_drafts_education, false).
slack_info(var, browsers_dismissed_initial_saved_education, false).
slack_info(var, browsers_dismissed_people_low_results_education, false).
slack_info(var, browsers_dismissed_user_groups_low_results_education, false).
slack_info(var, browsers_seen_initial_activity_education, false).
slack_info(var, browsers_seen_initial_channels_education, false).
slack_info(var, browsers_seen_initial_drafts_education, false).
slack_info(var, browsers_seen_initial_files_education, false).
slack_info(var, browsers_seen_initial_people_education, false).
slack_info(var, browsers_seen_initial_saved_education, false).
slack_info(var, browsers_seen_initial_user_groups_education, false).
slack_info(var, channel_sections, "").
slack_info(var, channel_sidebar_hide_invite, false).
slack_info(var, channel_sort, "default").
slack_info(var, client_logs_pri, "").
slack_info(var, color_names_in_list, true).
slack_info(var, confirm_clear_all_unreads, true).
slack_info(var, confirm_sh_call_start, true).
slack_info(var, confirm_user_marked_away, true).
slack_info(var, contextual_message_shortcuts_modal_was_seen, true).
slack_info(var, convert_emoticons, true).
slack_info(var, deprecation_modal_last_seen, 0).
slack_info(var, deprecation_toast_last_seen, 0).
slack_info(var, dismissed_app_launcher_limit, false).
slack_info(var, dismissed_app_launcher_welcome, false).
slack_info(var, dismissed_installed_app_dm_suggestions, "").
slack_info(var, dismissed_scroll_search_tooltip_count, 0).
slack_info(var, display_display_names, true).
slack_info(var, display_real_names_override, 0).
slack_info(var, dnd_after_friday, "22:00").
slack_info(var, dnd_after_monday, "22:00").
slack_info(var, dnd_after_saturday, "22:00").
slack_info(var, dnd_after_sunday, "22:00").
slack_info(var, dnd_after_thursday, "22:00").
slack_info(var, dnd_after_tuesday, "22:00").
slack_info(var, dnd_after_wednesday, "22:00").
slack_info(var, dnd_before_friday, "8:00").
slack_info(var, dnd_before_monday, "8:00").
slack_info(var, dnd_before_saturday, "8:00").
slack_info(var, dnd_before_sunday, "8:00").
slack_info(var, dnd_before_thursday, "8:00").
slack_info(var, dnd_before_tuesday, "8:00").
slack_info(var, dnd_before_wednesday, "8:00").
slack_info(var, dnd_custom_new_badge_seen, false).
slack_info(var, dnd_days, "every_day").
slack_info(var, dnd_enabled, true).
slack_info(var, dnd_enabled_friday, "partial").
slack_info(var, dnd_enabled_monday, "partial").
slack_info(var, dnd_enabled_saturday, "partial").
slack_info(var, dnd_enabled_sunday, "partial").
slack_info(var, dnd_enabled_thursday, "partial").
slack_info(var, dnd_enabled_tuesday, "partial").
slack_info(var, dnd_enabled_wednesday, "partial").
slack_info(var, dnd_end_hour, "8:00").
slack_info(var, dnd_notification_schedule_new_badge_seen, false).
slack_info(var, dnd_start_hour, "22:00").
slack_info(var, dnd_weekdays_off_allday, false).
slack_info(var, dropbox_enabled, false).
slack_info(var, edge_upload_proxy_check_completed, 0).
slack_info(var, email_alerts, "instant").
slack_info(var, email_alerts_sleep_until, 0).
slack_info(var, email_developer, true).
slack_info(var, email_offers, true).
slack_info(var, email_research, true).
slack_info(var, email_tips, true).
slack_info(var, email_weekly, true).
slack_info(var, emoji_autocomplete_big, false).
slack_info(var, emoji_mode, "default").
slack_info(var, emoji_use, "").
slack_info(var, enable_react_emoji_picker, true).
slack_info(var, enable_unread_view, false).
slack_info(var, ent_org_wide_channels_sidebar, true).
slack_info(var, enter_is_special_in_tbt, false).
slack_info(var, enterprise_excluded_app_teams, null).
slack_info(var, enterprise_mdm_custom_msg, "").
slack_info(var, enterprise_migration_seen, true).
slack_info(var, expand_inline_imgs, true).
slack_info(var, expand_internal_inline_imgs, true).
slack_info(var, expand_non_media_attachments, true).
slack_info(var, expand_snippets, false).
slack_info(var, f_key_search, false).
slack_info(var, failover_proxy_check_completed, 0).
slack_info(var, flannel_server_pool, "random").
slack_info(var, folder_data, "[]").
slack_info(var, folders_enabled, false).
slack_info(var, frecency_ent_jumper, "").
slack_info(var, frecency_ent_jumper_backup, "").
slack_info(var, frecency_jumper, "").
slack_info(var, fuller_timestamps, false).
slack_info(var, graphic_emoticons, false).
slack_info(var, growls_enabled, true).
slack_info(var, growth_all_banners_prefs, "").
slack_info(var, growth_msg_limit_approaching_cta_count, 0).
slack_info(var, growth_msg_limit_approaching_cta_ts, 0).
slack_info(var, growth_msg_limit_long_reached_cta_count, 0).
slack_info(var, growth_msg_limit_long_reached_cta_last_ts, 0).
slack_info(var, growth_msg_limit_reached_cta_count, 0).
slack_info(var, growth_msg_limit_reached_cta_last_ts, 0).
slack_info(var, growth_msg_limit_sixty_day_banner_cta_count, 0).
slack_info(var, growth_msg_limit_sixty_day_banner_cta_last_ts, 0).
slack_info(var, has_acknowledged_shortcut_speedbump, false).
slack_info(var, has_created_channel, false).
slack_info(var, has_drafted_message, false).
slack_info(var, has_installed_apps, null).
slack_info(var, has_invited, false).
slack_info(var, has_received_mention_or_reaction, true).
slack_info(var, has_received_threaded_message, false).
slack_info(var, has_recently_shared_a_channel, false).
slack_info(var, has_searched, false).
slack_info(var, has_starred_item, false).
slack_info(var, has_uploaded, false).
slack_info(var, has_used_quickswitcher_shortcut, false).
slack_info(var, hide_hex_swatch, false).
slack_info(var, hide_user_group_info_pane, false).
slack_info(var, highlight_words, "").
slack_info(var, ia_platform_actions_lab, 1).
slack_info(var, ia_slackbot_survey_timestamp_48h, 0).
slack_info(var, ia_slackbot_survey_timestamp_7d, 0).
slack_info(var, ia_top_nav_theme, "").
slack_info(var, iap1_lab, 0).
slack_info(var, in_interactive_mas_migration_flow, false).
slack_info(var, in_prod_surveys_enabled, true).
slack_info(var, jumbomoji, true).
slack_info(var, k_key_omnibox, true).
slack_info(var, k_key_omnibox_auto_hide_count, 0).
slack_info(var, keyboard, null).
slack_info(var, last_dismissed_scroll_search_tooltip_timestamp, 0).
slack_info(var, last_seen_at_channel_warning, 0).
slack_info(var, last_snippet_type, "").
slack_info(var, last_tos_acknowledged, null).
slack_info(var, lessons_enabled, false).
slack_info(var, load_lato_2, false).
slack_info(var, locale, "en-US").
slack_info(var, 'de-DE', "Deutsch (Deutschland)").
slack_info(var, 'en-GB', "English (UK)").
slack_info(var, 'en-US', "English (US)").
slack_info(var, 'es-ES', "Español (España)").
slack_info(var, 'es-LA', "Español (Latinoamérica)").
slack_info(var, 'fr-FR', "Français (France)").
slack_info(var, 'ja-JP', "???").
slack_info(var, 'pt-BR', "Português (Brasil)").
slack_info(var, loud_channels, "").
slack_info(var, loud_channels_set, "").
slack_info(var, ls_disabled, false).
slack_info(var, mac_ssb_bounce, "").
slack_info(var, mac_ssb_bullet, true).
slack_info(var, mark_msgs_read_immediately, true).
slack_info(var, mentions_exclude_at_channels, true).
slack_info(var, mentions_exclude_at_user_groups, false).
slack_info(var, mentions_exclude_reactions, false).
slack_info(var, message_navigation_toast_was_seen, false).
slack_info(var, messages_theme, "default").
slack_info(var, msg_input_send_btn, false).
slack_info(var, msg_input_send_btn_auto_set, false).
slack_info(var, msg_input_sticky_composer, true).
slack_info(var, mute_sounds, false).
slack_info(var, muted_channels, "").
slack_info(var, needs_initial_password_set, false).
slack_info(var, never_channels, "").
slack_info(var, new_msg_snd, "knock_brush.mp3").
slack_info(var, newxp_seen_last_message, 0).
slack_info(var, newxp_suggested_channels, "C010LM69Q9X,C010WLMHEJH,C010WLMHTPF,C011BE1SZGF,C011CJSP6P9,C011HV9FHQT,C011LNF6F9P,C011M65UV8R,C011XJ80QQG").
slack_info(var, no_created_overlays, false).
slack_info(var, no_invites_widget_in_sidebar, false).
slack_info(var, no_joined_overlays, false).
slack_info(var, no_macelectron_banner, false).
slack_info(var, no_macssb1_banner, false).
slack_info(var, no_macssb2_banner, false).
slack_info(var, no_omnibox_in_channels, false).
slack_info(var, no_text_in_notifications, false).
slack_info(var, no_winssb1_banner, false).
slack_info(var, obey_inline_img_limit, true).
slack_info(var, onboarding_cancelled, false).
slack_info(var, onboarding_complete, false).
slack_info(var, onboarding_role_apps, null).
slack_info(var, onboarding_slackbot_conversation_step, 0).
slack_info(var, onboarding_state, 0).
slack_info(var, opened_slackbot_dm, false).
slack_info(var, overloaded_message_enabled, true).
slack_info(var, pagekeys_handled, true).
slack_info(var, plain_text_mode, false).
slack_info(var, posts_formatting_guide, true).
slack_info(var, preferred_skin_tone, "").
slack_info(var, privacy_policy_seen, true).
slack_info(var, prompted_for_email_disabling, false).
slack_info(var, purchaser, false).
slack_info(var, push_at_channel_suppressed_channels, "").
slack_info(var, push_dm_alert, true).
slack_info(var, push_everything, false).
slack_info(var, push_idle_wait, 0).
slack_info(var, push_loud_channels, "").
slack_info(var, push_loud_channels_set, "").
slack_info(var, push_mention_alert, true).
slack_info(var, push_mention_channels, "").
slack_info(var, push_show_preview, true).
slack_info(var, push_sound, "b2.mp3").
slack_info(var, require_at, true).
slack_info(var, saved_view, "page").
slack_info(var, search_channel_sort, "relevant").
slack_info(var, search_exclude_bots, false).
slack_info(var, search_exclude_channels, "").
slack_info(var, search_file_sort, "score").
slack_info(var, search_hide_deactivated_users, false).
slack_info(var, search_hide_my_channels, false).
slack_info(var, search_only_current_team, false).
slack_info(var, search_only_my_channels, false).
slack_info(var, search_only_show_online, false).
slack_info(var, search_people_sort, "relevant").
slack_info(var, search_sort, "not_set").
slack_info(var, seen_administration_menu, false).
slack_info(var, seen_app_space_coachmark, false).
slack_info(var, seen_app_space_tutorial, false).
slack_info(var, seen_calls_interactive_coachmark, false).
slack_info(var, seen_channel_browser_admin_coachmark, false).
slack_info(var, seen_channel_search, false).
slack_info(var, seen_contextual_message_shortcuts_modal, false).
slack_info(var, seen_corporate_export_alert, false).
slack_info(var, seen_custom_status_badge, false).
slack_info(var, seen_custom_status_callout, false).
slack_info(var, seen_custom_status_expiration_badge, false).
slack_info(var, seen_domain_invite_reminder, false).
slack_info(var, seen_drafts_section_coachmark, false).
slack_info(var, seen_emoji_update_overlay_coachmark, false).
slack_info(var, seen_gdrive_coachmark, false).
slack_info(var, seen_guest_admin_slackbot_announcement, false).
slack_info(var, seen_highlights_arrows_coachmark, false).
slack_info(var, seen_highlights_coachmark, false).
slack_info(var, seen_highlights_warm_welcome, false).
slack_info(var, seen_ia_education, false).
slack_info(var, seen_intl_channel_names_coachmark, false).
slack_info(var, seen_japanese_locale_change_message, false).
slack_info(var, seen_keyboard_shortcuts_coachmark, false).
slack_info(var, seen_locale_change_message, 0).
slack_info(var, seen_markdown_paste_shortcut, 0).
slack_info(var, seen_markdown_paste_toast, 0).
slack_info(var, seen_member_invite_reminder, false).
slack_info(var, seen_message_navigation_educational_toast, false).
slack_info(var, seen_name_tagging_coachmark, false).
slack_info(var, seen_new_search_ui, false).
slack_info(var, seen_onboarding_banner, false).
slack_info(var, seen_onboarding_channels, false).
slack_info(var, seen_onboarding_direct_messages, false).
slack_info(var, seen_onboarding_invites, false).
slack_info(var, seen_onboarding_private_groups, false).
slack_info(var, seen_onboarding_recent_mentions, false).
slack_info(var, seen_onboarding_search, false).
slack_info(var, seen_onboarding_slackbot_conversation, false).
slack_info(var, seen_onboarding_starred_items, false).
slack_info(var, seen_onboarding_start, false).
slack_info(var, seen_p2_locale_change_message, 0).
slack_info(var, seen_people_search, true).
slack_info(var, seen_people_search_count, 0).
slack_info(var, seen_quickswitcher_shortcut_tip_count, 0).
slack_info(var, seen_shared_channels_coachmark, false).
slack_info(var, seen_shared_channels_opt_in_change_message, false).
slack_info(var, seen_shdep_slackbot_message, false).
slack_info(var, seen_single_emoji_msg, false).
slack_info(var, seen_sonic_deluxe_toast, 0).
slack_info(var, seen_ssb_prompt, false).
slack_info(var, seen_threads_notification_banner, false).
slack_info(var, seen_unread_view_coachmark, false).
slack_info(var, seen_welcome_2, false).
slack_info(var, seen_workflow_builder_deluxe_toast, false).
slack_info(var, seen_wysiwyg_deluxe_toast, false).
slack_info(var, send_your_first_message_banner_enabled, null).
slack_info(var, separate_private_channels, false).
slack_info(var, separate_shared_channels, true).
slack_info(var, set_tz_automatically, true).
slack_info(var, shdep_promo_code_submitted, false).
slack_info(var, show_all_skin_tones, false).
slack_info(var, show_autocomplete_help, 1).
slack_info(var, show_ent_onboarding, true).
slack_info(var, show_ia_tour_relaunch, 0).
slack_info(var, show_jumper_scores, false).
slack_info(var, show_memory_instrument, false).
slack_info(var, show_shared_channels_education_banner, true).
slack_info(var, show_sidebar_quickswitcher_button, false).
slack_info(var, show_typing, true).
slack_info(var, sidebar_behavior, "").
slack_info(var, sidebar_theme, "default").
slack_info(var, sidebar_theme_custom_values, "").
slack_info(var, snippet_editor_wrap_long_lines, false).
slack_info(var, spaces_new_xp_banner_dismissed, false).
slack_info(var, ss_emojis, true).
slack_info(var, ssb_space_window, "").
slack_info(var, start_scroll_at_oldest, true).
slack_info(var, sunset_interactive_message_views, 0).
slack_info(var, suppress_link_warning, false).
slack_info(var, tab_ui_return_selects, true).
slack_info(var, threads_everything, true).
slack_info(var, time24, false).
slack_info(var, tractor_enabled, false).
slack_info(var, tractor_experiment_group, "").
slack_info(var, two_factor_auth_enabled, false).
slack_info(var, two_factor_backup_type, null).
slack_info(var, two_factor_type, null).
slack_info(var, tz, "America/Los_Angeles").
slack_info(var, unread_collapsed_channels, null).
slack_info(var, up_to_browse_kb_shortcut, true).
slack_info(var, used_custom_status_kb_shortcut, false).
slack_info(var, user_colors, "").
slack_info(var, webapp_spellcheck, true).
slack_info(var, welcome_message_hidden, false).
slack_info(var, welcome_place_state, "none").
slack_info(var, whats_new_read, 1586665440).
slack_info(var, whocanseethis_dm_mpdm_badge, true).
slack_info(var, workflow_builder_coachmarks, "{}").
slack_info(var, workflow_builder_intro_modal_clicked_through, false).
slack_info(var, all, []).
slack_info(var, self, []).
slack_info(var, avatar_base_url, "https://ca.slack-edge.com/").
slack_info(var, date_create, 1585303370).
slack_info(var, domain, "prologclassworkspace").
slack_info(var, email_domain, "").
slack_info(var, image_102, "https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-102.png").
slack_info(var, image_132, "https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-132.png").
slack_info(var, image_230, "https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-230.png").
slack_info(var, image_34, "https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-34.png").
slack_info(var, image_44, "https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-44.png").
slack_info(var, image_68, "https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-68.png").
slack_info(var, image_88, "https://a.slack-edge.com/80588/img/avatars-teams/ava_0015-88.png").
slack_info(var, image_default, true).
slack_info(var, id, "T010WMWBAGY").
slack_info(var, limit_ts, 0).
slack_info(var, messages_count, 753).
slack_info(var, msg_edit_window_mins, -1).
slack_info(var, name, "PrologClass").
slack_info(var, onboarding_channel_id, "C010WLMHTPF").
slack_info(var, over_storage_limit, false).
slack_info(var, plan, "").
slack_info(var, all_users_can_purchase, true).
slack_info(var, allow_calls, true).
slack_info(var, allow_calls_interactive_screen_sharing, true).
slack_info(var, allow_message_deletion, true).
slack_info(var, allow_retention_override, false).
slack_info(var, app_whitelist_enabled, false).
slack_info(var, auth_mode, "normal").
slack_info(var, block_file_download, false).
slack_info(var, box_app_installed, false).
slack_info(var, calling_app_name, "Slack").
slack_info(var, audio, []).
slack_info(var, profile_field_options, []).
slack_info(var, instance, 'A00').
slack_info('A00', id, "A00").
slack_info('A00', image, "img/slack_hash_128.png").
slack_info('A00', name, "Slack").
slack_info(var, can_receive_shared_channels_invites, true).
slack_info(var, channel_email_addresses_enabled, true).
slack_info(var, compliance_export_start, 0).
slack_info(var, custom_contact_email, null).
slack_info(var, custom_status_default_emoji, ":speech_balloon:").
slack_info(var, default_channel_creation_enabled, true).
slack_info(var, dev_company_segment, null).
slack_info(var, disable_email_ingestion, false).
slack_info(var, disable_file_deleting, false).
slack_info(var, disable_file_editing, false).
slack_info(var, disable_file_uploads, "allow_all").
slack_info(var, disable_sidebar_connect_prompts, []).
slack_info(var, disable_sidebar_install_prompts, []).
slack_info(var, disallow_public_file_urls, false).
slack_info(var, discoverable, "unlisted").
slack_info(var, display_email_addresses, true).
slack_info(var, display_real_names, false).
slack_info(var, dm_retention_duration, 0).
slack_info(var, dm_retention_type, 0).
slack_info(var, dnd_after_friday, "22:00").
slack_info(var, dnd_after_monday, "22:00").
slack_info(var, dnd_after_saturday, "22:00").
slack_info(var, dnd_after_sunday, "22:00").
slack_info(var, dnd_after_thursday, "22:00").
slack_info(var, dnd_after_tuesday, "22:00").
slack_info(var, dnd_after_wednesday, "22:00").
slack_info(var, dnd_before_friday, "08:00").
slack_info(var, dnd_before_monday, "08:00").
slack_info(var, dnd_before_saturday, "08:00").
slack_info(var, dnd_before_sunday, "08:00").
slack_info(var, dnd_before_thursday, "08:00").
slack_info(var, dnd_before_tuesday, "08:00").
slack_info(var, dnd_before_wednesday, "08:00").
slack_info(var, dnd_days, "every_day").
slack_info(var, dnd_enabled, true).
slack_info(var, dnd_enabled_friday, "partial").
slack_info(var, dnd_enabled_monday, "partial").
slack_info(var, dnd_enabled_saturday, "partial").
slack_info(var, dnd_enabled_sunday, "partial").
slack_info(var, dnd_enabled_thursday, "partial").
slack_info(var, dnd_enabled_tuesday, "partial").
slack_info(var, dnd_enabled_wednesday, "partial").
slack_info(var, dnd_end_hour, "08:00").
slack_info(var, dnd_start_hour, "22:00").
slack_info(var, dnd_weekdays_off_allday, false).
slack_info(var, dropbox_legacy_picker, false).
slack_info(var, enable_shared_channels, 2).
slack_info(var, ent_required_browser, null).
slack_info(var, enterprise_default_channels, []).
slack_info(var, enterprise_mandatory_channels, []).
slack_info(var, enterprise_mdm_disable_file_download, false).
slack_info(var, enterprise_mobile_device_check, false).
slack_info(var, external_shared_channel_requests_approval_channel, null).
slack_info(var, file_limit_whitelisted, false).
slack_info(var, file_retention_duration, 0).
slack_info(var, file_retention_type, 0).
slack_info(var, gdrive_enabled_team, false).
slack_info(var, gg_enabled, false).
slack_info(var, group_retention_duration, 0).
slack_info(var, group_retention_type, 0).
slack_info(var, has_hipaa_compliance, false).
slack_info(var, has_seen_partner_promo, false).
slack_info(var, hide_referers, true).
slack_info(var, invite_requests_enabled, true).
slack_info(var, invites_limit, true).
slack_info(var, invites_only_admins, false).
slack_info(var, locale, "en-US").
slack_info(var, loud_channel_mentions_limit, 10000).
slack_info(var, mobile_passcode_timeout_in_seconds, -1).
slack_info(var, msg_edit_window_mins, -1).
slack_info(var, onedrive_app_installed, false).
slack_info(var, onedrive_enabled_team, false).
slack_info(var, org_calls_apps, null).
slack_info(var, received_esc_route_to_channel_awareness_message, false).
slack_info(var, required_minimum_mobile_version, null).
slack_info(var, retention_duration, 0).
slack_info(var, retention_type, 0).
slack_info(var, self_serve_select, false).
slack_info(var, show_join_leave, false).
slack_info(var, single_user_exports, false).
slack_info(var, slackbot_responses_disabled, false).
slack_info(var, subteams_auto_create_admin, false).
slack_info(var, subteams_auto_create_owner, false).
slack_info(var, uses_customized_custom_status_presets, false).
slack_info(var, warn_before_at_channel, "always").
slack_info(var, welcome_place_enabled, true).
slack_info(var, who_can_archive_channels, "regular").
slack_info(var, who_can_at_channel, "ra").
slack_info(var, who_can_at_everyone, "regular").
slack_info(var, who_can_change_team_profile, "admin").
slack_info(var, who_can_create_channels, "regular").
slack_info(var, who_can_create_delete_user_groups, "admin").
slack_info(var, who_can_create_groups, "ra").
slack_info(var, who_can_create_shared_channels, "admin").
slack_info(var, who_can_edit_user_groups, "admin").
slack_info(var, who_can_kick_channels, "admin").
slack_info(var, who_can_kick_groups, "regular").
slack_info(var, who_can_manage_channel_posting_prefs, "ra").
slack_info(var, type, []).
slack_info(var, user, []).
slack_info(var, type, []).
slack_info(var, user, []).
slack_info(var, who_can_post_general, "ra").
slack_info(var, workflow_builder_enabled, true).
slack_info(var, workflows_export_csv_enabled, true).
slack_info(var, workflows_webhook_trigger_enabled, true).
slack_info(var, thread_only_channels, []).
slack_info(var, url, "wss://cerberus-xxxx.lb.slack-msgs.com/websocket/yMVca33O9XnBPOnjyGJ_0xPxRW4qAZ6wHXOAjShQ1G71T63zCTDTGZsAL8L9WKMZZiEsKC8UmP8TxEv9aj0HJ5NRAz3c1gBUyFCCwvgDMms=").
slack_info(var, instance, 'U010LM68Y65').
slack_info('U010LM68Y65', color, "9f69e7").
slack_info('U010LM68Y65', deleted, false).
slack_info('U010LM68Y65', id, "U010LM68Y65").
slack_info('U010LM68Y65', is_admin, true).
slack_info('U010LM68Y65', is_app_user, false).
slack_info('U010LM68Y65', is_bot, false).
slack_info('U010LM68Y65', is_owner, true).
slack_info('U010LM68Y65', is_primary_owner, true).
slack_info('U010LM68Y65', is_restricted, false).
slack_info('U010LM68Y65', is_ultra_restricted, false).
slack_info('U010LM68Y65', name, "annie").
slack_info('U010LM68Y65', presence, "away").
slack_info('U010LM68Y65', profile, _{avatar_hash:"95bd769325fe", display_name:"Anne Ogborn (Instructor)", display_name_normalized:"Anne Ogborn (Instructor)", email:"annie@theelginworks.com", fields:[], first_name:"Anne", image_1024:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-08/1046602607538_95bd769325fe75c2a148_original.jpg", is_custom_image:true, last_name:"Ogborn", phone:"", real_name:"Anne Ogborn", real_name_normalized:"Anne Ogborn", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:"Teach Prolog!"}).
slack_info('U010LM68Y65', real_name, "Anne Ogborn").
slack_info('U010LM68Y65', team_id, "T010WMWBAGY").
slack_info('U010LM68Y65', tz, "Europe/Amsterdam").
slack_info('U010LM68Y65', tz_label, "Central European Summer Time").
slack_info('U010LM68Y65', tz_offset, 7200).
slack_info('U010LM68Y65', updated, 1586333062).
slack_info(var, instance, 'U010W7LKG84').
slack_info('U010W7LKG84', color, "4bbe2e").
slack_info('U010W7LKG84', deleted, false).
slack_info('U010W7LKG84', id, "U010W7LKG84").
slack_info('U010W7LKG84', is_admin, false).
slack_info('U010W7LKG84', is_app_user, false).
slack_info('U010W7LKG84', is_bot, false).
slack_info('U010W7LKG84', is_invited_user, true).
slack_info('U010W7LKG84', is_owner, false).
slack_info('U010W7LKG84', is_primary_owner, false).
slack_info('U010W7LKG84', is_restricted, false).
slack_info('U010W7LKG84', is_ultra_restricted, false).
slack_info('U010W7LKG84', name, "anne").
slack_info('U010W7LKG84', presence, "away").
slack_info('U010W7LKG84', profile, _{avatar_hash:"g18794bbfdc1", display_name:"", display_name_normalized:"", email:"anne@swi-prolog.org", fields:null, image_192:"https://secure.gravatar.com/avatar/18794bbfdc1916c36b7eb27c4352ef06.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-192.png", image_24:"https://secure.gravatar.com/avatar/18794bbfdc1916c36b7eb27c4352ef06.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-24.png", image_32:"https://secure.gravatar.com/avatar/18794bbfdc1916c36b7eb27c4352ef06.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-32.png", image_48:"https://secure.gravatar.com/avatar/18794bbfdc1916c36b7eb27c4352ef06.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-48.png", image_512:"https://secure.gravatar.com/avatar/18794bbfdc1916c36b7eb27c4352ef06.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-512.png", image_72:"https://secure.gravatar.com/avatar/18794bbfdc1916c36b7eb27c4352ef06.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-72.png", phone:"", real_name:"anne", real_name_normalized:"anne", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U010W7LKG84', real_name, "anne").
slack_info('U010W7LKG84', team_id, "T010WMWBAGY").
slack_info('U010W7LKG84', tz, "Europe/Amsterdam").
slack_info('U010W7LKG84', tz_label, "Central European Summer Time").
slack_info('U010W7LKG84', tz_offset, 7200).
slack_info('U010W7LKG84', updated, 1585303396).
slack_info(var, instance, 'U0115Q96815').
slack_info('U0115Q96815', color, "e7392d").
slack_info('U0115Q96815', deleted, false).
slack_info('U0115Q96815', id, "U0115Q96815").
slack_info('U0115Q96815', is_admin, true).
slack_info('U0115Q96815', is_app_user, false).
slack_info('U0115Q96815', is_bot, false).
slack_info('U0115Q96815', is_owner, false).
slack_info('U0115Q96815', is_primary_owner, false).
slack_info('U0115Q96815', is_restricted, false).
slack_info('U0115Q96815', is_ultra_restricted, false).
slack_info('U0115Q96815', name, "iandrich87").
slack_info('U0115Q96815', presence, "away").
slack_info('U0115Q96815', profile, _{avatar_hash:"e58fb09dc2b0", display_name:"Ian Andrich", display_name_normalized:"Ian Andrich", email:"iandrich87@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_192.jpg", image_24:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_24.jpg", image_32:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_32.jpg", image_48:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_48.jpg", image_512:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_512.jpg", image_72:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_72.jpg", image_original:"https://avatars.slack-edge.com/2020-03-31/1039145513061_e58fb09dc2b0a3ebc905_original.jpg", is_custom_image:true, phone:"", real_name:"Ian Andrich", real_name_normalized:"Ian Andrich", skype:"", status_emoji:":speech_balloon:", status_expiration:1586671200, status_text:"On Break", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U0115Q96815', real_name, "Ian Andrich").
slack_info('U0115Q96815', team_id, "T010WMWBAGY").
slack_info('U0115Q96815', tz, "America/Los_Angeles").
slack_info('U0115Q96815', tz_label, "Pacific Daylight Time").
slack_info('U0115Q96815', tz_offset, -25200).
slack_info('U0115Q96815', updated, 1586661520).
slack_info(var, instance, 'U0119NXLRR8').
slack_info('U0119NXLRR8', color, "674b1b").
slack_info('U0119NXLRR8', deleted, false).
slack_info('U0119NXLRR8', id, "U0119NXLRR8").
slack_info('U0119NXLRR8', is_admin, false).
slack_info('U0119NXLRR8', is_app_user, false).
slack_info('U0119NXLRR8', is_bot, false).
slack_info('U0119NXLRR8', is_owner, false).
slack_info('U0119NXLRR8', is_primary_owner, false).
slack_info('U0119NXLRR8', is_restricted, false).
slack_info('U0119NXLRR8', is_ultra_restricted, false).
slack_info('U0119NXLRR8', name, "jens").
slack_info('U0119NXLRR8', presence, "away").
slack_info('U0119NXLRR8', profile, _{avatar_hash:"g7de1bd2ce2d", display_name:"Jens Schauder", display_name_normalized:"Jens Schauder", email:"jens@schauderhaft.de", fields:null, image_192:"https://secure.gravatar.com/avatar/7de1bd2ce2d7b597f0e7f54de638e59c.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-192.png", image_24:"https://secure.gravatar.com/avatar/7de1bd2ce2d7b597f0e7f54de638e59c.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-24.png", image_32:"https://secure.gravatar.com/avatar/7de1bd2ce2d7b597f0e7f54de638e59c.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-32.png", image_48:"https://secure.gravatar.com/avatar/7de1bd2ce2d7b597f0e7f54de638e59c.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-48.png", image_512:"https://secure.gravatar.com/avatar/7de1bd2ce2d7b597f0e7f54de638e59c.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-512.png", image_72:"https://secure.gravatar.com/avatar/7de1bd2ce2d7b597f0e7f54de638e59c.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-72.png", phone:"", real_name:"Jens Schauder", real_name_normalized:"Jens Schauder", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U0119NXLRR8', real_name, "Jens Schauder").
slack_info('U0119NXLRR8', team_id, "T010WMWBAGY").
slack_info('U0119NXLRR8', tz, "Europe/Amsterdam").
slack_info('U0119NXLRR8', tz_label, "Central European Summer Time").
slack_info('U0119NXLRR8', tz_offset, 7200).
slack_info('U0119NXLRR8', updated, 1586261481).
slack_info(var, instance, 'U0119TB8NGJ').
slack_info('U0119TB8NGJ', color, "53b759").
slack_info('U0119TB8NGJ', deleted, false).
slack_info('U0119TB8NGJ', id, "U0119TB8NGJ").
slack_info('U0119TB8NGJ', is_admin, false).
slack_info('U0119TB8NGJ', is_app_user, false).
slack_info('U0119TB8NGJ', is_bot, false).
slack_info('U0119TB8NGJ', is_owner, false).
slack_info('U0119TB8NGJ', is_primary_owner, false).
slack_info('U0119TB8NGJ', is_restricted, false).
slack_info('U0119TB8NGJ', is_ultra_restricted, false).
slack_info('U0119TB8NGJ', name, "mikemps").
slack_info('U0119TB8NGJ', presence, "away").
slack_info('U0119TB8NGJ', profile, _{avatar_hash:"c337ddec90e5", display_name:"msuarz", display_name_normalized:"msuarz", email:"mikemps@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1045059034259_c337ddec90e532020cdd_original.jpg", is_custom_image:true, phone:"", real_name:"msuarz", real_name_normalized:"msuarz", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U0119TB8NGJ', real_name, "msuarz").
slack_info('U0119TB8NGJ', team_id, "T010WMWBAGY").
slack_info('U0119TB8NGJ', tz, "America/New_York").
slack_info('U0119TB8NGJ', tz_label, "Eastern Daylight Time").
slack_info('U0119TB8NGJ', tz_offset, -14400).
slack_info('U0119TB8NGJ', updated, 1586265641).
slack_info(var, instance, 'U0119UJNZJN').
slack_info('U0119UJNZJN', color, "385a86").
slack_info('U0119UJNZJN', deleted, false).
slack_info('U0119UJNZJN', id, "U0119UJNZJN").
slack_info('U0119UJNZJN', is_admin, false).
slack_info('U0119UJNZJN', is_app_user, false).
slack_info('U0119UJNZJN', is_bot, false).
slack_info('U0119UJNZJN', is_owner, false).
slack_info('U0119UJNZJN', is_primary_owner, false).
slack_info('U0119UJNZJN', is_restricted, false).
slack_info('U0119UJNZJN', is_ultra_restricted, false).
slack_info('U0119UJNZJN', name, "bcobb").
slack_info('U0119UJNZJN', presence, "away").
slack_info('U0119UJNZJN', profile, _{avatar_hash:"g269b40217df", display_name:"bcobb", display_name_normalized:"bcobb", email:"bcobb@uwalumni.com", fields:null, first_name:"Brian", image_192:"https://secure.gravatar.com/avatar/269b40217dfecccd37c71650992dd2cc.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-192.png", image_24:"https://secure.gravatar.com/avatar/269b40217dfecccd37c71650992dd2cc.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-24.png", image_32:"https://secure.gravatar.com/avatar/269b40217dfecccd37c71650992dd2cc.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-32.png", image_48:"https://secure.gravatar.com/avatar/269b40217dfecccd37c71650992dd2cc.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-48.png", image_512:"https://secure.gravatar.com/avatar/269b40217dfecccd37c71650992dd2cc.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-512.png", image_72:"https://secure.gravatar.com/avatar/269b40217dfecccd37c71650992dd2cc.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-72.png", last_name:"Cobb", phone:"", real_name:"Brian Cobb", real_name_normalized:"Brian Cobb", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:"Software Engineer @ Harry’s"}).
slack_info('U0119UJNZJN', real_name, "Brian Cobb").
slack_info('U0119UJNZJN', team_id, "T010WMWBAGY").
slack_info('U0119UJNZJN', tz, "America/New_York").
slack_info('U0119UJNZJN', tz_label, "Eastern Daylight Time").
slack_info('U0119UJNZJN', tz_offset, -14400).
slack_info('U0119UJNZJN', updated, 1586265837).
slack_info(var, instance, 'U011A1XQ5JN').
slack_info('U011A1XQ5JN', color, "d55aef").
slack_info('U011A1XQ5JN', deleted, false).
slack_info('U011A1XQ5JN', id, "U011A1XQ5JN").
slack_info('U011A1XQ5JN', is_admin, false).
slack_info('U011A1XQ5JN', is_app_user, false).
slack_info('U011A1XQ5JN', is_bot, false).
slack_info('U011A1XQ5JN', is_owner, false).
slack_info('U011A1XQ5JN', is_primary_owner, false).
slack_info('U011A1XQ5JN', is_restricted, false).
slack_info('U011A1XQ5JN', is_ultra_restricted, false).
slack_info('U011A1XQ5JN', name, "heiko.angermann").
slack_info('U011A1XQ5JN', presence, "away").
slack_info('U011A1XQ5JN', profile, _{avatar_hash:"094e2a661ac4", display_name:"Heiko Angermann", display_name_normalized:"Heiko Angermann", email:"heiko.angermann@dhbw-heidenheim.de", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_1024.png", image_192:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_192.png", image_24:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_24.png", image_32:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_32.png", image_48:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_48.png", image_512:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_512.png", image_72:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_72.png", image_original:"https://avatars.slack-edge.com/2020-04-08/1046660914402_094e2a661ac43addd28f_original.png", is_custom_image:true, phone:"", real_name:"Heiko Angermann", real_name_normalized:"Heiko Angermann", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011A1XQ5JN', real_name, "Heiko Angermann").
slack_info('U011A1XQ5JN', team_id, "T010WMWBAGY").
slack_info('U011A1XQ5JN', tz, "Europe/Amsterdam").
slack_info('U011A1XQ5JN', tz_label, "Central European Summer Time").
slack_info('U011A1XQ5JN', tz_offset, 7200).
slack_info('U011A1XQ5JN', updated, 1586333804).
slack_info(var, instance, 'U011A2HJEHY').
slack_info('U011A2HJEHY', color, "e06b56").
slack_info('U011A2HJEHY', deleted, false).
slack_info('U011A2HJEHY', id, "U011A2HJEHY").
slack_info('U011A2HJEHY', is_admin, false).
slack_info('U011A2HJEHY', is_app_user, false).
slack_info('U011A2HJEHY', is_bot, false).
slack_info('U011A2HJEHY', is_owner, false).
slack_info('U011A2HJEHY', is_primary_owner, false).
slack_info('U011A2HJEHY', is_restricted, false).
slack_info('U011A2HJEHY', is_ultra_restricted, false).
slack_info('U011A2HJEHY', name, "yasinovskyy").
slack_info('U011A2HJEHY', presence, "away").
slack_info('U011A2HJEHY', profile, _{avatar_hash:"gf2651671335", display_name:"Roman", display_name_normalized:"Roman", email:"yasinovskyy@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/f2651671335b57d18f5d3faee2baec5f.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0021-192.png", image_24:"https://secure.gravatar.com/avatar/f2651671335b57d18f5d3faee2baec5f.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0021-24.png", image_32:"https://secure.gravatar.com/avatar/f2651671335b57d18f5d3faee2baec5f.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0021-32.png", image_48:"https://secure.gravatar.com/avatar/f2651671335b57d18f5d3faee2baec5f.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0021-48.png", image_512:"https://secure.gravatar.com/avatar/f2651671335b57d18f5d3faee2baec5f.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0021-512.png", image_72:"https://secure.gravatar.com/avatar/f2651671335b57d18f5d3faee2baec5f.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0021-72.png", phone:"", real_name:"Roman", real_name_normalized:"Roman", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011A2HJEHY', real_name, "Roman").
slack_info('U011A2HJEHY', team_id, "T010WMWBAGY").
slack_info('U011A2HJEHY', tz, "America/Chicago").
slack_info('U011A2HJEHY', tz_label, "Central Daylight Time").
slack_info('U011A2HJEHY', tz_offset, -18000).
slack_info('U011A2HJEHY', updated, 1586267544).
slack_info(var, instance, 'U011A8S1MCJ').
slack_info('U011A8S1MCJ', color, "e23f99").
slack_info('U011A8S1MCJ', deleted, false).
slack_info('U011A8S1MCJ', id, "U011A8S1MCJ").
slack_info('U011A8S1MCJ', is_admin, false).
slack_info('U011A8S1MCJ', is_app_user, false).
slack_info('U011A8S1MCJ', is_bot, false).
slack_info('U011A8S1MCJ', is_owner, false).
slack_info('U011A8S1MCJ', is_primary_owner, false).
slack_info('U011A8S1MCJ', is_restricted, false).
slack_info('U011A8S1MCJ', is_ultra_restricted, false).
slack_info('U011A8S1MCJ', name, "koeng101").
slack_info('U011A8S1MCJ', presence, "away").
slack_info('U011A8S1MCJ', profile, _{avatar_hash:"gdbd3cc24b09", display_name:"Keoni Gandall", display_name_normalized:"Keoni Gandall", email:"koeng101@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/dbd3cc24b092b17d53c09de6bab7969c.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-192.png", image_24:"https://secure.gravatar.com/avatar/dbd3cc24b092b17d53c09de6bab7969c.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-24.png", image_32:"https://secure.gravatar.com/avatar/dbd3cc24b092b17d53c09de6bab7969c.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-32.png", image_48:"https://secure.gravatar.com/avatar/dbd3cc24b092b17d53c09de6bab7969c.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-48.png", image_512:"https://secure.gravatar.com/avatar/dbd3cc24b092b17d53c09de6bab7969c.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-512.png", image_72:"https://secure.gravatar.com/avatar/dbd3cc24b092b17d53c09de6bab7969c.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-72.png", phone:"", real_name:"Keoni Gandall", real_name_normalized:"Keoni Gandall", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011A8S1MCJ', real_name, "Keoni Gandall").
slack_info('U011A8S1MCJ', team_id, "T010WMWBAGY").
slack_info('U011A8S1MCJ', tz, "America/Los_Angeles").
slack_info('U011A8S1MCJ', tz_label, "Pacific Daylight Time").
slack_info('U011A8S1MCJ', tz_offset, -25200).
slack_info('U011A8S1MCJ', updated, 1586270914).
slack_info(var, instance, 'U011A97HCQ2').
slack_info('U011A97HCQ2', color, "e475df").
slack_info('U011A97HCQ2', deleted, false).
slack_info('U011A97HCQ2', id, "U011A97HCQ2").
slack_info('U011A97HCQ2', is_admin, false).
slack_info('U011A97HCQ2', is_app_user, false).
slack_info('U011A97HCQ2', is_bot, false).
slack_info('U011A97HCQ2', is_owner, false).
slack_info('U011A97HCQ2', is_primary_owner, false).
slack_info('U011A97HCQ2', is_restricted, false).
slack_info('U011A97HCQ2', is_ultra_restricted, false).
slack_info('U011A97HCQ2', name, "daan.v.berkel.1980_pr").
slack_info('U011A97HCQ2', presence, "away").
slack_info('U011A97HCQ2', profile, _{avatar_hash:"391d326e2fde", display_name:"dvberkel", display_name_normalized:"dvberkel", email:"daan.v.berkel.1980+prolog@gmail.com", fields:[], first_name:"Daan", image_1024:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1050804858773_391d326e2fde7aa85dd2_original.jpg", is_custom_image:true, last_name:"van Berkel", phone:"", real_name:"Daan van Berkel", real_name_normalized:"Daan van Berkel", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:"Inspired by life, the universe and everything"}).
slack_info('U011A97HCQ2', real_name, "Daan van Berkel").
slack_info('U011A97HCQ2', team_id, "T010WMWBAGY").
slack_info('U011A97HCQ2', tz, "Europe/Amsterdam").
slack_info('U011A97HCQ2', tz_label, "Central European Summer Time").
slack_info('U011A97HCQ2', tz_offset, 7200).
slack_info('U011A97HCQ2', updated, 1586271591).
slack_info(var, instance, 'U011AMX30FL').
slack_info('U011AMX30FL', color, "e85d72").
slack_info('U011AMX30FL', deleted, false).
slack_info('U011AMX30FL', id, "U011AMX30FL").
slack_info('U011AMX30FL', is_admin, false).
slack_info('U011AMX30FL', is_app_user, false).
slack_info('U011AMX30FL', is_bot, false).
slack_info('U011AMX30FL', is_owner, false).
slack_info('U011AMX30FL', is_primary_owner, false).
slack_info('U011AMX30FL', is_restricted, false).
slack_info('U011AMX30FL', is_ultra_restricted, false).
slack_info('U011AMX30FL', name, "fgiraffe").
slack_info('U011AMX30FL', presence, "away").
slack_info('U011AMX30FL', profile, _{avatar_hash:"6939a209b8d9", display_name:"Frank Giraffe", display_name_normalized:"Frank Giraffe", email:"fgiraffe@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_1024.png", image_192:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_192.png", image_24:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_24.png", image_32:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_32.png", image_48:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_48.png", image_512:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_512.png", image_72:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_72.png", image_original:"https://avatars.slack-edge.com/2020-04-07/1049492644678_6939a209b8d9a7d257e9_original.png", is_custom_image:true, phone:"", real_name:"Frank Giraffe", real_name_normalized:"Frank Giraffe", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011AMX30FL', real_name, "Frank Giraffe").
slack_info('U011AMX30FL', team_id, "T010WMWBAGY").
slack_info('U011AMX30FL', tz, "America/Los_Angeles").
slack_info('U011AMX30FL', tz_label, "Pacific Daylight Time").
slack_info('U011AMX30FL', tz_offset, -25200).
slack_info('U011AMX30FL', updated, 1586278290).
slack_info(var, instance, 'U011BHGS69M').
slack_info('U011BHGS69M', color, "e96699").
slack_info('U011BHGS69M', deleted, false).
slack_info('U011BHGS69M', id, "U011BHGS69M").
slack_info('U011BHGS69M', is_admin, false).
slack_info('U011BHGS69M', is_app_user, false).
slack_info('U011BHGS69M', is_bot, false).
slack_info('U011BHGS69M', is_owner, false).
slack_info('U011BHGS69M', is_primary_owner, false).
slack_info('U011BHGS69M', is_restricted, false).
slack_info('U011BHGS69M', is_ultra_restricted, false).
slack_info('U011BHGS69M', name, "-").
slack_info('U011BHGS69M', presence, "away").
slack_info('U011BHGS69M', profile, _{avatar_hash:"58e98c991c54", display_name:"Abe", display_name_normalized:"Abe", email:"_@abevoelker.com", fields:[], first_name:"Abe", image_1024:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1049174083094_58e98c991c54c8ef12e9_original.jpg", is_custom_image:true, last_name:"Voelker", phone:"", real_name:"Abe Voelker", real_name_normalized:"Abe Voelker", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011BHGS69M', real_name, "Abe Voelker").
slack_info('U011BHGS69M', team_id, "T010WMWBAGY").
slack_info('U011BHGS69M', tz, "America/Chicago").
slack_info('U011BHGS69M', tz_label, "Central Daylight Time").
slack_info('U011BHGS69M', tz_offset, -18000).
slack_info('U011BHGS69M', updated, 1586263057).
slack_info(var, instance, 'U011BHHPHST').
slack_info('U011BHHPHST', color, "2b6836").
slack_info('U011BHHPHST', deleted, false).
slack_info('U011BHHPHST', id, "U011BHHPHST").
slack_info('U011BHHPHST', is_admin, false).
slack_info('U011BHHPHST', is_app_user, false).
slack_info('U011BHHPHST', is_bot, false).
slack_info('U011BHHPHST', is_owner, false).
slack_info('U011BHHPHST', is_primary_owner, false).
slack_info('U011BHHPHST', is_restricted, false).
slack_info('U011BHHPHST', is_ultra_restricted, false).
slack_info('U011BHHPHST', name, "alvinng15").
slack_info('U011BHHPHST', presence, "away").
slack_info('U011BHHPHST', profile, _{avatar_hash:"gff5a6c73495", display_name:"", display_name_normalized:"", email:"alvinng15@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/ff5a6c73495eaebee9593b5082425858.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-192.png", image_24:"https://secure.gravatar.com/avatar/ff5a6c73495eaebee9593b5082425858.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-24.png", image_32:"https://secure.gravatar.com/avatar/ff5a6c73495eaebee9593b5082425858.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-32.png", image_48:"https://secure.gravatar.com/avatar/ff5a6c73495eaebee9593b5082425858.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-48.png", image_512:"https://secure.gravatar.com/avatar/ff5a6c73495eaebee9593b5082425858.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-512.png", image_72:"https://secure.gravatar.com/avatar/ff5a6c73495eaebee9593b5082425858.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-72.png", phone:"", real_name:"Alvin", real_name_normalized:"Alvin", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011BHHPHST', real_name, "Alvin").
slack_info('U011BHHPHST', team_id, "T010WMWBAGY").
slack_info('U011BHHPHST', tz, "Asia/Kuala_Lumpur").
slack_info('U011BHHPHST', tz_label, "Singapore Standard Time").
slack_info('U011BHHPHST', tz_offset, 28800).
slack_info('U011BHHPHST', updated, 1586262066).
slack_info(var, instance, 'U011BHN1971').
slack_info('U011BHN1971', color, "5a4592").
slack_info('U011BHN1971', deleted, false).
slack_info('U011BHN1971', id, "U011BHN1971").
slack_info('U011BHN1971', is_admin, false).
slack_info('U011BHN1971', is_app_user, false).
slack_info('U011BHN1971', is_bot, false).
slack_info('U011BHN1971', is_owner, false).
slack_info('U011BHN1971', is_primary_owner, false).
slack_info('U011BHN1971', is_restricted, false).
slack_info('U011BHN1971', is_ultra_restricted, false).
slack_info('U011BHN1971', name, "stephynb").
slack_info('U011BHN1971', presence, "away").
slack_info('U011BHN1971', profile, _{avatar_hash:"49dd028f9955", display_name:"Stephyn Butcher", display_name_normalized:"Stephyn Butcher", email:"stephynb@gmail.com", fields:[], first_name:"Stephyn", image_1024:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_1024.png", image_192:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_192.png", image_24:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_24.png", image_32:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_32.png", image_48:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_48.png", image_512:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_512.png", image_72:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_72.png", image_original:"https://avatars.slack-edge.com/2020-04-07/1051192703061_49dd028f9955d3cf4143_original.png", is_custom_image:true, last_name:"Butcher", phone:"", real_name:"Stephyn Butcher", real_name_normalized:"Stephyn Butcher", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:"Data Chef @ PXY Data"}).
slack_info('U011BHN1971', real_name, "Stephyn Butcher").
slack_info('U011BHN1971', team_id, "T010WMWBAGY").
slack_info('U011BHN1971', tz, "America/New_York").
slack_info('U011BHN1971', tz_label, "Eastern Daylight Time").
slack_info('U011BHN1971', tz_offset, -14400).
slack_info('U011BHN1971', updated, 1586282414).
slack_info(var, instance, 'U011BHT4Y31').
slack_info('U011BHT4Y31', color, "9e3997").
slack_info('U011BHT4Y31', deleted, false).
slack_info('U011BHT4Y31', id, "U011BHT4Y31").
slack_info('U011BHT4Y31', is_admin, false).
slack_info('U011BHT4Y31', is_app_user, false).
slack_info('U011BHT4Y31', is_bot, false).
slack_info('U011BHT4Y31', is_owner, false).
slack_info('U011BHT4Y31', is_primary_owner, false).
slack_info('U011BHT4Y31', is_restricted, false).
slack_info('U011BHT4Y31', is_ultra_restricted, false).
slack_info('U011BHT4Y31', name, "lucid9").
slack_info('U011BHT4Y31', presence, "away").
slack_info('U011BHT4Y31', profile, _{avatar_hash:"b5b18b9ca75c", display_name:"Lucian Dragus", display_name_normalized:"Lucian Dragus", email:"lucid9@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_1024.png", image_192:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_192.png", image_24:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_24.png", image_32:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_32.png", image_48:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_48.png", image_512:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_512.png", image_72:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_72.png", image_original:"https://avatars.slack-edge.com/2020-04-07/1051231096465_b5b18b9ca75cc7d3ad70_original.png", is_custom_image:true, phone:"", real_name:"Lucian Dragus", real_name_normalized:"Lucian Dragus", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011BHT4Y31', real_name, "Lucian Dragus").
slack_info('U011BHT4Y31', team_id, "T010WMWBAGY").
slack_info('U011BHT4Y31', tz, "America/Bogota").
slack_info('U011BHT4Y31', tz_label, "South America Pacific Standard Time").
slack_info('U011BHT4Y31', tz_offset, -18000).
slack_info('U011BHT4Y31', updated, 1586265325).
slack_info(var, instance, 'U011BJCDXDM').
slack_info('U011BJCDXDM', color, "3c8c69").
slack_info('U011BJCDXDM', deleted, false).
slack_info('U011BJCDXDM', id, "U011BJCDXDM").
slack_info('U011BJCDXDM', is_admin, false).
slack_info('U011BJCDXDM', is_app_user, false).
slack_info('U011BJCDXDM', is_bot, false).
slack_info('U011BJCDXDM', is_owner, false).
slack_info('U011BJCDXDM', is_primary_owner, false).
slack_info('U011BJCDXDM', is_restricted, false).
slack_info('U011BJCDXDM', is_ultra_restricted, false).
slack_info('U011BJCDXDM', name, "rand.fitzpatrick").
slack_info('U011BJCDXDM', presence, "away").
slack_info('U011BJCDXDM', profile, _{avatar_hash:"g6a912025c15", display_name:"Rand Fitzpatrick", display_name_normalized:"Rand Fitzpatrick", email:"rand.fitzpatrick@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/6a912025c15dbd97306c6efcc4cc5f46.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-192.png", image_24:"https://secure.gravatar.com/avatar/6a912025c15dbd97306c6efcc4cc5f46.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-24.png", image_32:"https://secure.gravatar.com/avatar/6a912025c15dbd97306c6efcc4cc5f46.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-32.png", image_48:"https://secure.gravatar.com/avatar/6a912025c15dbd97306c6efcc4cc5f46.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-48.png", image_512:"https://secure.gravatar.com/avatar/6a912025c15dbd97306c6efcc4cc5f46.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-512.png", image_72:"https://secure.gravatar.com/avatar/6a912025c15dbd97306c6efcc4cc5f46.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-72.png", phone:"", real_name:"Rand Fitzpatrick", real_name_normalized:"Rand Fitzpatrick", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011BJCDXDM', real_name, "Rand Fitzpatrick").
slack_info('U011BJCDXDM', team_id, "T010WMWBAGY").
slack_info('U011BJCDXDM', tz, "America/Denver").
slack_info('U011BJCDXDM', tz_label, "Mountain Daylight Time").
slack_info('U011BJCDXDM', tz_offset, -21600).
slack_info('U011BJCDXDM', updated, 1586269075).
slack_info(var, instance, 'U011BL67NT1').
slack_info('U011BL67NT1', color, "9d8eee").
slack_info('U011BL67NT1', deleted, false).
slack_info('U011BL67NT1', id, "U011BL67NT1").
slack_info('U011BL67NT1', is_admin, false).
slack_info('U011BL67NT1', is_app_user, false).
slack_info('U011BL67NT1', is_bot, false).
slack_info('U011BL67NT1', is_owner, false).
slack_info('U011BL67NT1', is_primary_owner, false).
slack_info('U011BL67NT1', is_restricted, false).
slack_info('U011BL67NT1', is_ultra_restricted, false).
slack_info('U011BL67NT1', name, "eike.spang").
slack_info('U011BL67NT1', presence, "away").
slack_info('U011BL67NT1', profile, _{avatar_hash:"g8533db67b21", display_name:"", display_name_normalized:"", email:"eike.spang@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/8533db67b21527af773b6df7947c2c71.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-192.png", image_24:"https://secure.gravatar.com/avatar/8533db67b21527af773b6df7947c2c71.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-24.png", image_32:"https://secure.gravatar.com/avatar/8533db67b21527af773b6df7947c2c71.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-32.png", image_48:"https://secure.gravatar.com/avatar/8533db67b21527af773b6df7947c2c71.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-48.png", image_512:"https://secure.gravatar.com/avatar/8533db67b21527af773b6df7947c2c71.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-512.png", image_72:"https://secure.gravatar.com/avatar/8533db67b21527af773b6df7947c2c71.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-72.png", phone:"", real_name:"Eike Spang", real_name_normalized:"Eike Spang", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011BL67NT1', real_name, "Eike Spang").
slack_info('U011BL67NT1', team_id, "T010WMWBAGY").
slack_info('U011BL67NT1', tz, "Europe/London").
slack_info('U011BL67NT1', tz_label, "British Summer Time").
slack_info('U011BL67NT1', tz_offset, 3600).
slack_info('U011BL67NT1', updated, 1586284344).
slack_info(var, instance, 'U011BP5EN4X').
slack_info('U011BP5EN4X', color, "e96699").
slack_info('U011BP5EN4X', deleted, false).
slack_info('U011BP5EN4X', id, "U011BP5EN4X").
slack_info('U011BP5EN4X', is_admin, false).
slack_info('U011BP5EN4X', is_app_user, false).
slack_info('U011BP5EN4X', is_bot, false).
slack_info('U011BP5EN4X', is_owner, false).
slack_info('U011BP5EN4X', is_primary_owner, false).
slack_info('U011BP5EN4X', is_restricted, false).
slack_info('U011BP5EN4X', is_ultra_restricted, false).
slack_info('U011BP5EN4X', name, "contact").
slack_info('U011BP5EN4X', presence, "away").
slack_info('U011BP5EN4X', profile, _{avatar_hash:"gd63157fafa1", display_name:"David", display_name_normalized:"David", email:"contact@davidporter.id.au", fields:null, image_192:"https://secure.gravatar.com/avatar/d63157fafa10faa7e96e05f4de8b6d9c.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-192.png", image_24:"https://secure.gravatar.com/avatar/d63157fafa10faa7e96e05f4de8b6d9c.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-24.png", image_32:"https://secure.gravatar.com/avatar/d63157fafa10faa7e96e05f4de8b6d9c.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-32.png", image_48:"https://secure.gravatar.com/avatar/d63157fafa10faa7e96e05f4de8b6d9c.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-48.png", image_512:"https://secure.gravatar.com/avatar/d63157fafa10faa7e96e05f4de8b6d9c.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-512.png", image_72:"https://secure.gravatar.com/avatar/d63157fafa10faa7e96e05f4de8b6d9c.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-72.png", phone:"", real_name:"David", real_name_normalized:"David", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011BP5EN4X', real_name, "David").
slack_info('U011BP5EN4X', team_id, "T010WMWBAGY").
slack_info('U011BP5EN4X', tz, "America/Los_Angeles").
slack_info('U011BP5EN4X', tz_label, "Pacific Daylight Time").
slack_info('U011BP5EN4X', tz_offset, -25200).
slack_info('U011BP5EN4X', updated, 1586311052).
slack_info(var, instance, 'U011BQ5L0RM').
slack_info('U011BQ5L0RM', color, "684b6c").
slack_info('U011BQ5L0RM', deleted, false).
slack_info('U011BQ5L0RM', id, "U011BQ5L0RM").
slack_info('U011BQ5L0RM', is_admin, false).
slack_info('U011BQ5L0RM', is_app_user, false).
slack_info('U011BQ5L0RM', is_bot, false).
slack_info('U011BQ5L0RM', is_owner, false).
slack_info('U011BQ5L0RM', is_primary_owner, false).
slack_info('U011BQ5L0RM', is_restricted, false).
slack_info('U011BQ5L0RM', is_ultra_restricted, false).
slack_info('U011BQ5L0RM', name, "kscraja").
slack_info('U011BQ5L0RM', presence, "away").
slack_info('U011BQ5L0RM', profile, _{avatar_hash:"g957dcae7974", display_name:"Chandra Koduru", display_name_normalized:"Chandra Koduru", email:"kscraja@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/957dcae7974cf60bebd964894ca2ac79.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-192.png", image_24:"https://secure.gravatar.com/avatar/957dcae7974cf60bebd964894ca2ac79.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-24.png", image_32:"https://secure.gravatar.com/avatar/957dcae7974cf60bebd964894ca2ac79.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-32.png", image_48:"https://secure.gravatar.com/avatar/957dcae7974cf60bebd964894ca2ac79.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-48.png", image_512:"https://secure.gravatar.com/avatar/957dcae7974cf60bebd964894ca2ac79.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-512.png", image_72:"https://secure.gravatar.com/avatar/957dcae7974cf60bebd964894ca2ac79.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-72.png", phone:"", real_name:"Chandra Koduru", real_name_normalized:"Chandra Koduru", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011BQ5L0RM', real_name, "Chandra Koduru").
slack_info('U011BQ5L0RM', team_id, "T010WMWBAGY").
slack_info('U011BQ5L0RM', tz, "Asia/Kolkata").
slack_info('U011BQ5L0RM', tz_label, "India Standard Time").
slack_info('U011BQ5L0RM', tz_offset, 19800).
slack_info('U011BQ5L0RM', updated, 1586327148).
slack_info(var, instance, 'U011CAED9LP').
slack_info('U011CAED9LP', color, "bb86b7").
slack_info('U011CAED9LP', deleted, false).
slack_info('U011CAED9LP', id, "U011CAED9LP").
slack_info('U011CAED9LP', is_admin, false).
slack_info('U011CAED9LP', is_app_user, false).
slack_info('U011CAED9LP', is_bot, false).
slack_info('U011CAED9LP', is_owner, false).
slack_info('U011CAED9LP', is_primary_owner, false).
slack_info('U011CAED9LP', is_restricted, false).
slack_info('U011CAED9LP', is_ultra_restricted, false).
slack_info('U011CAED9LP', name, "perplexedcoding").
slack_info('U011CAED9LP', presence, "away").
slack_info('U011CAED9LP', profile, _{avatar_hash:"a152cb76da74", display_name:"Pilne", display_name_normalized:"Pilne", email:"perplexedcoding@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_1024.png", image_192:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_192.png", image_24:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_24.png", image_32:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_32.png", image_48:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_48.png", image_512:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_512.png", image_72:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_72.png", image_original:"https://avatars.slack-edge.com/2020-04-09/1062705640961_a152cb76da74d3a49298_original.png", is_custom_image:true, phone:"", real_name:"Pilne", real_name_normalized:"Pilne", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011CAED9LP', real_name, "Pilne").
slack_info('U011CAED9LP', team_id, "T010WMWBAGY").
slack_info('U011CAED9LP', tz, "America/Chicago").
slack_info('U011CAED9LP', tz_label, "Central Daylight Time").
slack_info('U011CAED9LP', tz_offset, -18000).
slack_info('U011CAED9LP', updated, 1586469956).
slack_info(var, instance, 'U011CAHLMGF').
slack_info('U011CAHLMGF', color, "5a4592").
slack_info('U011CAHLMGF', deleted, false).
slack_info('U011CAHLMGF', id, "U011CAHLMGF").
slack_info('U011CAHLMGF', is_admin, false).
slack_info('U011CAHLMGF', is_app_user, false).
slack_info('U011CAHLMGF', is_bot, false).
slack_info('U011CAHLMGF', is_owner, false).
slack_info('U011CAHLMGF', is_primary_owner, false).
slack_info('U011CAHLMGF', is_restricted, false).
slack_info('U011CAHLMGF', is_ultra_restricted, false).
slack_info('U011CAHLMGF', name, "dedgrant").
slack_info('U011CAHLMGF', presence, "away").
slack_info('U011CAHLMGF', profile, _{avatar_hash:"gfe2441e0be5", display_name:"Darren Grant", display_name_normalized:"Darren Grant", email:"dedgrant@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/fe2441e0be5232fdf5a435ed04b10c67.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-192.png", image_24:"https://secure.gravatar.com/avatar/fe2441e0be5232fdf5a435ed04b10c67.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-24.png", image_32:"https://secure.gravatar.com/avatar/fe2441e0be5232fdf5a435ed04b10c67.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-32.png", image_48:"https://secure.gravatar.com/avatar/fe2441e0be5232fdf5a435ed04b10c67.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-48.png", image_512:"https://secure.gravatar.com/avatar/fe2441e0be5232fdf5a435ed04b10c67.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-512.png", image_72:"https://secure.gravatar.com/avatar/fe2441e0be5232fdf5a435ed04b10c67.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-72.png", phone:"", real_name:"Darren Grant", real_name_normalized:"Darren Grant", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011CAHLMGF', real_name, "Darren Grant").
slack_info('U011CAHLMGF', team_id, "T010WMWBAGY").
slack_info('U011CAHLMGF', tz, "America/Los_Angeles").
slack_info('U011CAHLMGF', tz_label, "Pacific Daylight Time").
slack_info('U011CAHLMGF', tz_offset, -25200).
slack_info('U011CAHLMGF', updated, 1586470311).
slack_info(var, instance, 'U011CEGJRT4').
slack_info('U011CEGJRT4', color, "2b6836").
slack_info('U011CEGJRT4', deleted, false).
slack_info('U011CEGJRT4', id, "U011CEGJRT4").
slack_info('U011CEGJRT4', is_admin, false).
slack_info('U011CEGJRT4', is_app_user, false).
slack_info('U011CEGJRT4', is_bot, false).
slack_info('U011CEGJRT4', is_owner, false).
slack_info('U011CEGJRT4', is_primary_owner, false).
slack_info('U011CEGJRT4', is_restricted, false).
slack_info('U011CEGJRT4', is_ultra_restricted, false).
slack_info('U011CEGJRT4', name, "mathijs.claassen").
slack_info('U011CEGJRT4', presence, "away").
slack_info('U011CEGJRT4', profile, _{avatar_hash:"g57786800758", display_name:"Mathijs C", display_name_normalized:"Mathijs C", email:"mathijs.claassen@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/57786800758fa14d4d3e4dd86f011bd8.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-192.png", image_24:"https://secure.gravatar.com/avatar/57786800758fa14d4d3e4dd86f011bd8.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-24.png", image_32:"https://secure.gravatar.com/avatar/57786800758fa14d4d3e4dd86f011bd8.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-32.png", image_48:"https://secure.gravatar.com/avatar/57786800758fa14d4d3e4dd86f011bd8.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-48.png", image_512:"https://secure.gravatar.com/avatar/57786800758fa14d4d3e4dd86f011bd8.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-512.png", image_72:"https://secure.gravatar.com/avatar/57786800758fa14d4d3e4dd86f011bd8.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-72.png", phone:"", real_name:"Mathijs C", real_name_normalized:"Mathijs C", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011CEGJRT4', real_name, "Mathijs C").
slack_info('U011CEGJRT4', team_id, "T010WMWBAGY").
slack_info('U011CEGJRT4', tz, "Europe/Amsterdam").
slack_info('U011CEGJRT4', tz_label, "Central European Summer Time").
slack_info('U011CEGJRT4', tz_offset, 7200).
slack_info('U011CEGJRT4', updated, 1586329501).
slack_info(var, instance, 'U011CEPL7GT').
slack_info('U011CEPL7GT', color, "73769d").
slack_info('U011CEPL7GT', deleted, false).
slack_info('U011CEPL7GT', id, "U011CEPL7GT").
slack_info('U011CEPL7GT', is_admin, false).
slack_info('U011CEPL7GT', is_app_user, false).
slack_info('U011CEPL7GT', is_bot, false).
slack_info('U011CEPL7GT', is_owner, false).
slack_info('U011CEPL7GT', is_primary_owner, false).
slack_info('U011CEPL7GT', is_restricted, false).
slack_info('U011CEPL7GT', is_ultra_restricted, false).
slack_info('U011CEPL7GT', name, "dijkstra.arjen").
slack_info('U011CEPL7GT', presence, "away").
slack_info('U011CEPL7GT', profile, _{avatar_hash:"932acf64c769", display_name:"Arjen Dijkstra", display_name_normalized:"Arjen Dijkstra", email:"dijkstra.arjen@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-09/1060092174756_932acf64c76965ba6b90_original.jpg", is_custom_image:true, phone:"", real_name:"Arjen Dijkstra", real_name_normalized:"Arjen Dijkstra", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011CEPL7GT', real_name, "Arjen Dijkstra").
slack_info('U011CEPL7GT', team_id, "T010WMWBAGY").
slack_info('U011CEPL7GT', tz, "Europe/Amsterdam").
slack_info('U011CEPL7GT', tz_label, "Central European Summer Time").
slack_info('U011CEPL7GT', tz_offset, 7200).
slack_info('U011CEPL7GT', updated, 1586450013).
slack_info(var, instance, 'U011CF6QXT4').
slack_info('U011CF6QXT4', color, "99a949").
slack_info('U011CF6QXT4', deleted, false).
slack_info('U011CF6QXT4', id, "U011CF6QXT4").
slack_info('U011CF6QXT4', is_admin, false).
slack_info('U011CF6QXT4', is_app_user, false).
slack_info('U011CF6QXT4', is_bot, false).
slack_info('U011CF6QXT4', is_owner, false).
slack_info('U011CF6QXT4', is_primary_owner, false).
slack_info('U011CF6QXT4', is_restricted, false).
slack_info('U011CF6QXT4', is_ultra_restricted, false).
slack_info('U011CF6QXT4', name, "nimurphy").
slack_info('U011CF6QXT4', presence, "away").
slack_info('U011CF6QXT4', profile, _{avatar_hash:"gcac3b9e4941", display_name:"", display_name_normalized:"", email:"nimurphy@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/cac3b9e49415330fe95cde52d15cbd49.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0014-192.png", image_24:"https://secure.gravatar.com/avatar/cac3b9e49415330fe95cde52d15cbd49.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0014-24.png", image_32:"https://secure.gravatar.com/avatar/cac3b9e49415330fe95cde52d15cbd49.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0014-32.png", image_48:"https://secure.gravatar.com/avatar/cac3b9e49415330fe95cde52d15cbd49.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0014-48.png", image_512:"https://secure.gravatar.com/avatar/cac3b9e49415330fe95cde52d15cbd49.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0014-512.png", image_72:"https://secure.gravatar.com/avatar/cac3b9e49415330fe95cde52d15cbd49.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0014-72.png", phone:"", real_name:"Niall Murphy", real_name_normalized:"Niall Murphy", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011CF6QXT4', real_name, "Niall Murphy").
slack_info('U011CF6QXT4', team_id, "T010WMWBAGY").
slack_info('U011CF6QXT4', tz, "Europe/London").
slack_info('U011CF6QXT4', tz_label, "British Summer Time").
slack_info('U011CF6QXT4', tz_offset, 3600).
slack_info('U011CF6QXT4', updated, 1586330183).
slack_info(var, instance, 'U011D15H1T5').
slack_info('U011D15H1T5', color, "674b1b").
slack_info('U011D15H1T5', deleted, false).
slack_info('U011D15H1T5', id, "U011D15H1T5").
slack_info('U011D15H1T5', is_admin, false).
slack_info('U011D15H1T5', is_app_user, false).
slack_info('U011D15H1T5', is_bot, false).
slack_info('U011D15H1T5', is_owner, false).
slack_info('U011D15H1T5', is_primary_owner, false).
slack_info('U011D15H1T5', is_restricted, false).
slack_info('U011D15H1T5', is_ultra_restricted, false).
slack_info('U011D15H1T5', name, "anthony").
slack_info('U011D15H1T5', presence, "away").
slack_info('U011D15H1T5', profile, _{avatar_hash:"16d2b3d3ae9f", display_name:"Tony", display_name_normalized:"Tony", email:"anthony@anthonymiller.tech", fields:null, first_name:"Tony", image_1024:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1049929173494_16d2b3d3ae9f29db2103_original.jpg", is_custom_image:true, last_name:"Miller", phone:"", real_name:"Tony Miller", real_name_normalized:"Tony Miller", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:"Software Engineer (Node/React) "}).
slack_info('U011D15H1T5', real_name, "Tony Miller").
slack_info('U011D15H1T5', team_id, "T010WMWBAGY").
slack_info('U011D15H1T5', tz, "America/New_York").
slack_info('U011D15H1T5', tz_label, "Eastern Daylight Time").
slack_info('U011D15H1T5', tz_offset, -14400).
slack_info('U011D15H1T5', updated, 1586306287).
slack_info(var, instance, 'U011DD9K459').
slack_info('U011DD9K459', color, "e0a729").
slack_info('U011DD9K459', deleted, false).
slack_info('U011DD9K459', id, "U011DD9K459").
slack_info('U011DD9K459', is_admin, false).
slack_info('U011DD9K459', is_app_user, false).
slack_info('U011DD9K459', is_bot, false).
slack_info('U011DD9K459', is_owner, false).
slack_info('U011DD9K459', is_primary_owner, false).
slack_info('U011DD9K459', is_restricted, false).
slack_info('U011DD9K459', is_ultra_restricted, false).
slack_info('U011DD9K459', name, "mgondan77").
slack_info('U011DD9K459', presence, "away").
slack_info('U011DD9K459', profile, _{avatar_hash:"g112dbc3412e", display_name:"Matthias", display_name_normalized:"Matthias", email:"mgondan77@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/112dbc3412ec0d602d06bfb3cd366cb1.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-192.png", image_24:"https://secure.gravatar.com/avatar/112dbc3412ec0d602d06bfb3cd366cb1.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-24.png", image_32:"https://secure.gravatar.com/avatar/112dbc3412ec0d602d06bfb3cd366cb1.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-32.png", image_48:"https://secure.gravatar.com/avatar/112dbc3412ec0d602d06bfb3cd366cb1.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-48.png", image_512:"https://secure.gravatar.com/avatar/112dbc3412ec0d602d06bfb3cd366cb1.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-512.png", image_72:"https://secure.gravatar.com/avatar/112dbc3412ec0d602d06bfb3cd366cb1.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-72.png", phone:"", real_name:"Matthias", real_name_normalized:"Matthias", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011DD9K459', real_name, "Matthias").
slack_info('U011DD9K459', team_id, "T010WMWBAGY").
slack_info('U011DD9K459', tz, "Europe/Amsterdam").
slack_info('U011DD9K459', tz_label, "Central European Summer Time").
slack_info('U011DD9K459', tz_offset, 7200).
slack_info('U011DD9K459', updated, 1586325132).
slack_info(var, instance, 'U011E5YEVD0').
slack_info('U011E5YEVD0', color, "9b3b45").
slack_info('U011E5YEVD0', deleted, false).
slack_info('U011E5YEVD0', id, "U011E5YEVD0").
slack_info('U011E5YEVD0', is_admin, false).
slack_info('U011E5YEVD0', is_app_user, false).
slack_info('U011E5YEVD0', is_bot, false).
slack_info('U011E5YEVD0', is_owner, false).
slack_info('U011E5YEVD0', is_primary_owner, false).
slack_info('U011E5YEVD0', is_restricted, false).
slack_info('U011E5YEVD0', is_ultra_restricted, false).
slack_info('U011E5YEVD0', name, "gilbertbgarza").
slack_info('U011E5YEVD0', presence, "away").
slack_info('U011E5YEVD0', profile, _{avatar_hash:"g44d05cf6268", display_name:"Gilbert", display_name_normalized:"Gilbert", email:"gilbertbgarza@gmail.com", fields:[], first_name:"Gilbert", image_192:"https://secure.gravatar.com/avatar/44d05cf62687c3489b863e4677414ca6.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-192.png", image_24:"https://secure.gravatar.com/avatar/44d05cf62687c3489b863e4677414ca6.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-24.png", image_32:"https://secure.gravatar.com/avatar/44d05cf62687c3489b863e4677414ca6.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-32.png", image_48:"https://secure.gravatar.com/avatar/44d05cf62687c3489b863e4677414ca6.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-48.png", image_512:"https://secure.gravatar.com/avatar/44d05cf62687c3489b863e4677414ca6.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-512.png", image_72:"https://secure.gravatar.com/avatar/44d05cf62687c3489b863e4677414ca6.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-72.png", last_name:"", phone:"", real_name:"Gilbert", real_name_normalized:"Gilbert", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:"https://pubb.substack.com"}).
slack_info('U011E5YEVD0', real_name, "Gilbert").
slack_info('U011E5YEVD0', team_id, "T010WMWBAGY").
slack_info('U011E5YEVD0', tz, "America/Los_Angeles").
slack_info('U011E5YEVD0', tz_label, "Pacific Daylight Time").
slack_info('U011E5YEVD0', tz_offset, -25200).
slack_info('U011E5YEVD0', updated, 1586495820).
slack_info(var, instance, 'U011F5LT91C').
slack_info('U011F5LT91C', color, "d58247").
slack_info('U011F5LT91C', deleted, false).
slack_info('U011F5LT91C', id, "U011F5LT91C").
slack_info('U011F5LT91C', is_admin, false).
slack_info('U011F5LT91C', is_app_user, false).
slack_info('U011F5LT91C', is_bot, false).
slack_info('U011F5LT91C', is_owner, false).
slack_info('U011F5LT91C', is_primary_owner, false).
slack_info('U011F5LT91C', is_restricted, false).
slack_info('U011F5LT91C', is_ultra_restricted, false).
slack_info('U011F5LT91C', name, "tamir.bahar").
slack_info('U011F5LT91C', presence, "away").
slack_info('U011F5LT91C', profile, _{avatar_hash:"13ce838e7e15", display_name:"Tamir Bahar", display_name_normalized:"Tamir Bahar", email:"tamir.bahar@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1045562515539_13ce838e7e159038404f_original.jpg", is_custom_image:true, phone:"", real_name:"Tamir Bahar", real_name_normalized:"Tamir Bahar", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011F5LT91C', real_name, "Tamir Bahar").
slack_info('U011F5LT91C', team_id, "T010WMWBAGY").
slack_info('U011F5LT91C', tz, "Asia/Jerusalem").
slack_info('U011F5LT91C', tz_label, "Israel Daylight Time").
slack_info('U011F5LT91C', tz_offset, 10800).
slack_info('U011F5LT91C', updated, 1586273775).
slack_info(var, instance, 'U011F5NMK1C').
slack_info('U011F5NMK1C', color, "bb86b7").
slack_info('U011F5NMK1C', deleted, false).
slack_info('U011F5NMK1C', id, "U011F5NMK1C").
slack_info('U011F5NMK1C', is_admin, false).
slack_info('U011F5NMK1C', is_app_user, false).
slack_info('U011F5NMK1C', is_bot, false).
slack_info('U011F5NMK1C', is_owner, false).
slack_info('U011F5NMK1C', is_primary_owner, false).
slack_info('U011F5NMK1C', is_restricted, false).
slack_info('U011F5NMK1C', is_ultra_restricted, false).
slack_info('U011F5NMK1C', name, "lee.yi.jie.joel").
slack_info('U011F5NMK1C', presence, "away").
slack_info('U011F5NMK1C', profile, _{avatar_hash:"gfa549aca537", display_name:"Joel Lee", display_name_normalized:"Joel Lee", email:"lee.yi.jie.joel@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/fa549aca53739ab4e97bde3cd6a6bed5.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-192.png", image_24:"https://secure.gravatar.com/avatar/fa549aca53739ab4e97bde3cd6a6bed5.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-24.png", image_32:"https://secure.gravatar.com/avatar/fa549aca53739ab4e97bde3cd6a6bed5.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-32.png", image_48:"https://secure.gravatar.com/avatar/fa549aca53739ab4e97bde3cd6a6bed5.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-48.png", image_512:"https://secure.gravatar.com/avatar/fa549aca53739ab4e97bde3cd6a6bed5.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-512.png", image_72:"https://secure.gravatar.com/avatar/fa549aca53739ab4e97bde3cd6a6bed5.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-72.png", phone:"", real_name:"Joel Lee", real_name_normalized:"Joel Lee", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011F5NMK1C', real_name, "Joel Lee").
slack_info('U011F5NMK1C', team_id, "T010WMWBAGY").
slack_info('U011F5NMK1C', tz, "Asia/Kuala_Lumpur").
slack_info('U011F5NMK1C', tz_label, "Singapore Standard Time").
slack_info('U011F5NMK1C', tz_offset, 28800).
slack_info('U011F5NMK1C', updated, 1586263196).
slack_info(var, instance, 'U011F63HYFQ').
slack_info('U011F63HYFQ', color, "db3150").
slack_info('U011F63HYFQ', deleted, false).
slack_info('U011F63HYFQ', id, "U011F63HYFQ").
slack_info('U011F63HYFQ', is_admin, false).
slack_info('U011F63HYFQ', is_app_user, false).
slack_info('U011F63HYFQ', is_bot, false).
slack_info('U011F63HYFQ', is_owner, false).
slack_info('U011F63HYFQ', is_primary_owner, false).
slack_info('U011F63HYFQ', is_restricted, false).
slack_info('U011F63HYFQ', is_ultra_restricted, false).
slack_info('U011F63HYFQ', name, "g").
slack_info('U011F63HYFQ', presence, "away").
slack_info('U011F63HYFQ', profile, _{avatar_hash:"b29c8c21e4ee", display_name:"Gustav", display_name_normalized:"Gustav", email:"g@gu.pe", fields:null, first_name:"Gustav", image_1024:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1049287099030_b29c8c21e4eeb1799aaf_original.jpg", is_custom_image:true, last_name:"Melck", phone:"", real_name:"Gustav Melck", real_name_normalized:"Gustav Melck", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011F63HYFQ', real_name, "Gustav Melck").
slack_info('U011F63HYFQ', team_id, "T010WMWBAGY").
slack_info('U011F63HYFQ', tz, "Africa/Harare").
slack_info('U011F63HYFQ', tz_label, "Central Africa Time").
slack_info('U011F63HYFQ', tz_offset, 7200).
slack_info('U011F63HYFQ', updated, 1586268238).
slack_info(var, instance, 'U011F6CKX46').
slack_info('U011F6CKX46', color, "235e5b").
slack_info('U011F6CKX46', deleted, false).
slack_info('U011F6CKX46', id, "U011F6CKX46").
slack_info('U011F6CKX46', is_admin, false).
slack_info('U011F6CKX46', is_app_user, false).
slack_info('U011F6CKX46', is_bot, false).
slack_info('U011F6CKX46', is_owner, false).
slack_info('U011F6CKX46', is_primary_owner, false).
slack_info('U011F6CKX46', is_restricted, false).
slack_info('U011F6CKX46', is_ultra_restricted, false).
slack_info('U011F6CKX46', name, "minhnhdo").
slack_info('U011F6CKX46', presence, "away").
slack_info('U011F6CKX46', profile, _{avatar_hash:"g36eb3ff17c5", display_name:"", display_name_normalized:"", email:"minhnhdo@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/36eb3ff17c5f928c0a177f401badb386.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-192.png", image_24:"https://secure.gravatar.com/avatar/36eb3ff17c5f928c0a177f401badb386.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-24.png", image_32:"https://secure.gravatar.com/avatar/36eb3ff17c5f928c0a177f401badb386.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-32.png", image_48:"https://secure.gravatar.com/avatar/36eb3ff17c5f928c0a177f401badb386.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-48.png", image_512:"https://secure.gravatar.com/avatar/36eb3ff17c5f928c0a177f401badb386.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-512.png", image_72:"https://secure.gravatar.com/avatar/36eb3ff17c5f928c0a177f401badb386.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-72.png", phone:"", real_name:"Matthew", real_name_normalized:"Matthew", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011F6CKX46', real_name, "Matthew").
slack_info('U011F6CKX46', team_id, "T010WMWBAGY").
slack_info('U011F6CKX46', tz, "America/New_York").
slack_info('U011F6CKX46', tz_label, "Eastern Daylight Time").
slack_info('U011F6CKX46', tz_offset, -14400).
slack_info('U011F6CKX46', updated, 1586264618).
slack_info(var, instance, 'U011F6R5ZML').
slack_info('U011F6R5ZML', color, "c386df").
slack_info('U011F6R5ZML', deleted, false).
slack_info('U011F6R5ZML', id, "U011F6R5ZML").
slack_info('U011F6R5ZML', is_admin, false).
slack_info('U011F6R5ZML', is_app_user, false).
slack_info('U011F6R5ZML', is_bot, false).
slack_info('U011F6R5ZML', is_owner, false).
slack_info('U011F6R5ZML', is_primary_owner, false).
slack_info('U011F6R5ZML', is_restricted, false).
slack_info('U011F6R5ZML', is_ultra_restricted, false).
slack_info('U011F6R5ZML', name, "yolaine.bourda").
slack_info('U011F6R5ZML', presence, "away").
slack_info('U011F6R5ZML', profile, _{avatar_hash:"g1d510b7c106", display_name:"Yolaine", display_name_normalized:"Yolaine", email:"yolaine.bourda@wanadoo.fr", fields:null, image_192:"https://secure.gravatar.com/avatar/1d510b7c1065e0b2e95075a4f0207063.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-192.png", image_24:"https://secure.gravatar.com/avatar/1d510b7c1065e0b2e95075a4f0207063.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-24.png", image_32:"https://secure.gravatar.com/avatar/1d510b7c1065e0b2e95075a4f0207063.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-32.png", image_48:"https://secure.gravatar.com/avatar/1d510b7c1065e0b2e95075a4f0207063.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-48.png", image_512:"https://secure.gravatar.com/avatar/1d510b7c1065e0b2e95075a4f0207063.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-512.png", image_72:"https://secure.gravatar.com/avatar/1d510b7c1065e0b2e95075a4f0207063.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-72.png", phone:"", real_name:"Yolaine", real_name_normalized:"Yolaine", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011F6R5ZML', real_name, "Yolaine").
slack_info('U011F6R5ZML', team_id, "T010WMWBAGY").
slack_info('U011F6R5ZML', tz, "Europe/Brussels").
slack_info('U011F6R5ZML', tz_label, "Central European Summer Time").
slack_info('U011F6R5ZML', tz_offset, 7200).
slack_info('U011F6R5ZML', updated, 1586265314).
slack_info(var, instance, 'U011F6ZP8S2').
slack_info('U011F6ZP8S2', color, "a63024").
slack_info('U011F6ZP8S2', deleted, false).
slack_info('U011F6ZP8S2', id, "U011F6ZP8S2").
slack_info('U011F6ZP8S2', is_admin, false).
slack_info('U011F6ZP8S2', is_app_user, false).
slack_info('U011F6ZP8S2', is_bot, false).
slack_info('U011F6ZP8S2', is_owner, false).
slack_info('U011F6ZP8S2', is_primary_owner, false).
slack_info('U011F6ZP8S2', is_restricted, false).
slack_info('U011F6ZP8S2', is_ultra_restricted, false).
slack_info('U011F6ZP8S2', name, "brendan").
slack_info('U011F6ZP8S2', presence, "away").
slack_info('U011F6ZP8S2', profile, _{avatar_hash:"2d76eb68e694", display_name:"Brendan Molloy", display_name_normalized:"Brendan Molloy", email:"brendan@bbqsrc.net", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1050611767605_2d76eb68e69450dc6e22_original.jpg", is_custom_image:true, phone:"", real_name:"Brendan Molloy", real_name_normalized:"Brendan Molloy", skype:"", status_emoji:":flag-se:", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011F6ZP8S2', real_name, "Brendan Molloy").
slack_info('U011F6ZP8S2', team_id, "T010WMWBAGY").
slack_info('U011F6ZP8S2', tz, "Europe/Amsterdam").
slack_info('U011F6ZP8S2', tz_label, "Central European Summer Time").
slack_info('U011F6ZP8S2', tz_offset, 7200).
slack_info('U011F6ZP8S2', updated, 1586272992).
slack_info(var, instance, 'U011F7VBCCE').
slack_info('U011F7VBCCE', color, "43761b").
slack_info('U011F7VBCCE', deleted, false).
slack_info('U011F7VBCCE', id, "U011F7VBCCE").
slack_info('U011F7VBCCE', is_admin, false).
slack_info('U011F7VBCCE', is_app_user, false).
slack_info('U011F7VBCCE', is_bot, false).
slack_info('U011F7VBCCE', is_owner, false).
slack_info('U011F7VBCCE', is_primary_owner, false).
slack_info('U011F7VBCCE', is_restricted, false).
slack_info('U011F7VBCCE', is_ultra_restricted, false).
slack_info('U011F7VBCCE', name, "jesse").
slack_info('U011F7VBCCE', presence, "away").
slack_info('U011F7VBCCE', profile, _{avatar_hash:"041f88fb82ec", display_name:"esbe", display_name_normalized:"esbe", email:"jesse@toomanybees.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1061820135600_041f88fb82ec28b7a17f_original.jpg", is_custom_image:true, phone:"", real_name:"esbe", real_name_normalized:"esbe", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011F7VBCCE', real_name, "esbe").
slack_info('U011F7VBCCE', team_id, "T010WMWBAGY").
slack_info('U011F7VBCCE', tz, "America/New_York").
slack_info('U011F7VBCCE', tz_label, "Eastern Daylight Time").
slack_info('U011F7VBCCE', tz_offset, -14400).
slack_info('U011F7VBCCE', updated, 1586268088).
slack_info(var, instance, 'U011FBA5FPG').
slack_info('U011FBA5FPG', color, "7d414c").
slack_info('U011FBA5FPG', deleted, false).
slack_info('U011FBA5FPG', id, "U011FBA5FPG").
slack_info('U011FBA5FPG', is_admin, false).
slack_info('U011FBA5FPG', is_app_user, false).
slack_info('U011FBA5FPG', is_bot, false).
slack_info('U011FBA5FPG', is_owner, false).
slack_info('U011FBA5FPG', is_primary_owner, false).
slack_info('U011FBA5FPG', is_restricted, false).
slack_info('U011FBA5FPG', is_ultra_restricted, false).
slack_info('U011FBA5FPG', name, "p.meled.in").
slack_info('U011FBA5FPG', presence, "away").
slack_info('U011FBA5FPG', profile, _{avatar_hash:"1a5ae3ec46e3", display_name:"Pavel Meledin", display_name_normalized:"Pavel Meledin", email:"p.meled.in@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_1024.png", image_192:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_192.png", image_24:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_24.png", image_32:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_32.png", image_48:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_48.png", image_512:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_512.png", image_72:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_72.png", image_original:"https://avatars.slack-edge.com/2020-04-07/1045515274899_1a5ae3ec46e373c643d6_original.png", is_custom_image:true, phone:"", real_name:"Pavel Meledin", real_name_normalized:"Pavel Meledin", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011FBA5FPG', real_name, "Pavel Meledin").
slack_info('U011FBA5FPG', team_id, "T010WMWBAGY").
slack_info('U011FBA5FPG', tz, "Asia/Jerusalem").
slack_info('U011FBA5FPG', tz_label, "Israel Daylight Time").
slack_info('U011FBA5FPG', tz_offset, 10800).
slack_info('U011FBA5FPG', updated, 1586273037).
slack_info(var, instance, 'U011FCJFGB0').
slack_info('U011FCJFGB0', color, "4d5e26").
slack_info('U011FCJFGB0', deleted, false).
slack_info('U011FCJFGB0', id, "U011FCJFGB0").
slack_info('U011FCJFGB0', is_admin, false).
slack_info('U011FCJFGB0', is_app_user, false).
slack_info('U011FCJFGB0', is_bot, false).
slack_info('U011FCJFGB0', is_owner, false).
slack_info('U011FCJFGB0', is_primary_owner, false).
slack_info('U011FCJFGB0', is_restricted, false).
slack_info('U011FCJFGB0', is_ultra_restricted, false).
slack_info('U011FCJFGB0', name, "haasis_noah").
slack_info('U011FCJFGB0', presence, "away").
slack_info('U011FCJFGB0', profile, _{avatar_hash:"gd7f8d0a8465", display_name:"Noah Haasis", display_name_normalized:"Noah Haasis", email:"haasis_noah@yahoo.de", fields:null, image_192:"https://secure.gravatar.com/avatar/d7f8d0a84659a0531b808a41a47a35a8.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-192.png", image_24:"https://secure.gravatar.com/avatar/d7f8d0a84659a0531b808a41a47a35a8.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-24.png", image_32:"https://secure.gravatar.com/avatar/d7f8d0a84659a0531b808a41a47a35a8.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-32.png", image_48:"https://secure.gravatar.com/avatar/d7f8d0a84659a0531b808a41a47a35a8.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-48.png", image_512:"https://secure.gravatar.com/avatar/d7f8d0a84659a0531b808a41a47a35a8.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-512.png", image_72:"https://secure.gravatar.com/avatar/d7f8d0a84659a0531b808a41a47a35a8.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-72.png", phone:"", real_name:"Noah Haasis", real_name_normalized:"Noah Haasis", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011FCJFGB0', real_name, "Noah Haasis").
slack_info('U011FCJFGB0', team_id, "T010WMWBAGY").
slack_info('U011FCJFGB0', tz, "Europe/Amsterdam").
slack_info('U011FCJFGB0', tz_label, "Central European Summer Time").
slack_info('U011FCJFGB0', tz_offset, 7200).
slack_info('U011FCJFGB0', updated, 1586274913).
slack_info(var, instance, 'U011FT66V38').
slack_info('U011FT66V38', color, "3c989f").
slack_info('U011FT66V38', deleted, false).
slack_info('U011FT66V38', id, "U011FT66V38").
slack_info('U011FT66V38', is_admin, false).
slack_info('U011FT66V38', is_app_user, false).
slack_info('U011FT66V38', is_bot, false).
slack_info('U011FT66V38', is_owner, false).
slack_info('U011FT66V38', is_primary_owner, false).
slack_info('U011FT66V38', is_restricted, false).
slack_info('U011FT66V38', is_ultra_restricted, false).
slack_info('U011FT66V38', name, "kai").
slack_info('U011FT66V38', presence, "away").
slack_info('U011FT66V38', profile, _{avatar_hash:"g30966c6e1a5", display_name:"Kai Koenig", display_name_normalized:"Kai Koenig", email:"kai@ventego-creative.co.nz", fields:null, image_192:"https://secure.gravatar.com/avatar/30966c6e1a52faf6b287b59a01195938.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-192.png", image_24:"https://secure.gravatar.com/avatar/30966c6e1a52faf6b287b59a01195938.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-24.png", image_32:"https://secure.gravatar.com/avatar/30966c6e1a52faf6b287b59a01195938.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-32.png", image_48:"https://secure.gravatar.com/avatar/30966c6e1a52faf6b287b59a01195938.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-48.png", image_512:"https://secure.gravatar.com/avatar/30966c6e1a52faf6b287b59a01195938.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-512.png", image_72:"https://secure.gravatar.com/avatar/30966c6e1a52faf6b287b59a01195938.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-72.png", phone:"", real_name:"Kai Koenig", real_name_normalized:"Kai Koenig", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011FT66V38', real_name, "Kai Koenig").
slack_info('U011FT66V38', team_id, "T010WMWBAGY").
slack_info('U011FT66V38', tz, "Pacific/Auckland").
slack_info('U011FT66V38', tz_label, "New Zealand Standard Time").
slack_info('U011FT66V38', tz_offset, 43200).
slack_info('U011FT66V38', updated, 1586305801).
slack_info(var, instance, 'U011GEQMWJZ').
slack_info('U011GEQMWJZ', color, "5b89d5").
slack_info('U011GEQMWJZ', deleted, false).
slack_info('U011GEQMWJZ', id, "U011GEQMWJZ").
slack_info('U011GEQMWJZ', is_admin, false).
slack_info('U011GEQMWJZ', is_app_user, false).
slack_info('U011GEQMWJZ', is_bot, false).
slack_info('U011GEQMWJZ', is_owner, false).
slack_info('U011GEQMWJZ', is_primary_owner, false).
slack_info('U011GEQMWJZ', is_restricted, false).
slack_info('U011GEQMWJZ', is_ultra_restricted, false).
slack_info('U011GEQMWJZ', name, "dbryan.green").
slack_info('U011GEQMWJZ', presence, "away").
slack_info('U011GEQMWJZ', profile, _{avatar_hash:"gd50aca5de10", display_name:"Bryan Green", display_name_normalized:"Bryan Green", email:"dbryan.green@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/d50aca5de10df94a1a772ae616f8a6c6.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-192.png", image_24:"https://secure.gravatar.com/avatar/d50aca5de10df94a1a772ae616f8a6c6.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-24.png", image_32:"https://secure.gravatar.com/avatar/d50aca5de10df94a1a772ae616f8a6c6.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-32.png", image_48:"https://secure.gravatar.com/avatar/d50aca5de10df94a1a772ae616f8a6c6.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-48.png", image_512:"https://secure.gravatar.com/avatar/d50aca5de10df94a1a772ae616f8a6c6.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-512.png", image_72:"https://secure.gravatar.com/avatar/d50aca5de10df94a1a772ae616f8a6c6.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-72.png", phone:"", real_name:"Bryan Green", real_name_normalized:"Bryan Green", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011GEQMWJZ', real_name, "Bryan Green").
slack_info('U011GEQMWJZ', team_id, "T010WMWBAGY").
slack_info('U011GEQMWJZ', tz, "America/Chicago").
slack_info('U011GEQMWJZ', tz_label, "Central Daylight Time").
slack_info('U011GEQMWJZ', tz_offset, -18000).
slack_info('U011GEQMWJZ', updated, 1586262056).
slack_info(var, instance, 'U011GFBHBE1').
slack_info('U011GFBHBE1', color, "9b3b45").
slack_info('U011GFBHBE1', deleted, false).
slack_info('U011GFBHBE1', id, "U011GFBHBE1").
slack_info('U011GFBHBE1', is_admin, false).
slack_info('U011GFBHBE1', is_app_user, false).
slack_info('U011GFBHBE1', is_bot, false).
slack_info('U011GFBHBE1', is_owner, false).
slack_info('U011GFBHBE1', is_primary_owner, false).
slack_info('U011GFBHBE1', is_restricted, false).
slack_info('U011GFBHBE1', is_ultra_restricted, false).
slack_info('U011GFBHBE1', name, "me1").
slack_info('U011GFBHBE1', presence, "away").
slack_info('U011GFBHBE1', profile, _{avatar_hash:"7bf4bc6ab9a8", display_name:"", display_name_normalized:"", email:"me@thedanabrams.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-11/1046646488887_7bf4bc6ab9a826864eb9_original.jpg", is_custom_image:true, phone:"", real_name:"Dan Abrams", real_name_normalized:"Dan Abrams", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011GFBHBE1', real_name, "Dan Abrams").
slack_info('U011GFBHBE1', team_id, "T010WMWBAGY").
slack_info('U011GFBHBE1', tz, "America/New_York").
slack_info('U011GFBHBE1', tz_label, "Eastern Daylight Time").
slack_info('U011GFBHBE1', tz_offset, -14400).
slack_info('U011GFBHBE1', updated, 1586641521).
slack_info(var, instance, 'U011GJME5C5').
slack_info('U011GJME5C5', color, "50a0cf").
slack_info('U011GJME5C5', deleted, false).
slack_info('U011GJME5C5', id, "U011GJME5C5").
slack_info('U011GJME5C5', is_admin, false).
slack_info('U011GJME5C5', is_app_user, false).
slack_info('U011GJME5C5', is_bot, false).
slack_info('U011GJME5C5', is_owner, false).
slack_info('U011GJME5C5', is_primary_owner, false).
slack_info('U011GJME5C5', is_restricted, false).
slack_info('U011GJME5C5', is_ultra_restricted, false).
slack_info('U011GJME5C5', name, "coreyhaines").
slack_info('U011GJME5C5', presence, "away").
slack_info('U011GJME5C5', profile, _{avatar_hash:"g3d7807bb66e", display_name:"corey haines", display_name_normalized:"corey haines", email:"coreyhaines@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/3d7807bb66e1a0c68c73ab2daaa77d8f.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0003-192.png", image_24:"https://secure.gravatar.com/avatar/3d7807bb66e1a0c68c73ab2daaa77d8f.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0003-24.png", image_32:"https://secure.gravatar.com/avatar/3d7807bb66e1a0c68c73ab2daaa77d8f.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0003-32.png", image_48:"https://secure.gravatar.com/avatar/3d7807bb66e1a0c68c73ab2daaa77d8f.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0003-48.png", image_512:"https://secure.gravatar.com/avatar/3d7807bb66e1a0c68c73ab2daaa77d8f.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0003-512.png", image_72:"https://secure.gravatar.com/avatar/3d7807bb66e1a0c68c73ab2daaa77d8f.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0003-72.png", phone:"", real_name:"corey haines", real_name_normalized:"corey haines", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011GJME5C5', real_name, "corey haines").
slack_info('U011GJME5C5', team_id, "T010WMWBAGY").
slack_info('U011GJME5C5', tz, "America/Chicago").
slack_info('U011GJME5C5', tz_label, "Central Daylight Time").
slack_info('U011GJME5C5', tz_offset, -18000).
slack_info('U011GJME5C5', updated, 1586266536).
slack_info(var, instance, 'U011GKNKBNH').
slack_info('U011GKNKBNH', color, "8f4a2b").
slack_info('U011GKNKBNH', deleted, false).
slack_info('U011GKNKBNH', id, "U011GKNKBNH").
slack_info('U011GKNKBNH', is_admin, false).
slack_info('U011GKNKBNH', is_app_user, false).
slack_info('U011GKNKBNH', is_bot, false).
slack_info('U011GKNKBNH', is_owner, false).
slack_info('U011GKNKBNH', is_primary_owner, false).
slack_info('U011GKNKBNH', is_restricted, false).
slack_info('U011GKNKBNH', is_ultra_restricted, false).
slack_info('U011GKNKBNH', name, "n_she").
slack_info('U011GKNKBNH', presence, "away").
slack_info('U011GKNKBNH', profile, _{avatar_hash:"gbd80d814a4a", display_name:"nadia sheikh", display_name_normalized:"nadia sheikh", email:"n_she@encs.concordia.ca", fields:null, image_192:"https://secure.gravatar.com/avatar/bd80d814a4a185e167dbc60e44f72e12.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-192.png", image_24:"https://secure.gravatar.com/avatar/bd80d814a4a185e167dbc60e44f72e12.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-24.png", image_32:"https://secure.gravatar.com/avatar/bd80d814a4a185e167dbc60e44f72e12.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-32.png", image_48:"https://secure.gravatar.com/avatar/bd80d814a4a185e167dbc60e44f72e12.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-48.png", image_512:"https://secure.gravatar.com/avatar/bd80d814a4a185e167dbc60e44f72e12.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-512.png", image_72:"https://secure.gravatar.com/avatar/bd80d814a4a185e167dbc60e44f72e12.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0018-72.png", phone:"", real_name:"nadia sheikh", real_name_normalized:"nadia sheikh", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011GKNKBNH', real_name, "nadia sheikh").
slack_info('U011GKNKBNH', team_id, "T010WMWBAGY").
slack_info('U011GKNKBNH', tz, "America/New_York").
slack_info('U011GKNKBNH', tz_label, "Eastern Daylight Time").
slack_info('U011GKNKBNH', tz_offset, -14400).
slack_info('U011GKNKBNH', updated, 1586267581).
slack_info(var, instance, 'U011GN7T96Z').
slack_info('U011GN7T96Z', color, "84b22f").
slack_info('U011GN7T96Z', deleted, false).
slack_info('U011GN7T96Z', id, "U011GN7T96Z").
slack_info('U011GN7T96Z', is_admin, false).
slack_info('U011GN7T96Z', is_app_user, false).
slack_info('U011GN7T96Z', is_bot, false).
slack_info('U011GN7T96Z', is_owner, false).
slack_info('U011GN7T96Z', is_primary_owner, false).
slack_info('U011GN7T96Z', is_restricted, false).
slack_info('U011GN7T96Z', is_ultra_restricted, false).
slack_info('U011GN7T96Z', name, "charleslee592").
slack_info('U011GN7T96Z', presence, "away").
slack_info('U011GN7T96Z', profile, _{avatar_hash:"84ed3433263f", display_name:"Charles", display_name_normalized:"Charles", email:"charleslee592@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1044249449202_84ed3433263f16419089_original.jpg", is_custom_image:true, phone:"", real_name:"Charles", real_name_normalized:"Charles", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011GN7T96Z', real_name, "Charles").
slack_info('U011GN7T96Z', team_id, "T010WMWBAGY").
slack_info('U011GN7T96Z', tz, "America/New_York").
slack_info('U011GN7T96Z', tz_label, "Eastern Daylight Time").
slack_info('U011GN7T96Z', tz_offset, -14400).
slack_info('U011GN7T96Z', updated, 1586270117).
slack_info(var, instance, 'U011GQW1T3P').
slack_info('U011GQW1T3P', color, "619a4f").
slack_info('U011GQW1T3P', deleted, false).
slack_info('U011GQW1T3P', id, "U011GQW1T3P").
slack_info('U011GQW1T3P', is_admin, false).
slack_info('U011GQW1T3P', is_app_user, false).
slack_info('U011GQW1T3P', is_bot, false).
slack_info('U011GQW1T3P', is_owner, false).
slack_info('U011GQW1T3P', is_primary_owner, false).
slack_info('U011GQW1T3P', is_restricted, false).
slack_info('U011GQW1T3P', is_ultra_restricted, false).
slack_info('U011GQW1T3P', name, "dogenpunk").
slack_info('U011GQW1T3P', presence, "away").
slack_info('U011GQW1T3P', profile, _{avatar_hash:"gbeea7193413", display_name:"Matthew M. Nelson", display_name_normalized:"Matthew M. Nelson", email:"dogenpunk@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/beea7193413a677625692f93e6e5b222.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-192.png", image_24:"https://secure.gravatar.com/avatar/beea7193413a677625692f93e6e5b222.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-24.png", image_32:"https://secure.gravatar.com/avatar/beea7193413a677625692f93e6e5b222.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-32.png", image_48:"https://secure.gravatar.com/avatar/beea7193413a677625692f93e6e5b222.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-48.png", image_512:"https://secure.gravatar.com/avatar/beea7193413a677625692f93e6e5b222.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-512.png", image_72:"https://secure.gravatar.com/avatar/beea7193413a677625692f93e6e5b222.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0024-72.png", phone:"", real_name:"Matthew M. Nelson", real_name_normalized:"Matthew M. Nelson", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011GQW1T3P', real_name, "Matthew M. Nelson").
slack_info('U011GQW1T3P', team_id, "T010WMWBAGY").
slack_info('U011GQW1T3P', tz, "America/Chicago").
slack_info('U011GQW1T3P', tz_label, "Central Daylight Time").
slack_info('U011GQW1T3P', tz_offset, -18000).
slack_info('U011GQW1T3P', updated, 1586272290).
slack_info(var, instance, 'U011GR47SUV').
slack_info('U011GR47SUV', color, "a72f79").
slack_info('U011GR47SUV', deleted, false).
slack_info('U011GR47SUV', id, "U011GR47SUV").
slack_info('U011GR47SUV', is_admin, false).
slack_info('U011GR47SUV', is_app_user, false).
slack_info('U011GR47SUV', is_bot, false).
slack_info('U011GR47SUV', is_owner, false).
slack_info('U011GR47SUV', is_primary_owner, false).
slack_info('U011GR47SUV', is_restricted, false).
slack_info('U011GR47SUV', is_ultra_restricted, false).
slack_info('U011GR47SUV', name, "jamie").
slack_info('U011GR47SUV', presence, "away").
slack_info('U011GR47SUV', profile, _{avatar_hash:"098bc76f6326", display_name:"Jamie", display_name_normalized:"Jamie", email:"jamie@lowerarchy.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1045640357111_098bc76f632666afe1a2_original.jpg", is_custom_image:true, phone:"", real_name:"Jamie", real_name_normalized:"Jamie", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011GR47SUV', real_name, "Jamie").
slack_info('U011GR47SUV', team_id, "T010WMWBAGY").
slack_info('U011GR47SUV', tz, "America/Los_Angeles").
slack_info('U011GR47SUV', tz_label, "Pacific Daylight Time").
slack_info('U011GR47SUV', tz_offset, -25200).
slack_info('U011GR47SUV', updated, 1586272594).
slack_info(var, instance, 'U011H0V8DQR').
slack_info('U011H0V8DQR', color, "99a949").
slack_info('U011H0V8DQR', deleted, false).
slack_info('U011H0V8DQR', id, "U011H0V8DQR").
slack_info('U011H0V8DQR', is_admin, false).
slack_info('U011H0V8DQR', is_app_user, false).
slack_info('U011H0V8DQR', is_bot, false).
slack_info('U011H0V8DQR', is_owner, false).
slack_info('U011H0V8DQR', is_primary_owner, false).
slack_info('U011H0V8DQR', is_restricted, false).
slack_info('U011H0V8DQR', is_ultra_restricted, false).
slack_info('U011H0V8DQR', name, "mail").
slack_info('U011H0V8DQR', presence, "away").
slack_info('U011H0V8DQR', profile, _{avatar_hash:"g8bdd4fab8a1", display_name:"Kien", display_name_normalized:"Kien", email:"mail@kien.ai", fields:null, image_192:"https://secure.gravatar.com/avatar/8bdd4fab8a1978914c4f36cb23d01d76.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-192.png", image_24:"https://secure.gravatar.com/avatar/8bdd4fab8a1978914c4f36cb23d01d76.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-24.png", image_32:"https://secure.gravatar.com/avatar/8bdd4fab8a1978914c4f36cb23d01d76.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-32.png", image_48:"https://secure.gravatar.com/avatar/8bdd4fab8a1978914c4f36cb23d01d76.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-48.png", image_512:"https://secure.gravatar.com/avatar/8bdd4fab8a1978914c4f36cb23d01d76.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-512.png", image_72:"https://secure.gravatar.com/avatar/8bdd4fab8a1978914c4f36cb23d01d76.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-72.png", phone:"", real_name:"Kien", real_name_normalized:"Kien", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011H0V8DQR', real_name, "Kien").
slack_info('U011H0V8DQR', team_id, "T010WMWBAGY").
slack_info('U011H0V8DQR', tz, "Asia/Kuala_Lumpur").
slack_info('U011H0V8DQR', tz_label, "Singapore Standard Time").
slack_info('U011H0V8DQR', tz_offset, 28800).
slack_info('U011H0V8DQR', updated, 1586262184).
slack_info(var, instance, 'U011H1UFX6H').
slack_info('U011H1UFX6H', color, "dc7dbb").
slack_info('U011H1UFX6H', deleted, false).
slack_info('U011H1UFX6H', id, "U011H1UFX6H").
slack_info('U011H1UFX6H', is_admin, false).
slack_info('U011H1UFX6H', is_app_user, false).
slack_info('U011H1UFX6H', is_bot, false).
slack_info('U011H1UFX6H', is_owner, false).
slack_info('U011H1UFX6H', is_primary_owner, false).
slack_info('U011H1UFX6H', is_restricted, false).
slack_info('U011H1UFX6H', is_ultra_restricted, false).
slack_info('U011H1UFX6H', name, "akash").
slack_info('U011H1UFX6H', presence, "away").
slack_info('U011H1UFX6H', profile, _{avatar_hash:"g654dd3aa731", display_name:"Akash", display_name_normalized:"Akash", email:"akash@akash.im", fields:null, image_192:"https://secure.gravatar.com/avatar/654dd3aa73158b8aabbab50f719038bb.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0006-192.png", image_24:"https://secure.gravatar.com/avatar/654dd3aa73158b8aabbab50f719038bb.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0006-24.png", image_32:"https://secure.gravatar.com/avatar/654dd3aa73158b8aabbab50f719038bb.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0006-32.png", image_48:"https://secure.gravatar.com/avatar/654dd3aa73158b8aabbab50f719038bb.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0006-48.png", image_512:"https://secure.gravatar.com/avatar/654dd3aa73158b8aabbab50f719038bb.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0006-512.png", image_72:"https://secure.gravatar.com/avatar/654dd3aa73158b8aabbab50f719038bb.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0006-72.png", phone:"", real_name:"Akash", real_name_normalized:"Akash", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011H1UFX6H', real_name, "Akash").
slack_info('U011H1UFX6H', team_id, "T010WMWBAGY").
slack_info('U011H1UFX6H', tz, "Asia/Kolkata").
slack_info('U011H1UFX6H', tz_label, "India Standard Time").
slack_info('U011H1UFX6H', tz_offset, 19800).
slack_info('U011H1UFX6H', updated, 1586278377).
slack_info(var, instance, 'U011HB1ED17').
slack_info('U011HB1ED17', color, "d1707d").
slack_info('U011HB1ED17', deleted, false).
slack_info('U011HB1ED17', id, "U011HB1ED17").
slack_info('U011HB1ED17', is_admin, false).
slack_info('U011HB1ED17', is_app_user, false).
slack_info('U011HB1ED17', is_bot, false).
slack_info('U011HB1ED17', is_owner, false).
slack_info('U011HB1ED17', is_primary_owner, false).
slack_info('U011HB1ED17', is_restricted, false).
slack_info('U011HB1ED17', is_ultra_restricted, false).
slack_info('U011HB1ED17', name, "simonjameskelly").
slack_info('U011HB1ED17', presence, "away").
slack_info('U011HB1ED17', profile, _{avatar_hash:"c2a374ea5b67", display_name:"Simon Kelly", display_name_normalized:"Simon Kelly", email:"simonjameskelly@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-08/1045953208983_c2a374ea5b679e06f8b5_original.jpg", is_custom_image:true, phone:"", real_name:"Simon Kelly", real_name_normalized:"Simon Kelly", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011HB1ED17', real_name, "Simon Kelly").
slack_info('U011HB1ED17', team_id, "T010WMWBAGY").
slack_info('U011HB1ED17', tz, "Europe/London").
slack_info('U011HB1ED17', tz_label, "British Summer Time").
slack_info('U011HB1ED17', tz_offset, 3600).
slack_info('U011HB1ED17', updated, 1586358276).
slack_info(var, instance, 'U011HDG1KLM').
slack_info('U011HDG1KLM', color, "b14cbc").
slack_info('U011HDG1KLM', deleted, false).
slack_info('U011HDG1KLM', id, "U011HDG1KLM").
slack_info('U011HDG1KLM', is_admin, false).
slack_info('U011HDG1KLM', is_app_user, false).
slack_info('U011HDG1KLM', is_bot, false).
slack_info('U011HDG1KLM', is_owner, false).
slack_info('U011HDG1KLM', is_primary_owner, false).
slack_info('U011HDG1KLM', is_restricted, false).
slack_info('U011HDG1KLM', is_ultra_restricted, false).
slack_info('U011HDG1KLM', name, "thijs").
slack_info('U011HDG1KLM', presence, "away").
slack_info('U011HDG1KLM', profile, _{avatar_hash:"g942f6a63d4a", display_name:"Thijs van Dien", display_name_normalized:"Thijs van Dien", email:"thijs@vandien.net", fields:null, image_192:"https://secure.gravatar.com/avatar/942f6a63d4a85a1fc660ea6662e85832.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-192.png", image_24:"https://secure.gravatar.com/avatar/942f6a63d4a85a1fc660ea6662e85832.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-24.png", image_32:"https://secure.gravatar.com/avatar/942f6a63d4a85a1fc660ea6662e85832.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-32.png", image_48:"https://secure.gravatar.com/avatar/942f6a63d4a85a1fc660ea6662e85832.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-48.png", image_512:"https://secure.gravatar.com/avatar/942f6a63d4a85a1fc660ea6662e85832.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-512.png", image_72:"https://secure.gravatar.com/avatar/942f6a63d4a85a1fc660ea6662e85832.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-72.png", phone:"", real_name:"Thijs van Dien", real_name_normalized:"Thijs van Dien", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011HDG1KLM', real_name, "Thijs van Dien").
slack_info('U011HDG1KLM', team_id, "T010WMWBAGY").
slack_info('U011HDG1KLM', tz, "Europe/Warsaw").
slack_info('U011HDG1KLM', tz_label, "Central European Summer Time").
slack_info('U011HDG1KLM', tz_offset, 7200).
slack_info('U011HDG1KLM', updated, 1586291227).
slack_info(var, instance, 'U011HDRGSQ1').
slack_info('U011HDRGSQ1', color, "a2a5dc").
slack_info('U011HDRGSQ1', deleted, false).
slack_info('U011HDRGSQ1', id, "U011HDRGSQ1").
slack_info('U011HDRGSQ1', is_admin, false).
slack_info('U011HDRGSQ1', is_app_user, false).
slack_info('U011HDRGSQ1', is_bot, false).
slack_info('U011HDRGSQ1', is_owner, false).
slack_info('U011HDRGSQ1', is_primary_owner, false).
slack_info('U011HDRGSQ1', is_restricted, false).
slack_info('U011HDRGSQ1', is_ultra_restricted, false).
slack_info('U011HDRGSQ1', name, "bmschwar").
slack_info('U011HDRGSQ1', presence, "away").
slack_info('U011HDRGSQ1', profile, _{avatar_hash:"g04ec229feb1", display_name:"", display_name_normalized:"", email:"bmschwar@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/04ec229feb1755d8319db1b89179f823.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-192.png", image_24:"https://secure.gravatar.com/avatar/04ec229feb1755d8319db1b89179f823.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-24.png", image_32:"https://secure.gravatar.com/avatar/04ec229feb1755d8319db1b89179f823.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-32.png", image_48:"https://secure.gravatar.com/avatar/04ec229feb1755d8319db1b89179f823.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-48.png", image_512:"https://secure.gravatar.com/avatar/04ec229feb1755d8319db1b89179f823.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-512.png", image_72:"https://secure.gravatar.com/avatar/04ec229feb1755d8319db1b89179f823.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-72.png", phone:"", real_name:"Ben Schwartz", real_name_normalized:"Ben Schwartz", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011HDRGSQ1', real_name, "Ben Schwartz").
slack_info('U011HDRGSQ1', team_id, "T010WMWBAGY").
slack_info('U011HDRGSQ1', tz, "America/Los_Angeles").
slack_info('U011HDRGSQ1', tz_label, "Pacific Daylight Time").
slack_info('U011HDRGSQ1', tz_offset, -25200).
slack_info('U011HDRGSQ1', updated, 1586268306).
slack_info(var, instance, 'U011HDST18D').
slack_info('U011HDST18D', color, "9f69e7").
slack_info('U011HDST18D', deleted, false).
slack_info('U011HDST18D', id, "U011HDST18D").
slack_info('U011HDST18D', is_admin, false).
slack_info('U011HDST18D', is_app_user, false).
slack_info('U011HDST18D', is_bot, false).
slack_info('U011HDST18D', is_owner, false).
slack_info('U011HDST18D', is_primary_owner, false).
slack_info('U011HDST18D', is_restricted, false).
slack_info('U011HDST18D', is_ultra_restricted, false).
slack_info('U011HDST18D', name, "norbert").
slack_info('U011HDST18D', presence, "away").
slack_info('U011HDST18D', profile, _{avatar_hash:"613b726a79a0", display_name:"Norbert Hartl", display_name_normalized:"Norbert Hartl", email:"norbert@hartl.name", fields:null, first_name:"Norbert", image_1024:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-09/1054781144485_613b726a79a0956ec882_original.jpg", is_custom_image:true, last_name:"Hartl", phone:"", real_name:"Norbert Hartl", real_name_normalized:"Norbert Hartl", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:"Smalltalk mostly. @NorbertHartl on twitter"}).
slack_info('U011HDST18D', real_name, "Norbert Hartl").
slack_info('U011HDST18D', team_id, "T010WMWBAGY").
slack_info('U011HDST18D', tz, "Europe/Amsterdam").
slack_info('U011HDST18D', tz_label, "Central European Summer Time").
slack_info('U011HDST18D', tz_offset, 7200).
slack_info('U011HDST18D', updated, 1586442777).
slack_info(var, instance, 'U011HJ34T89').
slack_info('U011HJ34T89', color, "4ec0d6").
slack_info('U011HJ34T89', deleted, false).
slack_info('U011HJ34T89', id, "U011HJ34T89").
slack_info('U011HJ34T89', is_admin, false).
slack_info('U011HJ34T89', is_app_user, false).
slack_info('U011HJ34T89', is_bot, false).
slack_info('U011HJ34T89', is_owner, false).
slack_info('U011HJ34T89', is_primary_owner, false).
slack_info('U011HJ34T89', is_restricted, false).
slack_info('U011HJ34T89', is_ultra_restricted, false).
slack_info('U011HJ34T89', name, "ara").
slack_info('U011HJ34T89', presence, "away").
slack_info('U011HJ34T89', profile, _{avatar_hash:"gc62d1931b65", display_name:"Ara Hacopian", display_name_normalized:"Ara Hacopian", email:"ara@tehanu.net", fields:null, image_192:"https://secure.gravatar.com/avatar/c62d1931b6571728464c9a7ca7a99075.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-192.png", image_24:"https://secure.gravatar.com/avatar/c62d1931b6571728464c9a7ca7a99075.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-24.png", image_32:"https://secure.gravatar.com/avatar/c62d1931b6571728464c9a7ca7a99075.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-32.png", image_48:"https://secure.gravatar.com/avatar/c62d1931b6571728464c9a7ca7a99075.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-48.png", image_512:"https://secure.gravatar.com/avatar/c62d1931b6571728464c9a7ca7a99075.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-512.png", image_72:"https://secure.gravatar.com/avatar/c62d1931b6571728464c9a7ca7a99075.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0012-72.png", phone:"", real_name:"Ara Hacopian", real_name_normalized:"Ara Hacopian", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011HJ34T89', real_name, "Ara Hacopian").
slack_info('U011HJ34T89', team_id, "T010WMWBAGY").
slack_info('U011HJ34T89', tz, "Europe/Amsterdam").
slack_info('U011HJ34T89', tz_label, "Central European Summer Time").
slack_info('U011HJ34T89', tz_offset, 7200).
slack_info('U011HJ34T89', updated, 1586270077).
slack_info(var, instance, 'U011HUGFX8R').
slack_info('U011HUGFX8R', color, "aba727").
slack_info('U011HUGFX8R', deleted, false).
slack_info('U011HUGFX8R', id, "U011HUGFX8R").
slack_info('U011HUGFX8R', is_admin, false).
slack_info('U011HUGFX8R', is_app_user, false).
slack_info('U011HUGFX8R', is_bot, false).
slack_info('U011HUGFX8R', is_owner, false).
slack_info('U011HUGFX8R', is_primary_owner, false).
slack_info('U011HUGFX8R', is_restricted, false).
slack_info('U011HUGFX8R', is_ultra_restricted, false).
slack_info('U011HUGFX8R', name, "adrian.arroyocalle").
slack_info('U011HUGFX8R', presence, "away").
slack_info('U011HUGFX8R', profile, _{avatar_hash:"b83c9855c884", display_name:"Adrián Arroyo Calle", display_name_normalized:"Adrian Arroyo Calle", email:"adrian.arroyocalle@gmail.com", fields:[], first_name:"Adrián", image_1024:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1045649586039_b83c9855c884584703d4_original.jpg", is_custom_image:true, last_name:"Arroyo Calle", phone:"", real_name:"Adrián Arroyo Calle", real_name_normalized:"Adrian Arroyo Calle", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011HUGFX8R', real_name, "Adrián Arroyo Calle").
slack_info('U011HUGFX8R', team_id, "T010WMWBAGY").
slack_info('U011HUGFX8R', tz, "Europe/Amsterdam").
slack_info('U011HUGFX8R', tz_label, "Central European Summer Time").
slack_info('U011HUGFX8R', tz_offset, 7200).
slack_info('U011HUGFX8R', updated, 1586360287).
slack_info(var, instance, 'U011J6E2TCM').
slack_info('U011J6E2TCM', color, "df3dc0").
slack_info('U011J6E2TCM', deleted, false).
slack_info('U011J6E2TCM', id, "U011J6E2TCM").
slack_info('U011J6E2TCM', is_admin, false).
slack_info('U011J6E2TCM', is_app_user, false).
slack_info('U011J6E2TCM', is_bot, false).
slack_info('U011J6E2TCM', is_owner, false).
slack_info('U011J6E2TCM', is_primary_owner, false).
slack_info('U011J6E2TCM', is_restricted, false).
slack_info('U011J6E2TCM', is_ultra_restricted, false).
slack_info('U011J6E2TCM', name, "ghoen49").
slack_info('U011J6E2TCM', presence, "away").
slack_info('U011J6E2TCM', profile, _{avatar_hash:"g0378c6e734c", display_name:"Ghoen", display_name_normalized:"Ghoen", email:"ghoen49@hotmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/0378c6e734ca66a0478066777870de3f.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-192.png", image_24:"https://secure.gravatar.com/avatar/0378c6e734ca66a0478066777870de3f.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-24.png", image_32:"https://secure.gravatar.com/avatar/0378c6e734ca66a0478066777870de3f.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-32.png", image_48:"https://secure.gravatar.com/avatar/0378c6e734ca66a0478066777870de3f.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-48.png", image_512:"https://secure.gravatar.com/avatar/0378c6e734ca66a0478066777870de3f.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-512.png", image_72:"https://secure.gravatar.com/avatar/0378c6e734ca66a0478066777870de3f.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-72.png", phone:"", real_name:"Ghoen", real_name_normalized:"Ghoen", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011J6E2TCM', real_name, "Ghoen").
slack_info('U011J6E2TCM', team_id, "T010WMWBAGY").
slack_info('U011J6E2TCM', tz, "Europe/Amsterdam").
slack_info('U011J6E2TCM', tz_label, "Central European Summer Time").
slack_info('U011J6E2TCM', tz_offset, 7200).
slack_info('U011J6E2TCM', updated, 1586335429).
slack_info(var, instance, 'U011JBDQK1U').
slack_info('U011JBDQK1U', color, "235e5b").
slack_info('U011JBDQK1U', deleted, false).
slack_info('U011JBDQK1U', id, "U011JBDQK1U").
slack_info('U011JBDQK1U', is_admin, false).
slack_info('U011JBDQK1U', is_app_user, false).
slack_info('U011JBDQK1U', is_bot, false).
slack_info('U011JBDQK1U', is_owner, false).
slack_info('U011JBDQK1U', is_primary_owner, false).
slack_info('U011JBDQK1U', is_restricted, false).
slack_info('U011JBDQK1U', is_ultra_restricted, false).
slack_info('U011JBDQK1U', name, "jace42").
slack_info('U011JBDQK1U', presence, "away").
slack_info('U011JBDQK1U', profile, _{avatar_hash:"74118c3e8b0b", display_name:"jacob", display_name_normalized:"jacob", email:"jace42@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-09/1073799829808_74118c3e8b0bad0bea72_original.jpg", is_custom_image:true, phone:"", real_name:"jacob", real_name_normalized:"jacob", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011JBDQK1U', real_name, "jacob").
slack_info('U011JBDQK1U', team_id, "T010WMWBAGY").
slack_info('U011JBDQK1U', tz, "America/Los_Angeles").
slack_info('U011JBDQK1U', tz_label, "Pacific Daylight Time").
slack_info('U011JBDQK1U', tz_offset, -25200).
slack_info('U011JBDQK1U', updated, 1586494257).
slack_info(var, instance, 'U011JQEHLM7').
slack_info('U011JQEHLM7', color, "8469bc").
slack_info('U011JQEHLM7', deleted, false).
slack_info('U011JQEHLM7', id, "U011JQEHLM7").
slack_info('U011JQEHLM7', is_admin, false).
slack_info('U011JQEHLM7', is_app_user, false).
slack_info('U011JQEHLM7', is_bot, false).
slack_info('U011JQEHLM7', is_owner, false).
slack_info('U011JQEHLM7', is_primary_owner, false).
slack_info('U011JQEHLM7', is_restricted, false).
slack_info('U011JQEHLM7', is_ultra_restricted, false).
slack_info('U011JQEHLM7', name, "konjevic").
slack_info('U011JQEHLM7', presence, "away").
slack_info('U011JQEHLM7', profile, _{avatar_hash:"g9976060bfb2", display_name:"Mihael Konjevic", display_name_normalized:"Mihael Konjevic", email:"konjevic@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/9976060bfb26220dd042340394d06b31.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-192.png", image_24:"https://secure.gravatar.com/avatar/9976060bfb26220dd042340394d06b31.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-24.png", image_32:"https://secure.gravatar.com/avatar/9976060bfb26220dd042340394d06b31.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-32.png", image_48:"https://secure.gravatar.com/avatar/9976060bfb26220dd042340394d06b31.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-48.png", image_512:"https://secure.gravatar.com/avatar/9976060bfb26220dd042340394d06b31.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-512.png", image_72:"https://secure.gravatar.com/avatar/9976060bfb26220dd042340394d06b31.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0023-72.png", phone:"", real_name:"Mihael Konjevic", real_name_normalized:"Mihael Konjevic", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011JQEHLM7', real_name, "Mihael Konjevic").
slack_info('U011JQEHLM7', team_id, "T010WMWBAGY").
slack_info('U011JQEHLM7', tz, "Europe/Amsterdam").
slack_info('U011JQEHLM7', tz_label, "Central European Summer Time").
slack_info('U011JQEHLM7', tz_offset, 7200).
slack_info('U011JQEHLM7', updated, 1586285633).
slack_info(var, instance, 'U011KM298TW').
slack_info('U011KM298TW', color, "3c989f").
slack_info('U011KM298TW', deleted, false).
slack_info('U011KM298TW', id, "U011KM298TW").
slack_info('U011KM298TW', is_admin, false).
slack_info('U011KM298TW', is_app_user, false).
slack_info('U011KM298TW', is_bot, false).
slack_info('U011KM298TW', is_owner, false).
slack_info('U011KM298TW', is_primary_owner, false).
slack_info('U011KM298TW', is_restricted, false).
slack_info('U011KM298TW', is_ultra_restricted, false).
slack_info('U011KM298TW', name, "pbh101").
slack_info('U011KM298TW', presence, "away").
slack_info('U011KM298TW', profile, _{avatar_hash:"109c92769476", display_name:"Patrick", display_name_normalized:"Patrick", email:"pbh101@gmail.com", fields:null, first_name:"Patrick", image_1024:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_1024.png", image_192:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_192.png", image_24:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_24.png", image_32:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_32.png", image_48:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_48.png", image_512:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_512.png", image_72:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_72.png", image_original:"https://avatars.slack-edge.com/2020-04-07/1045599125383_109c92769476bc10424d_original.png", is_custom_image:true, last_name:"Braga-Henebry", phone:"", real_name:"Patrick Braga-Henebry", real_name_normalized:"Patrick Braga-Henebry", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011KM298TW', real_name, "Patrick Braga-Henebry").
slack_info('U011KM298TW', team_id, "T010WMWBAGY").
slack_info('U011KM298TW', tz, "America/Chicago").
slack_info('U011KM298TW', tz_label, "Central Daylight Time").
slack_info('U011KM298TW', tz_offset, -18000).
slack_info('U011KM298TW', updated, 1586263046).
slack_info(var, instance, 'U011KMR024U').
slack_info('U011KMR024U', color, "e0a729").
slack_info('U011KMR024U', deleted, false).
slack_info('U011KMR024U', id, "U011KMR024U").
slack_info('U011KMR024U', is_admin, false).
slack_info('U011KMR024U', is_app_user, false).
slack_info('U011KMR024U', is_bot, false).
slack_info('U011KMR024U', is_owner, false).
slack_info('U011KMR024U', is_primary_owner, false).
slack_info('U011KMR024U', is_restricted, false).
slack_info('U011KMR024U', is_ultra_restricted, false).
slack_info('U011KMR024U', name, "n.gagliani").
slack_info('U011KMR024U', presence, "away").
slack_info('U011KMR024U', profile, _{avatar_hash:"40680a4173a5", display_name:"", display_name_normalized:"", email:"n.gagliani@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-08/1047286897090_40680a4173a5db07f7c7_original.jpg", is_custom_image:true, phone:"", real_name:"Nicolas", real_name_normalized:"Nicolas", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011KMR024U', real_name, "Nicolas").
slack_info('U011KMR024U', team_id, "T010WMWBAGY").
slack_info('U011KMR024U', tz, "Europe/Amsterdam").
slack_info('U011KMR024U', tz_label, "Central European Summer Time").
slack_info('U011KMR024U', tz_offset, 7200).
slack_info('U011KMR024U', updated, 1586348986).
slack_info(var, instance, 'U011KMTBGN8').
slack_info('U011KMTBGN8', color, "684b6c").
slack_info('U011KMTBGN8', deleted, false).
slack_info('U011KMTBGN8', id, "U011KMTBGN8").
slack_info('U011KMTBGN8', is_admin, false).
slack_info('U011KMTBGN8', is_app_user, false).
slack_info('U011KMTBGN8', is_bot, false).
slack_info('U011KMTBGN8', is_owner, false).
slack_info('U011KMTBGN8', is_primary_owner, false).
slack_info('U011KMTBGN8', is_restricted, false).
slack_info('U011KMTBGN8', is_ultra_restricted, false).
slack_info('U011KMTBGN8', name, "wernher.behrendt").
slack_info('U011KMTBGN8', presence, "away").
slack_info('U011KMTBGN8', profile, _{avatar_hash:"e36a7342e518", display_name:"Wernher", display_name_normalized:"Wernher", email:"wernher.behrendt@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1053990432900_e36a7342e51806307fae_original.jpg", is_custom_image:true, phone:"", real_name:"Wernher", real_name_normalized:"Wernher", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011KMTBGN8', real_name, "Wernher").
slack_info('U011KMTBGN8', team_id, "T010WMWBAGY").
slack_info('U011KMTBGN8', tz, "Europe/Amsterdam").
slack_info('U011KMTBGN8', tz_label, "Central European Summer Time").
slack_info('U011KMTBGN8', tz_offset, 7200).
slack_info('U011KMTBGN8', updated, 1586267877).
slack_info(var, instance, 'U011KT0C952').
slack_info('U011KT0C952', deleted, true).
slack_info('U011KT0C952', id, "U011KT0C952").
slack_info('U011KT0C952', is_app_user, false).
slack_info('U011KT0C952', is_bot, false).
slack_info('U011KT0C952', name, "alexander.shendi").
slack_info('U011KT0C952', presence, "away").
slack_info('U011KT0C952', profile, _{avatar_hash:"g0dda380b62b", display_name:"alexshendi", display_name_normalized:"alexshendi", email:"alexander.shendi@web.de", fields:null, first_name:"Alexander", image_192:"https://secure.gravatar.com/avatar/0dda380b62b9a6900bd8ffa56c6a1710.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-192.png", image_24:"https://secure.gravatar.com/avatar/0dda380b62b9a6900bd8ffa56c6a1710.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-24.png", image_32:"https://secure.gravatar.com/avatar/0dda380b62b9a6900bd8ffa56c6a1710.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-32.png", image_48:"https://secure.gravatar.com/avatar/0dda380b62b9a6900bd8ffa56c6a1710.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-48.png", image_512:"https://secure.gravatar.com/avatar/0dda380b62b9a6900bd8ffa56c6a1710.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-512.png", image_72:"https://secure.gravatar.com/avatar/0dda380b62b9a6900bd8ffa56c6a1710.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-72.png", last_name:"Shendi", phone:"", real_name:"Alexander Shendi", real_name_normalized:"Alexander Shendi", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011KT0C952', team_id, "T010WMWBAGY").
slack_info('U011KT0C952', updated, 1586641780).
slack_info(var, instance, 'U011KUZT45S').
slack_info('U011KUZT45S', color, "902d59").
slack_info('U011KUZT45S', deleted, false).
slack_info('U011KUZT45S', id, "U011KUZT45S").
slack_info('U011KUZT45S', is_admin, false).
slack_info('U011KUZT45S', is_app_user, false).
slack_info('U011KUZT45S', is_bot, false).
slack_info('U011KUZT45S', is_owner, false).
slack_info('U011KUZT45S', is_primary_owner, false).
slack_info('U011KUZT45S', is_restricted, false).
slack_info('U011KUZT45S', is_ultra_restricted, false).
slack_info('U011KUZT45S', name, "alex.kelley").
slack_info('U011KUZT45S', presence, "away").
slack_info('U011KUZT45S', profile, _{avatar_hash:"g6bce4d4273b", display_name:"", display_name_normalized:"", email:"alex.kelley@adkpartners.com", fields:null, image_192:"https://secure.gravatar.com/avatar/6bce4d4273be9134f2367e62f629265c.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-192.png", image_24:"https://secure.gravatar.com/avatar/6bce4d4273be9134f2367e62f629265c.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-24.png", image_32:"https://secure.gravatar.com/avatar/6bce4d4273be9134f2367e62f629265c.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-32.png", image_48:"https://secure.gravatar.com/avatar/6bce4d4273be9134f2367e62f629265c.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-48.png", image_512:"https://secure.gravatar.com/avatar/6bce4d4273be9134f2367e62f629265c.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-512.png", image_72:"https://secure.gravatar.com/avatar/6bce4d4273be9134f2367e62f629265c.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0011-72.png", phone:"", real_name:"Alex Kelley", real_name_normalized:"Alex Kelley", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011KUZT45S', real_name, "Alex Kelley").
slack_info('U011KUZT45S', team_id, "T010WMWBAGY").
slack_info('U011KUZT45S', tz, "America/Los_Angeles").
slack_info('U011KUZT45S', tz_label, "Pacific Daylight Time").
slack_info('U011KUZT45S', tz_offset, -25200).
slack_info('U011KUZT45S', updated, 1586267786).
slack_info(var, instance, 'U011L2C91N0').
slack_info('U011L2C91N0', color, "8d4b84").
slack_info('U011L2C91N0', deleted, false).
slack_info('U011L2C91N0', id, "U011L2C91N0").
slack_info('U011L2C91N0', is_admin, false).
slack_info('U011L2C91N0', is_app_user, false).
slack_info('U011L2C91N0', is_bot, false).
slack_info('U011L2C91N0', is_owner, false).
slack_info('U011L2C91N0', is_primary_owner, false).
slack_info('U011L2C91N0', is_restricted, false).
slack_info('U011L2C91N0', is_ultra_restricted, false).
slack_info('U011L2C91N0', name, "olivier.mathieu314").
slack_info('U011L2C91N0', presence, "away").
slack_info('U011L2C91N0', profile, _{avatar_hash:"g9da71f110ae", display_name:"o314", display_name_normalized:"o314", email:"olivier.mathieu314@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/9da71f110ae187230e855a9a56c3d077.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-192.png", image_24:"https://secure.gravatar.com/avatar/9da71f110ae187230e855a9a56c3d077.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-24.png", image_32:"https://secure.gravatar.com/avatar/9da71f110ae187230e855a9a56c3d077.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-32.png", image_48:"https://secure.gravatar.com/avatar/9da71f110ae187230e855a9a56c3d077.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-48.png", image_512:"https://secure.gravatar.com/avatar/9da71f110ae187230e855a9a56c3d077.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-512.png", image_72:"https://secure.gravatar.com/avatar/9da71f110ae187230e855a9a56c3d077.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-72.png", phone:"", real_name:"o314", real_name_normalized:"o314", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011L2C91N0', real_name, "o314").
slack_info('U011L2C91N0', team_id, "T010WMWBAGY").
slack_info('U011L2C91N0', tz, "Europe/Brussels").
slack_info('U011L2C91N0', tz_label, "Central European Summer Time").
slack_info('U011L2C91N0', tz_offset, 7200).
slack_info('U011L2C91N0', updated, 1586269599).
slack_info(var, instance, 'U011LBM8A7N').
slack_info('U011LBM8A7N', color, "dd8527").
slack_info('U011LBM8A7N', deleted, false).
slack_info('U011LBM8A7N', id, "U011LBM8A7N").
slack_info('U011LBM8A7N', is_admin, false).
slack_info('U011LBM8A7N', is_app_user, false).
slack_info('U011LBM8A7N', is_bot, false).
slack_info('U011LBM8A7N', is_owner, false).
slack_info('U011LBM8A7N', is_primary_owner, false).
slack_info('U011LBM8A7N', is_restricted, false).
slack_info('U011LBM8A7N', is_ultra_restricted, false).
slack_info('U011LBM8A7N', name, "evanwhackett").
slack_info('U011LBM8A7N', presence, "away").
slack_info('U011LBM8A7N', profile, _{avatar_hash:"gb79bcc93f12", display_name:"Evan Hackett", display_name_normalized:"Evan Hackett", email:"evanwhackett@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/b79bcc93f1242a3ef916f2ab0fcd1fc5.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-192.png", image_24:"https://secure.gravatar.com/avatar/b79bcc93f1242a3ef916f2ab0fcd1fc5.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-24.png", image_32:"https://secure.gravatar.com/avatar/b79bcc93f1242a3ef916f2ab0fcd1fc5.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-32.png", image_48:"https://secure.gravatar.com/avatar/b79bcc93f1242a3ef916f2ab0fcd1fc5.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-48.png", image_512:"https://secure.gravatar.com/avatar/b79bcc93f1242a3ef916f2ab0fcd1fc5.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-512.png", image_72:"https://secure.gravatar.com/avatar/b79bcc93f1242a3ef916f2ab0fcd1fc5.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-72.png", phone:"", real_name:"Evan Hackett", real_name_normalized:"Evan Hackett", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011LBM8A7N', real_name, "Evan Hackett").
slack_info('U011LBM8A7N', team_id, "T010WMWBAGY").
slack_info('U011LBM8A7N', tz, "America/Los_Angeles").
slack_info('U011LBM8A7N', tz_label, "Pacific Daylight Time").
slack_info('U011LBM8A7N', tz_offset, -25200).
slack_info('U011LBM8A7N', updated, 1586275601).
slack_info(var, instance, 'U011LD9GRHA').
slack_info('U011LD9GRHA', color, "bd9336").
slack_info('U011LD9GRHA', deleted, false).
slack_info('U011LD9GRHA', id, "U011LD9GRHA").
slack_info('U011LD9GRHA', is_admin, false).
slack_info('U011LD9GRHA', is_app_user, false).
slack_info('U011LD9GRHA', is_bot, false).
slack_info('U011LD9GRHA', is_owner, false).
slack_info('U011LD9GRHA', is_primary_owner, false).
slack_info('U011LD9GRHA', is_restricted, false).
slack_info('U011LD9GRHA', is_ultra_restricted, false).
slack_info('U011LD9GRHA', name, "jason.rothfuss").
slack_info('U011LD9GRHA', presence, "away").
slack_info('U011LD9GRHA', profile, _{avatar_hash:"358cce1eb3b5", display_name:"Jason Rothfuss", display_name_normalized:"Jason Rothfuss", email:"jason.rothfuss@icloud.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1052518618465_358cce1eb3b586700f55_original.jpg", is_custom_image:true, phone:"", real_name:"Jason Rothfuss", real_name_normalized:"Jason Rothfuss", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011LD9GRHA', real_name, "Jason Rothfuss").
slack_info('U011LD9GRHA', team_id, "T010WMWBAGY").
slack_info('U011LD9GRHA', tz, "America/New_York").
slack_info('U011LD9GRHA', tz_label, "Eastern Daylight Time").
slack_info('U011LD9GRHA', tz_offset, -14400).
slack_info('U011LD9GRHA', updated, 1586281244).
slack_info(var, instance, 'U011LFELS4R').
slack_info('U011LFELS4R', color, "5b89d5").
slack_info('U011LFELS4R', deleted, false).
slack_info('U011LFELS4R', id, "U011LFELS4R").
slack_info('U011LFELS4R', is_admin, false).
slack_info('U011LFELS4R', is_app_user, false).
slack_info('U011LFELS4R', is_bot, true).
slack_info('U011LFELS4R', is_owner, false).
slack_info('U011LFELS4R', is_primary_owner, false).
slack_info('U011LFELS4R', is_restricted, false).
slack_info('U011LFELS4R', is_ultra_restricted, false).
slack_info('U011LFELS4R', name, "googledrive").
slack_info('U011LFELS4R', presence, "away").
slack_info('U011LFELS4R', profile, _{always_active:true, api_app_id:"A6NL8MJ6Q", avatar_hash:"8ffce3229e29", bot_id:"B011LFELQKB", display_name:"", display_name_normalized:"", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_1024.png", image_192:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_192.png", image_24:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_24.png", image_32:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_32.png", image_48:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_48.png", image_512:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_512.png", image_72:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_72.png", image_original:"https://avatars.slack-edge.com/2020-04-07/1055918317972_8ffce3229e2952cf3555_original.png", is_custom_image:true, phone:"", real_name:"Google Drive", real_name_normalized:"Google Drive", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011LFELS4R', real_name, "Google Drive").
slack_info('U011LFELS4R', team_id, "T010WMWBAGY").
slack_info('U011LFELS4R', tz, "America/Los_Angeles").
slack_info('U011LFELS4R', tz_label, "Pacific Daylight Time").
slack_info('U011LFELS4R', tz_offset, -25200).
slack_info('U011LFELS4R', updated, 1586327495).
slack_info(var, instance, 'U011M3W2WRJ').
slack_info('U011M3W2WRJ', color, "e7392d").
slack_info('U011M3W2WRJ', deleted, false).
slack_info('U011M3W2WRJ', id, "U011M3W2WRJ").
slack_info('U011M3W2WRJ', is_admin, false).
slack_info('U011M3W2WRJ', is_app_user, false).
slack_info('U011M3W2WRJ', is_bot, false).
slack_info('U011M3W2WRJ', is_owner, false).
slack_info('U011M3W2WRJ', is_primary_owner, false).
slack_info('U011M3W2WRJ', is_restricted, false).
slack_info('U011M3W2WRJ', is_ultra_restricted, false).
slack_info('U011M3W2WRJ', name, "terri").
slack_info('U011M3W2WRJ', presence, "away").
slack_info('U011M3W2WRJ', profile, _{avatar_hash:"eb3d07dc3d5c", display_name:"Terri Yu", display_name_normalized:"Terri Yu", email:"terri@terriyu.info", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-11/1063803754244_eb3d07dc3d5c917f75a5_original.jpg", is_custom_image:true, phone:"", real_name:"Terri Yu", real_name_normalized:"Terri Yu", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011M3W2WRJ', real_name, "Terri Yu").
slack_info('U011M3W2WRJ', team_id, "T010WMWBAGY").
slack_info('U011M3W2WRJ', tz, "America/New_York").
slack_info('U011M3W2WRJ', tz_label, "Eastern Daylight Time").
slack_info('U011M3W2WRJ', tz_offset, -14400).
slack_info('U011M3W2WRJ', updated, 1586655163).
slack_info(var, instance, 'U011MJN89SR').
slack_info('U011MJN89SR', color, "db3150").
slack_info('U011MJN89SR', deleted, false).
slack_info('U011MJN89SR', id, "U011MJN89SR").
slack_info('U011MJN89SR', is_admin, false).
slack_info('U011MJN89SR', is_app_user, false).
slack_info('U011MJN89SR', is_bot, false).
slack_info('U011MJN89SR', is_owner, false).
slack_info('U011MJN89SR', is_primary_owner, false).
slack_info('U011MJN89SR', is_restricted, false).
slack_info('U011MJN89SR', is_ultra_restricted, false).
slack_info('U011MJN89SR', name, "gfleetwood").
slack_info('U011MJN89SR', presence, "away").
slack_info('U011MJN89SR', profile, _{avatar_hash:"g91c06ec38a8", display_name:"gordonf", display_name_normalized:"gordonf", email:"gfleetwood@protonmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/91c06ec38a8978d595ccc3797ae090f2.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-192.png", image_24:"https://secure.gravatar.com/avatar/91c06ec38a8978d595ccc3797ae090f2.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-24.png", image_32:"https://secure.gravatar.com/avatar/91c06ec38a8978d595ccc3797ae090f2.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-32.png", image_48:"https://secure.gravatar.com/avatar/91c06ec38a8978d595ccc3797ae090f2.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-48.png", image_512:"https://secure.gravatar.com/avatar/91c06ec38a8978d595ccc3797ae090f2.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-512.png", image_72:"https://secure.gravatar.com/avatar/91c06ec38a8978d595ccc3797ae090f2.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0002-72.png", phone:"", real_name:"gordonf", real_name_normalized:"gordonf", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011MJN89SR', real_name, "gordonf").
slack_info('U011MJN89SR', team_id, "T010WMWBAGY").
slack_info('U011MJN89SR', tz, "America/New_York").
slack_info('U011MJN89SR', tz_label, "Eastern Daylight Time").
slack_info('U011MJN89SR', tz_offset, -14400).
slack_info('U011MJN89SR', updated, 1586471612).
slack_info(var, instance, 'U011MMUG8M8').
slack_info('U011MMUG8M8', color, "5870dd").
slack_info('U011MMUG8M8', deleted, false).
slack_info('U011MMUG8M8', id, "U011MMUG8M8").
slack_info('U011MMUG8M8', is_admin, false).
slack_info('U011MMUG8M8', is_app_user, false).
slack_info('U011MMUG8M8', is_bot, false).
slack_info('U011MMUG8M8', is_owner, false).
slack_info('U011MMUG8M8', is_primary_owner, false).
slack_info('U011MMUG8M8', is_restricted, false).
slack_info('U011MMUG8M8', is_ultra_restricted, false).
slack_info('U011MMUG8M8', name, "prologclass.slack.apr").
slack_info('U011MMUG8M8', presence, "away").
slack_info('U011MMUG8M8', profile, _{avatar_hash:"g92e5a895938", display_name:"Eoin Groat", display_name_normalized:"Eoin Groat", email:"prologclass.slack.apr.2020@normal-gaussian.com", fields:null, image_192:"https://secure.gravatar.com/avatar/92e5a89593845e9615daebce7df45683.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-192.png", image_24:"https://secure.gravatar.com/avatar/92e5a89593845e9615daebce7df45683.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-24.png", image_32:"https://secure.gravatar.com/avatar/92e5a89593845e9615daebce7df45683.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-32.png", image_48:"https://secure.gravatar.com/avatar/92e5a89593845e9615daebce7df45683.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-48.png", image_512:"https://secure.gravatar.com/avatar/92e5a89593845e9615daebce7df45683.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-512.png", image_72:"https://secure.gravatar.com/avatar/92e5a89593845e9615daebce7df45683.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-72.png", phone:"", real_name:"Eoin Groat", real_name_normalized:"Eoin Groat", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011MMUG8M8', real_name, "Eoin Groat").
slack_info('U011MMUG8M8', team_id, "T010WMWBAGY").
slack_info('U011MMUG8M8', tz, "Europe/London").
slack_info('U011MMUG8M8', tz_label, "British Summer Time").
slack_info('U011MMUG8M8', tz_offset, 3600).
slack_info('U011MMUG8M8', updated, 1586620024).
slack_info(var, instance, 'U011MQ9GB9R').
slack_info('U011MQ9GB9R', color, "c386df").
slack_info('U011MQ9GB9R', deleted, false).
slack_info('U011MQ9GB9R', id, "U011MQ9GB9R").
slack_info('U011MQ9GB9R', is_admin, false).
slack_info('U011MQ9GB9R', is_app_user, false).
slack_info('U011MQ9GB9R', is_bot, false).
slack_info('U011MQ9GB9R', is_owner, false).
slack_info('U011MQ9GB9R', is_primary_owner, false).
slack_info('U011MQ9GB9R', is_restricted, false).
slack_info('U011MQ9GB9R', is_ultra_restricted, false).
slack_info('U011MQ9GB9R', name, "ac.russell").
slack_info('U011MQ9GB9R', presence, "away").
slack_info('U011MQ9GB9R', profile, _{avatar_hash:"g8d1532e30b8", display_name:"Adam Russell", display_name_normalized:"Adam Russell", email:"ac.russell@live.com", fields:null, image_192:"https://secure.gravatar.com/avatar/8d1532e30b8e1aea9d9eb57d61eb4c55.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-192.png", image_24:"https://secure.gravatar.com/avatar/8d1532e30b8e1aea9d9eb57d61eb4c55.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-24.png", image_32:"https://secure.gravatar.com/avatar/8d1532e30b8e1aea9d9eb57d61eb4c55.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-32.png", image_48:"https://secure.gravatar.com/avatar/8d1532e30b8e1aea9d9eb57d61eb4c55.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-48.png", image_512:"https://secure.gravatar.com/avatar/8d1532e30b8e1aea9d9eb57d61eb4c55.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-512.png", image_72:"https://secure.gravatar.com/avatar/8d1532e30b8e1aea9d9eb57d61eb4c55.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0022-72.png", phone:"", real_name:"Adam Russell", real_name_normalized:"Adam Russell", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011MQ9GB9R', real_name, "Adam Russell").
slack_info('U011MQ9GB9R', team_id, "T010WMWBAGY").
slack_info('U011MQ9GB9R', tz, "America/New_York").
slack_info('U011MQ9GB9R', tz_label, "Eastern Daylight Time").
slack_info('U011MQ9GB9R', tz_offset, -14400).
slack_info('U011MQ9GB9R', updated, 1586554308).
slack_info(var, instance, 'U011NFAKS77').
slack_info('U011NFAKS77', color, "9e3997").
slack_info('U011NFAKS77', deleted, false).
slack_info('U011NFAKS77', id, "U011NFAKS77").
slack_info('U011NFAKS77', is_admin, false).
slack_info('U011NFAKS77', is_app_user, false).
slack_info('U011NFAKS77', is_bot, false).
slack_info('U011NFAKS77', is_owner, false).
slack_info('U011NFAKS77', is_primary_owner, false).
slack_info('U011NFAKS77', is_restricted, false).
slack_info('U011NFAKS77', is_ultra_restricted, false).
slack_info('U011NFAKS77', name, "xavier.rs").
slack_info('U011NFAKS77', presence, "away").
slack_info('U011NFAKS77', profile, _{avatar_hash:"77adeb64a92d", display_name:"Xavier", display_name_normalized:"Xavier", email:"xavier.rs@fastmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-10/1062359213476_77adeb64a92dc2e14626_original.jpg", is_custom_image:true, phone:"", real_name:"Xavier", real_name_normalized:"Xavier", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011NFAKS77', real_name, "Xavier").
slack_info('U011NFAKS77', team_id, "T010WMWBAGY").
slack_info('U011NFAKS77', tz, "Europe/Amsterdam").
slack_info('U011NFAKS77', tz_label, "Central European Summer Time").
slack_info('U011NFAKS77', tz_offset, 7200).
slack_info('U011NFAKS77', updated, 1586535245).
slack_info(var, instance, 'U011NGDBDLL').
slack_info('U011NGDBDLL', color, "4cc091").
slack_info('U011NGDBDLL', deleted, false).
slack_info('U011NGDBDLL', id, "U011NGDBDLL").
slack_info('U011NGDBDLL', is_admin, false).
slack_info('U011NGDBDLL', is_app_user, false).
slack_info('U011NGDBDLL', is_bot, false).
slack_info('U011NGDBDLL', is_owner, false).
slack_info('U011NGDBDLL', is_primary_owner, false).
slack_info('U011NGDBDLL', is_restricted, false).
slack_info('U011NGDBDLL', is_ultra_restricted, false).
slack_info('U011NGDBDLL', name, "gueletina.ksenia").
slack_info('U011NGDBDLL', presence, "away").
slack_info('U011NGDBDLL', profile, _{avatar_hash:"fe7767282f64", display_name:"", display_name_normalized:"", email:"gueletina.ksenia@gmail.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_1024.png", image_192:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_192.png", image_24:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_24.png", image_32:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_32.png", image_48:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_48.png", image_512:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_512.png", image_72:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_72.png", image_original:"https://avatars.slack-edge.com/2020-04-08/1046005745303_fe7767282f64f90b6b14_original.png", is_custom_image:true, phone:"", real_name:"Ksenia", real_name_normalized:"Ksenia", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011NGDBDLL', real_name, "Ksenia").
slack_info('U011NGDBDLL', team_id, "T010WMWBAGY").
slack_info('U011NGDBDLL', tz, "America/New_York").
slack_info('U011NGDBDLL', tz_label, "Eastern Daylight Time").
slack_info('U011NGDBDLL', tz_offset, -14400).
slack_info('U011NGDBDLL', updated, 1586369947).
slack_info(var, instance, 'U011NN3462X').
slack_info('U011NN3462X', color, "50a0cf").
slack_info('U011NN3462X', deleted, false).
slack_info('U011NN3462X', id, "U011NN3462X").
slack_info('U011NN3462X', is_admin, false).
slack_info('U011NN3462X', is_app_user, false).
slack_info('U011NN3462X', is_bot, false).
slack_info('U011NN3462X', is_owner, false).
slack_info('U011NN3462X', is_primary_owner, false).
slack_info('U011NN3462X', is_restricted, false).
slack_info('U011NN3462X', is_ultra_restricted, false).
slack_info('U011NN3462X', name, "thorsten.blum").
slack_info('U011NN3462X', presence, "away").
slack_info('U011NN3462X', profile, _{avatar_hash:"gc757cd5e7f8", display_name:"Thorsten", display_name_normalized:"Thorsten", email:"thorsten.blum@toblux.com", fields:null, image_192:"https://secure.gravatar.com/avatar/c757cd5e7f83c3be0dd187f234efa8e6.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-192.png", image_24:"https://secure.gravatar.com/avatar/c757cd5e7f83c3be0dd187f234efa8e6.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-24.png", image_32:"https://secure.gravatar.com/avatar/c757cd5e7f83c3be0dd187f234efa8e6.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-32.png", image_48:"https://secure.gravatar.com/avatar/c757cd5e7f83c3be0dd187f234efa8e6.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-48.png", image_512:"https://secure.gravatar.com/avatar/c757cd5e7f83c3be0dd187f234efa8e6.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-512.png", image_72:"https://secure.gravatar.com/avatar/c757cd5e7f83c3be0dd187f234efa8e6.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-72.png", phone:"", real_name:"Thorsten", real_name_normalized:"Thorsten", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011NN3462X', real_name, "Thorsten").
slack_info('U011NN3462X', team_id, "T010WMWBAGY").
slack_info('U011NN3462X', tz, "Europe/Amsterdam").
slack_info('U011NN3462X', tz_label, "Central European Summer Time").
slack_info('U011NN3462X', tz_offset, 7200).
slack_info('U011NN3462X', updated, 1586635045).
slack_info(var, instance, 'U011PF30S0M').
slack_info('U011PF30S0M', color, "ea2977").
slack_info('U011PF30S0M', deleted, false).
slack_info('U011PF30S0M', id, "U011PF30S0M").
slack_info('U011PF30S0M', is_admin, false).
slack_info('U011PF30S0M', is_app_user, false).
slack_info('U011PF30S0M', is_bot, false).
slack_info('U011PF30S0M', is_owner, false).
slack_info('U011PF30S0M', is_primary_owner, false).
slack_info('U011PF30S0M', is_restricted, false).
slack_info('U011PF30S0M', is_ultra_restricted, false).
slack_info('U011PF30S0M', name, "charlie.mac").
slack_info('U011PF30S0M', presence, "away").
slack_info('U011PF30S0M', profile, _{avatar_hash:"g9689c8e676c", display_name:"Charlie McMackin", display_name_normalized:"Charlie McMackin", email:"charlie.mac@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/9689c8e676c10e2b83062aa945c373e1.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-192.png", image_24:"https://secure.gravatar.com/avatar/9689c8e676c10e2b83062aa945c373e1.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-24.png", image_32:"https://secure.gravatar.com/avatar/9689c8e676c10e2b83062aa945c373e1.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-32.png", image_48:"https://secure.gravatar.com/avatar/9689c8e676c10e2b83062aa945c373e1.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-48.png", image_512:"https://secure.gravatar.com/avatar/9689c8e676c10e2b83062aa945c373e1.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-512.png", image_72:"https://secure.gravatar.com/avatar/9689c8e676c10e2b83062aa945c373e1.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-72.png", phone:"", real_name:"Charlie McMackin", real_name_normalized:"Charlie McMackin", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011PF30S0M', real_name, "Charlie McMackin").
slack_info('U011PF30S0M', team_id, "T010WMWBAGY").
slack_info('U011PF30S0M', tz, "America/Chicago").
slack_info('U011PF30S0M', tz_label, "Central Daylight Time").
slack_info('U011PF30S0M', tz_offset, -18000).
slack_info('U011PF30S0M', updated, 1586632541).
slack_info(var, instance, 'U011PGDJ07P').
slack_info('U011PGDJ07P', color, "d55aef").
slack_info('U011PGDJ07P', deleted, false).
slack_info('U011PGDJ07P', id, "U011PGDJ07P").
slack_info('U011PGDJ07P', is_admin, false).
slack_info('U011PGDJ07P', is_app_user, false).
slack_info('U011PGDJ07P', is_bot, true).
slack_info('U011PGDJ07P', is_owner, false).
slack_info('U011PGDJ07P', is_primary_owner, false).
slack_info('U011PGDJ07P', is_restricted, false).
slack_info('U011PGDJ07P', is_ultra_restricted, false).
slack_info('U011PGDJ07P', name, "prologclassbot").
slack_info('U011PGDJ07P', presence, "away").
slack_info('U011PGDJ07P', profile, _{always_active:false, api_app_id:"A011Z01HK0R", avatar_hash:"g019ffc4f191", bot_id:"B011PGDJ00M", display_name:"", display_name_normalized:"", fields:null, first_name:"OwlieBot", image_192:"https://secure.gravatar.com/avatar/019ffc4f191910bcd794f4546c14907c.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-192.png", image_24:"https://secure.gravatar.com/avatar/019ffc4f191910bcd794f4546c14907c.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-24.png", image_32:"https://secure.gravatar.com/avatar/019ffc4f191910bcd794f4546c14907c.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-32.png", image_48:"https://secure.gravatar.com/avatar/019ffc4f191910bcd794f4546c14907c.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-48.png", image_512:"https://secure.gravatar.com/avatar/019ffc4f191910bcd794f4546c14907c.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-512.png", image_72:"https://secure.gravatar.com/avatar/019ffc4f191910bcd794f4546c14907c.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0009-72.png", last_name:"", phone:"", real_name:"OwlieBot", real_name_normalized:"OwlieBot", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011PGDJ07P', real_name, "OwlieBot").
slack_info('U011PGDJ07P', team_id, "T010WMWBAGY").
slack_info('U011PGDJ07P', tz, "America/Los_Angeles").
slack_info('U011PGDJ07P', tz_label, "Pacific Daylight Time").
slack_info('U011PGDJ07P', tz_offset, -25200).
slack_info('U011PGDJ07P', updated, 1586659324).
slack_info(var, instance, 'U011TBU3DA4').
slack_info('U011TBU3DA4', color, "df3dc0").
slack_info('U011TBU3DA4', deleted, false).
slack_info('U011TBU3DA4', id, "U011TBU3DA4").
slack_info('U011TBU3DA4', is_admin, false).
slack_info('U011TBU3DA4', is_app_user, false).
slack_info('U011TBU3DA4', is_bot, false).
slack_info('U011TBU3DA4', is_owner, false).
slack_info('U011TBU3DA4', is_primary_owner, false).
slack_info('U011TBU3DA4', is_restricted, false).
slack_info('U011TBU3DA4', is_ultra_restricted, false).
slack_info('U011TBU3DA4', name, "danielkrueger").
slack_info('U011TBU3DA4', presence, "away").
slack_info('U011TBU3DA4', profile, _{avatar_hash:"g4f499cd8654", display_name:"Daniel Krueger", display_name_normalized:"Daniel Krueger", email:"danielkrueger@protonmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/4f499cd8654e9f924a0c2804844aee32.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-192.png", image_24:"https://secure.gravatar.com/avatar/4f499cd8654e9f924a0c2804844aee32.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-24.png", image_32:"https://secure.gravatar.com/avatar/4f499cd8654e9f924a0c2804844aee32.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-32.png", image_48:"https://secure.gravatar.com/avatar/4f499cd8654e9f924a0c2804844aee32.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-48.png", image_512:"https://secure.gravatar.com/avatar/4f499cd8654e9f924a0c2804844aee32.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-512.png", image_72:"https://secure.gravatar.com/avatar/4f499cd8654e9f924a0c2804844aee32.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-72.png", phone:"", real_name:"Daniel Krueger", real_name_normalized:"Daniel Krueger", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011TBU3DA4', real_name, "Daniel Krueger").
slack_info('U011TBU3DA4', team_id, "T010WMWBAGY").
slack_info('U011TBU3DA4', tz, "Europe/Amsterdam").
slack_info('U011TBU3DA4', tz_label, "Central European Summer Time").
slack_info('U011TBU3DA4', tz_offset, 7200).
slack_info('U011TBU3DA4', updated, 1586262305).
slack_info(var, instance, 'U011TBYESUQ').
slack_info('U011TBYESUQ', color, "4cc091").
slack_info('U011TBYESUQ', deleted, false).
slack_info('U011TBYESUQ', id, "U011TBYESUQ").
slack_info('U011TBYESUQ', is_admin, false).
slack_info('U011TBYESUQ', is_app_user, false).
slack_info('U011TBYESUQ', is_bot, false).
slack_info('U011TBYESUQ', is_owner, false).
slack_info('U011TBYESUQ', is_primary_owner, false).
slack_info('U011TBYESUQ', is_restricted, false).
slack_info('U011TBYESUQ', is_ultra_restricted, false).
slack_info('U011TBYESUQ', name, "vdamjanovic").
slack_info('U011TBYESUQ', presence, "away").
slack_info('U011TBYESUQ', profile, _{avatar_hash:"g0dbc3897b9a", display_name:"Violeta Damjanovic-Behrendt", display_name_normalized:"Violeta Damjanovic-Behrendt", email:"vdamjanovic@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/0dbc3897b9ad7a917fb68872e75661e3.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-192.png", image_24:"https://secure.gravatar.com/avatar/0dbc3897b9ad7a917fb68872e75661e3.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-24.png", image_32:"https://secure.gravatar.com/avatar/0dbc3897b9ad7a917fb68872e75661e3.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-32.png", image_48:"https://secure.gravatar.com/avatar/0dbc3897b9ad7a917fb68872e75661e3.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-48.png", image_512:"https://secure.gravatar.com/avatar/0dbc3897b9ad7a917fb68872e75661e3.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-512.png", image_72:"https://secure.gravatar.com/avatar/0dbc3897b9ad7a917fb68872e75661e3.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-72.png", phone:"", real_name:"Violeta Damjanovic-Behrendt", real_name_normalized:"Violeta Damjanovic-Behrendt", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011TBYESUQ', real_name, "Violeta Damjanovic-Behrendt").
slack_info('U011TBYESUQ', team_id, "T010WMWBAGY").
slack_info('U011TBYESUQ', tz, "Europe/Amsterdam").
slack_info('U011TBYESUQ', tz_label, "Central European Summer Time").
slack_info('U011TBYESUQ', tz_offset, 7200).
slack_info('U011TBYESUQ', updated, 1586262387).
slack_info(var, instance, 'U011TKT3WJC').
slack_info('U011TKT3WJC', color, "5870dd").
slack_info('U011TKT3WJC', deleted, false).
slack_info('U011TKT3WJC', id, "U011TKT3WJC").
slack_info('U011TKT3WJC', is_admin, false).
slack_info('U011TKT3WJC', is_app_user, false).
slack_info('U011TKT3WJC', is_bot, false).
slack_info('U011TKT3WJC', is_owner, false).
slack_info('U011TKT3WJC', is_primary_owner, false).
slack_info('U011TKT3WJC', is_restricted, false).
slack_info('U011TKT3WJC', is_ultra_restricted, false).
slack_info('U011TKT3WJC', name, "ben.eyal.89").
slack_info('U011TKT3WJC', presence, "away").
slack_info('U011TKT3WJC', profile, _{avatar_hash:"g27acb697cc7", display_name:"Ben Eyal", display_name_normalized:"Ben Eyal", email:"ben.eyal.89@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/27acb697cc7dfdeb1c3dedd2b145609e.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-192.png", image_24:"https://secure.gravatar.com/avatar/27acb697cc7dfdeb1c3dedd2b145609e.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-24.png", image_32:"https://secure.gravatar.com/avatar/27acb697cc7dfdeb1c3dedd2b145609e.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-32.png", image_48:"https://secure.gravatar.com/avatar/27acb697cc7dfdeb1c3dedd2b145609e.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-48.png", image_512:"https://secure.gravatar.com/avatar/27acb697cc7dfdeb1c3dedd2b145609e.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-512.png", image_72:"https://secure.gravatar.com/avatar/27acb697cc7dfdeb1c3dedd2b145609e.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0000-72.png", phone:"", real_name:"Ben Eyal", real_name_normalized:"Ben Eyal", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011TKT3WJC', real_name, "Ben Eyal").
slack_info('U011TKT3WJC', team_id, "T010WMWBAGY").
slack_info('U011TKT3WJC', tz, "Asia/Jerusalem").
slack_info('U011TKT3WJC', tz_label, "Israel Daylight Time").
slack_info('U011TKT3WJC', tz_offset, 10800).
slack_info('U011TKT3WJC', updated, 1586266280).
slack_info(var, instance, 'U011TQPPFJL').
slack_info('U011TQPPFJL', color, "de5f24").
slack_info('U011TQPPFJL', deleted, false).
slack_info('U011TQPPFJL', id, "U011TQPPFJL").
slack_info('U011TQPPFJL', is_admin, false).
slack_info('U011TQPPFJL', is_app_user, false).
slack_info('U011TQPPFJL', is_bot, false).
slack_info('U011TQPPFJL', is_owner, false).
slack_info('U011TQPPFJL', is_primary_owner, false).
slack_info('U011TQPPFJL', is_restricted, false).
slack_info('U011TQPPFJL', is_ultra_restricted, false).
slack_info('U011TQPPFJL', name, "evan").
slack_info('U011TQPPFJL', presence, "away").
slack_info('U011TQPPFJL', profile, _{avatar_hash:"g223daca478d", display_name:"evan", display_name_normalized:"evan", email:"evan@10000coins.com", fields:null, first_name:"Evan", image_192:"https://secure.gravatar.com/avatar/223daca478dc985e7d641e7bca08cfc3.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-192.png", image_24:"https://secure.gravatar.com/avatar/223daca478dc985e7d641e7bca08cfc3.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-24.png", image_32:"https://secure.gravatar.com/avatar/223daca478dc985e7d641e7bca08cfc3.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-32.png", image_48:"https://secure.gravatar.com/avatar/223daca478dc985e7d641e7bca08cfc3.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-48.png", image_512:"https://secure.gravatar.com/avatar/223daca478dc985e7d641e7bca08cfc3.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-512.png", image_72:"https://secure.gravatar.com/avatar/223daca478dc985e7d641e7bca08cfc3.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0025-72.png", last_name:"Pipho", phone:"", real_name:"Evan Pipho", real_name_normalized:"Evan Pipho", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011TQPPFJL', real_name, "Evan Pipho").
slack_info('U011TQPPFJL', team_id, "T010WMWBAGY").
slack_info('U011TQPPFJL', tz, "America/Los_Angeles").
slack_info('U011TQPPFJL', tz_label, "Pacific Daylight Time").
slack_info('U011TQPPFJL', tz_offset, -25200).
slack_info('U011TQPPFJL', updated, 1586268333).
slack_info(var, instance, 'U011TS37NMN').
slack_info('U011TS37NMN', color, "827327").
slack_info('U011TS37NMN', deleted, false).
slack_info('U011TS37NMN', id, "U011TS37NMN").
slack_info('U011TS37NMN', is_admin, false).
slack_info('U011TS37NMN', is_app_user, false).
slack_info('U011TS37NMN', is_bot, false).
slack_info('U011TS37NMN', is_owner, false).
slack_info('U011TS37NMN', is_primary_owner, false).
slack_info('U011TS37NMN', is_restricted, false).
slack_info('U011TS37NMN', is_ultra_restricted, false).
slack_info('U011TS37NMN', name, "xjoke").
slack_info('U011TS37NMN', presence, "away").
slack_info('U011TS37NMN', profile, _{avatar_hash:"2f0564d1fb2a", display_name:"Jonas", display_name_normalized:"Jonas", email:"xjoke@me.com", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1051552097697_2f0564d1fb2a9fce5797_original.jpg", is_custom_image:true, phone:"", real_name:"Jonas", real_name_normalized:"Jonas", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011TS37NMN', real_name, "Jonas").
slack_info('U011TS37NMN', team_id, "T010WMWBAGY").
slack_info('U011TS37NMN', tz, "Europe/Amsterdam").
slack_info('U011TS37NMN', tz_label, "Central European Summer Time").
slack_info('U011TS37NMN', tz_offset, 7200).
slack_info('U011TS37NMN', updated, 1586269352).
slack_info(var, instance, 'U011UA961R6').
slack_info('U011UA961R6', color, "965d1b").
slack_info('U011UA961R6', deleted, false).
slack_info('U011UA961R6', id, "U011UA961R6").
slack_info('U011UA961R6', is_admin, false).
slack_info('U011UA961R6', is_app_user, false).
slack_info('U011UA961R6', is_bot, false).
slack_info('U011UA961R6', is_owner, false).
slack_info('U011UA961R6', is_primary_owner, false).
slack_info('U011UA961R6', is_restricted, false).
slack_info('U011UA961R6', is_ultra_restricted, false).
slack_info('U011UA961R6', name, "andrekw").
slack_info('U011UA961R6', presence, "away").
slack_info('U011UA961R6', profile, _{avatar_hash:"g0839f3eb2f6", display_name:"Andre", display_name_normalized:"Andre", email:"andrekw@gmail.com", fields:null, first_name:"Andre", image_192:"https://secure.gravatar.com/avatar/0839f3eb2f61f264c57a1626766eaa8a.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0008-192.png", image_24:"https://secure.gravatar.com/avatar/0839f3eb2f61f264c57a1626766eaa8a.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0008-24.png", image_32:"https://secure.gravatar.com/avatar/0839f3eb2f61f264c57a1626766eaa8a.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0008-32.png", image_48:"https://secure.gravatar.com/avatar/0839f3eb2f61f264c57a1626766eaa8a.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0008-48.png", image_512:"https://secure.gravatar.com/avatar/0839f3eb2f61f264c57a1626766eaa8a.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0008-512.png", image_72:"https://secure.gravatar.com/avatar/0839f3eb2f61f264c57a1626766eaa8a.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0008-72.png", last_name:"KW", phone:"", real_name:"Andre KW", real_name_normalized:"Andre KW", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:"Machine Learning"}).
slack_info('U011UA961R6', real_name, "Andre KW").
slack_info('U011UA961R6', team_id, "T010WMWBAGY").
slack_info('U011UA961R6', tz, "Europe/Amsterdam").
slack_info('U011UA961R6', tz_label, "Central European Summer Time").
slack_info('U011UA961R6', tz_offset, 7200).
slack_info('U011UA961R6', updated, 1586274514).
slack_info(var, instance, 'U011V30M664').
slack_info('U011V30M664', color, "bc3663").
slack_info('U011V30M664', deleted, false).
slack_info('U011V30M664', id, "U011V30M664").
slack_info('U011V30M664', is_admin, false).
slack_info('U011V30M664', is_app_user, false).
slack_info('U011V30M664', is_bot, false).
slack_info('U011V30M664', is_owner, false).
slack_info('U011V30M664', is_primary_owner, false).
slack_info('U011V30M664', is_restricted, false).
slack_info('U011V30M664', is_ultra_restricted, false).
slack_info('U011V30M664', name, "blutokyo").
slack_info('U011V30M664', presence, "away").
slack_info('U011V30M664', profile, _{avatar_hash:"geabb3a498ac", display_name:"curtosis", display_name_normalized:"curtosis", email:"blutokyo@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/eabb3a498acf7f72f7faff63a5be1029.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-192.png", image_24:"https://secure.gravatar.com/avatar/eabb3a498acf7f72f7faff63a5be1029.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-24.png", image_32:"https://secure.gravatar.com/avatar/eabb3a498acf7f72f7faff63a5be1029.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-32.png", image_48:"https://secure.gravatar.com/avatar/eabb3a498acf7f72f7faff63a5be1029.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-48.png", image_512:"https://secure.gravatar.com/avatar/eabb3a498acf7f72f7faff63a5be1029.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-512.png", image_72:"https://secure.gravatar.com/avatar/eabb3a498acf7f72f7faff63a5be1029.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0010-72.png", phone:"", real_name:"curtosis", real_name_normalized:"curtosis", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011V30M664', real_name, "curtosis").
slack_info('U011V30M664', team_id, "T010WMWBAGY").
slack_info('U011V30M664', tz, "America/New_York").
slack_info('U011V30M664', tz_label, "Eastern Daylight Time").
slack_info('U011V30M664', tz_offset, -14400).
slack_info('U011V30M664', updated, 1586283942).
slack_info(var, instance, 'U011VK86RS4').
slack_info('U011VK86RS4', color, "4bbe2e").
slack_info('U011VK86RS4', deleted, false).
slack_info('U011VK86RS4', id, "U011VK86RS4").
slack_info('U011VK86RS4', is_admin, false).
slack_info('U011VK86RS4', is_app_user, false).
slack_info('U011VK86RS4', is_bot, false).
slack_info('U011VK86RS4', is_owner, false).
slack_info('U011VK86RS4', is_primary_owner, false).
slack_info('U011VK86RS4', is_restricted, false).
slack_info('U011VK86RS4', is_ultra_restricted, false).
slack_info('U011VK86RS4', name, "andrzej").
slack_info('U011VK86RS4', presence, "away").
slack_info('U011VK86RS4', profile, _{avatar_hash:"6d89630a4e25", display_name:"Andrzej P", display_name_normalized:"Andrzej P", email:"andrzej@prochyra.name", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-07/1051487186117_6d89630a4e2545b6f0a8_original.jpg", is_custom_image:true, phone:"", real_name:"Andrzej P", real_name_normalized:"Andrzej P", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011VK86RS4', real_name, "Andrzej P").
slack_info('U011VK86RS4', team_id, "T010WMWBAGY").
slack_info('U011VK86RS4', tz, "Europe/London").
slack_info('U011VK86RS4', tz_label, "British Summer Time").
slack_info('U011VK86RS4', tz_offset, 3600).
slack_info('U011VK86RS4', updated, 1586292231).
slack_info(var, instance, 'U011VNH0SQ4').
slack_info('U011VNH0SQ4', color, "d1707d").
slack_info('U011VNH0SQ4', deleted, false).
slack_info('U011VNH0SQ4', id, "U011VNH0SQ4").
slack_info('U011VNH0SQ4', is_admin, false).
slack_info('U011VNH0SQ4', is_app_user, false).
slack_info('U011VNH0SQ4', is_bot, false).
slack_info('U011VNH0SQ4', is_owner, false).
slack_info('U011VNH0SQ4', is_primary_owner, false).
slack_info('U011VNH0SQ4', is_restricted, false).
slack_info('U011VNH0SQ4', is_ultra_restricted, false).
slack_info('U011VNH0SQ4', name, "adamjwolf").
slack_info('U011VNH0SQ4', presence, "away").
slack_info('U011VNH0SQ4', profile, _{avatar_hash:"09b72d9c4dbb", display_name:"Adam J Wolf", display_name_normalized:"Adam J Wolf", email:"adamjwolf@me.com", fields:[], first_name:"Adam", image_1024:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-11/1046651636343_09b72d9c4dbb87280f86_original.jpg", is_custom_image:true, last_name:"J Wolf", phone:"", real_name:"Adam J Wolf", real_name_normalized:"Adam J Wolf", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:"Principle Engineer @Emailage"}).
slack_info('U011VNH0SQ4', real_name, "Adam J Wolf").
slack_info('U011VNH0SQ4', team_id, "T010WMWBAGY").
slack_info('U011VNH0SQ4', tz, "America/Chicago").
slack_info('U011VNH0SQ4', tz_label, "Central Daylight Time").
slack_info('U011VNH0SQ4', tz_offset, -18000).
slack_info('U011VNH0SQ4', updated, 1586649360).
slack_info(var, instance, 'U011Y5Z45FB').
slack_info('U011Y5Z45FB', color, "a63024").
slack_info('U011Y5Z45FB', deleted, false).
slack_info('U011Y5Z45FB', id, "U011Y5Z45FB").
slack_info('U011Y5Z45FB', is_admin, false).
slack_info('U011Y5Z45FB', is_app_user, false).
slack_info('U011Y5Z45FB', is_bot, false).
slack_info('U011Y5Z45FB', is_owner, false).
slack_info('U011Y5Z45FB', is_primary_owner, false).
slack_info('U011Y5Z45FB', is_restricted, false).
slack_info('U011Y5Z45FB', is_ultra_restricted, false).
slack_info('U011Y5Z45FB', name, "swiclass.frijas").
slack_info('U011Y5Z45FB', presence, "away").
slack_info('U011Y5Z45FB', profile, _{avatar_hash:"1b23c8c1402b", display_name:"Gerardo Sanchez", display_name_normalized:"Gerardo Sanchez", email:"swiclass.frijas@xoxy.net", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_1024.jpg", image_192:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_192.jpg", image_24:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_24.jpg", image_32:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_32.jpg", image_48:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_48.jpg", image_512:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_512.jpg", image_72:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_72.jpg", image_original:"https://avatars.slack-edge.com/2020-04-10/1053136195190_1b23c8c1402be41e503b_original.jpg", is_custom_image:true, phone:"", real_name:"Gerardo Sanchez", real_name_normalized:"Gerardo Sanchez", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011Y5Z45FB', real_name, "Gerardo Sanchez").
slack_info('U011Y5Z45FB', team_id, "T010WMWBAGY").
slack_info('U011Y5Z45FB', tz, "America/Los_Angeles").
slack_info('U011Y5Z45FB', tz_label, "Pacific Daylight Time").
slack_info('U011Y5Z45FB', tz_offset, -25200).
slack_info('U011Y5Z45FB', updated, 1586576517).
slack_info(var, instance, 'U011Z6B31UH').
slack_info('U011Z6B31UH', color, "43761b").
slack_info('U011Z6B31UH', deleted, false).
slack_info('U011Z6B31UH', id, "U011Z6B31UH").
slack_info('U011Z6B31UH', is_admin, false).
slack_info('U011Z6B31UH', is_app_user, false).
slack_info('U011Z6B31UH', is_bot, false).
slack_info('U011Z6B31UH', is_owner, false).
slack_info('U011Z6B31UH', is_primary_owner, false).
slack_info('U011Z6B31UH', is_restricted, false).
slack_info('U011Z6B31UH', is_ultra_restricted, false).
slack_info('U011Z6B31UH', name, "logicmoo").
slack_info('U011Z6B31UH', presence, "away").
slack_info('U011Z6B31UH', profile, _{avatar_hash:"g7d3b9ee1332", display_name:"DouglasRMiles", display_name_normalized:"DouglasRMiles", email:"logicmoo@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/7d3b9ee133249361afc2b73eb4639038.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-192.png", image_24:"https://secure.gravatar.com/avatar/7d3b9ee133249361afc2b73eb4639038.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-24.png", image_32:"https://secure.gravatar.com/avatar/7d3b9ee133249361afc2b73eb4639038.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-32.png", image_48:"https://secure.gravatar.com/avatar/7d3b9ee133249361afc2b73eb4639038.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-48.png", image_512:"https://secure.gravatar.com/avatar/7d3b9ee133249361afc2b73eb4639038.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-512.png", image_72:"https://secure.gravatar.com/avatar/7d3b9ee133249361afc2b73eb4639038.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0019-72.png", phone:"", real_name:"DouglasRMiles", real_name_normalized:"DouglasRMiles", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U011Z6B31UH', real_name, "DouglasRMiles").
slack_info('U011Z6B31UH', team_id, "T010WMWBAGY").
slack_info('U011Z6B31UH', tz, "America/Los_Angeles").
slack_info('U011Z6B31UH', tz_label, "Pacific Daylight Time").
slack_info('U011Z6B31UH', tz_offset, -25200).
slack_info('U011Z6B31UH', updated, 1586663823).
slack_info(var, instance, 'U01201RSD8Q').
slack_info('U01201RSD8Q', color, "d58247").
slack_info('U01201RSD8Q', deleted, false).
slack_info('U01201RSD8Q', id, "U01201RSD8Q").
slack_info('U01201RSD8Q', is_admin, false).
slack_info('U01201RSD8Q', is_app_user, false).
slack_info('U01201RSD8Q', is_bot, false).
slack_info('U01201RSD8Q', is_owner, false).
slack_info('U01201RSD8Q', is_primary_owner, false).
slack_info('U01201RSD8Q', is_restricted, false).
slack_info('U01201RSD8Q', is_ultra_restricted, false).
slack_info('U01201RSD8Q', name, "steven.b.stone").
slack_info('U01201RSD8Q', presence, "away").
slack_info('U01201RSD8Q', profile, _{avatar_hash:"g07e32717609", display_name:"Steve Stone", display_name_normalized:"Steve Stone", email:"steven.b.stone@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/07e32717609b977581d0fe71682f61c3.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-192.png", image_24:"https://secure.gravatar.com/avatar/07e32717609b977581d0fe71682f61c3.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-24.png", image_32:"https://secure.gravatar.com/avatar/07e32717609b977581d0fe71682f61c3.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-32.png", image_48:"https://secure.gravatar.com/avatar/07e32717609b977581d0fe71682f61c3.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-48.png", image_512:"https://secure.gravatar.com/avatar/07e32717609b977581d0fe71682f61c3.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-512.png", image_72:"https://secure.gravatar.com/avatar/07e32717609b977581d0fe71682f61c3.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0005-72.png", phone:"", real_name:"Steve Stone", real_name_normalized:"Steve Stone", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U01201RSD8Q', real_name, "Steve Stone").
slack_info('U01201RSD8Q', team_id, "T010WMWBAGY").
slack_info('U01201RSD8Q', tz, "America/New_York").
slack_info('U01201RSD8Q', tz_label, "Eastern Daylight Time").
slack_info('U01201RSD8Q', tz_offset, -14400).
slack_info('U01201RSD8Q', updated, 1586374264).
slack_info(var, instance, 'U0127DHKV4G').
slack_info('U0127DHKV4G', color, "53b759").
slack_info('U0127DHKV4G', deleted, false).
slack_info('U0127DHKV4G', id, "U0127DHKV4G").
slack_info('U0127DHKV4G', is_admin, false).
slack_info('U0127DHKV4G', is_app_user, false).
slack_info('U0127DHKV4G', is_bot, false).
slack_info('U0127DHKV4G', is_owner, false).
slack_info('U0127DHKV4G', is_primary_owner, false).
slack_info('U0127DHKV4G', is_restricted, false).
slack_info('U0127DHKV4G', is_ultra_restricted, false).
slack_info('U0127DHKV4G', name, "josephtaber").
slack_info('U0127DHKV4G', presence, "away").
slack_info('U0127DHKV4G', profile, _{avatar_hash:"g01407b47e57", display_name:"Joe Taber", display_name_normalized:"Joe Taber", email:"josephtaber@gmail.com", fields:null, image_192:"https://secure.gravatar.com/avatar/01407b47e57ece60bf0220d6ba0b521c.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-192.png", image_24:"https://secure.gravatar.com/avatar/01407b47e57ece60bf0220d6ba0b521c.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-24.png", image_32:"https://secure.gravatar.com/avatar/01407b47e57ece60bf0220d6ba0b521c.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-32.png", image_48:"https://secure.gravatar.com/avatar/01407b47e57ece60bf0220d6ba0b521c.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-48.png", image_512:"https://secure.gravatar.com/avatar/01407b47e57ece60bf0220d6ba0b521c.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-512.png", image_72:"https://secure.gravatar.com/avatar/01407b47e57ece60bf0220d6ba0b521c.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0017-72.png", phone:"", real_name:"Joe Taber", real_name_normalized:"Joe Taber", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U0127DHKV4G', real_name, "Joe Taber").
slack_info('U0127DHKV4G', team_id, "T010WMWBAGY").
slack_info('U0127DHKV4G', tz, "America/Chicago").
slack_info('U0127DHKV4G', tz_label, "Central Daylight Time").
slack_info('U0127DHKV4G', tz_offset, -18000).
slack_info('U0127DHKV4G', updated, 1586537358).
slack_info(var, instance, 'U0128BERRAL').
slack_info('U0128BERRAL', color, "385a86").
slack_info('U0128BERRAL', deleted, false).
slack_info('U0128BERRAL', id, "U0128BERRAL").
slack_info('U0128BERRAL', is_admin, false).
slack_info('U0128BERRAL', is_app_user, false).
slack_info('U0128BERRAL', is_bot, false).
slack_info('U0128BERRAL', is_owner, false).
slack_info('U0128BERRAL', is_primary_owner, false).
slack_info('U0128BERRAL', is_restricted, false).
slack_info('U0128BERRAL', is_ultra_restricted, false).
slack_info('U0128BERRAL', name, "anvilforwork").
slack_info('U0128BERRAL', presence, "away").
slack_info('U0128BERRAL', profile, _{avatar_hash:"g90b5fa0ea32", display_name:"Josh Lefley", display_name_normalized:"Josh Lefley", email:"anvilforwork@gmail.com", fields:[], first_name:"Josh", image_192:"https://secure.gravatar.com/avatar/90b5fa0ea32490444e215b0b535c1a7d.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-192.png", image_24:"https://secure.gravatar.com/avatar/90b5fa0ea32490444e215b0b535c1a7d.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-24.png", image_32:"https://secure.gravatar.com/avatar/90b5fa0ea32490444e215b0b535c1a7d.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-32.png", image_48:"https://secure.gravatar.com/avatar/90b5fa0ea32490444e215b0b535c1a7d.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-48.png", image_512:"https://secure.gravatar.com/avatar/90b5fa0ea32490444e215b0b535c1a7d.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-512.png", image_72:"https://secure.gravatar.com/avatar/90b5fa0ea32490444e215b0b535c1a7d.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2Fdf10d%2Fimg%2Favatars%2Fava_0020-72.png", last_name:"Lefley", phone:"", real_name:"Josh Lefley", real_name_normalized:"Josh Lefley", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('U0128BERRAL', real_name, "Josh Lefley").
slack_info('U0128BERRAL', team_id, "T010WMWBAGY").
slack_info('U0128BERRAL', tz, "America/Chicago").
slack_info('U0128BERRAL', tz_label, "Central Daylight Time").
slack_info('U0128BERRAL', tz_offset, -18000).
slack_info('U0128BERRAL', updated, 1586556704).
slack_info(var, instance, 'U0129P82V6U').
slack_info('U0129P82V6U', color, "e06b56").
slack_info('U0129P82V6U', deleted, false).
slack_info('U0129P82V6U', id, "U0129P82V6U").
slack_info('U0129P82V6U', is_admin, false).
slack_info('U0129P82V6U', is_app_user, false).
slack_info('U0129P82V6U', is_bot, true).
slack_info('U0129P82V6U', is_owner, false).
slack_info('U0129P82V6U', is_primary_owner, false).
slack_info('U0129P82V6U', is_restricted, false).
slack_info('U0129P82V6U', is_ultra_restricted, false).
slack_info('U0129P82V6U', name, "prolog_bot").
slack_info('U0129P82V6U', presence, "away").
slack_info('U0129P82V6U', profile, _{always_active:false, api_app_id:"A00", avatar_hash:"6919abfbc0fe", bot_id:"B0129P82UTS", display_name:"", display_name_normalized:"", fields:null, image_1024:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_1024.gif", image_192:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_192.gif", image_24:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_24.gif", image_32:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_32.gif", image_48:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_48.gif", image_512:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_512.gif", image_72:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_72.gif", image_original:"https://avatars.slack-edge.com/2020-04-11/1056992749091_6919abfbc0fee9ccad66_original.gif", is_custom_image:true, phone:"", real_name:"prolog_bot", real_name_normalized:"prolog_bot", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:"This bot handles messages to and from slack such as evaluation of prolog code"}).
slack_info('U0129P82V6U', real_name, "prolog_bot").
slack_info('U0129P82V6U', team_id, "T010WMWBAGY").
slack_info('U0129P82V6U', tz, "America/Los_Angeles").
slack_info('U0129P82V6U', tz_label, "Pacific Daylight Time").
slack_info('U0129P82V6U', tz_offset, -25200).
slack_info('U0129P82V6U', updated, 1586665957).
slack_info(var, instance, 'USLACKBOT').
slack_info('USLACKBOT', color, "757575").
slack_info('USLACKBOT', deleted, false).
slack_info('USLACKBOT', id, "USLACKBOT").
slack_info('USLACKBOT', is_admin, false).
slack_info('USLACKBOT', is_app_user, false).
slack_info('USLACKBOT', is_bot, false).
slack_info('USLACKBOT', is_owner, false).
slack_info('USLACKBOT', is_primary_owner, false).
slack_info('USLACKBOT', is_restricted, false).
slack_info('USLACKBOT', is_ultra_restricted, false).
slack_info('USLACKBOT', name, "slackbot").
slack_info('USLACKBOT', presence, "active").
slack_info('USLACKBOT', profile, _{always_active:true, avatar_hash:"sv41d8cd98f0", display_name:"Slackbot", display_name_normalized:"Slackbot", fields:null, first_name:"slackbot", image_192:"https://a.slack-edge.com/80588/marketing/img/avatars/slackbot/avatar-slackbot.png", image_24:"https://a.slack-edge.com/80588/img/slackbot_24.png", image_32:"https://a.slack-edge.com/80588/img/slackbot_32.png", image_48:"https://a.slack-edge.com/80588/img/slackbot_48.png", image_512:"https://a.slack-edge.com/80588/img/slackbot_512.png", image_72:"https://a.slack-edge.com/80588/img/slackbot_72.png", last_name:"", phone:"", real_name:"Slackbot", real_name_normalized:"Slackbot", skype:"", status_emoji:"", status_expiration:0, status_text:"", status_text_canonical:"", team:"T010WMWBAGY", title:""}).
slack_info('USLACKBOT', real_name, "Slackbot").
slack_info('USLACKBOT', team_id, "T010WMWBAGY").
slack_info('USLACKBOT', tz, null).
slack_info('USLACKBOT', tz_label, "Pacific Daylight Time").
slack_info('USLACKBOT', tz_offset, -25200).
slack_info('USLACKBOT', updated, 0).

% unknown(slack_receive(var,_14418{type:"hello"}))

