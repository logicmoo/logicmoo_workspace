name(slack_prolog).
version('2.0.3').
title('Prolog interface to Slack http://www.slack.com').
keywords([chat,bots,slack]).
author( 'Douglas Miles', 'http://www.linkedin.com/in/logicmoo' ).
packager( 'logicmoo/LogicMoo', 'https://github.com/logicmoo/' ).
maintainer( 'logicmoo', 'https://github.com/logicmoo/' ).
home( 'https://github.com/swi-to-yap/slack_prolog' ).
download( 'https://github.com/swi-to-yap/slack_prolog/release/*.zip' ).
provides(slack_client).
requires(dictoo).
