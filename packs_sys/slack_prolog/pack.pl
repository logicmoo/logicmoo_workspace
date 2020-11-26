name(slack_prolog).
version('2.0.3').
title('Prolog interface to Slack http://www.slack.com').
keywords([chat,bots,slack]).
author( 'Douglas Miles', 'http://www.linkedin.com/in/logicmoo' ).
packager( 'TeamSPoon/LogicMoo', 'https://github.com/TeamSPoon/' ).
maintainer( 'TeamSPoon', 'https://github.com/TeamSPoon/' ).
home( 'https://github.com/swi-to-yap/slack_prolog' ).
download( 'https://github.com/swi-to-yap/slack_prolog/release/*.zip' ).
provides(slack_client).
requires(dictoo).
