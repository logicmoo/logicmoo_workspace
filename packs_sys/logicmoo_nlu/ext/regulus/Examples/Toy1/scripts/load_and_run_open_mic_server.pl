
:- compile('$REGULUS/Examples/Generic/Prolog/open_mic_server').

:- server(3975, 3995, '$REGULUS/Examples/Toy1/Generated/recogniser', 'client.TTSAddresses=localhost:32323 audio.OutputVolume=200').

:- halt.
