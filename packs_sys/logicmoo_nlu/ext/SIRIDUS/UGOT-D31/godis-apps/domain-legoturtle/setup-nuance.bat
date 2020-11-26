@rem **********************************************************************************
@rem
@rem CONVENIENCE SCRIPT FOR RUNNING GODIS-BASIC WITH NUANCE ASR AND TTS.
@rem SEE README.TXT IN DIRECTORY @ROOT_DIR@\examples

start recserver -package C:\MyCVS\godis\dist\prolog\godis\domain-legoturtle\asrg_legoturtle_english lm.Addresses=localhost
start vocalizer
java -cp C:\MyCVS\trindikit\dist\classes;C:\Program\Nuance\v8.0.0\java\nsc.jar;C:\Program\Nuance\v8.0.0\java\jsc-source.jar;C:\Program\Nuance\v8.0.0\java\vcomsc.jar;C:\MyCVS\trindikit\lib\jars\oaa2.jar se.gu.ling.trindikit.oaa.nuance.nsc.OAABasicNuanceSpeechChannel -package C:\MyCVS\godis\dist\prolog\godis\domain-legoturtle\asrg_legoturtle_english lm.Addresses=localhost client.TTSAddresses=localhost:32323
pause

