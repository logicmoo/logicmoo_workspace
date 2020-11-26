@rem **********************************************************************************
@rem
@rem CONVENIENCE SCRIPT FOR RUNNING GODIS-BASIC WITH NUANCE ASR AND TTS.
@rem SEE README.TXT IN DIRECTORY @ROOT_DIR@\examples


start recserver -package C:\Documents and Settings\becca\Mina dokument\AgendaTalk04\godis-apps\domain-agendatalk\Resources\asrg_agenda_english lm.Addresses=localhost
start vocalizer -language USEnglish
pause
java -cp C:\Documents and Settings\becca\Mina dokument\AgendaTalk04\trindikit\dist\classes;D:\Nuance8.5\v8.0.0\java\nsc.jar;D:\Nuance8.5\v8.0.0\java\jsc-source.jar;D:\Nuance8.5\v8.0.0\java\vcomsc.jar;C:\Documents and Settings\becca\Mina dokument\AgendaTalk04\trindikit\lib\jars\oaa2.jar se.gu.ling.trindikit.oaa.nuance.nsc.OAABasicNuanceSpeechChannel -package C:\Documents and Settings\becca\Mina dokument\AgendaTalk04\godis-apps\domain-agendatalk\Resources\asrg_agenda_english lm.Addresses=localhost client.TTSAddresses=localhost:32323
pause

