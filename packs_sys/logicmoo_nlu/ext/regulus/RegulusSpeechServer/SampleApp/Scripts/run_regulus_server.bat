del /q C:\Temp\RegulusServer\*.txt
cmd /k ..\..\runtime\RegulusServer.exe regulus_server.cfg -package ..\runtime\toy1 audio.Provider=native client.TTSAddresses=localhost:32323 