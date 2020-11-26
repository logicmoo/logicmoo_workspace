
The file apcontinuous.dll is an audio provider that provides
full-duplex capabilities, i.e. can support both input and output
simultaneously. This is essential for any application that is to allow
barge-in. To use apcontinuous.dll:

1) copy the file to $NUANCE/bin/win32
2) set audio.Provider=continuous in the Nuance parameters passed to your app

apcontinuous.dll only works on Windows machines. It was originally written for
Nuance 8.0, but appears to work correctly under 8.5 as well.

