/*************************************************************************

         name: stations_player.pl 
	 date: 2004-10-25
       author: Andreas Wallentin
  
*************************************************************************/

:-dynamic radio_station/1.

radio_station( X ):-
	radio_station(X,_)
	;
	atomic(X),
	atom_concat('http://',X,Station),
	radio_station(_,Station).

get_station( Station, IP ):-
	radio_station(Station,IP)
	;
	Station = IP.
	

%% station( +Name, +IpAddress )
%% station('RR_radio',www.xxx.yyy.zzz:portNo)
%radio_station( [digital,gunfire], '128.16.159.166:8000' ).
%radio_station( [rant,radio],      '130.240.207.88:9090' ).%http://198.172.235.21:8000
radio_station( [digital,gunfire], 'http://129.16.159.166:8000' ).
radio_station( [rant,radio],      'http://130.240.207.88:9090' ).
radio_station( [absolute,fm],     'http://stream.absoluutfm.org:8200' ).
radio_station( [chat,radio],      'http://chatradio.myftp.org:10010' ).
radio_station( [ebm,radio],       'http://213.158.118.85:7350' ).
