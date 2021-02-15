
:- expects_dialect(lps).

% golps('examples/forTesting/backgroundRain2.lps',[background(ID)])
% inject_events(ID,[rain(-1)],Result)
% inject_events(ID,[lps_terminate],Result)
% swish:
% go(_, [background(TableId),silent]).
% interpreter:inject_events(lps1,[rain(miguel)],Result).
% interpreter:inject_events(lps1,[lps_terminate],Result).
maxRealTime(60).
minCycleTime(1).
events rain(_), lps_terminate.
if rain(_) then writeln('IT RAINED!') from T1 to T2.
