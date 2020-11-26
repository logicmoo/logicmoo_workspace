
apache_log_hap(Content,Time) -->
    {Content = log(IP,Ident,UserID,ReadableTime,Request,Status,Size,Referer,Browser)},
    ip(IP)," ",
    ident(Ident)," ",
    user_id(UserID)," ",
    apache_time(ReadableTime,Time)," ",
    request(Request)," ",
    integer_na(Status)," ",
    integer_na(Size)," ",
    referer(Referer)," ",
    browser(Browser),"\n".

ip(IP) -->
    integer(I3), ".",
    integer(I2), ".",
    integer(I1), ".",
    integer(I0),
    {IP is I0+I1*256+I2*256*256+I3*256*256*256}.

ident(_) -->
    "-", {!}.
ident(H) -->
    alphanumeric_string(String),
    {(String = [F|R], F>=65, F=< 90 % If it is capitalised convert to lower case
        ->  F1 is F+32,
            String1 = [F1|R]
        ;   String1 = String),
    atom_codes(H,String1)}.
    %{write("FOUND IDENTITY: NEEDED IMPROVEMENT OF APACHE_LOG_PARSER"),nl}.

user_id(_) -->
    "-", {!}.
user_id(H) -->
    alphanumeric_string(String),
    {(String = [F|R], F>=65, F=< 90 % If it is capitalised convert to lower case
        ->  F1 is F+32,
            String1 = [F1|R]
        ;   String1 = String),
    atom_codes(H,String1)}.
    %{write("FOUND IDENTITY: NEEDED IMPROVEMENT OF APACHE_LOG_PARSER"),nl}.

apache_time(ReadableTime,Time) -->
    {ReadableTime = time(Day/Month/Year,Hour/Min/Sec,TimeZone)},
    "[", integer(Day), "/", month(Month,MonthNum), "/", integer(Year), ":", integer(Hour),
    ":", integer(Min), ":", integer(Sec), " ", signed_int(TZ), "]",
    {T1 is (Year-2000)+12*MonthNum,
    T2 is T1*30+Day,
    T3 is T2*24+Hour,
    T4 is T3*60+Min,
    Time is T4*60+Sec,
    TimeZone is TZ//100}.

month(jan,1) --> "Jan".
month(feb,2) --> "Feb".
month(mar,3) --> "Mar".
month(apr,4) --> "Apr".
month(may,5) --> "May".
month(jun,6) --> "Jun".
month(jul,7) --> "Jul".
month(aug,8) --> "Aug".
month(sep,9) --> "Sep".
month(oct,10) --> "Oct".
month(nov,11) --> "Nov".
month(dec,12) --> "Dec".

request(get(Path)) -->
    [34], % character '"'
    "GET ", path(Path), " HTTP/", version, [34]. 
request(post(Path)) -->
    [34], % character '"'
    "POST ", path(Path), " HTTP/", version, [34].

path(_) --> "-".
path([]) --> [].
path(L) -->
    path_separator,
    path(L).
path([H|T]) -->
    alphanumeric_string(String),
    {(String = [F|R], F>=65, F=< 90 % If it is capitalised convert to lower case
        ->  F1 is F+32,
            String1 = [F1|R]
        ;   String1 = String),
    atom_codes(H,String1)},
    path(T).

path_separator --> "/".
path_separator --> "?".
path_separator --> ".".
path_separator --> ":".
path_separator --> "=".
path_separator --> "&".
path_separator --> "+".

version --> integer(_).
version --> integer(_), ".", integer(_).

% Integer or not available
integer_na(Int) --> integer(Int).
integer_na(_) --> "-".

referer(Ref) --> [34], % character '"'
    path(Ref), [34].

browser(Browser) -->
    [34], any_string(CodeList), [34],
    {append([39|CodeList],[39],ListWithQuotations),
     atom_codes(Browser,ListWithQuotations)}.
    

any_string([]) --> [].
any_string([H|T]) --> any_char_excluded_nl(H), any_string(T).


any_char_excluded_nl(Code) -->
	[Code],
	{
		char_code('\n', A),
		Code =\= A
	}.
