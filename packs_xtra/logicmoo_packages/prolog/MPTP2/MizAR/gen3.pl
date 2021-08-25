#!/usr/bin/perl -w
# generate problems on mws server

use HTTP::Request::Common qw(POST);
      use LWP::UserAgent;
      $ua = new LWP::UserAgent;
$ua->timeout(100000);


my @names = @ARGV;
## my @names = ('jgraph_4');


foreach my $aname (@names)
  {

      print "\n $aname \n";

      my $req = POST 'http://mws.cs.ru.nl/~mptp/cgi-bin/MizAR1096.cgi',
                    [
ProblemSource => 'URL',
FormulaURL =>'http://mws.cs.ru.nl/~mptp/mml4.160.1126/mml/' . $aname . '.miz',
Name => $aname,
MMLVersion => '4.160.1126',
Verify => 1,
Parallelize => 1,
ProveUnsolved => 'All',
ATPMode => 7,
MODE => 'TEXT' ];


print $ua->request($req)->as_string;
    }
