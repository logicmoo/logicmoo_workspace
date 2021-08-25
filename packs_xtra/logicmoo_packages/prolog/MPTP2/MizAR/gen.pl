#!/usr/bin/perl -w
# generate problems on mws server

use HTTP::Request::Common qw(POST);
      use LWP::UserAgent;
      $ua = new LWP::UserAgent;

my @names = ('xcmplx_0', 'urysohn3', 'dynkin', 'euclid_7', 'binari_3',
'comseq_2', 'finseq_1', 'taxonom1', 'trees_1', 'connsp_1', 'group_10',
'setlim_1', 'numbers', 'scmfsa_7', 'topgen_5', 'integra6', 'nat_4',
'rat_1', 'sin_cos9', 'fuzzy_4', 'cfuncdom', 'grnilp_1', 'lmod_5',
'multop_1', 'msafree1', 'comseq_1', 'scmfsa6c', 'integra7', 'fdiff_3',
'jordan6');


foreach my $aname (@names)
  {

      print "\n $aname \n";

      my $req = POST 'http://mws.cs.ru.nl/~mptp/cgi-bin/MizAR1096.cgi',
                    [
ProblemSource => 'URL',
FormulaURL =>'http://mws.cs.ru.nl/~mptp/mml4.145.1096/mml/' . $aname . '.miz',
Name => 'q' . substr($aname,1),
MMLVersion => '4.145.1096',
Verify => 1,
HTMLize => 1,
Parallelize => 1,
GenATP => 1,
ProveUnsolved => 'None',
MODE => 'TEXT' ];


print $ua->request($req)->as_string;
    }
