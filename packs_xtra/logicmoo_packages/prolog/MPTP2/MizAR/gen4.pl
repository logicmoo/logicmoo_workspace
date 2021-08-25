#!/usr/bin/perl -w
# test MWS server

use HTTP::Request::Common qw(POST);
      use LWP::UserAgent;
      $ua = new LWP::UserAgent;
$ua->timeout(100000);


my @names = @ARGV;
## my @names = ('jgraph_4');


#foreach my $aname (@names)
  {

#      print "\n $aname \n";

 #      my $req = POST 'http://cds.omdoc.org:8080/:search?mizar',

# Content_Type => 'text/xml',


# # ProblemSource => 'URL',
# Content => '<Query><Qvar nr="1"/><Exists><Typ kind="M" nr="1" pid="8" aid="HIDDEN" absnr="1"><Cluster><Adjective nr="9" kind="V" aid="ORDINAL1" absnr="3"/></Cluster><Cluster><Adjective nr="7" kind="V" aid="ORDINAL1" absnr="1"/><Adjective nr="8" kind="V" aid="ORDINAL1" absnr="2"/><Adjective nr="9" kind="V" aid="ORDINAL1" absnr="3"/></Cluster></Typ><Pred kind="R" nr="1" pid="9" aid="HIDDEN" absnr="1"><Var nr="1"/><Func kind="K" nr="116" pid="87" aid="ORDINAL2" absnr="11"><Var nr="1"/><Var nr="2"/></Func></Pred></Exists></Query>',
# # FormulaURL =>'http://mws.cs.ru.nl/~mptp/mml4.160.1126/mml/' . $aname . '.miz',
# # Aid => 'ARYTM_3',
# # Size => 30,
# # Offset => 0,
# # MMLVersion => '4.166.1132'];

# print $req->header(-type => 'text/xml');

print $ua->request( POST 'http://cds.omdoc.org:8080/:search?mizar', Content_Type => 'text/xml',
# ProblemSource => 'URL',
Aid => 'Foo',
Content => 
#'<Query><Qvar nr="1"/><Exists><Typ kind="M" nr="1" pid="6" aid="HIDDEN" absnr="1"><Cluster><Adjective nr="6" kind="V" aid="ORDINAL1" absnr="3"/></Cluster><Cluster><Adjective nr="4" kind="V" aid="ORDINAL1" absnr="1"/><Adjective nr="5" kind="V" aid="ORDINAL1" absnr="2"/><Adjective nr="6" kind="V" aid="ORDINAL1" absnr="3"/></Cluster></Typ><Pred kind="R" nr="1" pid="9" aid="HIDDEN" absnr="1"><Func kind="K" aid="HIDDEN" absnr="1" nr="3" vid="47"/><Func kind="K" nr="53" pid="34" aid="ORDINAL2" absnr="11"><Func kind="K" aid="HIDDEN" absnr="1" nr="3" vid="47"/><Var nr="1"/></Func></Pred></Exists></Query>')
#'<Query><Qvar nr="1"/><Exists><Typ kind="M" nr="1" pid="8" aid="HIDDEN" absnr="1"><Cluster><Adjective nr="9" kind="V" aid="ORDINAL1" absnr="3"/></Cluster><Cluster><Adjective nr="7" kind="V" aid="ORDINAL1" absnr="1"/><Adjective nr="8" kind="V" aid="ORDINAL1" absnr="2"/><Adjective nr="9" kind="V" aid="ORDINAL1" absnr="3"/></Cluster></Typ><Pred kind="R" nr="1" pid="9" aid="HIDDEN" absnr="1"><Var nr="1"/><Func kind="K" nr="116" pid="87" aid="ORDINAL2" absnr="11"><Var nr="1"/><Var nr="2"/></Func></Pred></Exists></Query>')
'<Query>
<Exists><Typ kind="M" nr="1" pid="6" aid="HIDDEN" absnr="1"><Cluster><Adjective nr="6" kind="V" aid="ORDINAL1" absnr="3"/></Cluster><Cluster><Adjective nr="4" kind="V" aid="ORDINAL1" absnr="1"/><Adjective nr="5" kind="V" aid="ORDINAL1" absnr="2"/><Adjective nr="6" kind="V" aid="ORDINAL1" absnr="3"/></Cluster></Typ><Pred kind="R" nr="1" pid="9" aid="HIDDEN" absnr="1"><LocConst nr="3" vid="47"/><Func kind="K" nr="53" pid="34" aid="ORDINAL2" absnr="11"><LocConst nr="3" vid="47"/><Var nr="1"/></Func></Pred></Exists></Query>')
# '<Query><Exists><Typ kind="M" nr="1" pid="6" aid="HIDDEN"
# absnr="1"><Cluster><Adjective nr="6" kind="V" aid="ORDINAL1"
# absnr="3"/></Cluster><Cluster><Adjective nr="4" kind="V"
# aid="ORDINAL1" absnr="1"/><Adjective nr="5" kind="V" aid="ORDINAL1"
# absnr="2"/><Adjective nr="6" kind="V" aid="ORDINAL1"
# absnr="3"/></Cluster></Typ><Pred kind="R" nr="1" pid="9" aid="HIDDEN"
# absnr="1"><LocConst nr="3" vid="47"/><Func kind="K" nr="53" pid="34"
# aid="ORDINAL2" absnr="11"><LocConst nr="3" vid="47"/><Var
# nr="1"/></Func></Pred></Exists></Query>')
->as_string;
    }
# <Func kind="K" nr="1" pid="34" aid="SUBSET_1" absnr="1"/>
# <Func kind="K" nr="1" pid="34" aid="SUBSET_1" absnr="1"/>
