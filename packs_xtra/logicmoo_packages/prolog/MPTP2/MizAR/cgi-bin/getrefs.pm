package getrefs;

use strict;
use IO::Socket;

## GetRefs($advhost, $aport, $syms, $limit)
##
## Gets at most $limit references relevant for symbols $syms by asking trained bayes advisor
## running on host $advhost on port $aport.
##
## SYNOPSIS:
## my @symbols = ('+','0','succ');
## my $advisor_url = 'localhost';
## my $advisor_port = 50000;
## my $wanted_references_count = 30;
##
## my @references = GetRefs($advisor_url, $advisor_port, \@symbols, $wanted_references_count)

sub GetRefs
{
    my ($advhost, $aport, $syms, $limit) = @_;
    my ($msgin, @res1, @res);
    my $EOL = "\015\012";
    my $BLANK = $EOL x 2;
    my $remote = IO::Socket::INET->new( Proto     => "tcp",
					PeerAddr  => $advhost,
					PeerPort  => $aport,
				      );
    unless ($remote)
    {
	return ('DOWN');
    }
    $remote->autoflush(1);
    print $remote join(",",@$syms) . "\n";
    $msgin = <$remote>;
    @res1  = split(/\,/, $msgin);
    close $remote;
    my $outnr = min($limit, 1 + $#res1);
    @res  = @res1[0 .. $outnr];
    return @res;
}

1;
