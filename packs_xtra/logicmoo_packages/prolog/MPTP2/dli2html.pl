#!/usr/bin/perl
require LWP::UserAgent;
 $file = $ARGV[0];
 my $ua = LWP::UserAgent->new;
 $ua->timeout(10);
 $ua->env_proxy;
 
 my $response = $ua->post('http://mmlquery.mizar.org/cgi-bin/mmlquery/dli',
		   Content_Type => 'form-data',
		   Content      => [items   => [$file]]);
 
 if ($response->is_success) {
     print $response->content;  # or whatever
 }
 else {
     die $response->status_line;
 }
