use LWP::UserAgent;

my $ua = LWP::UserAgent->new;
 $ua->timeout(10);
 $ua->env_proxy;

 my $response = $ua->post('http://mmlquery.mizar.org/cgi-bin/mmlquery/dli',
                   Content_Type => 'form-data',
                   Content      => [items   => [$ARGV[0]]]);

if ($response->is_success) {
     print $response->content;  # or whatever
   }
 else {
     die $response->status_line;
   }

