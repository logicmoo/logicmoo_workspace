#!/usr/bin/perl -w

use strict;
use CGI;
use MPTPNames;

my $query	  = new CGI;
my $input_refs	  = $query->param('refs');

my $MizHtml       = "http://mizar.cs.ualberta.ca/~mptp/7.11.06_4.150.1103/html/";


my $atpres_dir    = "/home/mptp/big/proofcomp/probs3/vout200fixed1/proved200f1min";


sub print_iframe
{
    my $url = shift;
    print<<END1
<iframe name="mizpres" src ="$url" width="95%" height="75%" style="margin:5px" frameborder="1">
<p>Your user agent does not support iframes or is currently configured
  not to display iframes. However, you may visit
  <A href="$url">the related document.</A></p>
</iframe>
END1

}

my @refs = split(/\s/, $input_refs);

print $query->header;
print $query->start_html(-dtd=>'-//W3C//DTD HTML 3.2//EN');

# print $query->start_html();

print 'Green box shows ATP dependencies, black box is Mizar code. Clicking on proof will display the Mizar proof.';

print '<div style="height:20%; font-family: monospace; overflow: auto">';
foreach my $ref (@refs)
{
    print '<div style="border: .1em solid green; margin: 8px 8px 8px 8px; padding: 8px 8px 8px 8px;">';
    my ($href, $title) = MPTPNames::HTMLizeRef($ref,$MizHtml,'tst1');
    print $query->a({href=>$href,title=>$title,target=>"mizpres"}, $ref),":";
    if(open(F,"$atpres_dir/$ref.needed_vampire_orig"))
    {
	my @deps = <F>;
	foreach my $dep (@deps)
	{
	     chop;
	     my ($href1, $title1) = MPTPNames::HTMLizeRef($dep,$MizHtml,'tst1');
	     if(length($href1)>0)
	     {
		 print $query->a({href=>$href1,title=>$title1,target=>"mizpres"},$dep),", ";
	     }
	     elsif(length($title1)>0) 
	     {
		 print $query->font({color=>"Green",title=>$title1},$dep),", ";
	     }
	     else {print $dep,", ";}
	}
    }
    else { print 'No ATP proof found';}
    print '</div>';
}
print '</div>';

my ($href, $title) = MPTPNames::HTMLizeRef($refs[0],$MizHtml,'tst1');
print_iframe("$href");

print $query->end_html();
