#!/usr/bin/perl -w

## htmlize a file with comma-seprataed mptp references

## script/htmlize.pl filewithrefs

use strict;
use CGI;

my $MyUrl = 'http://mws.cs.ru.nl/~mptp';
my $MizHtml = $MyUrl . "/mml/html/";

my $input_article = "nnnnnnnnn";

sub HTMLize
{
    my ($ref) = @_;
    my $res = '';
    my $title = '';
#    print '<a href="foo">goo</a>'; $MizHtml="hj";
    if(($ref=~m/^([dtl][0-9]+)_(.*)$/) 
       || ($ref=~m/^(s[0-9]+)_(.*?)__.*$/) 
       || ($ref=~m/^([fcr]c[0-9]+)_(.*)$/) 
       || ($ref=~m/^(ie[0-9]+)_(.*)$/) 
       || ($ref=~m/^(rd[0-9]+)_(.*)$/) 
       || ($ref=~m/^dt_([klmugrv][0-9]+)_(.*)$/))
    {
	my ($kind,$ar) = ($1,$2);
	if($kind =~ m/^l(.*)/) { $kind = 'e' . $1; }
	elsif(($kind =~ m/^ie(.*)/)) { $kind = 'iy' . $1; }
	if($ar eq $input_article) {$res  = '#'.  uc($kind); }
	else {$res  = $MizHtml . $ar . '.html#' . uc($kind); }
	$title =  uc($ar) . ":" . uc($kind);
    }
    elsif($ref=~m/^(e[0-9]+)_(.*)__(.*)$/)
    {
	$title =  "proposition " . uc($1) . " in block " . $2;
	$res = '#' . uc($1) . ':' . $2; 
    }
    elsif($ref=~m/^d[et]_(c[0-9]+)_(.*)__(.*)$/)
    {
	$res = '#' . lc($1) . ':' . $2;
	$title =  "constant " . uc($1) . " in block " . $2;
    }
    elsif($ref=~m/^(abstractness|free|existence|redefinition|symmetry|antisymmetry|asymmetry|reflexivity|irreflexivity|connectedness|commutativity|idempotence|involutiveness|projectivity)_([klmugrv][0-9]+)_(.*)$/)
    {
	$title = $1 . " of " . uc($3) . ":" . uc($2);
	if($3 eq $input_article) {$res  = '#'.  uc($2); }
	else { $res = $MizHtml . $3 . '.html#' . uc($2); }
    }
    elsif($ref=~m/^spc([0-9]+)_boole$/) 
    {
	if($1 eq "0") { $title = $1 . " is empty"; } 
	else { $title = $1 . " is non empty"; } 
    }
    elsif($ref=~m/^spc([0-9]+)_numerals$/)
    {
	if($1 eq "0") { $title = $1 . " is Element of NAT"; }
	else { $title = $1 . " is positive Element of NAT"; }
    }
    elsif($ref=~m/^rq.*$/) { $title = "arithmetic evaluation"; }
    elsif($ref=~m/^fraenkel_.*$/) { $title = "fraenkel functor first-order instance"; }
    return ($res, $title);
}


my $query = new CGI;
print $query->header;
print $query->start_html();

while($_=<>)
{
    chop;
    my @refs = split(/\,/,$_);
    foreach my $ref (@refs)
    {
	my ($href, $title) = HTMLize($ref);
	if(length($href)>0)
	{
	    if(length($title)>0) 
	    {
		print $query->a({href=>$href,title=>$title}, $ref),", ";
	    }
	    else
	    {
		print $query->font({color=>"Green",title=>$title}, $ref),", ";
	    }
	}
	elsif(length($title)>0) 
	{
	    print $query->font({color=>"Green",title=>$title}, $ref),", ";
	}
	else {print $ref,", ";}
    }
    print "<br>\n";
}

print $query->end_html();

