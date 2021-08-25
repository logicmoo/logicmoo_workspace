package MPTPNames;

use strict;
use warnings;
use CGI;

## provide links and titles to various MPTP references
sub HTMLizeRef
{
    my ($ref,$MizHtml,$input_article) = @_;
    my $res = '';
    my $title = '';
#    print '<a href="foo">goo</a>'; $MizHtml="hj";
    if(($ref=~m/^([dtl][0-9]+)_(.*)$/) 
       || ($ref=~m/^(s[0-9]+)_(.*?)__.*$/) 
       || ($ref=~m/^(s)([0-9]+)_(.*)$/) 
       || ($ref=~m/^([fcr]c[0-9]+)_(.*)$/) 
       || ($ref=~m/^dt_([klmugrv][0-9]+)_(.*)$/))
    {
	my ($kind,$ar) = ($1,$2);
	if(($ref=~m/^l.*/) && ($kind =~ m/^l(.*)/)) { $kind = 'e' . $1; }
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


sub HTMLizeLines
{
    my ($MizHtml,$input_article) = @_;

    my $query = new CGI;
    print $query->header;
    print $query->start_html();

    while($_=<>)
    {
	chop;
	my @refs = split(/\,/,$_);
	foreach my $ref (@refs)
	{
	    my ($href, $title) = HTMLizeRef($ref,$MizHtml,$input_article);
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
}

my %kind2miz = 
(
 't' => '',
 'd' => 'def',
 'fc' => 'funcreg',
 'cc' => 'condreg',
 'rc' => 'exreg',
 's' => 'sch',
 'k' => 'func',
 'l' => 'struct',
 'm' => 'mode',
 'u' => 'sel',
 'g' => 'aggr',
 'r' => 'pred',
 'v' => 'attr',
 'e' => 'lemma'
);


## provide Mizar names to various MPTP references
sub MizarizeRef
{
    my ($ref,$input_article,$fla2name, $line, $pos) = @_;
    my $res = '';
    my $simpres = '';

    if(defined($line) && defined($pos) && ($pos=~m/(.*):.*/))
    {
	$pos = $1;
	if($line == $pos) { $simpres = '::conj::'; }
	elsif($line == $pos + 1) { $simpres = 'previous'; }
    }


    if(($ref=~m/^([dtl])([0-9]+)_(.*)$/) 
       || ($ref=~m/^(s)([0-9]+)_(.*?)__.*$/) 
       || ($ref=~m/^(s)([0-9]+)_(.*)$/) 
       || ($ref=~m/^([fcr]c)([0-9]+)_(.*)$/) 
       || ($ref=~m/^dt_([klmugrv])([0-9]+)_(.*)$/))
    {
	my ($kind,$nr,$ar) = ($1,$2,$3);

	my $aruc = uc($ar);
	if(($ref=~m/^l.*/) && ($kind =~ m/^l/)) { $kind = 'e'; }

	my $mizkind = $kind2miz{$kind};
	my $here = ($aruc eq $input_article) ? 1 : 0;

	if($here==0)
	{
	    $res = $aruc . ':' . $mizkind . ((length($mizkind)>0) ? ' ' : '') . $nr;
	}
	elsif(defined($fla2name) && exists($fla2name->{$ref}))
	{
	    $res = $fla2name->{$ref};
	}
	else
	{
	    if($mizkind eq '') {$mizkind = 'th'; }
	    $res = ucfirst($mizkind) . $nr;
	}

	$simpres = ($simpres eq '') ? $res : $simpres;
    }
    elsif($ref=~m/^(e([0-9]+)_(.*))__(.*)$/)
    {
	if(defined($fla2name) && exists($fla2name->{$1}))
	{
	    $res = $fla2name->{$1};
	    $simpres = $res;
	}
	else
	{ 
	    $res = 'Proposition' . $2 . '__Block' . $3;
	    if($simpres eq '')
	    {
		if(defined($pos)) { $simpres = "Proposition_at_line_" . $pos; }
		else { $simpres = $res}
	    }
	}

    }
    elsif($ref=~m/^d[et]_c([0-9]+)_(.*)__(.*)$/)
    {
	$res = 'Constant' . $1 . '__Block' . $2;
    }
    elsif($ref=~m/^(abstractness|free|existence|redefinition|symmetry|antisymmetry|asymmetry|reflexivity|irreflexivity|connectedness|commutativity|idempotence|involutiveness|projectivity)_([klmugrv])([0-9]+)_(.*)$/)
    {

	my ($prop,$kind,$nr,$ar) = ($1,$2,$3,$4);

	my $mizkind = $kind2miz{$kind};
	my $here = ($ar eq $input_article) ? 1 : 0;

	if($here==0)
	{
	    $res = uc($ar) . ':' . $mizkind . ((length($mizkind)>0) ? ' ' : '') . $nr;
	}
	else { $res = ucfirst($mizkind) . $nr; }
    }
    elsif($ref=~m/^spc([0-9]+)_boole$/) 
    {
	if($1 eq "0") { $res = $1 . "_is_empty"; } 
	else { $res = $1 . "_is_non_empty"; } 
    }
    elsif($ref=~m/^spc([0-9]+)_numerals$/)
    {
	if($1 eq "0") { $res = $1 . "_is_Element_of_NAT"; }
	else { $res = $1 . "_is_positive_Element_of_NAT"; }
    }
    elsif($ref=~m/^rq.*$/) { $res = "arithmetic_evaluation"; }
    elsif($ref=~m/^fraenkel_.*$/) { $res = "fraenkel_functor"; }
    $simpres = '' if($simpres eq '::conj::'); ## undo the temp setting
    return ($res, $simpres);
}




1;
