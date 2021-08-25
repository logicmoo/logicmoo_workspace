#!/usr/bin/perl -w

# html-zing filter for mptp problems/results using 
# the mptp notation

# SYNOPSIS: 
# mptp_html.pl t10_abcmiz_0 > t10_abcmiz_0.html

$docroot = "http://lipa.ms.mff.cuni.cz/~urban/xmlmml/html.930/";

$header = "<html><head><title></title></head><body><pre>";

$footer = "</pre></body></html>";

print "$header \n";

while (<>)
{
    s/\b(([dgklmrtuv]|rc|cc|fc)[0-9]+)_([a-z0-9_]{1,8}\b)/<a href=\"$docroot$3#\U$1\E\">$1_$3<\/a>/g;
    print $_;
}

print "$footer\n";
