#!/usr/bin/perl
#
# Produce one file per each training example, pad short labels with zeros.
#
# mkdir docs
# ./one_doc_format.pl data2
#

while(<>)
{ 
    chop; 
    @k = split("[,;]",$_); 
    @k1 = map {$l=length($_); if($l==1) {"00". $_} elsif($l==2) {"0" . $_} else {$_}   } @k; 
    open(F,">docs/$k1[0]") or die; 
    print F join("\n",@k1),"\n";
    close(F);
}
