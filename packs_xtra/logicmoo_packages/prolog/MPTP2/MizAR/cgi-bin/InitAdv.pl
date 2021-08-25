#!/usr/bin/perl -w

# train from the first argument symbolically and start the daemon
#
# usage: ./InitAdv.pl filestem advlimit

use lib '/home/urban/gr/MPTP2/MizAR/cgi-bin';
use AIAdvise;

my $MizAR_path = '/home/urban/gr/MPTP2/MizAR/cgi-bin';
my $snow = '/home/urban/ec/Snow_v3.2/snow';
# $MizAR_path . '/bin/snow';
my $advisor = $MizAR_path . '/advisor_lean.pl';
my $symoffset=500000;
my $filestem= shift;
my $advlimit= shift;
my ($grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref) = AIAdvise::CreateTables($symoffset, $filestem);
my $proved_by = AIAdvise::PrintProvedBy0($symoffset, $filestem, $grefnr);

AIAdvise::PrintTrainingFromHash($filestem,0,$proved_by,$grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref);

my $gtargetsnr = (scalar @$gnrref) - 1;

AIAdvise::Learn0( $snow, $filestem, $gtargetsnr);
my ($aport, $sport, $adv_pid, $snow_pid) = 
  AIAdvise::StartSNoW($snow, $advisor, $symoffset, $filestem, $advlimit, 1);
print "$aport,$sport\n";
