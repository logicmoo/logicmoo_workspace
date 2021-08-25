package AIAdvise;

use strict;
use IO::Socket;

sub min { my ($x,$y) = @_; ($x <= $y)? $x : $y }

## test: perl -e 'use AIAdvise;   AIAdvise::LeancopClausify("swipl","00allBushy","uuu",".");'

# clausifies consistently, assuming that leancop is in the directory $prologdir
sub LeancopClausify
{
    my ($prolog,$filelist,$filestem,$prologdir,$options) = @_;
    my $dollar = '$';
    my $command = "[tptp2leancop], time((read_file_lines('$filelist',L)," .
	"tptp2leancop(L,[nodef,dumpsyms('$filestem.refsyms'),dumpterms('$filestem.terms')$options])))," .
	    "halt. ";
    system("sed -e 's/$dollar/.leancop/' $filelist > $filestem.probs");
    system("cd $prologdir; $prolog -nodebug -L120M -G120M -T100M -q -t \"$command\"");
}

# return a hash with names to formulas mapping
## test: perl -e 'use AIAdvise;   $g=AIAdvise::LoadFlas2Hash("hoo.tptp");'
sub LoadFlas2Hash
{
   my ($file) = @_;
   my %res = ();

   open(F,$file);
   while(<F>)
   {
       chop;
       if(m/^ *fof\( *([^, ]+) *, *([^, ]+) *,(.*)/)
       {
           my ($nm,$status,$rest)=($1,$2,$3);
           $res{$nm} = $_;
       }
   }
   close(F);
   return \%res;
}



# CreateTables($symoffset, $filestem)
#
# Create the symbol and reference numbering files
# from the refsyms file. Loads these tables and the refsym table tooa dn return pointers to them.
# The initial refsyms file can be created from all (say bushy) problems by running:
# cat */* | bin/GetSymbols -- |sort -u > all.refsyms
sub CreateTables
{
    my ($symoffset, $filestem) = @_;
    my $i = 0;
    my ($ref,$sym,$trms,@syms,$psyms,$fsyms);

    open(REFSYMS, "$filestem.refsyms") or die "Cannot read refsyms file";
    open(REFNR, ">$filestem.refnr") or die "Cannot write refnr file";
    open(SYMNR, ">$filestem.symnr") or die "Cannot write symnr file";

    my %grefnr = ();	# Ref2Nr hash for references
    my %gsymnr = ();	# Sym2Nr hash for symbols
    my %gsymarity = ();	# for each symbol its arity and 'p' or 'f'
    my %grefsyms = ();	# Ref2Sym hash for each reference array of its symbols
    my @gnrsym = ();	# Nr2Sym array for symbols - takes symoffset into account!
    my @gnrref = ();	# Nr2Ref array for references

    while($_=<REFSYMS>)
    {
	chop; 
	m/^symbols\( *([a-z0-9A-Z_]+) *, *\[(.*)\] *, *\[(.*)\] *\)\./ 
	    or die "Bad symbols info: $_";
	($ref, $psyms, $fsyms) = ($1, $2, $3);
	my @psyms = split(/\,/, $psyms);
	my @fsyms = split(/\,/, $fsyms);
	die "Duplicate reference $ref in $_" if exists $grefnr{$ref};
	$grefsyms{$ref} = [];
	push(@gnrref, $ref);
	$grefnr{$ref} = $#gnrref;
	print REFNR "$ref\n";

	## this now also remembers arity and symbol kind in %gsymarity
	foreach $sym (@psyms)
	{
	    $sym =~ m/^ *([^\/ ]+) *[\/] *([0-9]+).*/ or die "Bad symbol $sym in $_";
	    $gsymarity{$1} = [$2, 'p'];
	    push(@{$grefsyms{$ref}}, $1);
	}
	foreach $sym (@fsyms)
	{
	    $sym =~ m/^ *([^\/ ]+) *[\/] *([0-9]+).*/ or die "Bad symbol $sym in $_";
	    $gsymarity{$1} = [$2, 'f'];
	    push(@{$grefsyms{$ref}}, $1);
	}

    }
    close REFNR;
    foreach $sym (keys %gsymarity)
    {
	print SYMNR "$sym\n";
	push(@gnrsym, $sym);
	$gsymnr{$sym} = $symoffset + $i++;
    }
    close SYMNR;
    close REFSYMS;

#    LoadTermTable("$filestem.trmstd",\%greftrmstd,$gstdtrmoffset) if($gdotrmstd > 0);
#    LoadTermTable("$filestem.trmnrm",\%greftrmnrm,$gnrmtrmoffset) if($gdotrmnrm > 0);

    my $gtargetsnr = $#gnrref;
    print $gtargetsnr . "\n";
    return (\%grefnr, \%gsymnr, \%gsymarity, \%grefsyms, \@gnrsym, \@gnrref);

}


## test:
## perl -e 'use AIAdvise; my ($grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref) = AIAdvise::CreateTables(500000, "uuu"); AIAdvise::CreateProb2Cl("uuu",$grefnr);'

# make the prob2cl (problem to clauses) and prob2conj hashes
sub CreateProb2Cl
{
    my ($filestem,$grefnr) = @_;

    open(BATCHFILE, "$filestem.probs") or die "Cannot read batchfile";
    open(PROB2CL, ">$filestem.prob2cl") or die "Cannot write prob2cl file";    
    open(PROB2CONJ, ">$filestem.prob2conj") or die "Cannot write prob2conj file";

    my %prob2cl = ();	# clause of each problem
    my %prob2conj = ();	# conjs for each problem

    while(<BATCHFILE>)
    {
	chop;
	my $prob=$_;
	$prob2cl{$prob} = {};
	$prob2conj{$prob} = {};
	open(PROB, "$prob") or die "Cannot read $prob file named in batchfile";
	while(<PROB>)
	{
	    if(m/^dnf. *([^, ]+) *, *([^, ]+) *,/)
	    {
		my ($cl, $role) = ($1,$2);
		exists $grefnr->{$cl} or die "1: Unknown reference $cl in $_";
		my $clnr = $grefnr->{$cl};
		$prob2cl{$prob}->{$clnr} = ();
		$prob2conj{$prob}->{$clnr} = () if($role=~m/conjecture/);
	    }
	}
	print PROB2CL ("prob2cl($prob,[", join(',', keys %{$prob2cl{$prob}})  ,"]).\n");
	print PROB2CONJ ("prob2conj($prob,[", join(',', keys %{$prob2conj{$prob}})  ,"]).\n");
	close(PROB);
    }
    close(PROB2CONJ);
    close(PROB2CL);
    close(BATCHFILE);

    return (\%prob2cl, \%prob2conj);
}


## test:
## perl -e 'use AIAdvise;  AIAdvise::LeancopClausify("swipl","00allBushy2","uu","."); my ($grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref) = AIAdvise::CreateTables(500000, "uu"); my ($prob2cl,$prob2conj) = AIAdvise::CreateProb2Cl("uu",$grefnr); AIAdvise::RunLeancopProblems("uu",$prob2cl,"./leancop_dnf.sh", ".", 0, "", 1);'

# run leancop with params for $problems, do minimization if minimize==1 in order to
# get only useful proofchoices info
sub RunLeancopProblems
{
    my ($filestem, $problems, $leancop, $prologdir, $iter, $params, $minimize, $parallel) = @_;

    if($parallel > 1)
    {
	my $dolist = join(' ', (sort keys %$problems));
	system("cd $prologdir; echo $dolist | tr \\   \\\\n  | parallel -j$parallel time \" $leancop {} $params > {}.outf_$iter 2> {}.errf_$iter \" ");
    }

    foreach my $problem (sort keys %$problems)
    {
	print "$problem: ";
	if($parallel == 1)
	{
	    system("cd $prologdir; time $leancop $problem $params > $problem.outf_$iter 2> $problem.errf_$iter");
	}

	if (system("grep --quiet Proof $problem.outf_$iter") == 0)
	{
	    print "Theorem\n";
	    if($minimize == 1)
	    {
		my %used = ();
		open(P,"$problem.outf_$iter");
		while(<P>) { if(m/.*Then clause .(\d+).*/) {  $used{$1} = (); } }
		close(P);
		my $regexp = '^dnf.\(' . join('\|', keys %used) . '\),';
		system("grep '$regexp' $problem > $problem.small ");
		system("cd $prologdir; time $leancop $problem.small $params > $problem.out_$iter 2> $problem.err_$iter");
	    }
	    else
	    {
		system("cp $problem.outf_$iter$problem.out_$iter; cp $problem.errf_$iter $problem.err_$iter");
	    }
	}
	else
	{
	    print "Unsolved\n";
	}
    }
}


## test:
## perl -e 'use AIAdvise;   my ($grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref) = AIAdvise::CreateTables(500000, "zz"); AIAdvise::PrintProvedBy0(500000, "zz", $grefnr);'

# PrintProvedBy0($symoffset, $filestem, $loadprovedby)
#
# create the initial info for $filestem saying that each reference is
# provable by itself
sub PrintProvedBy0
{
    my ($symoffset, $filestem, $grefnr, $loadprovedby) = @_;
    my %proved_by_0 = ();
    open(PROVED_BY_0,">$filestem.proved_by_0");
    unless(defined($loadprovedby))
    {
	foreach my $i (keys %$grefnr)
	{
	    print PROVED_BY_0 "proved_by($i,[$i]).\n";
	    push( @{$proved_by_0{$i}}, $i);
	}
    }
    close(PROVED_BY_0);
    return \%proved_by_0;
}

## test:
## perl -e 'use AIAdvise;   my ($grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref) = AIAdvise::CreateTables(500000,"zzzz");    my ($clausenrs, $path_choices) = AIAdvise::GetLeancopProofData("zzzz", "zfmisc_1__t6_zfmisc_1.p.leancop", $grefnr,1);      print join(",", keys %$clausenrs),"\n"; foreach my $k (keys %$path_choices ) { print "$k:", join(",", keys %{$path_choices->{$k}}),"\n"; }'

# get the axioms used, get the decisions (path successes/choices) inside proofs
# no info from incomplete proofs here (do it in another function)
### TODO: a bit funny - the refs are translated but not the syms - should be unified with PrintTrainingFromHash
### todo: collect the second number in proof choices for learning big shortcuts on unsuccessful runs
### $learnflags & 1 ...
sub  GetLeancopProofData
{
    my ($filestem,$filebase,$grefnr,$iter,$learnflags) = @_;
    my %clausenrs = ();
    my %path_choices = ();
    my %fail_choices = ();
    my $res = 0;

    $learnflags = 3 unless(defined($learnflags));
    my $learnproofs = $learnflags & 1;
    my $learnchoices = $learnflags & 2;
    my $learnfailchoices = $learnflags & 4;

    my $proof = $filebase . '.out_' . $iter;
    $proof = $filebase . '.outf_' . $iter unless(-e $proof);

    if((-e $proof))
    {
	$res = (system("grep --quiet Proof $proof") == 0)? 1 : 0;

	if($learnfailchoices or ($res == 1))
	{
	    open(PROOF, $proof);
	    while(<PROOF>) 
	    {
		if(m/.*Then clause .(\d+).*/)
		{
		    my ($cl) = ($1);
		    exists $grefnr->{$cl} or die "2: Unknown reference $cl in $_";
		    my $clnr = $grefnr->{$cl};
		    $clausenrs{$clnr} = () if($learnproofs);
		}
		elsif(m/^&[^\[]*\[([^\]]*)\] *\>\>\> * (\d+)/)
		{
		    if(($learnchoices && ($res==1)) || $learnfailchoices)
		    {
			my $choices = ($learnchoices && ($res==1))? \%path_choices : \%fail_choices;
			my ($syms0, $cl) = ($1,$2);
			exists $grefnr->{$cl} or die "3: Unknown reference $cl in $_";
			my $clnr = $grefnr->{$cl};
			my $syms = join(',', sort split(/ *, */, $syms0));

			if(!exists $choices->{$syms})
			{
			    $choices->{$syms} = {};
			}
			$choices->{$syms}->{$clnr} = ();
		    }
		}
	    }
	}
    }
    return ($res, \%clausenrs, \%path_choices, \%fail_choices);
}


## test:
## perl -e 'use AIAdvise;   my $filestem="uu"; my ($grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref) = AIAdvise::CreateTables(500000,$filestem);    my  ($prob2cl, $prob2conj)=AIAdvise::CreateProb2Cl($filestem,$grefnr); AIAdvise::CollectProvedByN(500000,$filestem, 1, $grefnr, $prob2conj);'

# CollectProvedByN($symoffset, $filestem, $iter, $grefnr, $prob2conj)
#
# From .out_$iter files collect those that were proved.
# The result kept is for each solved problem a triple:
# {hash_of_used_conjectures}, {hash_of_all_used_clauses}, {$path_choices}.
sub CollectProvedByN
{
    my ($symoffset, $filestem, $iter, $grefnr, $prob2conj,$learnflags) = @_;
    my %proved_byN = ();
    my %unproved_byN = ();
    open(PROVED_BY,">$filestem.proved_by_$iter");
    foreach my $i (keys %$prob2conj)
    {
	my ($res, $clausenrs, $path_choices, $fail_choices) = GetLeancopProofData($filestem,$i,$grefnr,$iter,$learnflags);
	if($res == 1)
#	if(((scalar (keys %$clausenrs)) > 0) || ((scalar (keys %$path_choices)) > 0))
	{
	    my @conjs = grep { exists $prob2conj->{$i}->{$_} } keys %$clausenrs;
	    $proved_byN{$i}->[1] = [@conjs];
	    $proved_byN{$i}->[2] = $clausenrs;
	    $proved_byN{$i}->[3] = $path_choices;
	    print PROVED_BY "proved_by([", join(',', @conjs), '],[', join(',', keys %$clausenrs), "]).\n";
	}
	elsif(($learnflags & 4) > 0)
	{
	    $unproved_byN{$i}->[3] = $fail_choices;
	}
    }
    close(PROVED_BY);
    return (\%proved_byN, \%unproved_byN);
}


## test:  perl -e 'use AIAdvise;   my $filestem ="uu"; my ($grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref) = AIAdvise::CreateTables(500000,$filestem);    my  ($prob2cl, $prob2conj)=AIAdvise::CreateProb2Cl($filestem,$grefnr); my $proved_byN=AIAdvise::CollectProvedByN(500000,$filestem, 1, $grefnr, $prob2conj); AIAdvise::PrintTrainingFromClauseHash($filestem,1,$prob2cl,$proved_byN,$grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref,3); '


# Create a .train_$iter file from the %proved_byN hash, where keys are
# proved problems, and values conjectures and references and pathinfo
# needed for the proof.  Mode encodes the info printed. If (mode & 1)
# > 0, print the clauses, if (mode & 2) > 0, print the pathinfo for proved problems. Thus,
# use 3 for doing both. if (mode & 4) > 0, print the pathinfo for failed attempts.
#
# All the $filestem.train_* files are afterwards cat-ed to $filestem.alltrain_$iter
# file, on which Learn() works.
sub PrintTrainingFromClauseHash
{
    my ($filestem,$iter,$unproved_byN,$proved_byN, $grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref, $mode) = @_;
    open(TRAIN, ">$filestem.train_$iter") or die "Cannot write train_$iter file";
    if(($mode & 4) > 0) { open(FAILTRAIN, ">$filestem.failtrain_$iter") or die "Cannot write train_$iter file"; }

    foreach my $i (sort keys %$proved_byN)
    {
	if(($mode & 1) > 0)
	{
	    my @conjs = @{$proved_byN->{$i}->[1]};
	    my @clausenrs = keys %{$proved_byN->{$i}->[2]};
	    my @all_sym_nrs =  ();

	    foreach my $refnr (@conjs)
	    {
		my @syms = @{$grefsyms->{$gnrref->[$refnr]}};
		my @syms_nrs   = map { $gsymnr->{$_} if(exists($gsymnr->{$_})) } @syms;
		push(@all_sym_nrs, @syms_nrs);
	    }

	    my $training_exmpl = join(",", (@all_sym_nrs, @clausenrs));
	    print TRAIN "$training_exmpl:\n";
	}

	if(($mode & 2) > 0)
	{
	    my $path_choices = $proved_byN->{$i}->[3];
	    foreach my $syms (keys %$path_choices)
	    {
		my @clausenrs = keys %{$path_choices->{$syms}};
		my @syms_nrs   = map { $gsymnr->{$_} if(exists($gsymnr->{$_})) }  split(/,/, $syms);
		my $training_exmpl = join(",", (@syms_nrs, @clausenrs));
		print TRAIN "$training_exmpl:\n";
	    }
	}
    }

    close TRAIN;

    if(($mode & 4) > 0)
    {
	open(FAILTRAIN, ">$filestem.failtrain_$iter") or die "Cannot write train_$iter file";
	foreach my $i (sort keys %$unproved_byN)
	{
	    exists($unproved_byN->{$i}) or die "Problem not unproved: $i\n";
	    my $path_choices = $unproved_byN->{$i}->[3];
	    foreach my $syms (keys %$path_choices)
	    {
		my @clausenrs = keys %{$path_choices->{$syms}};
		my @syms_nrs   = map { $gsymnr->{$_} if(exists($gsymnr->{$_})) }  split(/,/, $syms);
		my $training_exmpl = join(",", (@syms_nrs, @clausenrs));
		print FAILTRAIN "$training_exmpl:\n";
	    }
	}
	close(FAILTRAIN);
    }
}

# Create a .train_$iter file from the %proved_by hash, where keys are proved
# conjectures and values are arrays of references needed for the proof.
# All the $filestem.train_* files are afterwards cat-ed to $filestem.alltrain_$iter
# file, on which Learn() works.
sub PrintTrainingFromHash
{
    my ($filestem,$iter,$proved_by,$grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref) = @_;
    open(TRAIN, ">$filestem.train_$iter") or die "Cannot write train_$iter file";
    foreach my $ref (sort keys %$proved_by)
    {
	my @refs = @{$proved_by->{$ref}};
	# if($ggeneralize > 0)
	# {
	#     foreach my $rr (@{$proved_by->{$ref}})
	#     {
	# 	if(exists $gref2gen{$rr}) { push(@refs, $gref2gen{$rr}); }
	#     }
	# }
	my @refs_nrs   = map { $grefnr->{$_} if(exists($grefnr->{$_})) } @refs;
	my @syms = @{$grefsyms->{$ref}};
#	push(@syms, $gggnewc) if exists $gref2gen{$ref};
	my @syms_nrs   = map { $gsymnr->{$_} if(exists($gsymnr->{$_})) } @syms;
	my @all_nrs = (@refs_nrs, @syms_nrs);
	# if($gdotrmstd > 0)
	# {
	#     my @trmstd_nrs   = @{$greftrmstd{$ref}};
	#     if(exists $gref2gen{$ref})
	#     {
	# 	my %tmp = ();
	# 	@tmp{ @trmstd_nrs } = ();
	# 	@tmp{ @{$greftrmstd{$gref2gen{$ref}}} } = ();
	# 	@trmstd_nrs = keys %tmp;
	#     }
	#     push(@all_nrs, @trmstd_nrs);
	# }
	# if($gdotrmnrm > 0)
	# {
	#     my @trmnrm_nrs   = @{$greftrmnrm{$ref}};
	#     if(exists $gref2gen{$ref})
	#     {
	# 	my %tmp = ();
	# 	@tmp{ @trmnrm_nrs } = ();
	# 	@tmp{ @{$greftrmnrm{$gref2gen{$ref}}} } = ();
	# 	@trmnrm_nrs = keys %tmp;
	#     }
	#     push(@all_nrs, @trmnrm_nrs);
	# }
	# if(($guseposmodels > 0) && (exists $grefposmods{$ref}))
	# {
	#     my @posmod_nrs   = map { $gposmodeloffset + $_ } @{$grefposmods{$ref}};
	#     push(@all_nrs, @posmod_nrs);
	# }
	# if(($gusenegmodels > 0) && (exists $grefnegmods{$ref}))
	# {
	#     my @negmod_nrs   = map { $gnegmodeloffset + $_ } @{$grefnegmods{$ref}};
	#     push(@all_nrs, @negmod_nrs);
	# }
	# just a sanity check
	foreach $ref (@refs)
	{
	    exists $grefsyms->{$ref} or die "4: Unknown reference $ref in refs: @refs";
	    exists $grefnr->{$ref} or die "5: Unknown reference $ref in refs: @refs";
	}

	# for 0th iteration, we allow small boost of axioms of
	# small specifications by $gboostweight
	# if(($iter == 0) && ($gboostlimit > 0) && (exists $gspec{$ref}))
	# {
	#     my @all_refs = keys %{$gspec{$ref}};
	#     # all_refs contains the conjecture too, so we don't have to add 1 to $#all_refs
	#     if($#all_refs <= ($gboostlimit * $gtargetsnr))
	#     {
	# 	my @ax_nrs   = map { $grefnr{$_} . '(' . $gboostweight . ')'
	# 				 if(exists($grefnr{$_})) } @all_refs;
	# 	push(@all_nrs, @ax_nrs);
	#     }
	# }

	my $training_exmpl = join(",", @all_nrs);
	print TRAIN "$training_exmpl:\n";
    }
    close TRAIN;
}

sub CreateArch
{
    my ($filestem, $targetsnr) = @_;
    open(ARCH, ">$filestem.arch") or die "Cannot write arch file";
    print ARCH "-B :0-$targetsnr\n";
    close(ARCH);
}

## test: perl -e 'use AIAdvise; my ($filestem,$symoffset)=("zz",500000); my ($grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref) = AIAdvise::CreateTables($symoffset, $filestem); my $proved_by = AIAdvise::PrintProvedBy0($symoffset, $filestem, $grefnr); AIAdvise::PrintTrainingFromHash($filestem,0,$proved_by,$grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref); '

sub Learn0
{
    my ($path2snow, $filestem, $targetsnr) = @_;
    `$path2snow -train -I $filestem.train_0 -F $filestem.net_1  -B :0-$targetsnr`;
    CreateArch($filestem, $targetsnr);
}

# test:
# perl -e 'use AIAdvise; AIAdvise::Learn0("/home/urban/gr/MPTP2/MizAR/cgi-bin/bin/snow", "zz", 43);'


# Learn from the .alltrain_$iter file, which was created by PrintTrainingFromHash
sub Learn
{
    my ($path2snow, $filestem, $targetsnr, $iter) = @_;
    my $next_iter = 1 + $iter;
    print "LEARNING:$iter\n";
    `cat $filestem.train_* $filestem.failtrain_$iter > $filestem.alltrain_$iter`;
    `$path2snow -train -I $filestem.alltrain_$iter -F $filestem.net_$next_iter  -B :0-$targetsnr`;
}



##  StartSNoW($path2snow, $path2advisor, $symoffset, $filestem, $outlimit, $netiter);
##
## Get unused ports for SNoW and for the symbol translation daemon
## (advisor), start them, and return the ports and the pids of snow
## and advisor.  $symoffset tells the translation daemon where the
## symbol numbering starts. $outlimit is the number of advises,
## $netiter is the serial number of the network file.
##
## Be sure to sleep for sufficient amount of time (ca 40s for all MML)
## until SNoW loads before asking queries to it.
##
## Note that the SNoW and the advisor need the .net, .arch, .refnr,
## .symnr files created by CreateTables from .refsyms and later training.
##
##
## SYNOPSIS:
## my $BinDir = "/home/urban/bin";
##
## my ($aport, $sport, $adv_pid, $snow_pid) = StartSNoW("$BinDir/snow", "$BinDir/advisor.pl", 500000, 'test1', 64, 1);
sub StartSNoW
{
    my ($path2snow, $path2advisor, $symoffset, $filestem, $outlimit, $netiter) = @_;
    my $snow_net = $filestem . '.net_' . $netiter;
    my $snow_arch =     $filestem . '.arch';
#--- get unused port for SNoW
    socket(SOCK,PF_INET,SOCK_STREAM,(getprotobyname('tcp'))[2]);
    bind( SOCK,  sockaddr_in(0, INADDR_ANY));
    my $sport = (sockaddr_in(getsockname(SOCK)))[0];
#    print("snowport $sport\n");
    close(SOCK);

#--- start snow instance:
# ###TODO: wrap this in a script remembering a start time and pid, and self-destructing
#          in one day

    my $snow_pid = fork();
    if ($snow_pid == 0)
    {
	# in child, start snow
	open STDOUT, '>', $filestem . '.snow_out';
	open STDERR, '>', $filestem . '.snow_err';
	exec("$path2snow -server $sport -o allpredictions -L $outlimit -F $snow_net -A $snow_arch ")
	    or print STDERR "couldn't exec $path2snow: $!";
	close(STDOUT);
	close(STDERR);
	exit(0);
    }

#--- get unused port for advisor
    socket(SOCK1,PF_INET,SOCK_STREAM,(getprotobyname('tcp'))[2]);
    bind( SOCK1,  sockaddr_in(0, INADDR_ANY));
    my $aport = (sockaddr_in(getsockname(SOCK1)))[0];
#    print("advisorport $aport\n");
    close(SOCK1);

    my $adv_pid = fork();
    if ($adv_pid == 0)
    {
	# in child, start advisor
	open STDOUT, '>', $filestem . '.adv_out';
	open STDERR, '>', $filestem . '.adv_err';
	exec("$path2advisor -p $sport -a $aport -o $symoffset $filestem")
	    or print STDERR "couldn't exec $path2advisor: $!";
	exit(0);
    }
    return ($aport, $sport, $adv_pid, $snow_pid);
}

## this starts snow in pipe inside advisor
sub StartAdvisor
{
    my ($path2snow, $path2advisor, $symoffset, $filestem, $outlimit, $netiter) = @_;
    my $snow_net = $filestem . '.net_' . $netiter;
    my $snow_arch =     $filestem . '.arch';

#--- get unused port for advisor
    socket(SOCK1,PF_INET,SOCK_STREAM,(getprotobyname('tcp'))[2]);
    bind( SOCK1,  sockaddr_in(0, INADDR_ANY));
    my $aport = (sockaddr_in(getsockname(SOCK1)))[0];
#    print("advisorport $aport\n");
    close(SOCK1);

    my $adv_pid = fork();
    if ($adv_pid == 0)
    {
	# in child, start advisor
	open STDOUT, '>', $filestem . ".adv_out_$$";
	open STDERR, '>', $filestem . ".adv_err_$$";
	exec("$path2advisor -a $aport -W2 --snowpath=$path2snow -L $outlimit -I $netiter -b1 -o $symoffset $filestem")
	    or print STDERR "couldn't exec $path2advisor: $!";
	exit(0);
    }
    return ($aport, $adv_pid);
}






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

## test: 
## perl -e 'use AIAdvise;  AIAdvise::TstLoop1("swipl","00allBushy2","uuu",".","/home/urban/ec/Snow_v3.2/snow");'

sub TstLoop1
{
    my ($prolog, $filelist, $filestem, $prologdir, $path2snow, $initrunfile) = @_;
    my $symoffset = 500000;
    my $advisor = "/home/urban/gr/MPTP2/MizAR/cgi-bin/advisor_lean.pl";
    my $advlimit = 64;
    LeancopClausify($prolog, $filelist, $filestem, $prologdir);
    my ($grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref) =  CreateTables($symoffset, $filestem);
    my ($prob2cl,$prob2conj) = CreateProb2Cl($filestem,$grefnr);

    my $targetsnr = (scalar @$gnrref) - 1;
#    CreateArch($filestem, $targetsnr);

    my $proved_by = PrintProvedBy0($symoffset, $filestem, $grefnr); 
    PrintTrainingFromHash($filestem,0,$proved_by,$grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref);

    my $iter = 1;

    my $initprobs = $prob2cl;
    if(defined $initrunfile)
    {
	open(I,$initrunfile); my %h=();
	while(<I>) { chop; $h{$_} = (); }
	close(I);
	$initprobs = \%h;
    }

    RunLeancopProblems($filestem,$initprobs,"./leancop_dnf.sh", $prologdir, $iter, "",1,1);

    my ($proved_byN, $unproved_byN) = CollectProvedByN($symoffset, $filestem, $iter, $grefnr, $prob2conj); 
    PrintTrainingFromClauseHash($filestem,$iter,$unproved_byN,$proved_byN,$grefnr,$gsymnr,$gsymarity,$grefsyms,$gnrsym,$gnrref,3);
    Learn($path2snow, $filestem, $targetsnr, $iter);

    my ($aport, $adv_pid) =
	StartAdvisor($path2snow, $advisor, $symoffset, $filestem, $advlimit, $iter+1);
    print "$aport,##\n";

}


## test: 
## perl -e 'use AIAdvise;  AIAdvise::TstLoop2("swipl","00allBushy2","uuu",".","/home/urban/ec/Snow_v3.2/snow", 16);'
##
## time perl -e 'use AIAdvise;  AIAdvise::TstLoop2("swipl","00allChainy","u2u",".","/home/urban/rsrch/lean3/snow", 900, 3, 12);'

sub TstLoop2
{
    my ($prolog, $filelist, $filestem, $prologdir, $path2snow, $gtimelimit, $learnflags, $parallel, $initrunfile) = @_;
    my $symoffset = 500000;
    my $advisor = "/home/urban/gr/MPTP2/MizAR/cgi-bin/advisor_lean.pl";
    my $advlimit = 64;
    LeancopClausify($prolog, $filelist, $filestem, $prologdir);
    my ($grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref) =  CreateTables($symoffset, $filestem);
    my ($prob2cl,$prob2conj) = CreateProb2Cl($filestem,$grefnr);

    my $targetsnr = (scalar @$gnrref) - 1;
#    CreateArch($filestem, $targetsnr);

    my $proved_by = PrintProvedBy0($symoffset, $filestem, $grefnr); 
    PrintTrainingFromHash($filestem,0,$proved_by,$grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref);

    my $iter = 1;

    my %initprobs = ();
    @initprobs{ keys %$prob2cl} = ();

    if(defined $initrunfile)
    {
	open(I,$initrunfile); my %h=();
	while(<I>) { chop; $h{$_} = (); }
	close(I);
	%initprobs = %h;
    }

    RunLeancopProblems($filestem,\%initprobs,"./leancop_dnf.sh", $prologdir, $iter, "",1,$parallel);

    while (0==0)
    {
	# hash of solved problems only, each with learning info
	my ($proved_byN, $unproved_byN) = CollectProvedByN($symoffset, $filestem, $iter, $grefnr, $prob2conj,$learnflags);
	PrintTrainingFromClauseHash($filestem,$iter,$unproved_byN,$proved_byN,$grefnr,$gsymnr,$gsymarity,$grefsyms,$gnrsym,$gnrref,$learnflags);
	Learn($path2snow, $filestem, $targetsnr, $iter);

	my ($aport, $adv_pid) =
	    StartAdvisor($path2snow, $advisor, $symoffset, $filestem, $advlimit, $iter+1);
	print "$aport,##\n";

	delete @initprobs{ keys %$proved_byN };   # delete the old ones

	$iter++;
	RunLeancopProblems($filestem,\%initprobs,"./leancopscale1.sh1", $prologdir, $iter, " localhost:$aport $gtimelimit",1,1);

	`kill $adv_pid`;
    }
}

## test: load advisor on pre-learned data
## 
# perl -e 'use AIAdvise;  my ($aport, $adv_pid)=AIAdvise::StartAdvisor("/home/urban/ec/Snow_v3.2/snow","/home/urban/gr/MPTP2/MizAR/cgi-bin/advisor_lean.pl",500000,"uu",64,2); print "$aport,##\n";'


## test: load snow/advisor on thms3, send it a simple request and print result, kill both

sub Tst1
{
    my $BinDir = "/home/urban/gr/MPTP2/MizAR/cgi-bin/bin";
    my ($aport, $sport, $adv_pid, $snow_pid) = StartSNoW("$BinDir/snow", "$BinDir/advisor.pl", 500000, 'thms3', 64, 1);
    print "Advisor PID: $adv_pid, SNoW PID: $snow_pid\n";
    sleep 110;
    my $input1 = ['k3_csspace3'];
    my $input2 = ['v2_rearran1'];
    my @refs1 = GetRefs('localhost', $aport, $input1, 10);
    print join(',',@refs1) . "\n\n";
    my @refs2 = GetRefs('localhost', $aport, $input2, 10);
    print join(',',@refs2) . "\n\n";
}


1;
