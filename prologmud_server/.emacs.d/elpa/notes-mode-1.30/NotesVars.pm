#!/usr/bin/perl -w

#
# NotesVars.pm
# $Id: NotesVars.pm,v 1.8 2003/05/23 16:26:22 johnh Exp $
#
# Copyright (C) 1996,2012  Free Software Foundation, Inc.
# Comments to <johnh@isi.edu>.
#
# This file is under the Gnu Public License, version 2.
# For details see the COPYING which accompanies this distribution.
#

require 5.000;

#
# basic initialization
#
BEGIN {
    no strict 'vars';   # avoid %::notes
    $notes{'home'} = ((getpwuid($<))[7]);
    my(@config) = `"$ENV{'NOTES_BIN_DIR'}/mkconfig" perl`;
    die "$0: mkconfig failed\n" if ($#config == -1);
    eval join("", @config);
    unshift(@INC, $notes{'bin_dir'});
}

package NotesVars;
require Exporter;

@ISA = Exporter;
#my(%notes);
@EXPORT = qw(pathname_to_Ymd Ymd_to_epoch
	    pathname_to_epoch epoch_to_pathname
	    url_to_pathname
	    strftime_epoch
	    );
use Time::Local;
use POSIX qw(strftime);
use strict;


##
## Strftime
##
## Because "use POSIX" is so slow, I wrote a standalone strftime program.
## I'm not distributing it because mknew caching seems to solve this problem.
##
#
#my($strftime_inited) = 0;
#sub init_strftime {
#    my($x);
#    # We currently don't support an external strftime.
#    if (-x "$::notes{'bin_dir'}/strftime" ) {
#	# run the program
#	$x = q<
#		sub strftime_backend {
#		    my($output) = `> .$::notes{'bin_dir'} .
#				q</strftime '$_[0]' $_[1]`;
#		    chomp $output;
#		    return $output;
#	        }
#		>;
#    } else {
#	# use POSIX
#	# This option is about 10 times slower to load.
#	# Unfortunately eval'ing this code causes perl5.002
#	# to crash on exit.
#	$x = q<
#		use POSIX;
#		sub strftime_backend {
#		    return POSIX::strftime($_[0],localtime($_[1])) ;
#		}
#		>;
#    };
#    eval $x;
#    $strftime_inited = 1;
#}
#
#sub strftime_epoch {
#    &init_strftime() unless ($strftime_inited);
#    return &strftime_backend(@_);
#}

sub strftime_epoch {
    return POSIX::strftime($_[0], localtime($_[1]));
}

sub pathname_to_Ymd {
    my($pathname) = @_;
    # NEEDSWORK:  not general (assumes file_form is %y%m%d)
    my($Y, $m, $d) = ($pathname =~ /(..)(..)(..)$/);
    $Y += 1900 if ($Y >= 90 && $Y < 100);
    $Y += 2000 if ($Y < 90 && $Y < 100);
    return ($Y, $m, $d);
}

sub Ymd_to_epoch {
    my($y, $m, $d) = @_;
    $y -= 1900 if ($y > 1000);   # convert possible $Y to $y
    return timelocal(0, 0, 12, $d, ($m-1), $y);
}

sub pathname_to_epoch {
    my($pathname) = @_;
    my($Y, $m, $d) = &pathname_to_Ymd($pathname);
    return &Ymd_to_epoch($Y, $m, $d);
}

sub epoch_to_pathname {
    my($epoch) = @_;
    return strftime_epoch("$::notes{dir}/$::notes{int_form}/$::notes{file_form}", $epoch);
}

sub url_to_pathname {
    my($url) = @_;
    $url =~ s@^file:///@@;
    my($home) = $::notes{home};
    $url =~ s@^\~@${home}@;
    $url =~ s@\#\* .*$@@;
    return $url;
}
