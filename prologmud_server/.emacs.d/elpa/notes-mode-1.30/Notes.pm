#!/usr/bin/perl

#
# Notes.pm
# $Id: Notes.pm,v 1.9 2010/06/20 18:31:22 johnh Exp $
#
# Copyright (C) 1996-2006,2012  Free Software Foundation, Inc.
# Comments to <johnh@isi.edu>.
#
# This file is under the Gnu Public License, version 2.
# For details see the COPYING which accompanies this distribution.
#

#
# A Perl module implement a notes class. 
#

require 5.000;
package Notes;
use Carp;
use strict;

#----------------------------------------------------------------------

# my($Notes::revsion) = '$Id: Notes.pm,v 1.9 2010/06/20 18:31:22 johnh Exp $'; #' font-lock hack
# my($Notes::VERSION) = 1.00;

# public method
# optional argument: pathname to read
sub new {
    my ($class, $file) = @_;
    my $self = bless {};
    if (defined($file)) {
	$self->read_from_file($file);
    } else {
        $self->init();
    };
    return $self;
}


# public method
sub read_from_file {
    my ($self, $filename) = @_;

    $self->init();

    open(FILE, "<$filename") || croak "Cannot open $filename";
    my(@lines) = <FILE>;
    close FILE;
    my($i);
    my($start, $Subject) = (0, undef);
    my(%entries);

    for ($i = 0; $i < $#lines; $i++) {
	if ($lines[$i] =~ /^\* / &&
		$lines[$i+1] =~ /^-+\r?$/) {
	    $self->push_entry($Subject, join("", @lines[$start .. $i-1]));
	    $start = $i;
	    ($Subject) = ($lines[$i] =~ /^\*\s+(.*)\r?$/);
	};
    };
    $i = $#lines + 1;
    $self->push_entry($Subject, join("", @lines[$start .. $i-1]));
    return 1;
}

# public_method
sub subjects {
    my($self) = @_;
    return $self->{'subjects'};
}

# public method
sub by_subject {
    my($self, $Subject) = @_;
    my($subject) = lc($Subject);
    return wantarray ? () : undef
	if (!defined($self->{'entryis_by_subject'}{$subject}));
    my(@ret) = ();
    foreach (@{$self->{'entryis_by_subject'}{$subject}}) {
	push (@ret, $self->{'entries'}[$_]);
    };
    return @ret;
}

# public method
sub prelude {
    my($self) = @_;
    return $self->{'pre'};
}


# private method
sub push_entry {
    my($self, $Subject, $entry) = @_;
    if (!defined($Subject)) {
	$self->{'pre'} = $entry;
	return;
    };
    my($subject) = lc($Subject);
    push (@{ $self->{'subjects'} }, $Subject);
    push (@{ $self->{'entries'} }, $entry);
    push (@{ $self->{'entryis_by_subject'}{$subject} }, $#{$self->{'entries'}});
}


# private method
sub init {
    my($self) = @_;
# These inits break things.  Go figure.
#    @{ $self->{'subjects'} } =  @{$self->{'entries'}} = ();
#    %{ $self->{'entryis_by_subject'} } = ();
    $self->{'pre'} = '';
}

