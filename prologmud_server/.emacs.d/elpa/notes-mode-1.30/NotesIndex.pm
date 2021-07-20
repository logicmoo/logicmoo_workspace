#!/usr/bin/perl

#
# NotesIndex.pm
# $Id: NotesIndex.pm,v 1.3 2003/05/23 16:26:19 johnh Exp $
#
# Copyright (C) 1996,2012  Free Software Foundation, Inc.
# Comments to <johnh@isi.edu>.
#
# This file is under the Gnu Public License, version 2.
# For details see the COPYING which accompanies this distribution.
#

#
# A Perl module implement a notes-index class. 
#

package NotesIndex;
use Carp;
use strict;

require 5.000;
$Notes::revsion = '$Id: NotesIndex.pm,v 1.3 2003/05/23 16:26:19 johnh Exp $';
$Notes::VERSION = 1.00;

=head1 NAME

NotesIndex - a simple class for notes-index files

=head1 Public Methods

new, read_from_file, subjects, by_subject, prelude

=cut
#' # font-lock hack

#----------------------------------------------------------------------


# public method
sub new {
    my ($class, $file) = @_;
    my $self = bless {};
    if (!defined($file)) {
        $self->init();
    } else {
	$self->read_from_file($file);
    };
    return $self;
}


# public method
sub read_from_file {
    my ($self, $filename) = @_;

    $self->init();

    open(FILE, "<$filename") || croak "Cannot open $filename";

    while (<FILE>) {
	chomp;
	$self->push_link($_);
    };	

    close FILE;
    return 1;
}

# public_method
sub subjects {
    my($self) = @_;
    return keys %{$self->{'links_by_subject'}};
}

# public method
sub by_subject {
    my($self, $subject) = @_;
    $subject = lc($subject);
    my($resref) = $self->{'links_by_subject'}{$subject};
    return wantarray ? () : undef
	if (!defined($resref));
    return wantarray ? @$resref : 1;
}


# private method
sub push_link {
    my($self, $link) = @_;
    my($subject) = ($link =~ m/\d{2}\#\* (.*)$/);
    $subject = lc($subject);
    push (@{ $self->{'links_by_subject'}{$subject} }, $link);
}


# private method
sub init {
    my($self) = @_;
}

