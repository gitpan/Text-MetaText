#============================================================================
#
# Text::MetaText::Directive
#
# DESCRIPTION
#   Directive class used by Text::MetaText.
#
# AUTHOR
#   Andy Wardley   <abw@kfs.org>
#
# COPYRIGHT
#   Copyright (C) 1996-1998 Andy Wardley.  All Rights Reserved.
#
#   This module is free software; you can redistribute it and/or
#   modify it under the terms of the Perl Artistic Licence.
#
#----------------------------------------------------------------------------
#
# $Id: Directive.pm,v 0.2 1998/03/13 15:40:14 abw Exp abw $
#
#============================================================================
 
package Text::MetaText::Directive;

use strict;
use vars qw( $VERSION $ERROR @ISA );

require 5.004;



#========================================================================
#                      -----  CONFIGURATION  -----
#========================================================================
 
@ISA     = qw();
$VERSION = sprintf("%d.%02d", q$Revision: 0.2 $ =~ /(\d+)\.(\d+)/);
$ERROR   = "";



#========================================================================
#                      -----  PUBLIC METHODS -----
#========================================================================
 
#========================================================================
#
# new($params, $config)
#
# Module constructor.  A text string is passed in which is parsed.
#
# Returns a reference to a newly created Text::MetaText::Directive
# object or undef on failure.
#
#========================================================================

sub new {
    my $class  = shift;
    my $self   = {};
    my $params = shift;
    my $config = shift || { };

    bless $self, $class;

    # call _parse and implicitly return value 
    $self->_parse($params);
}



#========================================================================
#                     -----  PRIVATE METHODS -----
#========================================================================
 
#========================================================================
#
# _parse($params)
#
# Splits the directive parameter string into it's component parts which
# may include a keyword (e.g. INCLUDE), an identifier (e.g. the name of the
# block/file to include), a conditional (e.g. if="day > 3") and a number
# of parameters (e.g. name=Larry animal=camel).  The combination of the
# elements present depends partly on the keyword type, so we examine this
# and break down the data accordingly.
#
# Returns $self on success.  On error, undef is returned and $ERROR is 
# set to contain a meaningful error message.
#
#========================================================================

sub _parse {
    my $self   = shift;
    my $params = shift;
    my ($keyword, $identifier);
    my (@tokens, $token);
    my ($name, $value);

    # some simple definitions of elements we use in the regex
    my $word     = q((\S+));         # a word
    my $space    = q(\s*);           # optional space
    my $quote    = q(");             # single or double quote characters
    my $escape   = "\\\\";           # an escape \\ (both '\' escaped)
    my $anyquote = "[$quote]";       # class for quote chars
    my $equals   = "$space=$space";  # '=', with optional whitespace

    # within a quoted phrase we might find escaped quotes, e.g. 
    # name = "Andy \"Froth\" Wardley";  to detect this, we scan
    # for sequences of legal characters (not quotes or escapes) up until
    # the first quote or escape;  if we find an escape, we jump past the
    # next character (possible a quote) and repeat the process, and repeat
    # the process, and so on until we *don't* find an escape as the next 
    # character;  that implies it's an unescaped quote and the string ends.
    # (don't worry if that slipped you by - just think of it as magic)

    my $okchars = "[^$quote$escape]*";
    my $qphrase = "$anyquote ( $okchars ($escape.$okchars)* ) $anyquote";

    # build hash array to lookup "reserved" variable names
    my %control = map { $_ => 1 } qw(IF UNLESS FILTER FORMAT PRINT);



    # save the original parameter text string
    $self->{ PARAMSTR } = $params;

    # identify the elements of the parameter string
    unless ($params  =~ /(\S+)\s*(.*)/) {
	$self->_error("Missing directive keyword");
	return undef;
    }

    $keyword = $1;
    $params  = $2;

    # split directive parameters; note that our definitions from 
    # above have embedded substrings ( ) so we need to be a little 
    # careful about counting backreferences accurately...
    while ($params =~ 
	    /
		$word $equals $qphrase    # $1 = $2    (NB: $2 contains $3)
		|                         # e.g. (foo) = "(bar baz)"
		$word $equals $word       # $4 = $5    
		|                         # e.g. (foo) = (bar)
		$qphrase                  # $6         (NB: $6 contains $7)
		|                         # e.g. "(foo bar)"
		$word                     # $8
					  # e.g. (foo)
	    /gxo) { # 'o' - compile regex once only

	if ($6 or $8) {
	    # if $6 or $8 is defined, we found a simple flag.  This gets
	    # pushed directly onto the tokens list
	    push(@tokens, defined($6) ? $6 : $8);
	} 
	else {
	    # $6 and $8 undefined so use $1 = $2, or $4 = $5.  This 
	    # "name = value" pair get pushed onto the token list as
	    # an array reference
	    push(@tokens, [ 
	    	    defined($1) ? $1 : $4, 
	    	    defined($1) ? $2 : $5 
		]);
	}
    }


    # parse the directive parameters according to the directive type
    KEYWORD: {
	
	# END(BLOCK|IF)? directive ignores everything
	$ keyword =~ /^END(BLOCK|IF)?$/i && do {
	    $identifier = '';
	    last KEYWORD;
	};

	# DEFINE directive has optional identifier and params
	$keyword =~ /^DEFINE$/i && do {

	    # identifier must be a simple variable
	    $identifier = (@tokens && !ref($tokens[0]))
		? shift(@tokens)
		: '';
	    last KEYWORD;
	};
		
	# INCLUDE/SUBST/BLOCK have mandatory identifier and 
	# optional params
	$keyword =~ /^(INCLUDE|SUBST|BLOCK)$/i && do {

	    # check there is a simple text identifier 
	    unless (@tokens && !ref($tokens[0])) {
		$self->_error("No identifier in $keyword directive");
		return undef;
	    };
	    $identifier = shift(@tokens);
	    last KEYWORD;
	};

	# if the keyword isn't recognised, we assume it's a basic SUBST
	$identifier = $keyword;
	$keyword    = 'SUBST';
    }

    # force keyword to upper case 
    $keyword = uc($keyword);

    # save directive information into a directive object
    $self->{ KEYWORD }    = $keyword;
    $self->{ IDENTIFIER } = $identifier || '';
    $self->{ PARAMS }     = {};

    # examine, process and store the additional directive parameters
    foreach $token (@tokens) {
    
	# extract/create a name, value pair from token (array or scalar)
	($name, $value) = ref($token) eq 'ARRAY'
	    ? @$token
	    : ($token, 0);
	
	# un-escape any escaped characters in the value
	$value =~ s/\\(.)/$1/g;

	# is this a "control" parameter?
	if (defined $control{ "\U$name" }) {

	    # control params are forced to upper case
	    $name = uc $name;
	    $self->{ $name } = $value;
	}
	# otherwise, it's a normal variable parameter
	else {
	    $self->{ PARAMS }->{ $name } = $value;
	} 
    }

    return $self;
}



#========================================================================
#
# sub error($errmsg, @params) 
#
# Formats the error message format, $errmsg, and any additional parameters,
# @params with sprintf and sets $ERROR package variable with the resulting
# string.  Used to indicate parse errors in _parse();
#
#========================================================================

sub _error {
    my $self = shift;

    $ERROR = sprintf(shift, @_);
}



1;

