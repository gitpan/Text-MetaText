#============================================================================
#
# Text::MetaText
#
# DESCRIPTION
#   Perl 5 module to process text files, featuring variable substitution,
#   file inclusion, conditional operations, print filters and formatting,
#   etc.
#
# AUTHOR
#   Andy Wardley   <abw@kfs.org>
#
# COPYRIGHT
#   Copyright (C) 1996-1998 Andy Wardley.  All Rights Reserved.
#
#   This module is free software; you can redistribute it and/or
#   modify it under the same terms as Perl itself.
#
#   Text::MetaText is based on a template processing language (with the
#   elegant working title of "Template::Base") I developed while working
#   at Peritas Ltd.  I am indebted to Peritas for allowing me to use this 
#   work as the basis for MetaText and to release it to the public domain.
#
#----------------------------------------------------------------------------
#
# $Id: MetaText.pm,v 0.15 1998/02/04 08:57:14 abw Exp abw $
#
#============================================================================
 
package Text::MetaText;

use strict;
use FileHandle;
use Date::Format;
use vars qw( $VERSION @ISA $AUTOLOAD );

require 5.004;
require AutoLoader;



#========================================================================
#                      -----  CONFIGURATION  -----
#========================================================================
 
@ISA     = qw(AutoLoader);
$VERSION = sprintf("%d.%02d", q$Revision: 0.15 $ =~ /(\d+)\.(\d+)/);

# pseudo-class name to identify directives
my $DIRECTIVE = 'DIRECTIVE';

# debug level constants (this will get nicer one day RSN)
use constant DBGNONE =>    0;  # no debugging
use constant DBGINFO =>    1;  # information message only
use constant DBGCONF =>    2;  # configuration details
use constant DBGPREP =>    4;  # show pre-processor operations
use constant DBGPROC =>    8;  # show process operation
use constant DBGPOST =>   16;  # show post-process operation
use constant DBGDATA =>   32;  # show data elements (parameters)
use constant DBGCONT =>   64;  # show content of blocks
use constant DBGFUNC =>  128;  # private function calls
use constant DBGEVAL =>  256;  # show conditional evaluation steps
use constant DBGTEST =>  512;  # test code
use constant DBGALL  => 1023;  # all debug information

my $DBGNAME = {
    'none'     => DBGNONE,
    'info'     => DBGINFO,
    'config'   => DBGCONF,
    'preproc'  => DBGPREP,
    'process'  => DBGPROC,
    'postproc' => DBGPOST,
    'data'     => DBGDATA,
    'content'  => DBGCONT,
    'function' => DBGFUNC,
    'evaluate' => DBGEVAL,
    'test'     => DBGTEST,
    'all'      => DBGALL,
};



#========================================================================
#                      -----  PUBLIC METHODS -----
#========================================================================
 
#========================================================================
#
# new($cfg)
#
# Module constructor.  Reference to a hash array containing configuration
# options may be passed as a parameter.  This is passed off to 
# _configure() for processing.
#
# Returns a reference to a newly created Text::MetaText object.
#
#========================================================================

sub new {
    my $class = shift;
    my $self  = {};
    bless $self, $class;

    $self->_configure(@_);
    return $self;
}



#========================================================================
#
# process($symbol, \%tags, $depth)
#
# This is the main (in fact, only) processing method for MetaText objects.
# The $symbol parameter identifies a file or defined BLOCK...ENDBLOCK
# that requires processing.  When process() comes across a symbol that
# it doesn't already know about, it calls _load_symbol($symbol) to find 
# the file, read and parse it and store the contents in an object symbol 
# table, $self->{ SYMTABLE }, which itself is a hash indexed by the symbol 
# name.  Assuming all goes to plan, process() can then read the contents
# of the symbol table entry and process accordingly.  Future invocations
# of process() for the same symbol are able to use the cached version in
# the symbol table without the need to reload and parse the file.
#
# Calling process(...) is equivalent to a %% INCLUDE %% directive in a 
# file.  In fact, when process() finds an INCLUDE directive in the symbol
# it is processing, it recursively calls itself to process the symbol
# and includes the output in its place. 
#
# The $tags parameter is an optional reference to a hash array which 
# can contain pre-defined tags for substitution while processing the 
# symbol.  Thus, a tags table that contains "{ 'foo' => 'bar' } will 
# produce the output "bar" for a corresponding "%% foo %%" directive.
#
# $depth is an internal counter which is used to identify and prevent 
# infinite recursion.  You can safely ignore this.
#
#========================================================================

sub process {
    my $self   = shift;
    my $symbol = shift;
    my $tags   = shift || {};
    my $depth  = shift || 1;

    my ($item, $keyword);
    my @output = ();


    $self->_DEBUG(DBGFUNC, "process($symbol, $tags, $depth)\n");


    # convert symbol to lower case unless CASE sensitivity is set
    $symbol = lc $symbol unless $self->{ CASE };

    # DEBUG code
    if ($self->{ DEBUGLEVEL } & DBGPROC) {
	$self->_DEBUG(DBGPROC, "Process ($depth): $symbol\n");

	foreach (keys %$tags) {
	    # print tags value, de-referencing if it's an array ref
	    my $datastr = $tags->{ $_ };
	    $datastr = "[ " . join(',', @$datastr) . " ]"
		    if ref($datastr) eq 'ARRAY';

	    # interpolate $variables
	    $datastr .= " (" . $self->_interpolate($tags->{ $_ }, $tags) . ")" 
		    if $tags->{ $_ } =~ /^\$/;

	    $self->_DEBUG(DBGDATA, "  %s => $datastr\n", $_);
	}
    }


    # check for excessive recursion
    if ($depth > $self->{ MAXDEPTH }) {
	$self->_warn("Maximum recursion exceeded\n");
	return;
    }

    # load the symbol if it's not already in the symbol table
    unless (defined($self->{ SYMTABLE }->{ $symbol })) {
	$self->_load_symbol($symbol) || return;
    }
 
    #
    # The symbol table entry for $symbol ($self->{ SYMTABLE }->{ $symbol })
    # is a reference to an array.  Each element in the array can be either
    # a plain text string or a hash array blessed into the $DIRECTIVE class.
    # The former represent normal text blocks in the processed file, the
    # latter represent pre-parsed MetaText directives (see _load_symbol(),
    # _pre_process() and _parse_directive() for more detail on this stage).
    # A directive will contain one or more of the following elements, based
    # on the directive type and other data defined in the directive block:
    #
    #  $directive->{ KEYWORD }     # directive type: INCLUDE, DEFINE, etc
    #  $directive->{ IDENTIFIER }  # KEYWORD target, i.e. INCLUDE <filename>
    #  $directive->{ PARAMS }      # hash ref of variables defined
    #  $directive->{ PARAMSTR }    # original parameter string
    #  $directive->{ IF }          # an "if=..." conditional
    #  $directive->{ UNLESS }      # ditto "unless=..."
    # 

    # process each each line from the block
    foreach $item (@{ $self->{ SYMTABLE }->{ $symbol } }) {

	# get rid of the non-directive cases first...
	unless (ref($item) eq $DIRECTIVE) {

	    # return content if we find the end-of-content marker 
	    return join("", @output)
		if $item =~ /^__END__$/;

	    # not a directive - so just push output and loop
	    push(@output, $item);

	    next;
	}


	# test any "if=<condition>" statement...
	if ($item->{ IF }) {
	    my $result = $self->_evaluate($item->{ IF }, $tags, 
			$item->{ DELIMITER } || $self->{ DELIMITER });
	    next unless defined($result) && $result > 0;
	}

	# ...and/or any "unless=<condition>" statement
	if ($item->{ UNLESS }) {
	    my $result = $self->_evaluate($item->{ UNLESS }, $tags, 
			$item->{ DELIMITER } || $self->{ DELIMITER });
	    next if defined($result) && $result != 0;
	}


	$keyword = $item->{ KEYWORD };

	$keyword eq 'DEFINE' && do {
	    # merge all the new definitions in $item->{ PARAMS } 
	    # into the current '$tags' definitions hash array 
	    $tags = { %$tags, %{ $item->{ PARAMS } } };

	    next;
	};

	$keyword eq 'INCLUDE' && do {
	    # create a new super-set of existing tags and those 
	    # defined within the INCLUDE directive
	    my $newtags = defined($item->{ PARAMS })
		    ? { %$tags, %{$item->{ PARAMS }} }
		    : $tags;

	    # interpolate any embedded tags within ident
	    my $ident = $self->_interpolate($item->{ IDENTIFIER }, $tags);

	    # process the INCLUDE'd symbol and push to output
	    push(@output, 
		$self->_post_process($item, 
		$self->process($ident, $newtags, $depth + 1)));

	    next;
	};

	$keyword eq 'SUBST' && do {

	    # call _substitute to handle token substitution
	    my $rep = $self->_substitute($item, $tags);
	    if (defined($rep)) {
		$rep = $self->_post_process($item, $rep);
	    }
	    else {
		# unrecognised token
	    	$self->_warn("Unrecognised token: $item->{ IDENTIFIER }\n")
		    if grep(/\bwarn\b/, $self->{ ROGUE });

	    	# resolve nothing if 'delete' is defined as a ROGUE option
		$rep = grep(/\bdelete\b/, $self->{ ROGUE })
		       ? ""
		       : $self->{ MAGIC }->[ 0 ]       # rebuild directive
		         . " $item->{ IDENTIFIER } "
		         . ($item->{ PARAMSTR } || "")
		         . " "
		         . ($self->{ MAGIC }->[ 1 ]);
	    }

	    push(@output, $rep);

	    next;
	};

	# invalid directive;  this shouldn't happen
	$self->_warn("Unrecognise directive: $keyword\n")
    }

    # post process output tokens and return as a single line
    join("", @output);
}



#========================================================================
#                     -----  PRIVATE METHODS -----
#========================================================================
 
#========================================================================
#
# _configure($cfg)
#
# Configuration method which examines the elements in the hash array 
# referenced by $cfg and sets the object's internal state accordingly.
# Errors/warnings are reported via $self->_warn();
#
#========================================================================

sub _configure {
    my $self = shift;
    my $cfg  = shift;


    # set configuration defaults
    $self->{ MAXDEPTH   } = 32;
    $self->{ DEBUGLEVEL } = DBGNONE;
    $self->{ MAGIC }      = [ '%%', '%%' ];
    $self->{ LIB }        = "";
    $self->{ ROGUE }      = "";   # how to handle rogue directives
    $self->{ CASE }       = 1;    # case sensitivity
    $self->{ CHOMP }      = 0;    # chomp straggling newlines 
    $self->{ EXECUTE }    = 0;    # execute SUBST as function?
    $self->{ DELIMITER }  = ',';  # what splits a list?
    $self->{ FILTER }     = {     # pre-defined filters
	'sr' => sub { 
	    my $m1 = $_[2] || ''; 
	    my $m2 = $_[3] || '';
	    $_[1] =~ s/$m1/$m2/g; 
	    $_[1];
	},
	'escape' => sub { 
	    my $cm = $_[2] || '';
	    $_[1] =~ s/($cm)/\\$1/g;
	    $_[1];
	},
    };

    # the config hash array reference, $cfg, may contain a number of 
    # different config options.  These are examined case-insensitively
    # (but converted to UPPER CASE when stored) and, depending on the
    # option, tested for correctness, manipulated or massaged in some
    # way;  invalid options generate a warning.

    return unless $cfg;

    # check usage
    unless (ref($cfg) eq 'HASH') {
	$self->_warn(ref($self) . "->new expects a hash array reference\n");
	return;
    };

    foreach (keys %$cfg) {

	# set simple config values (converting keyword to UPPER case)
	/^MAXDEPTH|LIB|DELIMITER|CASE|CHOMP|EXECUTE$/i && do {
	    $self->{ "\U$_" } = $cfg->{ $_ };
	    next;
	};

	# add any user-defined print filters to the pre-defined ones
	/^FILTER$/i && do {
	    my $filter;
	    foreach $filter (keys %{$cfg->{ $_ }}) {
		$self->{ "\U$_" }->{ $filter } = $cfg->{ $_ }->{ $filter };
	    }
	    next;
	};

	# debuglevel is defined as a series of non-word delimited words
	# which index into the $DBGNAME hash ref for values
	/^DEBUGLEVEL$/i && do {
	    foreach (split(/\W+/, $cfg->{ $_ })) {
		$self->_warn("Invalid debug option: $_\n"), next
			unless defined($DBGNAME->{ $_ });

		# logically OR in the new debug value
		$self->{ DEBUGLEVEL } |= $DBGNAME->{ $_ };
	    }
	    next;
	};

	# ROGUE defines how unrecognised (rogue) directives should
	# be handled.  
	/^ROGUE$/i && do {
	    # create a list reference of valid ROGUE options and
	    # print a warning message about invalid options
	    $self->{ ROGUE } = join(',',
		map {
		    /^warn|delete$/i
		    ? "\L$_"
		    : $self->_warn("Invalid roque option: \L$_\n") 
			&& ();  # warn, then return empty list for map
		} split(/\W+/, $cfg->{ $_ }) );
	    next;
	};


	# MAGIC needs a little processing to convert to a 2 element
	# ARRAY ref if a single string was specified (i.e. for both)
	/^MAGIC$/i && do {
	    if (ref($cfg->{ $_ }) eq 'ARRAY') {
		$self->{ MAGIC } = $cfg->{ $_ };
	    } 
	    else {
		# create a 2-element array reference
		$self->{ MAGIC } = [ ($cfg->{ $_ }) x 2 ];
	    }
	    next;
	};

	# set ERROR/DEBUG handling function, check for a CODE reference
	/^ERROR|DEBUG$/i && do {
	    # check this is a code reference	
	    $self->_warn("Invalid \L$_\E function\n"), next
		unless ref($cfg->{ $_ }) eq 'CODE';
	    $self->{ "\U$_" } = $cfg->{ $_ };
	    next;
	};

	# warn about unrecognised parameter
	$self->_warn("Invalid configuration parameter: $_\n");
    }

    # DEBUG code
    if ($self->{ DEBUGLEVEL } & DBGCONF) {
	$self->_DEBUG(DBGCONF, "$self Version $VERSION\n");

	foreach (keys %$self) {
	    $self->_DEBUG(DBGDATA, "  %-10s => %s\n", $_, $self->{ $_ });
	}
    }
}



#========================================================================
# 
# _load_symbol($symbol)
#
# Attempts to load the specified symbol from a file into the object's
# symbol table.  
#
# Returns 1 on success or 0 on failure.
#
#========================================================================

sub _load_symbol {
    my $self   = shift;
    my $symbol = shift;
    my $fp;


    $self->_DEBUG(DBGFUNC, "_load_symbol($symbol)\n");


    my $symfile = $self->_find_file($symbol) or return 0;

    # open file
    unless (defined($fp = new FileHandle "$symfile")) {
	$self->warn("$symfile: $!\n");
	return 0;
    }

    # call preprocess to load the symbol and any self-contained blocks
    # into our current symbol table
    $self->_pre_process($fp, $symbol);

    # NB: file is closed automatically when $fp goes out of scope

    # return OK status
    1;
}



#========================================================================
#
# _find_file($symbol)
#
# Attempts to locate a file with the same name as the specified symbol.
# If the symbol starts with a '/', it is assumed to be an absolute file
# path.  Otherwise, the current directory and all the directories specified
# in the LIB entry in the config hash array are searched.
#
# Returns a full file path or undef on failure.
#
#========================================================================

sub _find_file {
    my $self   = shift;
    my $symbol = shift;
    my ($dir, $fullpath);


    $self->_DEBUG(DBGFUNC, "_find_file($symbol)\n");


    # default $fullpath to $symbol (may be an absolute path)
    $fullpath = $symbol;

    # file is relative to $tags->{ directory } unless it starts '/'
    if (defined($self->{ LIB }) && $symbol !~ /^\//) {

	foreach $dir ('.', split(/[|;:,]/, $self->{ LIB })) {
	    # construct a full file path
	    $fullpath  = $dir;
	    $fullpath .= '/' unless ($fullpath =~ /\/$/);
	    $fullpath .= $symbol;

	    # test if the file exists
	    last if -f $fullpath;
	}
    }

    # check we resolved a filename (this isn't superfluous because the
    # above loop may drop out of the bottom)
    unless (-r $fullpath) {
	$self->_warn("$fullpath: $!\n");
	return undef;
    }

    $self->_DEBUG(DBGINFO, "loading file: $fullpath\n");

    # return validated fullpath
    $fullpath;
}



#========================================================================
#
# _pre_process($fp, $symbol, $depth)
#
# Reads the file stream from $fp, loading the lines into the symbol entry
# in the internal symbol table indexed by the name $symbol.  Processing
# continues until an EOF, a line containing "__END__", or a lone 
# %% ENDBLOCK %% directive is encountered.  Any remaining text following 
# an %% ENDBLOCK %% directive, up to the end of the current line, is 
# returned for the calling process to handle or ignore (this simplifies 
# the recursion process described below).
#
# Blocks encountered that are bounded by a matched pair of %% BLOCK name %%
# ... %% ENDBLOCK %% directives will cause a recursive call to 
# $self->_pre_process() to be made to handle the block definition for
# the sub-block.  Block definitions can theoretically be nested indefinately 
# although in practice, the process ends when an upper recursion limit is 
# reached ($self->{ MAXDEPTH }).  To this effect,  $depth is used to 
# internally indicate the current recursion depth to each instance.
#
#========================================================================

sub _pre_process {
    my $self   = shift;
    my $fp     = shift;
    my $symbol = shift;
    my $depth  = shift || 1;
    my ($symtabent, $line, $nextline);	


    $self->_DEBUG(DBGFUNC, "_pre_process($fp, $symbol, $depth)\n");


    # check for excessive recursion
    if ($depth > $self->{ MAXDEPTH }) {
	$self->_warn("Maximum recursion exceeded\n");
	return;
    }

    # set the symbol table entry to a null list and get a local reference
    $symtabent = $self->{ SYMTABLE }->{ $symbol } = [];

    # get a local copy of the MAGIC symbols for efficiency
    my ($magic1, $magic2) = @{ $self->{ MAGIC } };

    # read each line from the input file
    READLINE: while (defined($line = <$fp>)) {
		
	# you may wonder why we don't read a paragraph at a time
	# ($/ = ''), but I'm allowing for the fact that directives
	# may contain blank lines and thus span paragraphs.

	# split the line into %%TAGS%% and non-tags
	while ($line =~ /
		    (.*?)        # anything before the %%  ($1)
		    $magic1      # indicates the start of a magic token
		    \s*          # some optional whitespace
		    (.*?)        # parameters ($2)
		    \s*          # even more whitespace 
		    (            # all of this is optional... ($3)
		      ($magic2)  # closing magic token ($4)
		      (.*)       # rest of the line ($5)
		    )?           # see, I said it was optional
		    $            # EOL, so that it all gets eaten
		    /x) {

	    # 
	    # if the closing magic symbol '%%' wasn't found in 
	    # $5 as expected, then it suggests that the directive 
	    # continues onto the next line, so we append the next 
	    # line and try again.
	    #
	    unless ($4) {
		# if we can't read another line, tack on the 
		# magic token to avoid a dangling directive
		unless (defined($nextline = <$fp>)) {
		    $nextline = $magic2;
		    $self->_warn("Closing directive tag missing in $symbol\n");
		}
		chomp($line);
		$line .= $nextline;
		next;
	    }

	    # push any pre-tag text onto the output list
	    push(@$symtabent, $1) if $1;

	    # save any post-tag text back into line for next time 
	    # around; if CHOMP is defined and the remainder of the 
	    # line is empty, it is disregarded and no effort is made
	    # to replace the newline which the earlier regex chomped 
	    # off.  The net result is that a newline coming immediately
	    # after a directive block is chomped.  If CHOMP is not
	    # set, the newline is added regardless.
	    $line = $self->{ CHOMP } 
	            ? ($5 ? "$5\n" : "")
		    : "$5\n";

	    # process any tag that was found
	    $2 && do {
		my $directive = $self->_parse_directive($2);

		$directive->{ KEYWORD } =~ /^BLOCK$/ && do {
		    # we're about to recursively call ourself to 
		    # process the BLOCK definition, but we may have 
		    # already read part of the line *after* the %% BLOCK %%
		    # directive.  It doesn't belong to us so we rewind 
		    # the file pointer a little so it can be re-read
		    # by the _pre_process() function we call.

		    seek($fp, -(length($line)), 1) if $line;

		    # call onself to preprocess the BLOCK
		    $line = $self->_pre_process($fp, 
			    $directive->{ IDENTIFIER }, $depth + 1);
	
		    # if "print" was specified as a BLOCK option, convert 
		    # the BLOCK directive to an INCLUDE and push it 
		    # onto the stack to ensure the original gets replicated.

		    $directive->{ KEYWORD } = 'INCLUDE';
		    push(@$symtabent, $directive)
    			if (defined($directive->{ PRINT }));

		    next;
		};

		$directive->{ KEYWORD } =~ /^END(BLOCK)?$/ && do {

		    if ($self->{ DEBUGLEVEL } & DBGCONT) {
			$self->_dump_symbol($symbol);
		    }

		    # return any remaining post-"%%ENDBLOCK%%" text
		    return $line;
		};

		$directive->{ KEYWORD } eq 'DEFINE' && do {
		    # DEFINE directives get pushed on as they are
		    push(@$symtabent, $directive);
		    next READLINE unless $line;
		};


		# no other processing required, so push on as it is
		push(@$symtabent, $directive);
	    };

	}   # while ($line =~ / ...

	# push any remaining text
	push(@$symtabent, $line) if $line;
    }

    # DEBUG code
    if ($self->{ DEBUGLEVEL } & DBGCONT) {
	    $self->_dump_symbol($symbol);
    }
}



#========================================================================
#
# _parse_directive($params)
#
# Splits the directive parameter string into it's component parts which
# may include a keyword (e.g. INCLUDE), an identifier (e.g. the name of the
# block/file to include), a conditional (e.g. if="day > 3") and a number
# of parameters (e.g. name=Larry animal=camel).  The combination of the
# elements present depends partly on the keyword type, so we examine this
# and break down the data accordingly.
#
# The elements are stored in a hash array which is then blessed into a 
# pseudo "DIRECTIVE" class.  This allows the process that reads the 
# symbol table list that this becomes part of, to readily identify a
# DIRECTIVE and distinguish it from normal text.  
#
#========================================================================

sub _parse_directive {
    my $self   = shift;
    my $prmstr = shift;
    my $direct = bless {}, $DIRECTIVE;

    my ($keyword, $identifier, $params, $tags);
    my @control = qw(IF UNLESS FILTER FORMAT PRINT);
    my $ctrlstr = join('|', @control);


    $self->_DEBUG(DBGFUNC, "_parse_directive($prmstr)\n");


    # identify the elements of the parameter string
    $prmstr  =~ /(\S+)\s*(.*)/;

    $keyword = $1;
    $params  = $2 || '';


    # check something was specified in the %% ... %%
    unless ($keyword) {
	$self->_warn("Missing directive keyword\n");
	return undef;
    }

	
    KEYWORD: {
	
	# END(BLOCK)? directive ignores everything
	$ keyword =~ /^END(BLOCK)?$/i && do {
	    $identifier = '';
	    $params     = '';
	    last KEYWORD;
	};

	# DEFINE directive expects parameters but no identifiers
	$keyword =~ /^DEFINE$/i && do {
	    $identifier = '';
	    last KEYWORD;
	};
		
	# INCLUDE/SUBST/BLOCK expect identifier and perhaps parameters
	$keyword =~ /^(INCLUDE|SUBST|BLOCK)$/i && do {
	    $params =~ /(\S+)\s*(.*)/;
	    $identifier = $1;
	    $params     = $2;
	    last KEYWORD;
	};


	# if the keyword isn't recognised, we assume it's a basic SUBST
	$identifier = $keyword;
	$keyword    = 'SUBST';
    }

    # force identifier to lower case unless CASE sensitive
    $identifier = lc $identifier unless $self->{ CASE };

    # force keyword to upper case (always happens even if CASE sensitive)
    $keyword = uc($keyword);

    # save directive information into a directive object
    $direct->{ KEYWORD }     = $keyword;
    $direct->{ IDENTIFIER }  = $identifier || '';


    if ($params) {

	# save original parameter string (tidied up slightly)
	$direct->{ PARAMSTR } = $self->_tidy($params);

	# ...and also as a hash array for convenience
	$tags = $self->_split_params($params);

	foreach (grep(/^$ctrlstr$/i, keys %$tags)) {

	    # control params are forced to upper case; normal params
	    # are forced to lower case unless the $self->{ CASE }
	    # case sensitivity flag is set.

	    # extract the control parameter, forcing name to UPPER case
	    $direct->{"\U$_"} = $tags->{ $_ };
	    delete $tags->{ $_ };
	}

	if ($self->{ CASE }) {
	    # save tags into $direct, preserving case
	    $direct->{ PARAMS } = $tags;
	} 
	else {
	    # copy tags into $direct, forcing names to lower case
	    foreach (keys %$tags) { 
		    $direct->{ PARAMS }->{"\L$_"} = $tags->{ $_ }
	    } 
	}
    }


    # DEBUG code
    if ($self->{ DEBUGLEVEL } & DBGPREP) {
	$self->_DEBUG(DBGPREP, "Directive: %-10s  identifier: %s\n",
	    $direct->{ KEYWORD },
	    $direct->{ IDENTIFIER } || "<none>");

	foreach (@control, 'PARAMSTR') {
	    $self->_DEBUG(DBGDATA, "  %s: %s\n", $_, $direct->{ $_ })
		    if defined($direct->{ $_ });
	}

	foreach (keys %{ $direct->{ PARAMS } }) {
	    # print param value, de-referencing if it's an array ref
	    my $datastr = $direct->{ PARAMS }->{ $_ };
	    $self->_DEBUG(DBGDATA, "  %s => $datastr\n", $_);
	}
    }

    # return directive object
    $direct;
}



#========================================================================
#
# _split_params($params)
#
# Splits the parameter list, $params, into name=value pairs, building a 
# hash array containing each pair.  Returns a reference to the hash array.
#
#========================================================================

sub _split_params {
    my $self   = shift;
    my $params = shift;
    my %tags;
    my ($name, $value);


    $self->_DEBUG(DBGFUNC, "_split_params($params)\n");


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


    # NOTE: our definitions above have embedded substrings ( ) so we need
    # to be a little careful about counting backreferences accurately...

    # split params and add to %newtags
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
	    # if $6 or $8 is defined, we found a simple flag
	    $name = defined($6) ? $6 : $8;
	    $value = 0;
	} 
	else {
	    # $6 and $8 undefined so use $1 = $2, or $4 = $5
	    $name  = defined($1) ? $1 : $4;
	    $value = defined($1) ? $2 : $5;
	}

	# check both name and value are defined
	$value = "" unless defined $value;
	next unless defined $name;

	# un-escape escaped characters 
	$value =~ s<\\(.)><$1>g;

	# push tag onto tags list
	$tags{ $name } = $value;
    }

    return \%tags;
}



#========================================================================
#
# _tidy($string)
#
# Tidies the specified string, $string, by compressing any whitespace 
# outside of quotes.
#
#========================================================================

sub _tidy {
    my $self   = shift;
    my $string = shift;
    my $newstr = "";


    $self->_DEBUG(DBGFUNC, "_tidy($string)\n");


    # see the comments in _split_params() explaining how this works

    my $word     = q((\S+));         # a word
    my $space    = q(\s*);           # optional space
    my $quote    = q("');            # single or double quote characters
    my $escape   = "\\\\";           # an escape \\ (both '\' escaped)
    my $anyquote = "[$quote]";       # class for quote chars
    my $equals   = "$space=$space";  # '=', with optional whitespace

    my $okchars = "[^$quote$escape]*";
    my $qphrase = "($anyquote $okchars ($escape.$okchars)* $anyquote)";

    while ($string =~ / $qphrase | $word /gxo) {
	$newstr .= " " if $newstr;
	$newstr .= defined($1) ? $1 : $3;
    }
	
    $newstr;
}



#========================================================================
#
# _evaluate($expr, \%tags, $delimiter)
#
# Evaluates the specified expression, $expr, using the token values in 
# the hash array referenced by $tags.  The $delimiter parameter may also
# be passed to over-ride the default delimiter ($self->{ DELIMITER })
# which is used when splitting 'in' lists for evalutation 
# (e.g. if="name in Tom,Dick,Harry").
#
# Returns 1 if the expression evaluates true, 0 if it evaluates false.
# On error (e.g. a badly formed expression), undef is returned.
#
#========================================================================

sub _evaluate {
    my $self  = shift;
    my $expr  = shift;
    my $tags  = shift;
    my $delim = shift || $self->{ DELIMITER };
    my ($lhs, $rhs, $sub, $op, $result);

    # save a copy of the original expression for debug purposes
    my $original = $expr;

    # a hash table of comparison operators and associated functions
    my $compare = {
	'=='  => sub { $_[0] eq  $_[1]  },
	'='   => sub { $_[0] eq  $_[1]  },  
	'!='  => sub { $_[0] ne  $_[1]  },
	'>='  => sub { $_[0] ge  $_[1]  },
	'<='  => sub { $_[0] le  $_[1]  },
	'>'   => sub { $_[0] gt  $_[1]  },
	'<'   => sub { $_[0] lt  $_[1]  },
	'=~'  => sub { $_[0] =~ /$_[1]/ },
	'!~'  => sub { $_[0] !~ /$_[1]/ },
	'in'  => sub { grep(/^$_[0]$/, split(/$delim/, $_[1])) },
    };
    # define a regex to match the comparison keys;  note that alpha words
    # (\w+) must be protected by "\b" boundary assertions and that order
    # is extremely important (so as to match '>=' before '>', for example)
    my $compkeys = join('|', qw( \bin\b <= >= < > =~ !~ != == = ));

    # a hash table of boolean operators and associated functions
    my $boolean = {
	'&&'  => sub { $_[0] &&  $_[1] },
	'||'  => sub { $_[0] ||  $_[1] },
	'^'   => sub { $_[0] ^   $_[1] },
	'and' => sub { $_[0] and $_[1] },
	'or'  => sub { $_[0] or  $_[1] },
	'xor' => sub { $_[0] xor $_[1] },
    };
    my $boolkeys = join('|', 
	map { /^\w+$/ ? "\\b$_\\b" : "\Q$_" } keys %$boolean);


    # DEBUG code
    $self->_DEBUG(DBGFUNC, "_evaluate($expr, $tags)\n");
    foreach (keys %$tags) {
	$self->_DEBUG(DBGEVAL | DBGDATA, "  eval: %-10s -> %s\n", 
		$_, $tags->{ $_ });
    } 


    # trounce leading and trailing whitespace
    foreach ($expr) {
	s/^\s+//;
	s/\s+$//g;
    }

    $self->_DEBUG(DBGEVAL, "EVAL: expr: [$expr]\n");

    # throw back expressions already fully simplified; note that we evaluate
    # expressions as strings to avoid implicit true/false evaluation
    if ($expr eq '1' or $expr eq '0') {
	$self->_DEBUG(DBGEVAL, "EVAL: fully simplified: $expr\n");
	return $expr;
    }


    # 
    # fully expand all expressions in parenthesis
    #

    while ($expr =~ /(.*?)\(([^\(\)]+)\)(.*)/) {
	$lhs = $1;
	$sub = $2;
	$rhs = $3;

	# parse the parenthesised expression
	return undef unless defined($sub = $self->_evaluate($sub, $tags));

	# build a new expression
	$expr = "$lhs $sub $rhs";
    }

    # check there aren't any hanging parenthesis
    $expr =~ /[\(\)]/ && do {
	$self->_warn("Unmatched parenthesis: $expr\n");
	return undef;
    };


    # 
    # divide expression by the first boolean operator
    #

    if ($expr =~ /(.*?)\s*($boolkeys)\s*(.*)/) {

	$lhs = $1;
	$op  = $2;
	$rhs = $3;

	$self->_DEBUG(DBGEVAL, "EVAL: boolean split:  [$lhs] [$op] [$rhs]\n");

	# evaluate expression using relevant operator
	$result = &{ $boolean->{ $op } }(
	    $lhs = $self->_evaluate($lhs, $tags), 
	    $rhs = $self->_evaluate($rhs, $tags)
	) ? 1 : 0;
		    
	$self->_DEBUG(DBGEVAL, 
		"EVAL: bool: [$original] => [$lhs] [$op] [$rhs] = $result\n");
	return $result;
    }


    #
    # divide expression by the first comparitor
    #

    $lhs = $expr;
    $rhs = $op = '';

    if ($expr =~ /^\s*(.*?)\s*($compkeys)\s*(.*?)\s*$/) {
    	$lhs  = $1 || $expr;
       	$op   = $2 || '';
    	$rhs  = $3 || '';

	$self->_DEBUG(DBGEVAL, "EVAL: compare: [$lhs] [$op] [$rhs]\n");
    }

    #
    # cleanup, rationalise and/or evaluate left-hand side
    #

    # left hand side is automatically dereferenced so remove any explicit
    # dereferencing '$' character at the start
    $lhs =~ s/^\$//;

    # convert lhs to lower case unless CASE sensitive
    $lhs = lc $lhs unless $self->{ CASE };

    $self->_DEBUG(DBGEVAL, "EVAL: expand lhs: \$$lhs => %s\n", 
	    $tags->{ $lhs } || "<undef>");

    # dereference the lhs variable 
    $lhs = $tags->{ $lhs } || 0;


    #
    # no comparitor implies lhs is a simple true/false evaluated variable
    #

    unless ($op) {
	$self->_DEBUG(DBGEVAL, "EVAL: simple: [$lhs] = %s\n", $lhs ? 1 : 0);
	return $lhs ? 1 : 0;
    }


    #
    # de-reference RHS of the equation ($comp) if it starts with a '$'
    #

    if ($rhs =~ s/^\$(.*)/$1/) {

	# convert variable name to lower case unless CASE sensitive
	$rhs = lc $rhs unless $self->{ CASE };

	$self->_DEBUG(DBGEVAL, "EVAL: expand rhs: $rhs => %s\n",
		    $tags->{ $rhs } || "<undef>");

	# de-reference variables
	$rhs = $tags->{ $rhs } || 0;
    }
    else {
	$self->_DEBUG(DBGEVAL, "EVAL: rhs: [$rhs]\n");
    }

    # remove surrounding quotes from rhs value
    foreach ($rhs) {
	s/^["']//;
	s/["']$//;
    }

    # force both LHS and RHS to lower case unless CASE sensitive
    unless ($self->{ CASE }) {
	$lhs = lc $lhs;
	$rhs = lc $rhs;
    }


    # 
    # evaluate the comparison statement
    #

    $result = &{ $compare->{"\L$op"} }($lhs, $rhs) ? 1 : 0;

    $self->_DEBUG(DBGEVAL, "EVAL: comp: [%s] => [%s] [%s] [%s] = %s\n", 
	    $original, $lhs, $op, $rhs, $result);

    $result;
}



#========================================================================
#
# _interpolate($expr, $tags)
#
# Examines the string expression, $expr, and attempts to replace any 
# elements within the string that relate to key names in the hash table
# referenced by $tags.  A simple "$variable" subsititution is identified 
# when separated by non-word characters 
#
#   e.g.  "foo/$bar/baz" => "foo/" . $tags->{'bar'} . "/baz"
#
# Ambiguous variable names can be explicitly resolved using braces as per 
# Unix shell syntax. 
#
#   e.g. "foo${bar}baz"  => "foo" . $tags{'bar'} . "baz"
#
# The function returns a newly constructed string.  If $expr is a reference
# to a scalar, the original scalar is modified and also returned.
#
#========================================================================

sub _interpolate {
    my $self = shift;
    my $expr = shift;
    my $tags = shift || {};


    $self->_DEBUG(DBGFUNC, "_interpolate($expr, $tags)\n");


    # if a reference is passed, work on the original, otherwise take a copy
    my $work = ref($expr) eq 'SCALAR' ? $expr : \$expr;

    # look for a "$identifier" or "${identifier}" and substitute
    $$work =~ s/(\$\{?(\w+)}?)/$tags->{ $2 } || $1/ge;

    # return modified string
    $$work;
}



#========================================================================
#
# _substitute($directive, $tags)
#
# Substitues the DIRECTIVE referenced by $directive with the corresponding
# hash entry contents in $tags, if a relevant entry exists.  
#
# If a relevant hash entry does not exist and $self->{ EXECUTE } is set 
# to a true value, _substitute attempts to run the directive name as a 
# class method, allowing derived (sub) classes to define member functions 
# that get called automagically by the base class.  If no such method
# exists, AUTOLOAD gets called and, if $self->{ EXECUTE } has a value
# > 1, it attempts to run a function in the main package with the same
# name as the identifier.  If all that fails, undef is returned.
#
#========================================================================

sub _substitute {
    my $self      = shift;
    my $directive = shift;
    my $tags      = shift;


    $self->_DEBUG(DBGFUNC, "_substitute($directive, $tags)\n");


    # extract these handy values
    my $ident     = $directive->{ IDENTIFIER } || return;
    my $prmhash   = $directive->{ PARAMS }     || {};
    my $prmstr    = $directive->{ PARAMSTR }   || '';


    # first look for symbol in tags hash
    if (defined($tags->{ $ident })) {

	my $replace = $tags->{ $ident };
	while ($replace =~ /\$/) {
	    my $repnew = $self->_interpolate($replace, $tags);
	    $self->_DEBUG(DBGINFO, "interpolating: $replace -> $repnew\n");

	    # bomb out if tag can't be resolved (hasn't changed)
	    last if $replace eq $repnew;

	    $replace = $repnew;
	}

	if (ref($replace) eq 'CODE') {

	    $self->_DEBUG(DBGINFO, "calling function: $ident($prmstr)\n");

	    # call the substition function
	    return &$replace($ident, $prmhash, $prmstr);   ## RETURN ##
	} else {
	    # replace with the substitution text
	    return $replace;                               ## RETURN ##
	}

	# we never get here
    }		

    # convert keyword to lower case (unless CASE sensitive)
    $ident = lc $ident unless $self->{ CASE };

    # special case(s)
    return time()
	if ($ident eq ($self->{ CASE } ? 'TIME' : 'time'));

    # if EXECUTE is defined, try to run it as a method and let AUTOLOAD 
    # have a go at resolving it
    $self->{ EXECUTE } 
	? $self->$ident($prmhash, $prmstr)
	: undef;
}



#========================================================================
#
# _post_process($directive, $string)
#
# This function is called to post-process the output generated when 
# process() conducts a SUBST or an INCLUDE operation.  The FILTER and 
# FORMAT parameters of the directive, $directive, are used to indicate 
# the type of post-processing required. 
#
# Returns a the processed string.
#
#========================================================================

sub _post_process {
    my $self      = shift;
    my $directive = shift;
    my $line      = shift || "";
    my @lines     = split(/\n/, $line);
    my ($pre, $post);


    # DEBUG code
    if ($self->{ DEBUGLEVEL } & DBGFUNC) {
	my $dbgline = $line;
	$dbgline = substr($dbgline, 0, 16) . "..." 
		if length($dbgline) > 16;
	$dbgline =~ s/\n/\\n/g;
	$dbgline =~ s/\t/\\t/g;
	$dbgline = "\"$dbgline\"";
	$self->_DEBUG(DBGFUNC, "_post_process($directive, $dbgline)\n");
    }


    # see if the "FILTER" option is specified
    if (defined($directive->{ FILTER })) {

	# extract the filter name and parameters: <name>(<params>)
	$directive->{ FILTER } =~ /([^(]+)(?:\((.*)\))?/;
	my $fltname   = $1;

	# split filter parameters and remove enclosing quotes
	my @fltparams = split(/\s*,\s*/, $2 || "");
	foreach (@fltparams) {
	    s/^"//;
	    s/"$//;
	}


	# is there a filter function with the name specified?
	if (ref($self->{ FILTER }->{ $fltname }) eq 'CODE') {

	    $self->_DEBUG(DBGINFO, "filter: $fltname(%s)\n",
		    join(", ", $fltname, @fltparams));

	    # deref filter code to speed up multi-line processing
	    my $fltfn = $self->{ FILTER }->{ $fltname };

	    # feed each line through filter function
	    foreach (@lines) {
		$pre = $_;
		$_ = &$fltfn($fltname, $_, @fltparams);
		$post = $_;

    		if ($self->{ DEBUGLEVEL } & DBGPOST) {
    		    $self->_DEBUG(DBGDATA, 
			"filter: [ $pre ]\n     -> [ $post ]\n");
    		}
	    }
	}
	else {
	    $self->_warn("$fltname: non-existant or invalid filter\n");
	}
    }


    # 
    # if the "format=<template>" option is specified, the output
    # is formatted in one of two ways.  If the format string contains
    # a sequence matching the pattern "%[^s]" (i.e. any %<character> 
    # marker other than '%s'), it is assumed to be a date and is 
    # processed using time2str() from Date::Format.
    #
    # If the format string contains no other percent marker than
    # "%s", it is assumed to be a printf()-like format and is treated
    # appropriately.  Luckily enough, "%s" produces the same output
    # from both printf() and time2str() functions ("%s" denotes number
    # of seconds since the epoch - the same value stored in the string
    # and interpolated as such by perl when doing sprintf("%s", $str)).
    #
    # To explicitly indicate a printf()-like format string, the marker
    # "%P" can be embedded anywhere in the string.  This is then 
    # ignored in the format process.  e.g. "%P%4.2f", 12.3 => "12.30"
    #
    if (defined($directive->{ FORMAT })) {
	my $format  = $directive->{ FORMAT };
	my $fmtdate = ($format =~ /%[^s]/); # use time2str()?

	# does the format include '%P' to request printf()?
	$fmtdate = 0 if ($fmtdate && ($format =~ s/%P//g));

	my $safefmt; # protect '%s' from printf in _DEBUG()
	($safefmt = $format) =~ s/%/%%/g;  

	$self->_DEBUG(DBGPOST, "format: $safefmt\n");

	# unescape quotes, newlines and tabs
	$format =~ s/\\"/"/g;
	$format =~ s/\\n/\n/g;
	$format =~ s/\\t/\t/g;

	foreach (@lines) {
	    $pre = $_;
	    $_ = $fmtdate 
		? time2str($format, $_)
		: sprintf($format, $_);
	    $post = $_;

	    if ($self->{ DEBUGLEVEL } & DBGPOST) {
		$self->_DEBUG(DBGDATA, 
			"format: [ $pre ]\n     -> [ $post ]\n");
	    }
	}
    }

    # reconstruct all lines back into a single string
    join("\n", @lines);
}



#========================================================================
#
# _dump_symbol($symbol)
#
# Dumps the contents of the symbol table entry indexed by $symbol using
# the _DEBUG function.  The output is processed to be easily readable.
#
#========================================================================

sub _dump_symbol {
    my $self   = shift;
    my $symbol = shift;
    my $copy;


    $self->_DEBUG(DBGCONT, "-- Pre-processed symbol: $symbol %s\n",
	    '-' x (72 - 26 - length($symbol)));

    foreach (@{ $self->{ SYMTABLE }->{ $symbol } }) {

	# is this a directive?
	ref($_) eq $DIRECTIVE && do {
	    $self->_DEBUG(DBGCONT, "%s %s %s %s\n",
			    $self->{ MAGIC }->[0],
			    $_->{ KEYWORD }, 
			    $_->{ IDENTIFIER } || "<none>",
			    $self->{ MAGIC }->[1]);
	    next;
	};

	# take a copy of the line and convert CR to visible \\n's
	($copy = $_) =~ s/\n/\\n/gm;

	map { $self->_DEBUG(DBGCONT, "[ $_ ]\n"); } split(/\n/, $copy);
    }

    $self->_DEBUG(DBGCONT, "%s\n", '-' x 72);
}



#========================================================================
#
# _warn($message, @params)
#
# Prints the specified warning message(s) using the warning function 
# specified in $self->{ ERROR } or "printf STDERR", if not specified.
#
#========================================================================

sub _warn {
    my $self = shift;

    return &{ $self->{ ERROR } }(@_) if defined($self->{ ERROR });

    printf(STDERR @_);
}



#========================================================================
#
# _DEBUG($level, $message, @params)
#
# If ($self->{ DEBUGLEVEL } & $level) equate trues, the specified message
# is printed using the debug function defined in $self->{ DEBUGFUNC }.
# If no debug function is defined, the ($message, @params) are formatted
# as per printf(3) and printed to STDERR, prefixing each line with "D> ".
#
#========================================================================

sub _DEBUG {
    my $self  = shift;
    my $level = shift;
    my $output;

    return unless (($self->{ DEBUGLEVEL } & $level) == $level);

    return &{ $self->{ DEBUG } }(@_) if defined($self->{ DEBUG });

    # sprintf expects a scalar first, so "sprintf(@_)" doesn't work
    $output = sprintf(shift, @_);

    # prefix each line with "D> " and print to STDERR
    $output =~ s/^/D> /mg;
    print STDERR $output;
}



#========================================================================
#
# AUTOLOAD
#
# The AUTLOAD method is called when an object method can't be resolved.
# One known instance when this happens is when an unresolved variable is
# encountered in a SUBST.  If the EXECUTE configuration option is set 
# true, the processs() method attempts to interpret the variable as a 
# method name and call it.  If no such method exists, the AUTOLOAD method
# will step in.  If EXECUTE is set to a value > 1, the AUTOLOAD method
# then tries to execute a function in the main package with the same name.
# If no such function exists, the AUTOLOAD will examine the ROGUE option
# to determine the required action for unresolved (ROGUE) variables.
#
# The AUTOLOAD method, or any function it calls, is expected to return 
# the appropriate substitution for the SUBST variable or undef to 
# indicate failure or non-execution (i.e. when EXECUTE is not set > 1).
# 
# Writers of derived classes should take care when overloading the 
# AUTOLOAD method so as not to break the EXECUTE chain.
#
#========================================================================

sub AUTOLOAD {
    my $self    = shift;
    my $fn;


    # ignore destructor
    $AUTOLOAD =~ /::DESTROY$/ && return;

    # remove the "Package::" prefix
    $AUTOLOAD =~ s/.*:://;

    # if EXECUTE is set > 1, we try to run it as a function in the main 
    # package.  We examine the main symbol table to see if the function
    # exists, otherwise we return undef.

    return undef unless $self->{ EXECUTE } > 1;

    # get a function reference from the main symbol table
    local *glob = $main::{ $AUTOLOAD };
    return undef 
	unless defined($fn = *glob{ CODE });

    $self->_DEBUG(DBGINFO, "AUTOLOAD executing main::$AUTOLOAD\n");

    # execute the function and implicitly return result
    &{ $fn }(@_);
}




1;
__END__

=head1 NAME

Text::MetaText - Perl extension implementing meta-language for processing 
"template" text files.

=head1 SYNOPSIS

    use Text::MetaText;
    my $mt = new Text::MetaText;
    print $mt->process($filename, \%vardefs);

=head1 SUMMARY OF METATEXT DIRECTIVES

    %% DEFINE variable=value %%   # define variable(s)

    %% SUBST variable  %%         # insert variable value
    %% variable %%                # short form of above

    %% BLOCK blockname %%         # define a block 'blockname'
       block text... 
    %% ENDBLOCK %%

    %% INCLUDE blockname %%       # include 'blockname' block text
    %% INCLUDE filename  %%       # include external file 'filename'

    %% INCLUDE file_or_block      # a more complete example...
       variable=value             # additional variable definition(s)
       if=condition               # conditional inclusion
       format=format_string       # printf-like format string with '%s'
       filter=fltname(params)     # post-process filter 
    %%

    %% TIME                       # current system time, as per time(2)
       format=format_string       # display format, as per strftime(3C) 
    %%

=head1 DESCRIPTION

MetaText is a text processing and markup meta-language which can be used for
processing "template" files.  This module is a Perl 5 extension implementing 
a MetaText object class which processes text files, interpreting and acting 
on the embedded MetaText directives within.

Like a glorified pre-processor, MetaText can; include files, define and 
substitute variable values, execute conditional actions based on variables,
call other perl functions or object methods and capture the resulting output 
back into the document, and more.  It can format the resulting output of any 
of these operations in a number of ways.  The objects, and inherently, the 
format and symantics of the MetaText langauge itself, are highly configurable.

MetaText was originally designed to aid in the creation of html documents in 
a large web site.  It remains well suited for this and similar tasks, being 
able to create web pages (dynamically or statically) that are consistent
with each other, yet easily customisable:

=over 4

=item *

standard headers, footers and other elements can be defined in separate 
files and then inserted into web documents:

    %% INCLUDE header %%

=item *

variables can be defined externally or from within a document, then can 
be substituted back into the text.  This is useful for including your 
B<%% name %%> or B<%% email %%> address or any other variable, and for 
encoding URL's or file paths that can then be changed en masse.  e.g.

    <img src="%% imgroot %%/foo/bar.gif">

=item *

conditional actions can be made based on variable definitions,
allowing easily and instantly customisable web pages. e.g

    %% INCLUDE higraphics/header if="higfx && userid != abw" %%

=item *

blocks of text can be internally defined simplifying the creation of
repetitive elements.  e.g.

    %% BLOCK table_row %%
    <tr> <td>%% userid %%</td> <td>%% name %%</td> </tr>
    %% ENDBLOCK %%

    %% INCLUDE table_row userid=lwall  name="Larry Wall"         %%
    %% INCLUDE table_row userid=tomc   name="Tom Christiansen"   %%
    %% INCLUDE table_row userid=merlyn name="Randal L. Schwartz" %%

=item *

in addition, the B<metapage> utility is a script which can automatically
traverse document trees, processing updated files to assist in web 
document management and other similar tasks.

=back

=head1 PREREQUISITES

MetaText requires Perl 5.004 or later.  The Date::Format module should
also be installed.  This is available from CPAN (in the "TimeDate"
distribution) as described in the following section.

=head1 OBTAINING AND INSTALLING THE METATEXT MODULE

The MetaText module is available from CPAN.  As the 'perlmod' man
page explains:

    CPAN stands for the Comprehensive Perl Archive Network.
    This is a globally replicated collection of all known Perl
    materials, including hundreds of unbunded modules.  

    [...]

    For an up-to-date listing of CPAN sites, see
    http://www.perl.com/perl/ or ftp://ftp.perl.com/perl/ .

Within the CPAN archive, MetaText is in the "Text::" group which forms 
part of the the category:

  *) String Processing, Language Text Processing, 
     Parsing and Searching

The module is available in the following directories:

    /modules/by-module/Text/Text-MetaText-<version>.tar.gz
    /authors/id/ABW/Text-MetaText-<version>.tar.gz

For the latest information on MetaText or to download the latest 
pre-release/beta version of the module, consult the definitive 
reference, the MetaText Home Page:

    http://www.kfs.org/~abw/perl/metatext/

MetaText is distributed as a single gzipped tar archive file:

    Text-MetaText-<version>.tar.gz

Note that "<version>" represents the current MetaText Revision number, 
of the form "0.14".  See L<REVISION> below to determine the current 
version number for Text::MetaText.

Unpack the archive to create a MetaText installation directory:

    gunzip Text-MetaText-<version>.tar.gz
    tar xvf Text-MetaText-<version>.tar

'cd' into that directory, make, test and install the MetaText module:

    cd Text-MetaText-<version>
    perl Makefile.PL
    make
    make test
    make install

The 't' sub-directory contains a number of small sample files which are 
processed by the test script (called by 'make test').  See the README file 
in that directory for more information.  A logfile (test.log) is generated 
to report any errors that occur during this process.  Please note that the
test suite is incomplete and very much in an 'alpha' state.  Any
further contributions here are welcome.

The 'make install' will install the module on your system.  You may need 
root access to perform this task.  If you install the module in a local 
directory (for example, by executing "perl Makefile.PL LIB=~/lib" in the 
above - see C<perldoc MakeMaker> for full details), you will need to ensure 
that the PERL5LIB environment variable is set to include the location, or 
add a line to your scripts explicitly naming the library location:

    use lib '/local/path/to/lib';

The B<metapage> utility is a script designed to automate MetaText processing 
of files.  It can traverse directory trees, identify modified files (by
comparing the time stamp of the equivalent file in both "source" and 
"destination" directories), process them and direct the resulting 
output to the appropriate file location in the destination tree.  One can 
think of B<metapage> as the MetaText equivalent of the Unix make(1S) utility.

The installation process detailed above should install B<metapage> in your
system's perl 'installbin' directory (try C<perl '-V:installbin'> to check 
this location).  See the B<metapage> documentation (C<perldoc metapage>) 
for more information on configuring and using B<metapage>.

=head1 USING THE METATEXT MODULE

To import and use the MetaText module the following line should appear 
in your Perl script:

    use Text::MetaText;

MetaText is implemented using object-oriented methods.  A new MetaText 
object is created and initialised using the Text::MetaText->new() method.  
This returns a reference to a new MetaText object.

    my $mt = Text::MetaText->new;

A number of configuration options can be specified when creating a 
MetaText object.  A reference to a hash array of options and
their associated values should be passed as a parameter to the 
new() method.

    $my $mt = Text::MetaText->new( { 'opt1' => 'val1', 'opt2' => 'val2' } );

The configurations options available are described in full below.  All
keywords are treated case-insensitively (i.e. "LIB", "lib" and "Lib" are
all considered equal).

=over

=item LIB

The INCLUDE directive causes the external file specified ("INCLUDE <file>")
to be imported into the current document.  The LIB option specifies 
one or more directories in which the file can be found.  Multiple 
directories should be separated by a colon or comma.  The 
current directory is also searched by default.

    my $mt = Text::MetaText->new( { LIB => "/tmp:/usr/metatext/lib" } );

=item CASE

The default behaviour for MetaText is to treat variable names and 
identifiers case insensitively.   Thus, the following are treated 
identically:

    %% INCLUDE foo %%
    %% INCLUDE FOO %%

Setting the CASE option to any non-zero value causes the document to be 
processed case sensitively.

    my $mt = Text::MetaText->new( { CASE => 1 } ); # case sensitive

Note that the configuration options described in this section are always 
treated case insensitively regardless of the CASE setting.

=item MAGIC

MetaText directives are identifed in the document being processed as
text blocks surrounded by special "magic" identifers.  The default
identifiers are a double percent string, "%%", for both opening and
closing identifiers.  Thus, a typical directive looks like:

    %% INCLUDE some/file %%
    
and may be embedded within other text:

    normal text, blah, blah %% INCLUDE some/file %% more normal text

The MAGIC option allows new identifiers to be defined.  A single
value assigned to MAGIC defines a token to be used for both opening 
and closing identifiers:

    my $mt = Text::MetaText->new( { MAGIC => '++' } );

    ++ INCLUDE file ++

A reference to an array providing two values (elements 0 and 1) indicates
separate tokens to be used for opening and closing identifiers:

    my $mt = Text::MetaText->new( { MAGIC => [ '<!--', '-->' ] } );

    <!-- INCLUDE file -->

=item CHOMP 

When MetaText processes a file it identifies directives and replaces them
with the result of whatever magical process the directive represents 
(e.g. file contents for an INCLUDE, variable value for a SUBST, etc).
Anything outside the directive, including newline characters, are left 
intact.  Where a directive is defined that has no corresponding output
(DEFINE, for example, which silently sets a variable value), the trailing
newline characters can leave large tracts of blank lines in the output 
documents.

For example:

  line 1
  %% DEFINE f="foo" %%
  %% DEFINE b="bar" %%
  line 2 

Produces the following output:

  line 1


  line 2

This happens because the newline characters at the end of the 
second and third lines are left intact in the output text.

Setting CHOMP to any true value will remove any newline characters that
appear B<immediately after> a MetaText directive.  Any characters 
coming between the directive and the newline, including whitespace, will
override this behaviour and cause the intervening characters and newline
to be output intact.

With CHOMP set, the following example demonstrates the behaviour:

  line 1
  %% DEFINE f="foo" %%
  %% DEFINE b="bar" %%<space>
  line 2

Produces the following output (Note that "E<lt>spaceE<gt>" is intended to 
represent a single space character, not the string "E<lt>spaceE<gt>" itself, 
although the effect would be identical):

  line 1
  <space>
  line 2

=item FILTER

There may be times when you may want to INCLUDE a file or element in a 
document but want to filter the contents in some way.  You may wish
to escape (i.e. prefix with a backslash '\') certain characters such
as quotes, search for certain text and replace with an alternative
phrase, or perform some other post-processing task.  The FILTER option
allows you to define one or more code blocks that can be called as filter
functions from an INCLUDE directive.  Each code block is given a unique
name to identify it and may have calling parameters (parenthesised and 
separated by commas) that can be specified as part of the directive.  
e.g.

    %% INCLUDE foo filter="slurp(prm1, prm2, ...)" %%

Two default filters are pre-defined: escape() and sr().  escape() takes
as a parameter a perl-like regular expression pattern that indicates 
characters that should be 'escaped' (i.e. prefixed by a backslash '\') in the 
text.  For example, to escape any of the character class C<["'\]> you would 
specify the filter as:

    %% INCLUDE foo filter="escape([\"'\\])" %%

The second filter, sr(), takes two arguments, a search string and a 
replace string.  A simple substitution is made on the included text.
e.g.

    %% INCLUDE foo filter="sr(spam, \"processed meat\")" %%

Note that quotes and other special metacharacters should be escaped
within the filter string as shown in the two examples above.

Additional filters can be specified by passing a reference to a hash 
array that contains the name of the filter and the code itself in 
each key/value pair.  Your filter function should be designed to accept
the name of the function as the first parameter, followed by a line of
text to be processed.  Any additional parameters specified in the INCLUDE 
directive follow.  The filter function is called for each line of an 
INCLUDE block and should return the modified text.  

Example:

    my $mt = Text::MetaText->new( { 
        FILTER => {
            'xyzzy' => sub { 
                 my ($filtername, $text, @params) = @_;
                 $text = # do something here...
		 $text;  # return modified text
            }
        }
    } );

    %% INCLUDE file1 filter="xyzzy(...)" %%

A new FILTER definition will replace any existing filter with the same name.

=item EXECUTE

The SUBST directive performs a simple substitution for the value of the 
named variable.  In the example shown below, the entire directive, including 
the surrounding 'magic' tokens '%%', is replaced with the value of the 
variable 'foo':

    %% SUBST foo %%  (or more succinctly, %% foo %%)

If the named variable has not been defined, MetaText can interpret the 
variable as the name of an object method in the current class or as a 
function in the main package.

If the EXECUTE flag is set to any true value, the MetaText processor will 
interpret the variable as an object method and attempt to apply it to its
own object instance (i.e. $self->$method(...)).  If the method is not 
defined, the processor fails quietly (but see ROGUE below to see what can 
happen next).  This allows classes to be derived from MetaText
that implement methods that can be called (when EXECUTE == 1) as follows:

    %% method1 ... %%       # calls $self->method1(...);
    %% method2 ... %%       # calls $self->method2(...);

The text returned from the method is used as a replacement value for the 
directive.

The following pseudo-code example demonstrates this:

    package MyMetaText;
    @ISA = qw( Text::MetaText );

    sub foo { "This is method 'foo'" }  # simple return string
    sub bar { "This is method 'bar'" }  # "        "         "

    package main;

    my $mt = MyMetaText->new( { EXECUTE => 1 } );
    print $mt->process("myfile");

which, for the file 'myfile':

    %% foo %%
    %% bar %%

generates the following output:

    This is method 'foo'
    This is method 'bar'

If the EXECUTE flag is set to a value E<gt> 1 and the variable name does not 
correspond to a class method, the processor tries to interpret the 
variable as a function in the main package.  Like the example above, 
the processor fails silently if the function is not defined (but see 
ROGUE below).

The following pseudo-code extract demonstrates this:

    my $mt = Text::MetaText->new( { EXECUTE => 2 } );
    print $mt->processs("myfile");

    sub foo { "This is function 'foo'" }  # simple return string
    sub bar { "This is fucntion 'bar'" }  # "        "         "
	
which, for the file 'myfile':

    %% foo %%
    %% bar %%

generates the following output:

    This is function 'foo'
    This is function 'bar'

Any additional parameters specified in the directive are passed to the 
class method or function as a hash array reference.  The original parameter
string is also passed.  Note that the first parameter passed to class 
methods is the MetaText (or derivative) object reference itself.

Example:

    %% foo name="Seuss" title="Dr" %%

causes the equivalent of (when EXECUTE is any true value):

    $self->foo(                                  # implicit $self ref
	{ 'name' => 'Seuss', 'title' => 'Dr' },  # hash ref of params
	  'name="Seuss" title="Dr"' );           # parameter string

and/or (when EXECUTE > 1):

    &main::foo(
	{ 'name' => 'Seuss', 'title' => 'Dr' },  # hash ref of params
	  'name="Seuss" title="Dr"' );           # parameter string


=item ROGUE

This configuration item determines how MetaText behaves when it encounters
a directive it does not recognise.  The ROGUE option may contain one or
more of the ROGUE keywords separated by any non-word character.  The 
keywords and their associated meanings are:

    warn    Issue a warning (via the ERROR function, if 
            specified) when the directive is encountered.

    delete  Delete any unrecognised directives.

The default behaviour is to silently leave any unrecognised directive
in the processed text.

Example:

    my $mt = Text::MetaText->new( { ROGUE => "delete,warn" } );

=item DELIMITER

The DELIMITER item specifies the character or character sequence that 
is used to delimit lists of data.  This is used, for example, by the "in"
operator which can be used in evaluation conditions.  e.g.

    %% INCLUDE hardenuf if="uid in abw,wrigley" %%

In this case, the condition evaluates true if the uid variable contains the 
value "abw" or "wrigley".  The default delimiter character is a comma.

The example:

    my $mt = Text::MetaText->new( { DELIMITER => ":" } );

would thus correctly process:

    %% INCLUDE hardenuf if="uid in abw:wrigley" %%

=item ERROR

The ERROR configuration item allows an alternative error reporting function 
to be specified for error handling.  The function should expect a printf()
like calling convention.

Example:

    my $mt = Text::MetaText->new( { 
        ERROR => sub {
            my ($format, @params) = @_;
            printf(STDERR "ERROR: $format", @params);
        }
    } );


=item DEBUG

The DEBUG item allows an alternative debug function to be provided.  The
function should expect a printf() like calling convention, as per the 
ERROR option described above.  The default DEBUG function sends debug 
messages to STDERR, prefixed by a debug string: 'DE<gt> '.

=item DEBUGLEVEL

The DEBUGLEVEL item specifies which, if any, of the debug messages are
displayed during the operation of the MetaText object.  Like the ROGUE
option described above, the DEBUGLEVEL value should be constructed from
one or more of the following keywords:

    none      no debugging information (default)
    info      general processing information
    config    MetaText object configuration items
    preproc   pre-processing phase
    process   processing phase
    postproc  post-processing phase
    data      additional data parameters in debug messages
    content   content of pre-processed INCLUDE blocks
    function  list functions calls as executed
    evaluate  trace conditional evaluations
    test      used for any temporary test code
    all       all of the above (excluding "none", obviously)

Example:

    my $mt = Text::MetaText->new( { 
	DEBUGLEVEL => "preproc,process,data" 
    } );

=item MAXDEPTH

It is possible for MetaText to become stuck in an endless loop if a 
circular dependancy exists between one or more files.  For example:

    foo:
        %% INCLUDE bar %%

    bar:
        %% INCLUDE foo %%

To detect and avoid such conditions, MetaText allows files to be 
nested up to MAXDEPTH times.  By default, this value is 32.  If you 
are processing a file which has nested INCLUDE directives to a depth greater 
than 32 and MetaText returns with a "Maximum recursion exceeded" warning, 
set this confiuration item to a higher value.  e.g.

    my $mt = Text::MetaText->new( { MAXDEPTH => 42 } );

=back 

=head1 PROCESSING TEXT

The MetaText process() method is called to process a file, 
interpreting any MetaText directives embedded within it.  The first
parameter should be the name of the file which  should reside in the
current working directory or in one of the directories specified in
the LIB configuration option.  A filename starting with a slash '/'
is considered to be an absolute path.  The optional second parameter 
may be a reference to a hash array containing a number of variable/value
definitions that should be pre-defined when processing the file.

    print $mt->process("somefile", { name => "Fred" });

If "somefile" contains:

    Hello %% name %%

then the output generated would be:

    Hello Fred

Pre-defining variables in this way is equivalent to using the DEFINE
directive (described below) at the start of the INCLUDE file

    %% DEFINE name="Fred" %%
    Hello %% name %%

The process() function will continue until it reaches the end of the file 
or a line containing the pattern "__END__" by itself ("END" enclosed by 
double underscores, no other characters or whitespace on the line).  
Note that the pre-processor (a private method which is called by process(), 
so feel free to forget all about it) I<does> scan past any __END__ marker.  
In practice, that means you can define blocks I<after>, but use them 
I<before>, the __END__ marker. e.g.

    Martin, %% INCLUDE taunt %%

    __END__                 << processor stops here and ignores 
                               everything following
    %% BLOCK taunt %%       << but the pre-processor has correctly 
    you Camper!                continued and parsed this block so that
    %% ENDBLOCK %%             it can be included in the main body

produces the output:

    Martin, you Camper!

The process() function returns a string containing the processed 
file or block output.

    my $output = $mt->process("myfile");
    print $output;

=head1 METATEXT DIRECTIVES

A MetaText directive is a block of text in a file that is enclosed
by the MAGIC identifiers (by default '%%').  A directive may span 
multiple lines and may include blank lines within in.  Whitespace
within a directive is generally ignored except where quoted as part
of a specific value.

    %% DEFINE
       name    = Yorick
       age     = 30
       comment = "A fellow of infinite jest"
    %%

The first word of the directive indicates the directive type.  Directives
may be specified in upper, lower or mixed case, irrespective of the CASE
sensitivity flag (which affects only variable names).  The general 
convention is to specify the directive type in UPPER CASE to aid clarity.  

The MetaText directives are: 

=over

=item DEFINE

Define the values for one or more variables 

=item SUBST

Substitute the value of a named variable

=item INCLUDE

Process and include the contents of the named file or block

=item BLOCK

Define a named block which can be subsequently INCLUDE'd

=item ENDBLOCK

Marks the end of a BLOCK definition

=back

To improve clarity and reduce excessive, unnecessary and altogether
undesirable verbosity, a directive block that doesn't start with a 
recognised MetaText directive is assumed to be a 'SUBST' variable 
substitution.  Thus,

    %% SUBST foo %%

can be written more succinctly as 

    %% foo %%

When MetaText processes directives, it is effectively performing a 
"search and replace".  The MetaText directive block is replaced with 
whatever text is appropriate for the directive specified.  Generally 
speaking, MetaText does not alter any text content or formatting outside of
directive blocks.  The only exception to this rule is when CHOMP is 
turned on (see L<USING THE METATEXT MODULE>) and newlines
immediately following a directive are subsequently deleted.

=head2 DEFINE 

The DEFINE directive allows simple variables to be assigned values.  
Multiple variables may be defined in a single DEFINE directive.

    %% DEFINE 
       name  = Caliban
       quote = "that, when I waked, I cried to dream again."
    %%

Variables defined within a file or passed to the process() function 
as a hash array remain defined until the file or block is processed
in entirety.  Variable values will be inherited by any nested files or 
blocks INCLUDE'd into the file.  Re-definitions of existing variables will 
persist within the file or block, masking any existing values, until the end
of the file or block when the previous values will be restored.

The following example illustrates this:

    foo:
        Hello %% name %%              # name assumes any predefined value
        %% DEFINE name=tom %%
	Hello %% name %%              # name = 'tom'
        %% INCLUDE bar name='dick' %% # name = 'dick' for "INCLUDE bar"
	Hello %% name %%              # name = 'tom'

    bar:
	Hello %% name %%              # name = 'dick'
        %% DEFINE name='harry' %%     # name = 'harry'
        Hello %% name %%

Processing the file 'foo' as follows:

    print $mt->process('foo', { 'name' => 'nobody' });

produces the following output (with explanatory comments added for clarity):

    Hello nobody                      # value from process() hash 
    Hello tom                         # from foo
    Hello dick                        # from bar
    Hello harry                       # re-defined in bar
    Hello tom                         # restored to previous value in foo

=head2 SUBST

A SUBST directive performs a simple variable substitution.  If the variable
is defined, its value will be inserted in place of the directive.  

Example:

    %% DEFINE name=Jake %%
    Hello %% SUBST name %%  

generates the following output:

    Hello Jake

The SUBST keyword can be omitted for brevity.  Thus "%% name %%" is
processed identically to "%% SUBST name %%".

If the variable is undefined, the MetaText processor will, according to the 
value of the EXECUTE configuration value, try to execute a class method or a 
function in the main package with the same name as the SUBST variable.  If 
EXECUTE is set to any true value, the processor will try to make a 
corresponding method call for the current object (that is, the current 
instantiation of the MetaText or derived class).  If no such method exists
and EXECUTE is set to any value greater than 1, the processor will then try 
to execute a function in the main package with the same name as the SUBST 
variable  In either case, the text returned from the method or function is 
included into the current block in place of the SUBST directive (non-text 
values are automatically coerced to text strings).  If neither a variable, 
method or function exists, the SUBST directive will either be deleted or 
left intact (and additionally, a warning may be issued), depending on the 
value of the ROGUE configuration item.

See EXTENDING METATEXT below for more information on deriving MetaText
classes and using EXECUTE to extend the meta-language.

The "format" and "filter" options as described in the INCLUDE section below 
are applied to the processed SUBST result before being inserted back 
into the document.

Some MetaText variables have a special meaning.  Unless specifically
defined otherwise, the variable(s) listed below generate the following
output:

    TIME    The current system time in seconds since the epoch, 
            00:00:00 Jan 1 1970.  Use the "format" option to 
            specify a time/date format.

=head2 INCLUDE

The INCLUDE directive instructs MetaText to load and process the 
contents of the file or block specified.  If the target is a 
file, it should reside in the current directory or a directory specified 
in the LIB configuration variable.  Alternatively, the target may be a 
text block specified with BLOCK..ENDBLOCK directives (see below).

    %% INCLUDE chapter1 %%

The target may also be a variable name and should be prefixed with a '$' to 
identify it as such.  On evaluation, the value of the named variable will be 
used as the target:

Example:

    %% DEFINE chapter=ch1 %%
    %% INCLUDE $chapter   %%  
    
is equivalent to:

    %% INCLUDE ch1 %%

Additional variables may be defined for substitution within the file:

    %% INCLUDE chapter2 bgcolor=#ffffff title="Chapter 2" %%

The contents of the file "chapter2":

    <html><head><title>%%title%%</title></head>
    <body bgcolor="%% bgcolor %">
      ...
    </body>

would produce the output:

    <head><title>Chapter 2</title></head>
    <body bgcolor="#ffffff">
      ...
    </body>

Defining variables in this way is equivalent to using the DEFINE directive.
Variables remain in scope for the lifetime of the file being processed and
then revert to any previously defined values (or undefined).  Any additional
files processed via further INCLUDE directives within the file will also 
inherit any defined variable values.

Example:

      %% INCLUDE file1 name="World" %%

for the files:

    file1:                   # name => "World" from INCLUDE directive
        %% INCLUDE file2 %% 
  
    file2:                   # inherits "name" variable from file1
        %% INCLUDE file3 %%    

    file3:                   # inherits "name" variable from file2
        Hello %% name %%

produces the output:

    Hello World

The output generated by INCLUDE and SUBST directives can be formatted 
using a printf-like template.  The format string should be specified as
a "format" option in the INCLUDE or SUBST directive.  Each line of the 
included text is formatted and concatentated to create the final output.
Within the format string, '%s' is used to represent the text.

For example, the 'author' element below could be used to display details
of the author of the current document.

    author:
        File:   %% file %%
        Author: %% name %%
	Date:   %% date %%

For inclusion in an HTML document, the text can be encapsulated in HTML
comment tags ("<!--" and "-->") using a format string:

    %% INCLUDE author 
       file   = index.html
       name   = "Andy Wardley" 
       date   = 19-Mar-1987
       format = "<!-- %-12s -->" 
    %%

Which produces the following output:

    <!-- File:   index.html   -->
    <!-- Author: Andy Wardley -->
    <!-- Date:   19-Mar-1987  -->

Note that the print format is applied to each line of the included text.  To
encapsulate the element as a whole, simply apply the formatting outside of
the INCLUDE directive:

    <!--
       %% INCLUDE author
       ...
       %%
    -->

In these examples, the formatting is applied as if the replacement value/line 
is a character string.  Any of the standard printf(3) format tokens can be 
used to coerce the value into a specific type.

As mentioned in the SUBST section above, the TIME variable is used to
represent the current system time in seconds since the epoch (see time(2)).  
The "format" option can also be employed to represent such values in a more
user-friendly format.  Any format string that does not contain a '%s' 
token is assumed to be a time-based value and is formatted using the 
time2str() function from the Date::Format module (distributed as part
of the TimeDate package).  

Example:

    The date is %% TIME format="%d-%b-%y" %%

Generates:

    The date is 19-Mar-98

See C<perldoc Date::Format> for information on the formatting characters
available.

The pragmatic token '%P' can be added to a format to override this behaviour 
and force the use of printf().  The '%P' token is otherwise ignored.

Example:

    %% DEFINE foo=123456789  %%
    %% foo format="%d-%b-%y" %%  # "day-month-year" using time2str
    %% foo format="%d"       %%  # "day" using timestr
    %% foo format="%P%d"     %%  # decimal value using printf
    %% foo format="%s"       %%  # string value using printf
 
Generates:

    29-Nov-73
    29
    123456789
    123456789

Text that is inserted with an INCLUDE or SUBST directive can also be filtered.
There are two default filters provided, 'escape' which can be used to escape
(prefix with a backslash '\') certain characters, and 'sr' which is used to
perform simple search and replace actions.  Other filters may be added with
the FILTER option when creating the object (see the FILTER section in 
L<USING THE METATEXT MODULE>, above).

Like the 'format' option, output filters work on a line of text at a time.
Any parameters required for the filter can be specified in parentheses after
the filter name.  The 'escape' filter expects a perl-style character class 
indicating the characters to escape.  The 'sr' filter expects two parameters, 
a search pattern and a replacement string, separated by a comma.  Note that 
parameters that include embedded spaces should be quoted.  The quote 
characters themselves must also be escaped as they already form part of a 
quoted string (the filter text).  (This way of representing parameters is
admittedly far from ideal and may be improved in a future version.)

Example:

    %% DEFINE text="Madam I'm Adam" %%
    %% SUBST  text filter="escape(['])"               %%
    %% SUBST  text filter="sr(Adam, \"Frank Bough\")" %%

Generates:

    Madam I\'m Adam
    Madam I'm Frank Bough

Conditional tests can be applied to INCLUDE blocks to determine if the 
block should evaluated or ignored.  Variables and absolute values can be 
used and can be evaluated in the following ways:

    a == b       # a is equal to b
    a != b       # a is not equal to b
    a >  b       # a is greater than b
    a <  b       # a is less than b
    a => b       # a is greater than or equal to b
    a <= b       # a is less than or equal to b
    a =~ b       # a matches the perl regex pattern b
    a !~ b       # a does not match the perl regex pattern b
    a in b,c,d   # a appears in the list b, c, d (see DELIMITER)

The items on the right of the evaluations can be absolute values or 
variable names which should be prefixed by a '$'.  The items on the left 
of the evaluation are assumed to be variable names.  There is no need to
prefix these with a '$', but you can if you choose.  

The single equality, "a = b", is treated identically to a double equality
"a == b" although the two traditionally represent different things (the 
first, an assignment, the second, a comparison).  In this context, I consider 
the former usage confusing and would recommend use of the latter at all times.

Variables without any comparison operator or operand are tested for a 
true/false value.

Examples:

    %% INCLUDE foo if="name==fred"        %%
    %% INCLUDE foo if="$name==fred"       %%  # equivalent to above
    %% INCLUDE foo if="name==$goodguy"    %%
    %% INCLUDE foo if="hour > 10"         %%
    %% INCLUDE foo if="tonk =~ [Ss]pl?at" %%
    %% INCLUDE foo if="camper"            %%

Multiple conditions can be joined using the following boolean operators

    a && b       # condition 'a' and 'b' 
    a || b       # condition 'a' or  'b' 
    a ^  b       # condition 'a' xor 'b'
    a and b      # same as "a && b" but with lower precedence
    a or  b      # same as "a || b" but with lower precedence
    a xor b      # same as "a ^  b" but with lower precedence

Conditional equations are evaluated left to right and may include parentheses
to explicitly set precedence.

Examples:

    %% INCLUDE tonk     
       if="hardenuf && uid in abw,wrigley"           
    %%
    %% INCLUDE tapestry 
       if="(girly && studly < 1) || uid == neilb"    
    %%
    %% INCLUDE tapestry 
       if="($girly && $studly < 1) || $uid == neilb" 
    %%

Note that the third example above is identical in meaning to the second, 
but explicitly prefixes variable names with '$'.  This is optional for
elements on the left hand side of comparison operators, but mandatory
for those on the right that might otherwise be interpreted as absolute
values.

=head2 BLOCK..ENDBLOCK

In some cases it is desirable to have a block of text available to be
inserted via INCLUDE without having to define it in an external file.  The
BLOCK..ENDBLOCK directives allow this.

A BLOCK directive with a unique identifier marks the start of a 
block definition.  The block continues, including any valid MetaText
directives, until an ENDBLOCK directive is found.  

A BLOCK..ENDBLOCK definition may appear anywhere in the file.  It is
in fact possible to INCLUDE the block before it has been defined as 
long as the block definition resides in the same file.

Processing of a file stops when it encounters the __END__ marker on a 
line by itself.  Blocks can be defined after this marker even though 
the contents of the file after __END__ are ignored by the processor.

    # include a block defined later
    %% INCLUDE greeting name=Prospero %%

    __END__
    %% BLOCK greeting %%
    Hello %% name %%
    %% ENDBLOCK %%

This produces the following output:

    # include a block defined later
    Hello Prospero

Additional variable definitions specified in an INCLUDE directive will be
applied to blocks just as they would to external files.

A BLOCK..ENDBLOCK definition that appears in the main part of a document
(i.e. before, or in the absence of an __END__ line) will not appear in 
the processed output.  A simple "print" flag added to the BLOCK directive
overrides this behaviour, causing a copy of the BLOCK to appear in it's 
place:

    %% DEFINE name=Caliban %%

    %% BLOCK greeting print %%
    Hello %% name %%
    %% ENDBLOCK %%

    %% INCLUDE greeting name="Prospero" %%

produces the following output:

    Hello Caliban

    Hello Prospero

Conditions ("if" and "unless") can be applied to BLOCK directives, but
they affect how and when the BLOCK itself is printed, rather than 
determining if the block gets defined or not.  Conditionals 
have no effect on BLOCK directives that do not include a "print" flag.  

=head1 EXTENDING METATEXT

MetaText may be used as a base class for deriving other text processing
modules.  Any member function of a derived class can be called directly
as a MetaText directive.  See the EXECUTE configuration option for more
details.

Pseudo-code example:

    package MyMetaText;
    @ISA = qw( Text::MetaText );

    # define a new derived class method, get_name()
    sub get_name {
        my $self   = shift;
        my $params = shift;

        # return name from an ID hash, for example
	$self->{ PEOPLE }->{ $params->{'id'} } || 'nobody';
    }

    package main;

    # use the new derived class
    my $mmt = MyMetaText { EXECUTE => 1 };

    # process 'myfile'
    print $mmt->process('myfile');

which, for a sample file, 'myfile':

    %% get_name id=foo %%
    %% get_name id=bar %%

is equivalent to:

    print $mmt->get_name({ 'id' => 'foo' }), "\n";
    print $mmt->get_name({ 'id' => 'bar' }), "\n";

Alternatively, a simple calling script can be written that defines
functions that themselves can be called from within a document:

    my $mt = Text::MetaText->new( { EXECUTE => 2 } );

    print $mt->process("myfile");

    sub get_name {
        my $params = shift;
        $global_people->{ $params->{'id'} } || 'nobody';
    }

Please note that the functionality provided by the EXECUTE option, and 
inherently, the extensibility possible by deriving MetaText sub-classes, 
relies in part on the operation of the AUTOLOAD method.  Authors of derived
MetaText classes should be aware of, and account for this, if re-defining 
the AUTOLOAD method.

=head1 WARNINGS AND ERRORS 

The following list indicates warning or error messages that MetaText can
generate and their associated meanings.

=over 4

=item "Closing directive tag missing in %s"

A MetaText directive was found that was not terminated before the end 
of the file.  e.g. C<%% INCLUDE something ...>  The processor attempts
to compensate, but check your source files and add any missing MAGIC
tokens.

=item "Invalid configuration parameter: %s"

An invalid configuration parameter was identified in the hash array 
passed to Text::MetaText->new().  See L<USING THE METATEXT MODULE>.

=item "Invalid debug/error function"

The debug or error handling routine specified for the ERROR or DEBUG
configuration options was not a code reference.  See the ERROR and/or
DEBUG sections for more details.

=item "Invalid debug option: %s"

A token was specified for the DEBUGLEVEL configuration item which was 
invalid.  See the DEBUGLEVEL section for a complete list of valid tokens.

=item "Invalid roque option: %s" 

A token was specified for the ROGUE configuration item which was 
invalid.  See the ROGUE section for a complete list of valid tokens.

=item "Maximum recursion exceeded"

The processed file had multiple INCLUDE directives that nested to a
depth greater than MAXDEPTH (default: 32).  Set MAXDEPTH higher to 
avoid this problem, or check your files for circular dependencies.

=item "Missing directive keyword"

A MetaText directive was identified that had no keyword or other content.
e.g. C<%%    %%>

=item "Text::MetaText->new expects a hash array reference"

The new() method can accept a reference to a hash array as the first
parameter which contains configuration variables and values.  This 
error is generated if the parameter is not a hash array reference.

=item "Unrecognise directive: %s"

An internal error that should never happen.  The pre-processor has 
identified a directive type that the processor then failed to recognise.

=item "Unrecognised token: %s"

A C<%% SUBST E<lt>variableE<gt> %%> or C<%% E<lt>variableE<gt> %%> 
directive was found for which there was no corresponding E<lt>variableE<gt>
defined.  This warning is only generated when the 'warn' token is set
for the ROGUE option.

=item "Unmatched parenthesis: %s"

A conditional evaluation ("if" or "unless") for a directive is missing
a closing parenthesis.  
e.g. C<%% INCLUDE foobar if="(foo && bar || baz" %%>

=item "%s: non-existant or invalid filter"

An INCLUDE or SUBST directive included a "filter" option that refers
to a non-existant filter.  e.g. C<%% INCLUDE foo filter=nosuchfilter() %%>

=back

=head1 AUTHOR

Andy Wardley E<lt>abw@kfs.orgE<gt>

See also:

    http://www.kfs.org/~abw/
    http://www.kfs.org/~abw/perl/metatext/

My thanks extend to the people who have used and tested MetaText.
In particular, the members of the Peritas Online team; Simon Matthews, 
Simon Millns and Gareth Scott; who brutally tested the software over a 
period of many months and provided valuable feedback, ideas and of course, 
bug reports.  I am also indebted to the members of the SAS Team at Canon 
Research Centre Europe Ltd; Tim O'Donoghue, Neil Bowers, Ave Wrigley, Martin
Portman, Channing Walton and Gareth Rees; although I'm not yet sure why.  :-)

I welcome bug reports, enhancement suggestions, comments, criticisms 
(hopefully constructive) and patches related to MetaText.  I would 
appreciate hearing from you if you find MetaText particularly useful or
indeed if it I<doesn't> do what you want, for whatever reason.  Hopefully
this will help me make MetaText help you more.

It pains me to say that MetaText comes without guarantee or warranty of
suitability for any purpose whatsoever.  That doesn't mean it doesn't do
anything good, but just that I don't want some scrupulous old git to sue me 
because they thought I implied it did something it doesn't.  I<E<lt>sighE<gt>>

=head1 REVISION

$Revision: 0.15 $

=head1 COPYRIGHT

Copyright (c) 1996-1998 Andy Wardley.  All Rights Reserved.

This program is free software; you can redistribute it and/or 
modify it under the same terms as Perl itself.

=head1 SEE ALSO

The B<metapage> utility, the Date::Format module.

=cut


