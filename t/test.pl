#!/usr/bin/perl -w
#
# test.pl: generic test functions used by test scripts
#

use Date::Format;

my $TESTDIR = 't';
my $SRCDIR  = "$TESTDIR/src";
my $DESTDIR = "$TESTDIR/dest";
my $EXPDIR  = "$TESTDIR/expect";
my $LOGFILE = "$TESTDIR/test.log";
my $ERROR   = "";
my $DEBUG   = 0;



sub init {
    unless (-d $DESTDIR) {
	mkdir $DESTDIR, 0755 || die "$DESTDIR: $!\n";
    }
    &log_init();
}


#
# test_file($ntest, $mt, $file, $defs)
#
# Simply calls process() and compare() in turn.  Prints "ok $ntest" on 
# success or "not ok $ntest" on error;
#

sub test_file {
    my $ntest = shift;
    my $mt    = shift;
    my $file  = shift;
    my $defs  = shift || {};
    my $score;
    
    ($score = process($mt, $file, $defs)) == 0 and $score = compare($file);
    print $score ? "not ok $ntest\n" : "ok $ntest\n";
   
    &log_entry("$file: %s\n", $score ? "FAILED - $ERROR" : "ok");

    print STDERR $score ? "not ok $ntest [$ERROR]\n" : "ok $ntest\n" if $DEBUG;
}



#
# process($mt, $file, $defs)
#
# Process the file "src/$file" using the MetaText object, $mt, adding any
# definitions in the hash array reference, $defs.  The output is written
# to a corresponding file in the "dest" directory.
#
# Returns 0 on success.  -1 is returned on error and $ERROR is set to contain
# an appropriate error message.
#

sub process {
    my $mt   = shift;
    my $file = shift;
    my $vars = shift || {};
    local (*OUTPUT);

    $ERROR = '';

    # process file 
    my @output = $mt->process("$SRCDIR/$file", $vars);

    # spit out processed text 
    open(OUTPUT, "> $DESTDIR/$file") || do {
	$ERROR = "$DESTDIR/$file: $!";
	return -1;
    };
    print OUTPUT join("", @output);
    close(OUTPUT);

    0;
}



#
# compare($file)
#
# Compares the file ./expected/$file against ./dest/$file, returning 0
# if they are identical or -1 if not.  $ERROR is set with an appropriate
# message if the files are not identical or another error occurs.
#

sub compare {
    my $file     = shift;
    my $expfile  = "$EXPDIR/$file";
    my $destfile = "$DESTDIR/$file";
    my ($exp, $dest);
    my $ignore = 0;
    local (*EXP, *DEST);

    $ERROR = '';


    # attempt to open files
    foreach ([ $expfile, *EXP ], [ $destfile, *DEST ]) {
	open ($_->[1], $_->[0]) || do { 
	    $ERROR = "$_->[0]: $!";
	    return -1;
	}
    }

    while (defined($exp = <EXP>)) {
	# dest may be shorter than expected
	defined($dest = <DEST>) || do {
	    $ERROR = "$destfile ends at line $.";
	    return -1;
	};

	# "#pragma ignore" tells the checker to ignore any 
	# subsequent lines up to the next "#pragma"
	$exp =~ /^#pragma\s+ignore/ && do {
	    $ignore = 1;
	    next;
	};
	$ignore && ($exp =~ /^#pragma\s+check/) && do {
	    $ignore = 0;
	    next;
	};
	next if ($ignore);


	# dest line may be different than expected
	unless ($exp eq $dest) {
	    foreach ($exp, $dest) {
		s/\n/\\n/g;
	    }
	    $ERROR = "files $expfile and $destfile differ at line $.\n" 
		. "  expected: [$exp]\n"
		. "       got: [$dest]";
	    return -1;
	}
    }

    # dest file may be longer than expected
    unless(eof DEST) {
	$ERROR = "$destfile is longer than $expfile";
	return -1;
    }


    # close files and be done
    close(EXP);
    close(DEST);

    0;
}



#
# log_init()
#
# Initialise the log file.
#

sub log_init {
    my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst)
		= localtime(time);

    &log_entry("Test logfile started at %s\n", 
	time2str("%H:%M:%S on %d:%b:%y", time));
}



#
# log_entry($format, @params)
#
# Writes an entry to the log file.  $format and @params are formatted as per
# printf(3C).
#

sub log_entry {
    open(LOGFILE, ">> $LOGFILE") && do {
	printf(LOGFILE shift, @_);
	close(LOGFILE);
    };
}


1;
