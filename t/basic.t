#
# basic.t 
#
# MetaText test script
#


#
# To add a test, modify the 1..n line below
#

BEGIN { $| = 1; print "1..7\n"; }
END   { print "not ok 1\n" unless $loaded; }

use Text::MetaText;
use lib qw(. t);
require "test.pl";

$loaded = 1;
print "ok 1\n";


&init();


# create a MetaText object
my $mt = new Text::MetaText { 
	'lib'      => "t/elements",
#    	DEBUGLEVEL => 'process'
    } || die "failed to create MetaText object\n";


#
# To add a test, include it in the list below
#

# define the test files and pre-defined variables
my @test_files = qw(define eval subst include print filter);
my $predefs = {
	"foo"    => "<** FOO **>",
	"bar"    => "<** BAR **>",
	"baz"    => "<** BAZ **>",
};

# test each file
foreach (@test_files) {
	&test_file(++$loaded, $mt, $_, $predefs);
}


