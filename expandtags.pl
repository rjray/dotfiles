#!/usr/bin/env perl

use strict;
use warnings;
use autodie qw(open close);

use Carp qw(croak);
use File::Spec;

use YAML::Syck;

my $home = $ENV{HOME};
my $file = shift;
if (! $file) {
    die "USAGE: $0 FILE\n";
}

if (! File::Spec->file_name_is_absolute($file)) {
    $file = File::Spec->catfile($home, $file);
}
if (! (-f $file && -r $file)) {
    croak "File '$file' does not exist or is not readable, stopped";
}
my $config = LoadFile $file;

# Set up the match-pattern for env vars
my $envpat = join q{|}, map { '\$' . $_ } (keys %ENV);

foreach my $target (sort keys %{$config}) {
    my $tfile = File::Spec->catfile($home, $target);
    my @keys = sort keys %{$config->{$target}};
    printf "Expanding %d tag%s in %s\n",
        scalar(@keys), ($#keys ? 's' : q{}), $tfile;

    my $pat = join q{|}, @keys;
    open my $ifh, '<', $tfile;
    open my $ofh, '>', "$tfile.new";

    while (defined($_ = <$ifh>)) {
        # Do env vars first, to catch the leading $
        s/($envpat)/$ENV{substr $1, 1}/g;
        s/($pat)/$config->{$target}->{$1}/g;
        print {$ofh} $_;
    }

    close $ofh;
    close $ifh;
    print "Moving $tfile.new to $tfile\n";
    unlink $tfile;
    rename "$tfile.new", $tfile;
}

exit 0;
