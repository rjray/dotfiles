#!/usr/bin/env perl

use 5.018;
use strict;
use warnings;
use autodie qw(open close);

use Carp qw(croak);
use File::Spec;

use YAML::Syck;

our $VERSION = '0.5';

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

# Grab and save any global settings:
my $globals = $config->{global} || {};
delete $config->{global};

# Set up the base table for environment vars and global values:
my %match_table = %ENV;
for my $key (keys %{$globals}) {
    $match_table{$key} = $globals->{$key};
}
my $global_count = scalar keys %{$globals};

foreach my $target (sort keys %{$config}) {
    my $tfile = File::Spec->catfile($home, $target);
    my $local_table = $config->{$target} || {};
    my @keys = sort keys %{$local_table};
    my $local_count = scalar @keys;
    my %ALL = (%match_table, %{$local_table});
    my @all_keys = (keys %match_table, @keys);
    my $pl = ($local_count + $global_count != 1) ? 's' : q{};

    printf "Expanding tag$pl in %s (%d global, %d specific)\n",
        $tfile, $global_count, $local_count;

    my $pat = join q{|}, map { q{\$} . $_ } (sort keys %ALL);
    print STDERR ">>> $pat\n";
    open my $ifh, '<', $tfile;
    my @lines = <$ifh>;
    close $ifh;

    open my $ofh, '>', "$tfile.new";

    for my $line (@lines) {
        $line =~ s/($pat)/$ALL{substr $1, 1}/g;
        print {$ofh} $line;
    }

    close $ofh;
    print "Moving $tfile.new to $tfile\n";
    unlink $tfile;
    rename "$tfile.new", $tfile;
}

exit 0;
