#!/usr/bin/perl

use strict;
use warnings;

use File::Spec;

use YAML::Syck;

my $home = $ENV{HOME};
die "USAGE: $0 FILE\n" unless my $file = shift;

unless (File::Spec->file_name_is_absolute($file))
{
	$file = File::Spec->catfile($home, $file);
}
unless (-f $file && -r $file)
{
	die "File '$file' does not exist or is not readable, stopped";
}

my $config = LoadFile $file;

foreach my $target (sort keys %$config)
{
	my $tfile = File::Spec->catfile($home, $target);
	my @keys = sort keys %{$config->{$target}};
	printf "Expanding %d tag%s in %s\n",
		scalar(@keys), ($#keys ? 's' : ''), $tfile;

	my $pat = join('|', @keys);
	open(my $ifh, '<', $tfile) or die "Error opening $tfile: $!";
	open(my $ofh, '>', "$tfile.new") or die "Error opening $tfile.new: $!";

	while (defined($_ = <$ifh>))
	{
		s/($pat)/$config->{$target}->{$1}/g;
		print $ofh $_;
	}

	close($ofh);
	close($ifh);
	print "Moving $tfile.new to $tfile\n";
	unlink $tfile;
	rename "$tfile.new", $tfile;
}

exit 0;
