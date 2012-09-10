#!/usr/bin/perl -w

#binMSW.pl  (Bins MassSpecWavelet data produced by Dongliang's R script)
#Generated using perl_script_template.pl 1.31
#Robert W. Leach
#rwleach@ccr.buffalo.edu
#Created on 3/19/2008
#Center for Computational Research
#Copyright 2007

#These variables (in main) are used by printVersion()
my $template_version_number = '1.32';
my $software_version_number = '1.0';

##
## Start Main
##

use strict;
use Getopt::Long;

#Declare & initialize variables.  Provide default values here.
my($outfile_suffix); #Not defined on purpose so a user can overwrite the input file
my @input_files         = ();
my $current_output_file = '';
my $help                = 0;
my $version             = 0;
my $force               = 0;
my $mz_play_fraction    = .0005;
my $amp_play_fraction   = .2;
my $snr_play_fraction   = .5;
my $scale_play_distance = 10;
my $use_amp             = 0;
my $use_scale           = 0;
my $use_snr             = 0;
my $ignore              = 0;
my $choose_most_intense = 0;

#These variables (in main) are used by the following subroutines:
#verbose, error, warning, debug, printVersion, getCommand and usage
my $preserve_args = [@ARGV];  #Preserve the agruments for getCommand
my $verbose       = 0;
my $quiet         = 0;
my $DEBUG         = 0;

my $GetOptHash =
  {'i|input-file=s'     => sub {push(@input_files,   #REQUIRED unless <> is
				     sglob($_[1]))}, #         supplied
   '<>'                 => sub {push(@input_files,   #REQUIRED unless -i is
				     sglob($_[0]))}, #         supplied
   'b|bin-center-egde-width-fraction=s' => \$mz_play_fraction,
   'a|use-amplitudes!'  => \$use_amp,                #OPTIONAL [Off]
   's|use-scales!'      => \$use_scale,              #OPTIONAL [Off]
   'n|use-signal2noise!'=> \$use_snr,                #OPTIONAL [Off]
   't|toss-weak-conflicts!'=> \$choose_most_intense, #OPTIONAL [Off]
   'ignore!'            => \$ignore,                 #OPTIONAL [Off]
   'o|outfile-suffix=s' => \$outfile_suffix,         #OPTIONAL [undef]
   'f|force!'           => \$force,                  #OPTIONAL [Off]
   'v|verbose!'         => \$verbose,                #OPTIONAL [Off]
   'q|quiet!'           => \$quiet,                  #OPTIONAL [Off]
   'h|help!'            => \$help,                   #OPTIONAL [Off]
   'debug!'             => \$DEBUG,                  #OPTIONAL [Off]
   'version!'           => \$version,                #OPTIONAL [Off]
  };

#If there are no arguments and no files directed or piped in
if(scalar(@ARGV) == 0 && isStandardInputFromTerminal())
  {
    usage();
    exit(0);
  }

#Get the input options
GetOptions(%$GetOptHash);

#Print the debug mode (it checks the value of the DEBUG global variable)
debug("Debug mode on.");

#If the user has asked for help, call the help subroutine
if($help)
  {
    help();
    exit(0);
  }

#If the user has asked for the software version, print it
if($version)
  {
    printVersion();
    exit(0);
  }

#Check validity of verbosity options
if($verbose && $quiet)
  {
    $quiet = 0;
    error("You cannot supply verbose and quiet flags at the same time.");
    exit(1);
  }

#Put standard input into the input_files array if standard input has been redirected in
if(!isStandardInputFromTerminal())
  {
    push(@input_files,'-');

    #Warn the user about the naming of the outfile when using STDIN
    if(defined($outfile_suffix))
      {warning("Input on STDIN detected along with an outfile suffix.  Your ",
	       "output file will be named STDIN$outfile_suffix")}
  }

#Make sure there is input
if(scalar(@input_files) == 0)
  {
    error("No input files detected.");
    usage(1);
    exit(2);
  }

#Check to make sure previously generated output files won't be over-written
if(!$force && defined($outfile_suffix))
  {
    my $existing_outfiles = [];
    foreach my $output_file (map {($_ eq '-' ? 'STDIN' : $_) . $outfile_suffix}
			     @input_files)
      {push(@$existing_outfiles,$output_file) if(-e $output_file)}

    if(scalar(@$existing_outfiles))
      {
	error("The output files: [@$existing_outfiles] already exist.  ",
	      "Use -f to force overwrite.  E.g.\n\t",
	      getCommand(1),' --force');
	exit(3);
      }
  }

if(isStandardOutputToTerminal() && !defined($outfile_suffix))
  {verbose("NOTE: VerboseOverMe functionality has been altered to yield ",
	   "clean STDOUT output.")}

verbose("Run conditions: ",getCommand(1),"\n");

my $data = {};    #{$input_file => [[mz,amplitude,signal2noise,scale]]}
my $all_mzs = {}; #{mz# => count within binning range}

#If output is going to STDOUT instead of output files with different extensions
if(!defined($outfile_suffix))
  {verbose("[STDOUT] Opened for all output.")}

#For each input file
foreach my $input_file (@input_files)
  {
    #Open the input file
    if(!open(INPUT,$input_file))
      {
	#Report an error and iterate if there was an error
	error("Unable to open input file: [$input_file]\n$!");
	next;
      }
    else
      {verboseOverMe("[",
		     ($input_file eq '-' ? 'STDIN' : $input_file),
		     "] Opened input file.")}

    #Keep track of input file's line number
    my $line_num = 0;
    my $close_count = 0;

    #For each line in the current input file
    while(getLine(*INPUT))
      {
	$line_num++;

	verboseOverMe("[",
		      ($input_file eq '-' ? 'STDIN' : $input_file),
		      "] Reading line $line_num.");

	next if(/peak/ || /^\s*$/ || /^#/);
	chomp;

	if(/[^\d\.\s\-\+]/)
	  {
	    warning("Non-numeric characters found on line: [$line_num] in ",
		    "file: [$input_file]: [$_] Skipping.");
	    next;
	  }

	s/^ +//;
	s/ +$//;
	push(@{$data->{$input_file}},[split(/\s+/,$_)]);

	if(scalar(@{$data->{$input_file}->[-1]}) < 2)
	  {
	    warning("Not enough columns on line: [$line_num] in file: ",
		    "[$input_file]: [$_] Skipping.");
	    next;
	  }

	#This is a kluge to allow files containing only mz and intensity
	if(scalar(@{$data->{$input_file}->[-1]}) == 2)
	  {
	    #Put an arbitrary index on the front
	    unshift(@{$data->{$input_file}->[-1]},1);
	    #Tack on a bogus SNR and scale value
	    push(@{$data->{$input_file}->[-1]},(1,1))
	  }

	$all_mzs->{$data->{$input_file}->[-1]->[1]}++;

	if(scalar(@{$data->{$input_file}}) > 1)
	  {
	    my $last_mz = $data->{$input_file}->[-1]->[1];
	    my $prev_mz = $data->{$input_file}->[-2]->[1];
	    my $lbound = $last_mz - ($mz_play_fraction * $last_mz);

	    my $last_amp = $data->{$input_file}->[-1]->[2];
	    my $prev_amp = $data->{$input_file}->[-2]->[2];
	    my $aplay = $amp_play_fraction *
	      ($last_amp > $prev_amp ? $last_amp : $prev_amp);

	    my $last_snr = $data->{$input_file}->[-1]->[3];
	    my $prev_snr = $data->{$input_file}->[-2]->[3];
	    my $splay = $snr_play_fraction *
	      ($last_snr > $prev_snr ? $last_snr : $prev_snr);

	    my $last_scale = $data->{$input_file}->[-1]->[4];
	    my $prev_scale = $data->{$input_file}->[-2]->[4];
	    my $play = $scale_play_distance;

	    #Assumes ascending MZ values in the file
	    if($prev_mz > $lbound &&
	       (!$use_amp || ($prev_amp >= ($last_amp - $aplay) &&
			      $prev_amp <= ($last_amp + $aplay))) &&
	       (!$use_snr || ($prev_snr >= ($last_snr - $splay) &&
			      $prev_snr <= ($last_snr + $splay))) &&
	       (!$use_scale || ($prev_scale >= ($last_scale - $play) &&
				$prev_scale <= ($last_scale + $play))))
	      {
#		warning("CLOSE PEAKS FOUND IN [$input_file]: $prev_mz ",
#			"AND $last_mz: @{$data->{$input_file}->[-2]} | ",
#			"@{$data->{$input_file}->[-1]}\n");
		$close_count++;

		if($choose_most_intense)
		  {
		    my $last = $data->{$input_file}->[-1];
		    my $prev = $data->{$input_file}->[-2];

		    pop(@{$data->{$input_file}});
		    pop(@{$data->{$input_file}});

		    if($last_amp > $prev_amp)
		      {
			debug("Choosing $last_mz");
			push(@{$data->{$input_file}},[@$last]);
			$all_mzs->{$prev_mz}--;
		      }
		    else
		      {
			debug("Choosing $prev_mz");
			push(@{$data->{$input_file}},[@$prev]);
			$all_mzs->{$last_mz}--;
		      }
		  }
	      }
	  }
      }

    warning("Input file: [$input_file] had [$close_count] peaks which fall ",
	    "into bins with other peaks.  ",
	    ($choose_most_intense ? "The most intense in each case was kept." :
	     "Use the -t flag to keep only the most intense peaks in each " .
	     "bin, lessen the bin width by editing the value of " .
	     "mz_play_fraction, or see the usage to select other spectrum " .
	     "features to use for binning.")) if($close_count);

    close(INPUT);

    verbose("[",
	    ($input_file eq '-' ? 'STDIN' : $input_file),
	    '] Input file done.  Time taken: [',
	    scalar(markTime()),
	    " Seconds].");
  }


#Now correct the all_mzs for ones that were removed.  Ideally, anything that was decremented above is at 0 in this hash, but if we choose the wrong peak to keep (i.e. the one that's consistent with mz values from others that were kept) then we've got a problem, but I'm going to assume we're always going to get the right peaks.
foreach my $mz_key (keys(%$all_mzs))
  {
    if($all_mzs->{$mz_key} < 1)
      {
	debug("Removing tossed mz value: $mz_key\n");
	delete($all_mzs->{$mz_key});
      }
    else #Reset the mz count to 0
      {$all_mzs->{$mz_key} = 0}
  }

#Let's see how many other mz values each mz value touches
foreach my $mz (keys(%$all_mzs))
  {
    foreach my $other_mz (sort {$a <=> $b} keys(%$all_mzs))
      {
	if($other_mz >= ($mz - ($mz_play_fraction * $mz)) &&
	   $other_mz <= ($mz + ($mz_play_fraction * $mz)))
	  {$all_mzs->{$mz}++}
	elsif($other_mz > $mz)
	  {last}
      }
  }

print("#MZ\tBin content count given $mz_play_fraction sized bins\n",
      join("\n",map {"$_\t$all_mzs->{$_}"} sort {$a <=> $b} keys(%$all_mzs)),
      "\n");

#Procedure to pick final bins:
#Go through the bins in order of decreasing number of contained peaks.
#See if there are break points between different groups of peaks and for each group...
#Choose the center value and add it as a final bin
#Remove values that fall into the bin from the all_mzs hash

#Go through the bins in order of decreasing number of contained peaks.
my $check = {};
my $bin_mz_hash = {};  #Points every mz to the mz of the bin its been assigned
my(@final_bins);

debug("VALUES of the all_mzs hash: [",join(' ',values(%$all_mzs)),"].");

#I couldn't do this in the foreach loop below.  I'm not sure why, but it
#appeared as though it was causing the loop to go through values AND keys.  I
#suspect this has to do with $_ getting changed inside the loop...
my @unique_sizes = grep {my $c = !exists($check->{$_});
			 $check->{$_}=0;
			 $c}
  sort {$b <=> $a}
  values(%$all_mzs);

debug("UNIQUE VALUES of the all_mzs hash: [",join(' ',@unique_sizes),"].");

foreach my $group_size (@unique_sizes)
  {
    debug("DOING GROUP SIZE: [$group_size].");

    #See if there are break points between different groups of peaks
    my @group = sort {$a <=> $b} #{$all_mzs->{$a} <=> $all_mzs->{$b}}
      grep {$all_mzs->{$_} == $group_size}
	keys(%$all_mzs);

    debug("There are [",scalar(@group),"] members left in this group.");

    #I'll be removing from the all_mzs hash in this loop.  It's possible I
    #could remove some group sizes because fringe peaks might fall into a
    #nearby bin.  So I need to check here that group has something before I
    #move on...
    next if(scalar(@group) == 0);

    #Segment the groups into clusters of close peaks based on the allowed play
    #When there's a jump to a peak larger than the allowed play, start a new
    #segment
    my(@group_segments);
    do
      {
	push(@group_segments,[shift(@group)]);

	while(scalar(@group) &&
	      $group_segments[-1]->[-1] >=
	      ($group[0] - ($group[0] * $mz_play_fraction)) &&
	      $group_segments[-1]->[-1] <=
	      ($group[0] + ($group[0] * $mz_play_fraction)))
	  {push(@{$group_segments[-1]},shift(@group))}
      }
	while(scalar(@group));

    debug("[",scalar(@group_segments),"] segments were made from this group.");

    #For each group segment...
    foreach my $segment (@group_segments)
      {
	#Choose the center value and add it as a final bin
	my $mid = scalar(@$segment) / 2;
	$mid = int($mid) + 1 if(scalar(@$segment) % 2);
	$mid--; #Array index starts from 0
	my $final_bin = $segment->[$mid];

	debug("Chose m/z [$final_bin] among [@$segment].");

	#Remove values that fall into the bin from the all_mzs hash
	foreach my $binned_mz (grep {$_ >= ($final_bin -
					    ($final_bin * $mz_play_fraction))
				       &&
					 $_ <= ($final_bin +
						($final_bin *
						 $mz_play_fraction))}
			       sort {$a <=> $b} keys(%$all_mzs))
	  {
	    debug("Deleting binned m/z: [$binned_mz].");
	    delete($all_mzs->{$binned_mz});
	    $bin_mz_hash->{$binned_mz} = $final_bin;
	  }

	#Check to make sure every mz from this segment got into the bin
	if(grep {$_ < ($final_bin - ($final_bin * $mz_play_fraction)) ||
		   $_ > ($final_bin + ($final_bin * $mz_play_fraction))}
	   @$segment)
	  {
	    error("Problem area identified around mass charge ratio: ",
		  "[$final_bin].  It's possible you can resolve this by ",
		  "using a larger fractional bin size than the current ",
		  "[$mz_play_fraction].  This program was not written to ",
		  "identify peaks, but to bin discrete peaks identified by ",
		  "another program.  If you do not have discrete peaks, ",
		  "using this program's not a good idea.  Use --ignore to ",
		  "move past this error and ignore the unbinned peaks (which ",
		  "could yet fall into other nearby bins).");
	    exit(1) unless($ignore);
	  }

	push(@final_bins,$final_bin);
      }
  }

#Print the final bin center values
print("Bin center values:\n",join("\n",sort {$a <=> $b} @final_bins),"\n\n");

#Now I need to alter the input to put in the bin data
#The first column is actually an artificial bin created by an alignment
#algorithm, so I'm going to replace that with an arbitrary global sequential
#bin number
#First create a bin number hash keyed on the final mz bins chosen above
my $bin_num = 0;
my $bin_num_hash = {};
foreach my $final_bin_mz (sort {$a <=> $b} @final_bins)
  {
    $bin_num++;
    $bin_num_hash->{$final_bin_mz} = $bin_num;
  }

#Now go through each input file
my $bin_crowding = {};
foreach my $input_file (keys(%$data))
  {
    #If an output file name suffix has been defined
    if(defined($outfile_suffix))
      {
	##
	## Open and select the next output file
	##

	#Set the current output file name
	$current_output_file = ($input_file eq '-' ? 'STDIN' : $input_file)
	  . $outfile_suffix;

	#Open the output file
	if(!open(OUTPUT,">$current_output_file"))
	  {
	    #Report an error and iterate if there was an error
	    error("Unable to open output file: [$current_output_file]\n$!");
	    next;
	  }
	else
	  {verboseOverMe("[$current_output_file] Opened output file.")}

	#Select the output file handle
	select(OUTPUT);
      }

    foreach my $row (@{$data->{$input_file}})
      {
	$row->[0] = $bin_num_hash->{$bin_mz_hash->{$row->[1]}};
	$bin_crowding->{$input_file}->{$row->[0]}++;
	print(join("\t",@$row),"\n");
      }

    #If an output file name suffix is set
    if(defined($outfile_suffix))
      {
	#Select standard out
	select(STDOUT);
	#Close the output file handle
	close(OUTPUT);

	verbose("[$current_output_file] Output file done.");
      }
  }


#Now print out a table of the data where each row is a sample and each column is a bin containing sub-columns of intensity, SNR, & scale.  We'll create a 2D hash to contain the table.
my $table_hash = {}; #{$input_file=>{$bin_num=>{INTENSITY=>$intensity,SNR=>$snr,SCALE=>$scale}}}
foreach my $input_file (keys(%$data))
  {
    #Fill the table with the existing values
    foreach my $bin_row (@{$data->{$input_file}})
      {$table_hash->{$input_file}->{$bin_row->[0]} =
	 {INTENSITY => $bin_row->[2],
	  SNR       => $bin_row->[3],
	  SCALE     => $bin_row->[4]}}

    #Fill in dots for the missing values (some spectra don't have all peaks)
    foreach my $bin (1..$bin_num)
      {$table_hash->{$input_file}->{$bin}={INTENSITY => '.',
					   SNR       => '.',
					   SCALE     => '.'}
	 unless(exists($table_hash->{$input_file}->{$bin}))}
  }

print("                                      \t\t",(map {"$bin_num_hash->{$_}:$_\t\t"} sort {$a <=> $b} keys(%$bin_num_hash)),"\n");
print("#FILE                                 \t",(map {"\t$_\_INT\t$_\_SNR\t$_\_SCL"} (1..$bin_num)),"\n");
foreach my $file (sort {$a cmp $b} keys(%$table_hash))
  {
    print("$file");
    foreach my $bin (sort {$a <=> $b} keys(%{$table_hash->{$file}}))
      {
	print("\t",sigdig($table_hash->{$file}->{$bin}->{INTENSITY},6),
	      "\t",sigdig($table_hash->{$file}->{$bin}->{SNR},6),
	      "\t",sigdig($table_hash->{$file}->{$bin}->{SCALE},6));
      }
    print("\n");
  }



#Warnings about multiple peaks from a single spectrum going into the same bin.
foreach my $file (keys(%$bin_crowding))
  {
    foreach my $bin_num (sort {$a <=> $b} keys(%{$bin_crowding->{$file}}))
      {
	warning("Bin: [$bin_num] in file: [$file] contains multiple peaks ",
		"from this spectrum.")
	  if($bin_crowding->{$file}->{$bin_num} > 1);
      }
  }

#Report the number of errors, warnings, and debugs
verbose("Done.  EXIT STATUS: [",
	"ERRORS: ",
	($main::error_number ? $main::error_number : 0),
	" WARNINGS: ",
	($main::warning_number ? $main::warning_number : 0),
	($DEBUG ?
	 " DEBUGS: " . ($main::debug_number ? $main::debug_number : 0) : ''),
        " TIME: ",scalar(markTime(0)),"s]");
if($main::error_number || $main::warning_number)
  {verbose("Scroll up to inspect errors and warnings.")}

##
## End Main
##






























##
## Subroutines
##

#Significant digits
sub sigdig
  {
    my $num = $_[0];
    my $num_digits = $_[1];

    return($num) unless($num =~ /\d/);

    if($num >= 10**($num_digits-1))
      {
	$num =~ s/\..*//;
	$num =~ s/(?<=\d{$num_digits})\d/0/g;
      }
    elsif($num =~ /([1-9]\d*)\./)
      {
	my $len = length($1);
	$num_digits -= $len;
	$num =~ s/(?<=\.\d{$num_digits}).*//;
      }
    elsif($num =~ /\.(0*)[1-9]\d*/)
      {
	my $len = length($1);
	$num_digits += $len;
	$num =~ s/^([\-\+]?)0+/$1/;
	$num =~ s/(?<=\d{$num_digits}).*//;
      }
    elsif($num =~ /^[\-\+]?[0\.]+$/)
      {$num = 0}
    return($num);
  }

##
## This subroutine prints a description of the script and it's input and output
## files.
##
sub help
  {
    my $script = $0;
    my $lmd = localtime((stat($script))[9]);
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Print a description of this program
    print << "end_print";

$script
Copyright 2007
Robert W. Leach
Created on 3/19/2008
Last Modified on $lmd
Center for Computational Research
701 Ellicott Street
Buffalo, NY 14203
rwleach\@ccr.buffalo.edu

* WHAT IS THIS: This script takes a series of peak data files (one for each
                sample) output by Dongliang's R script which uses the
                bioconductor package called MassSpecWavelet and putputs the
                data with assigned bins for comparison across samples.  The
                bins are numbered sequentially using one consistent sequence
                for all files.

* INPUT FORMAT: The input files are tab-delimited.  Comment lines (beginning
                with the # character are skipped as well as any line containing
                non-numeric characters).  Column order is peak center index,
                mass/charge value, peak value (intensity), signal to noise
                ratio, and peak scale (referring to the peak shape).  Example:

  "peakCenterIndex"	"m.z"	"peakValue"	"peakSNR"	"peakScale"
  16230	6821.0444	8541.58178466885	31.3368348864427	68
  17611	6989.0322	1982.07055028410	7.27169968436254	68
  27832	8295.8643	4876.83717002771	17.8918431056326	68
  30102	8601.291	1567.80770272717	5.75187738671977	68
  30921	8712.8418	84324.1677819194	309.363369611656	68


* OUTPUT FORMAT: The output format is the same as the input format except
                 comment/empty lines are not included and the peak center index
                 is replaced by an arbitrary/sequential bin index.  This index
                 will change based on the peak content of the files.  Example:

  1	6821.0444	8541.58178466885	31.3368348864427	68
  2	6989.0322	1982.07055028410	7.27169968436254	68
  4	8295.8643	4876.83717002771	17.8918431056326	68
  6	8601.291	1567.80770272717	5.75187738671977	68
  7	8712.8418	84324.1677819194	309.363369611656	68

end_print

    return(0);
  }

##
## This subroutine prints a usage statement in long or short form depending on
## whether "no descriptions" is true.
##
sub usage
  {
    my $no_descriptions = $_[0];

    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    print << "end_print";
USAGE: $script -i "input file(s)" [-o .ext] [-f] [-v] [-q] [-h] [--version] [--debug] < another_input_file
end_print

    if($no_descriptions)
      {print("Execute $script with no options to see a description of the ",
             "available parameters.\n")}
    else
      {
        print << 'end_print';

     -i|--input-file*     REQUIRED Space-separated peak file(s inside quotes)
                                   generated by Dongliang's R script.  See
                                   --help for a description of the input file
                                   format.
                                   *No flag required.  Standard input via
                                   redirection is acceptable.  Perl glob
                                   characters (e.g. '*') are acceptable inside
                                   quotes.
     -b|--bin-center-edge OPTIONAL [.0005] The fraction of the mass-charge
        -width-fraction            value chosen to be the bin center out from
                                   which to include peaks from other samples.
                                   A value of 0.05% is a fairly strict bin
                                   width determinant.  If you are getting
                                   warnings about peaks from the same spectra
                                   falling into the same bin, you can try to
                                   make this value smaller, but be careful that
                                   you are not creating excess bins containing
                                   peaks which should be in the same bin in
                                   the process.  Use the -t flag to throw out
                                   less intense peaks when multiple peaks fall
                                   into the same bin.
     -t|--toss-weak-      OPTIONAL [Off] Throws out less intense peaks when
        conflicts                  multiple peaks fall into the same bin.
     -i|--ignore          OPTIONAL [Off] Ignore problems and output anyway.
     -a|--use-amplitudes  OPTIONAL [Off] Use peak intensity to separate peaks
                                   in addition to the bin center-edge width
                                   fraction.  MassSpecWavelet will de-
                                   convolute the spectra such that a small
                                   narrow peak on top of a large broad peak
                                   will each be reported on.  If their centers
                                   fall into the same bin, then separating
                                   further based on intensity will put them in
                                   different bins.  This will only work on
                                   peaks from the same spectrum that fall into
                                   the same bin otherwise.  All samples should
                                   have both peaks present.  The separation
                                   fraction is not currently a parameter and is
                                   set at 0.2.  The peaks must differ in
                                   intensity by a 0.2 ratio to be put in
                                   different bins.
     -s|--use-scales      OPTIONAL [Off] Use peak scale to separate peaks in
                                   addition to the bin center-edge width
                                   fraction.  MassSpecWavelet will de-
                                   convolute the spectra such that a small
                                   narrow peak on top of a large broad peak
                                   will each be reported on.  If their centers
                                   fall into the same bin, then separating
                                   further based on scale will put them in
                                   different bins.  This will only work on
                                   peaks from the same spectrum that fall into
                                   the same bin otherwise.  All samples should
                                   have both peaks present.  The separation
                                   fraction is not currently a parameter and is
                                   set at 0.2.  The peaks must differ in scale
                                   by a 10 scales to be put in different bins.
     -n|--use-            OPTIONAL [Off] Use the signal-to-noise ratios to
        signal2noise               separate peaks in addition to the bin
                                   center-edge width fraction.
                                   MassSpecWavelet will de-convolutes the
                                   spectra such that a small narrow peak on top
                                   of a large broad peak will each be reported
                                   on.  If their centers fall into the same
                                   bin, then separating further based on
                                   signal to noise will put them in different
                                   bins.  This will only work on peaks from the
                                   same spectrum that fall into the same bin
                                   otherwise.  All samples should have both
                                   peaks present.  The separation fraction is
                                   not currently a parameter and is set at 0.5.
                                   The peaks must differ in signal to noise
                                   ratio values by a 0.2 ratio to be put in
                                   different bins.
     -o|--outfile-suffix  OPTIONAL [nothing] This suffix is added to the input
                                   file names to use as output files.
                                   Redirecting a file into this script will
                                   result in the output file name to be "STDIN"
                                   with your suffix appended.
     -f|--force           OPTIONAL [Off] Force overwrite of existing output
                                   files (generated from previous runs of this
                                   script).  Only used when the -o option is
                                   supplied.
     -v|--verbose         OPTIONAL [Off] Verbose mode.  Cannot be used with the
                                   quiet flag.
     -q|--quiet           OPTIONAL [Off] Quiet mode.  Turns off warnings and
                                   errors.  Cannot be used with the verbose
                                   flag.
     -h|--help            OPTIONAL [Off] Help.  Use this option to see an
                                   explanation of the script and its input and
                                   output files.
     --version            OPTIONAL [Off] Print software version number.  If
                                   verbose mode is on, it also prints the
                                   template version used to standard error.
     --debug              OPTIONAL [Off] Debug mode.

end_print
      }

    return(0);
  }


##
## Subroutine that prints formatted verbose messages.  Specifying a 1 as the
## first argument prints the message in overwrite mode (meaning subsequence
## verbose, error, warning, or debug messages will overwrite the message
## printed here.  However, specifying a hard return as the first character will
## override the status of the last line printed and keep it.  Global variables
## keep track of print length so that previous lines can be cleanly
## overwritten.
##
sub verbose
  {
    return(0) unless($verbose);

    #Read in the first argument and determine whether it's part of the message
    #or a value for the overwrite flag
    my $overwrite_flag = $_[0];

    #If a flag was supplied as the first parameter (indicated by a 0 or 1 and
    #more than 1 parameter sent in)
    if(scalar(@_) > 1 && ($overwrite_flag eq '0' || $overwrite_flag eq '1'))
      {shift(@_)}
    else
      {$overwrite_flag = 0}

    #Ignore the overwrite flag if STDOUT will be mixed in
    $overwrite_flag = 0 if(isStandardOutputToTerminal());

    #Read in the message
    my $verbose_message = join('',@_);

    $overwrite_flag = 1 if(!$overwrite_flag && $verbose_message =~ /\r/);

    #Initialize globals if not done already
    $main::last_verbose_size  = 0 if(!defined($main::last_verbose_size));
    $main::last_verbose_state = 0 if(!defined($main::last_verbose_state));
    $main::verbose_warning    = 0 if(!defined($main::verbose_warning));

    #Determine the message length
    my($verbose_length);
    if($overwrite_flag)
      {
	$verbose_message =~ s/\r$//;
	if(!$main::verbose_warning && $verbose_message =~ /\n|\t/)
	  {
	    warning("Hard returns and tabs cause overwrite mode to not work ",
		    "properly.");
	    $main::verbose_warning = 1;
	  }
      }
    else
      {chomp($verbose_message)}

    if(!$overwrite_flag)
      {$verbose_length = 0}
    elsif($verbose_message =~ /\n([^\n]*)$/)
      {$verbose_length = length($1)}
    else
      {$verbose_length = length($verbose_message)}

    #Overwrite the previous verbose message by appending spaces just before the
    #first hard return in the verbose message IF THE VERBOSE MESSAGE DOESN'T
    #BEGIN WITH A HARD RETURN.  However note that the length stored as the
    #last_verbose_size is the length of the last line printed in this message.
    if($verbose_message =~ /^([^\n]*)/ && $main::last_verbose_state &&
       $verbose_message !~ /^\n/)
      {
	my $append = ' ' x ($main::last_verbose_size - length($1));
	unless($verbose_message =~ s/\n/$append\n/)
	  {$verbose_message .= $append}
      }

    #If you don't want to overwrite the last verbose message in a series of
    #overwritten verbose messages, you can begin your verbose message with a
    #hard return.  This tells verbose() to not overwrite the last line that was
    #printed in overwrite mode.

    #Print the message to standard error
    print STDERR ($verbose_message,
		  ($overwrite_flag ? "\r" : "\n"));

    #Record the state
    $main::last_verbose_size  = $verbose_length;
    $main::last_verbose_state = $overwrite_flag;

    #Return success
    return(0);
  }

sub verboseOverMe
  {verbose(1,@_)}

##
## Subroutine that prints errors with a leading program identifier containing a
## trace route back to main to see where all the subroutine calls were from,
## the line number of each call, an error number, and the name of the script
## which generated the error (in case scripts are called via a system call).
##
sub error
  {
    return(0) if($quiet);

    #Gather and concatenate the error message and split on hard returns
    my @error_message = split("\n",join('',@_));
    pop(@error_message) if($error_message[-1] !~ /\S/);

    $main::error_number++;

    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Assign the values from the calling subroutines/main
    my @caller_info = caller(0);
    my $line_num = $caller_info[2];
    my $caller_string = '';
    my $stack_level = 1;
    while(@caller_info = caller($stack_level))
      {
	my $calling_sub = $caller_info[3];
	$calling_sub =~ s/^.*?::(.+)$/$1/ if(defined($calling_sub));
	$calling_sub = (defined($calling_sub) ? $calling_sub : 'MAIN');
	$caller_string .= "$calling_sub(LINE$line_num):"
	  if(defined($line_num));
	$line_num = $caller_info[2];
	$stack_level++;
      }
    $caller_string .= "MAIN(LINE$line_num):";

    my $leader_string = "ERROR$main::error_number:$script:$caller_string ";

    #Figure out the length of the first line of the error
    my $error_length = length(($error_message[0] =~ /\S/ ?
			       $leader_string : '') .
			      $error_message[0]);

    #Put location information at the beginning of each line of the message
    foreach my $line (@error_message)
      {print STDERR (($line =~ /\S/ ? $leader_string : ''),
		     $line,
		     ($verbose &&
		      defined($main::last_verbose_state) &&
		      $main::last_verbose_state ?
		      ' ' x ($main::last_verbose_size - $error_length) : ''),
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }


##
## Subroutine that prints warnings with a leader string containing a warning
## number
##
sub warning
  {
    return(0) if($quiet);

    $main::warning_number++;

    #Gather and concatenate the warning message and split on hard returns
    my @warning_message = split("\n",join('',@_));
    pop(@warning_message) if($warning_message[-1] !~ /\S/);

    my $leader_string = "WARNING$main::warning_number: ";

    #Figure out the length of the first line of the error
    my $warning_length = length(($warning_message[0] =~ /\S/ ?
				 $leader_string : '') .
				$warning_message[0]);

    #Put leader string at the beginning of each line of the message
    foreach my $line (@warning_message)
      {print STDERR (($line =~ /\S/ ? $leader_string : ''),
		     $line,
		     ($verbose &&
		      defined($main::last_verbose_state) &&
		      $main::last_verbose_state ?
		      ' ' x ($main::last_verbose_size - $warning_length) : ''),
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }


##
## Subroutine that gets a line of input and accounts for carriage returns that
## many different platforms use instead of hard returns.  Note, it uses a
## global array reference variable ($infile_line_buffer) to keep track of
## buffered lines from multiple file handles.
##
sub getLine
  {
    my $file_handle = $_[0];

    #Set a global array variable if not already set
    $main::infile_line_buffer = {} if(!defined($main::infile_line_buffer));
    if(!exists($main::infile_line_buffer->{$file_handle}))
      {$main::infile_line_buffer->{$file_handle}->{FILE} = []}

    #If this sub was called in array context
    if(wantarray)
      {
	#Check to see if this file handle has anything remaining in its buffer
	#and if so return it with the rest
	if(scalar(@{$main::infile_line_buffer->{$file_handle}->{FILE}}) > 0)
	  {
	    return(@{$main::infile_line_buffer->{$file_handle}->{FILE}},
		   map
		   {
		     #If carriage returns were substituted and we haven't
		     #already issued a carriage return warning for this file
		     #handle
		     if(s/\r\n|\n\r|\r/\n/g &&
			!exists($main::infile_line_buffer->{$file_handle}
				->{WARNED}))
		       {
			 $main::infile_line_buffer->{$file_handle}->{WARNED}
			   = 1;
			 warning("Carriage returns were found in your file ",
				 "and replaced with hard returns");
		       }
		     split(/(?<=\n)/,$_);
		   } <$file_handle>);
	  }
	
	#Otherwise return everything else
	return(map
	       {
		 #If carriage returns were substituted and we haven't already
		 #issued a carriage return warning for this file handle
		 if(s/\r\n|\n\r|\r/\n/g &&
		    !exists($main::infile_line_buffer->{$file_handle}
			    ->{WARNED}))
		   {
		     $main::infile_line_buffer->{$file_handle}->{WARNED}
		       = 1;
		     warning("Carriage returns were found in your file ",
			     "and replaced with hard returns");
		   }
		 split(/(?<=\n)/,$_);
	       } <$file_handle>);
      }

    #If the file handle's buffer is empty, put more on
    if(scalar(@{$main::infile_line_buffer->{$file_handle}->{FILE}}) == 0)
      {
	my $line = <$file_handle>;
	if(!eof($file_handle))
	  {
	    if($line =~ s/\r\n|\n\r|\r/\n/g &&
	       !exists($main::infile_line_buffer->{$file_handle}->{WARNED}))
	      {
		$main::infile_line_buffer->{$file_handle}->{WARNED} = 1;
		warning("Carriage returns were found in your file and ",
			"replaced with hard returns");
	      }
	    @{$main::infile_line_buffer->{$file_handle}->{FILE}} =
	      split(/(?<=\n)/,$line);
	  }
	else
	  {
	    #Do the \r substitution for the last line of files that have the
	    #eof character at the end of the last line instead of on a line by
	    #itself.  I tested this on a file that was causing errors for the
	    #last line and it works.
	    $line =~ s/\r/\n/g if(defined($line));
	    @{$main::infile_line_buffer->{$file_handle}->{FILE}} = ($line);
	  }
      }

    #Shift off and return the first thing in the buffer for this file handle
    return($_ = shift(@{$main::infile_line_buffer->{$file_handle}->{FILE}}));
  }

##
## This subroutine allows the user to print debug messages containing the line
## of code where the debug print came from and a debug number.  Debug prints
## will only be printed (to STDERR) if the debug option is supplied on the
## command line.
##
sub debug
  {
    return(0) unless($DEBUG);

    $main::debug_number++;

    #Gather and concatenate the error message and split on hard returns
    my @debug_message = split("\n",join('',@_));
    pop(@debug_message) if($debug_message[-1] !~ /\S/);

    #Assign the values from the calling subroutine
    #but if called from main, assign the values from main
    my($junk1,$junk2,$line_num,$calling_sub);
    (($junk1,$junk2,$line_num,$calling_sub) = caller(1)) ||
      (($junk1,$junk2,$line_num) = caller());

    #Edit the calling subroutine string
    $calling_sub =~ s/^.*?::(.+)$/$1:/ if(defined($calling_sub));

    my $leader_string = "DEBUG$main::debug_number:LINE$line_num:" .
      (defined($calling_sub) ? $calling_sub : '') .
	' ';

    #Figure out the length of the first line of the error
    my $debug_length = length(($debug_message[0] =~ /\S/ ?
			       $leader_string : '') .
			      $debug_message[0]);

    #Put location information at the beginning of each line of the message
    foreach my $line (@debug_message)
      {print STDERR (($line =~ /\S/ ? $leader_string : ''),
		     $line,
		     ($verbose &&
		      defined($main::last_verbose_state) &&
		      $main::last_verbose_state ?
		      ' ' x ($main::last_verbose_size - $debug_length) : ''),
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }


##
## This sub marks the time (which it pushes onto an array) and in scalar
## context returns the time since the last mark by default or supplied mark
## (optional) In array context, the time between all marks is always returned
## regardless of a supplied mark index
## A mark is not made if a mark index is supplied
## Uses a global time_marks array reference
##
sub markTime
  {
    #Record the time
    my $time = time();

    #Set a global array variable if not already set to contain (as the first
    #element) the time the program started (NOTE: "$^T" is a perl variable that
    #contains the start time of the script)
    $main::time_marks = [$^T] if(!defined($main::time_marks));

    #Read in the time mark index or set the default value
    my $mark_index = (defined($_[0]) ? $_[0] : -1);  #Optional Default: -1

    #Error check the time mark index sent in
    if($mark_index > (scalar(@$main::time_marks) - 1))
      {
	error("Supplied time mark index is larger than the size of the ",
	      "time_marks array.\nThe last mark will be set.");
	$mark_index = -1;
      }

    #Calculate the time since the time recorded at the time mark index
    my $time_since_mark = $time - $main::time_marks->[$mark_index];

    #Add the current time to the time marks array
    push(@$main::time_marks,$time)
      if(!defined($_[0]) || scalar(@$main::time_marks) == 0);

    #If called in array context, return time between all marks
    if(wantarray)
      {
	if(scalar(@$main::time_marks) > 1)
	  {return(map {$main::time_marks->[$_ - 1] - $main::time_marks->[$_]}
		  (1..(scalar(@$main::time_marks) - 1)))}
	else
	  {return(())}
      }

    #Return the time since the time recorded at the supplied time mark index
    return($time_since_mark);
  }

##
## This subroutine reconstructs the command entered on the command line
## (excluding standard input and output redirects).  The intended use for this
## subroutine is for when a user wants the output to contain the input command
## parameters in order to keep track of what parameters go with which output
## files.
##
sub getCommand
  {
    my $perl_path_flag = $_[0];
    my($command);

    #Determine the script name
    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Put quotes around any parameters containing un-escaped spaces or astericks
    my $arguments = [@$preserve_args];
    foreach my $arg (@$arguments)
      {if($arg =~ /(?<!\\)[\s\*]/ || $arg eq '')
	 {$arg = "'" . $arg . "'"}}

    #Determine the perl path used (dependent on the `which` unix built-in)
    if($perl_path_flag)
      {
	$command = `which $^X`;
	chomp($command);
	$command .= ' ';
      }

    #Build the original command
    $command .= join(' ',($0,@$arguments));

    #Note, this sub doesn't add any redirected files in or out

    return($command);
  }

##
## This subroutine checks to see if a parameter is a single file with spaces in
## the name before doing a glob (which would break up the single file name
## improperly).  The purpose is to allow the user to enter a single input file
## name using double quotes and un-escaped spaces as is expected to work with
## many programs which accept individual files as opposed to sets of files.  If
## the user wants to enter multiple files, it is assumed that space delimiting
## will prompt the user to realize they need to escape the spaces in the file
## names.
##
sub sglob
  {
    my $command_line_string = $_[0];
    return(-e $command_line_string ?
	   $command_line_string : glob($command_line_string));
  }


sub printVersion
  {
    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;
    print(($verbose ? "$script Version " : ''),
	  $software_version_number,
	  "\n");
    verbose("Generated using perl_script_template.pl\n",
	    "Version $template_version_number\n",
	    "Robert W. Leach\n",
	    "robleach\@lanl.gov\n",
	    "5/8/2006\n",
	    "Los Alamos National Laboratory\n",
	    "Copyright 2006");
    return(0);
  }

#This subroutine is a check to see if input is user-entered via a TTY (result is non-
#zero) or directed in (result is zero)
sub isStandardInputFromTerminal
  {return(-t STDIN || eof(STDIN))}

#This subroutine is a check to see if prints are going to a TTY.  Note, explicit prints
#to STDOUT when another output handle is selected are not considered and may defeat this
#subroutine.
sub isStandardOutputToTerminal
  {return(-t STDOUT && select() eq 'main::STDOUT')}
