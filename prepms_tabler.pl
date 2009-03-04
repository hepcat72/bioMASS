#!/usr/bin/perl -w

#Generated using perl_script_template.pl 1.37
#Robert W. Leach
#rwleach@ccr.buffalo.edu
#Center for Computational Research
#Copyright 2008

#These variables (in main) are used by getVersion() and usage()
my $software_version_number = '1.1';
my $created_on_date         = '12/9/2008';

##
## Start Main
##

use strict;
use Getopt::Long;

#Declare & initialize variables.  Provide default values here.
my $preprocessed_suffix = '.preprocessed';
my $normalized_suffix   = '.normalized';
my $peaks_suffix        = '.peaks';
my $table_suffix        = '.peaktable';
my $gnuplotcommand_suffix = '.gnuplotcommands';
my @input_files         = ();
my $current_output_file = '';
my $help                = 0;
my $version             = 0;
my $overwrite           = 0;
my $noheader            = 0;
my $windows_compatible  = 0;
my($preprocessed_file,$mz_file,$peaks_file);

#These variables (in main) are used by the following subroutines:
#verbose, error, warning, debug, getCommand, quit, and usage
my $preserve_args = [@ARGV];  #Preserve the agruments for getCommand
my $verbose       = 0;
my $quiet         = 0;
my $DEBUG         = 0;
my $ignore_errors = 0;

my $GetOptHash =
  {                                                  #REQUIRED - one of the
                                                     #following 2 individually
                                                     #optional params is
                                                     #required
   'r|preprocessed-file=s' => \$preprocessed_file,   #OPTIONAL - see above
   'p|peaks-file=s'     => \$peaks_file,             #OPTIONAL REQ'D if -m supl
   'm|mz-file=s'        => \$mz_file,                #OPTIONAL REQ'D if -p supl
   's|spectra-files=s'  => sub {$_[1]=~s/^.*[\/\\]//;#OPTIONAL
                                push(@input_files,
				     sglob($_[1]))},
   '<>'                 => sub {$_[0]=~s/^.*[\/\\]//;#OPTIONAL
                                push(@input_files,
				     sglob($_[0]))},
   'o|outfile-suffix=s' => \$preprocessed_suffix,    #OPTIONAL [.preprocessed]
   'on|normal-suffix=s' => \$normalized_suffix,      #OPTIONAL [.normalized]
   'op|peaks-suffix=s'  => \$peaks_suffix,           #OPTIONAL [.peaks]
   'ot|table-suffix=s'  => \$table_suffix,           #OPTIONAL [.peaktable]
   'og|gnuplot-suffix=s' => \$gnuplotcommand_suffix, #OPTIONAL [.gnuplotcommands]
   'force|overwrite'    => \$overwrite,              #OPTIONAL [Off]
   'ignore'             => \$ignore_errors,          #OPTIONAL [Off]
   'verbose:+'          => \$verbose,                #OPTIONAL [Off]
   'quiet'              => \$quiet,                  #OPTIONAL [Off]
   'debug:+'            => \$DEBUG,                  #OPTIONAL [Off]
   'help|?'             => \$help,                   #OPTIONAL [Off]
   'version'            => \$version,                #OPTIONAL [Off]
   'noheader'           => \$noheader,               #OPTIONAL [Off]
   'windows-compatible!'=> \$windows_compatible      #OPTIONAL [Off]
  };

my $nl = ($windows_compatible ? "\r" : "\n");

#If there are no arguments and no files directed or piped in
if(scalar(@ARGV) == 0 && isStandardInputFromTerminal())
  {
    usage();
    quit(0);
  }

#Get the input options & catch any errors in option parsing
unless(GetOptions(%$GetOptHash))
  {
    #Try to guess which arguments GetOptions is complaining about
    my @possibly_bad = grep {!(-e $_)} @input_files;

    error('Getopt::Long::GetOptions reported an error while parsing the ',
	  'command line arguments.  The error should be above.  Please ',
	  'correct the offending argument(s) and try again.');
    usage(1);
    quit(1);
  }

#Print the debug mode (it checks the value of the DEBUG global variable)
debug('Debug mode on.') if($DEBUG > 1);

#If the user has asked for help, call the help subroutine
if($help)
  {
    help();
    quit(0);
  }

#If the user has asked for the software version, print it
if($version)
  {
    print(getVersion($verbose),$nl);
    quit(0);
  }

#Check validity of verbosity options
if($quiet && ($verbose || $DEBUG))
  {
    $quiet = 0;
    error('You cannot supply the quiet and (verbose or debug) flags ',
	  'together.');
    quit(2);
  }

if((!defined($preprocessed_file) || $preprocessed_file eq '') &&
   (!defined($peaks_file) || $peaks_file eq ''))
  {
    error("One of these two input files is required: preprocessed.txt or ",
	  "peaks.txt as produced by PrepMS.  If peaks.txt is supplied, ",
	  "mz.txt is also required (also produced by PrepMS).");
    usage();
    quit(1);
  }

if((defined($peaks_file) && $peaks_file ne '') &&
   (!defined($mz_file) || $mz_file eq ''))
  {
    error("If peaks.txt is supplied, mz.txt is required.");
    usage();
    quit(2);
  }

#Put standard input into the input_files array if standard input has been redirected in
if(!isStandardInputFromTerminal())
  {
    push(@input_files,'-');

    #Warn the user about the naming of the outfile when using STDIN
    if(defined($preprocessed_suffix))
      {warning('Input on STDIN detected along with an outfile suffix.  Your ',
	       'output file will be named STDIN',$preprocessed_suffix)}
    #Warn users when they turn on verbose and output is to the terminal
    #(implied by no outfile suffix checked above) that verbose messages may be
    #uncleanly overwritten
    elsif($verbose && isStandardOutputToTerminal())
      {warning('You have enabled --verbose, but appear to possibly be ',
	       'outputting to the terminal.  Note that verbose messages can ',
	       'interfere with formatting of terminal output making it ',
	       'difficult to read.  You may want to either turn verbose off, ',
	       'redirect output to a file, or supply an outfile suffix (-o).')}
  }

verbose('Run conditions: ',getCommand(1));



##
## Read in the mz.txt file and put the mz's of the peaks into the @mz array
##

my $line_num     = 0;
my $verbose_freq = 100;
my(@mzs);
my $num_peaks    = 0;

#Open the mz file
if(defined($mz_file) && $mz_file ne '' && !open(MZ,$mz_file))
  {
    #Report an error and iterate if there was an error
    error("Unable to open mz file: [$mz_file].\n$!");
    quit(5);
  }
else
  {
    verbose('[',($mz_file eq '-' ? 'STDIN' : $mz_file),'] ',
	    'Opened mz file.');

    #For each line in the current input file
    while(getLine(*MZ))
      {
	$line_num++;
	verboseOverMe('[',($mz_file eq '-' ? 'STDIN' : $mz_file),'] ',
		      "Reading line: [$line_num].") unless($line_num %
							   $verbose_freq);

	next if(/^\s*#/ || /^\s*$/);

	chomp;
	if(/[^\d\.\+\-]/)
	  {
	    error("Invalid mz value found on line [$line_num] in file: ",
		  "[$mz_file]: [$_].  Skipping.");
	    next;
	  }
	push(@mzs,$_);
      }

    close(MZ);

    $num_peaks = scalar(@mzs);

    verbose('[',($mz_file eq '-' ? 'STDIN' : $mz_file),'] ',
	    'mz file done.  Time taken: [',scalar(markTime()),' Seconds].');
  }


##
## Read in the peaks.txt file and put the normalized intensities of the peaks
## into the $peaks 2D array with the first dimension being sample index and
## second dimension being all the normalized intensities of the peaks
##

$line_num     = 0;
$verbose_freq = 100;
my $peaks     = [];
my $num_cols  = 0;

#Open the peaks file
if(defined($peaks_file) && $peaks_file ne '' && open(PEAKS,$peaks_file))
  {
    verbose('[',($peaks_file eq '-' ? 'STDIN' : $peaks_file),'] ',
	    'Opened peaks file.');

    #For each line in the current input file
    while(getLine(*PEAKS))
      {
	$line_num++;
	verboseOverMe('[',($peaks_file eq '-' ? 'STDIN' : $peaks_file),'] ',
		      "Reading line: [$line_num].") unless($line_num %
							   $verbose_freq);

	next if(/^\s*#/ || /^\s*$/);

	chomp;
	s/^\s+//;
	s/\s+$//;
	if(/[^\d\.\+\-e\s]/i)
	  {
	    my @bad_chars = (/([^\d\.\+\-e\s])/g);
	    error("Invalid characters: [@bad_chars] found on line ",
		  "[$line_num] in file: [$peaks_file]: [$_].  Skipping.");
	    next;
	  }
	my @row = split(/\s+/,$_);
	if(scalar(@row) != $num_cols)
	  {
	    if($num_cols == 0)
	      {$num_cols = scalar(@row)}
	    else
	      {
		error("The number of columns on line: [$line_num]: [",
		      scalar(@row),
		      "] is not equal to the number of columns on previous ",
		      "line: [$num_cols] in file: [$peaks_file].  Cannot ",
		      "proceed.");
		quit(3);
	      }
	  }

	push(@$peaks,[@row]);
      }

    close(PEAKS);

    verbose('[',($peaks_file eq '-' ? 'STDIN' : $peaks_file),'] ',
	    'peaks file done.  Time taken: [',scalar(markTime()),' Seconds].');

    if($num_cols != scalar(@mzs) && scalar(@mzs) != 0)
      {
	error("The number of mz's in the mz file: [",scalar(@mzs),"] is not ",
	      "equal to the number of columns in the peaks file: ",
	      "[$num_cols].");
	quit(4);
      }
    elsif(scalar(@input_files) != 0 && scalar(@input_files) != scalar(@$peaks))
      {
	error("The number of spectra files: [",scalar(@input_files),
	      "] is not equal to the number of rows in the peaks file: [",
	      scalar(@$peaks),"].");
	quit(5);
      }
  }
elsif(defined($peaks_file) && $peaks_file ne '')
  {
    #Report an error and iterate if there was an error
    error("Unable to open peak file: [$peaks_file].\n$!");
  }




##
## Read in the preprocessed.txt file and put the baseline subtracted
## intensities of the entire spectrum into the $preprocessed array of hashes
## with the first dimension being sample index and second dimension being a
## hash of mz keys and raw intensity values
##

$line_num        = 0;
$verbose_freq    = 100;
my $preprocessed = [];
my(@allmzs);
my $num_samps    = 0;

#Open the preprocessed file
if(defined($preprocessed_file) && $preprocessed_file ne '' &&
   open(PREPRO,$preprocessed_file))
  {
    verbose('[',($preprocessed_file eq '-' ? 'STDIN' : $preprocessed_file),
	    '] Opened preprocessed file.');

    #For each line in the current input file
    while(getLine(*PREPRO))
      {
	$line_num++;
	verboseOverMe('[',($preprocessed_file eq '-' ? 'STDIN' :
			   $preprocessed_file),
		      "] Reading line: [$line_num].") unless($line_num %
							     $verbose_freq);

	next if(/^\s*#/ || /^\s*$/);

	chomp;
	s/^\s+//;
	s/\s+$//;
	if(/[^\d\.\+\-e\s]/i)
	  {
	    my @bad_chars = (/([^\d\.\+\-e\s])/g);
	    error("Invalid characters: [@bad_chars] found on line ",
		  "[$line_num] in file: [$preprocessed_file]: [$_].  ",
		  "Skipping.");
	    next;
	  }
	my @row = split(/\s+/,$_);
	my $mz  = 0;
	$mz     = shift(@row) if(scalar(@row));
	if(scalar(@row) != $num_samps)
	  {
	    if($num_samps == 0)
	      {$num_samps = scalar(@row)}
	    else
	      {
		error("The number of columns on line: [$line_num]: [",
		      scalar(@row),
		      "] is not equal to the number of columns on previous ",
		      "line: [$num_samps] in file: [$preprocessed_file].  ",
		      "Cannot proceed.");
		quit(6);
	      }
	  }
	if(scalar(@$peaks) && scalar(@$peaks) != $num_samps)
	  {
	    error("The number of samples/rows in the peaks file: ",
		  "[$peaks_file]: [",scalar(@$peaks),"] is not equal to the ",
		  "number of samples/columns in the preprocessed file: ",
		  "[$preprocessed_file]: [$num_samps].");
	    quit(7);
	  }
	my $index = 0;
	foreach my $intensity (@row)
	  {
	    if(scalar(@$preprocessed) < scalar(@row))
	      {push(@$preprocessed,{$mz=>$intensity})}
	    else
	      {$preprocessed->[$index]->{$mz} = $intensity}
	    $index++;
	  }
      }

    close(PREPRO);

    verbose('[',($preprocessed_file eq '-' ? 'STDIN' : $preprocessed_file),
	    '] preprocessed file done.  Time taken: [',scalar(markTime()),
	    ' Seconds].');

    if(scalar(keys(%{$preprocessed->[0]})) < $num_peaks && $num_peaks != 0)
      {
	error("The number of mz's in the mz file: [$num_peaks] is not ",
	      "greater than the number of mz's in the preprocessed file: ",
	      "[",scalar(@{$preprocessed->[0]}),"].");
	quit(7);
      }
    elsif(scalar(@input_files) != 0 &&
	  scalar(@input_files) != scalar(@$preprocessed))
      {
	error("The number of spectra files: [",scalar(@input_files),"] is ",
	      "not equal to the number of samples in the preprocessed file: [",
	      scalar(@$preprocessed),"].");
	quit(5);
      }
  }
elsif(defined($preprocessed_file) && $preprocessed_file ne '')
  {
    #Report an error and iterate if there was an error
    error("Unable to open peak file: [$preprocessed_file].\n$!");
  }


#Set the number of samples (num_samps) if preprocessed.txt was not supplied
#Note, it was already set if it was supplied
if(!defined($preprocessed_file) || $preprocessed_file eq '')
  {$num_samps = scalar(@$peaks)}



#If there are no original spectrum files, make up some file names to append
#suffixes to
if(scalar(@input_files) == 0)
  {foreach my $sample_num (1..$num_samps)
     {push(@input_files,$sample_num)}}

#Check to make sure previously generated output files won't be over-written
#Note, this does not account for output redirected on the command line
if(!$overwrite && defined($preprocessed_suffix))
  {
    my $existing_outfiles = [];

    push(@$existing_outfiles,"$peaks_file$table_suffix")
      if(defined($peaks_file) && $peaks_file ne '' &&
	 -e "$peaks_file$table_suffix");
    push(@$existing_outfiles,"$preprocessed_file$table_suffix")
      if(defined($preprocessed_file) && $preprocessed_file ne '' &&
	 -e "$preprocessed_file$table_suffix");
    push(@$existing_outfiles,"$preprocessed_file$gnuplotcommand_suffix")
      if(defined($preprocessed_file) && $preprocessed_file ne '' &&
	 -e "$preprocessed_file$gnuplotcommand_suffix");

    foreach my $output_file (map {($_ eq '-' ? 'STDIN' : $_)} @input_files)
      {
	push(@$existing_outfiles,"$output_file$preprocessed_suffix")
	  if(defined($preprocessed_file) && $preprocessed_file ne '' &&
	     -e "$output_file$preprocessed_suffix");
	push(@$existing_outfiles,
	     "$output_file$preprocessed_suffix$peaks_suffix")
	  if(defined($preprocessed_file) && $preprocessed_file ne '' &&
	     -e "$output_file$preprocessed_suffix$peaks_suffix");
	push(@$existing_outfiles,"$output_file$normalized_suffix$peaks_suffix")
	  if(defined($peaks_file) && $peaks_file ne '' &&
	     -e "$output_file$normalized_suffix$peaks_suffix");
      }

    if(scalar(@$existing_outfiles))
      {
	error("The output files: [@$existing_outfiles] already exist.  ",
	      'Use --overwrite to force an overwrite of existing files.  ',
	      "E.g.:\n",getCommand(1),' --overwrite');
	exit(4);
      }
  }





#Create some flags for telling which files are being output to make the code
#cleaner
my $output_indiv_blr_tables = (defined($preprocessed_file) &&
			       $preprocessed_file ne '' ?
			       1 : 0);
my $output_indiv_norm_peaks = (defined($peaks_file) && $peaks_file ne '' ?
			       1 : 0);

my $output_indiv_blr_peaks  = (defined($preprocessed_file) &&
			       $preprocessed_file ne '' ?
			       1 : 0);

#For each input file
my $sample_index = 0; #This assumes files are submitted in the same order as in
                      #the peaks.txt and preprocessed.txt files
foreach my $input_file (@input_files)
  {
    #Individual output file names
    my $output_indiv_blr_table_file = "$input_file$preprocessed_suffix";
    my $output_indiv_norm_peak_file =
      "$input_file$normalized_suffix$peaks_suffix";
    my $output_indiv_blr_peak_file  =
      "$input_file$preprocessed_suffix$peaks_suffix";

    #If an output file name suffix has been defined
    if($output_indiv_blr_tables)
      {
	#Open the output file
	if(!open(OIBTF,">$output_indiv_blr_table_file"))
	  {
	    #Report an error and iterate if there was an error
	    error("Unable to open output file: ",
		  "[$output_indiv_blr_table_file].\n$!");
	  }
	else
	  {
	    verbose("[$output_indiv_blr_table_file] Opened output file.");

	    #Store info. about the run as a comment at the top of the output
	    #file
	    print OIBTF ('#',join("$nl#",split(/\n|\r/,getVersion())),$nl,
			 '#',scalar(localtime($^T)),$nl,
			 '#',$ENV{CWD} || $ENV{PWD},$nl,
			 '#',getCommand(1),$nl) unless($noheader);

	    print OIBTF (map {"$_\t$preprocessed->[$sample_index]->{$_}$nl"}
			 sort {$a <=> $b}
			 keys(%{$preprocessed->[$sample_index]}));

	    #Close the output file handle
	    close(OIBTF);
	    verbose("[$output_indiv_blr_table_file] Output file done.");
	  }
      }

    #If an output file name suffix has been defined
    if($output_indiv_norm_peaks)
      {
	#Open the output file
	if(!open(OINPF,">$output_indiv_norm_peak_file"))
	  {
	    #Report an error and iterate if there was an error
	    error("Unable to open output file: ",
		  "[$output_indiv_norm_peak_file].\n$!");
	  }
	else
	  {
	    verbose("[$output_indiv_norm_peak_file] Opened output file.");

	    #Store info. about the run as a comment at the top of the output
	    #file
	    print OINPF ('#',join("$nl#",split(/\n|\r/,getVersion())),$nl,
			 '#',scalar(localtime($^T)),$nl,
			 '#',$ENV{CWD} || $ENV{PWD},$nl,
			 '#',getCommand(1),$nl) unless($noheader);

	    print OINPF (map {"$mzs[$_]\t$peaks->[$sample_index]->[$_]$nl"}
			 (0..(scalar(@mzs) - 1)));

	    #Close the output file handle
	    close(OINPF);
	    verbose("[$output_indiv_norm_peak_file] Output file done.");
	  }
      }

    #If an output file name suffix has been defined
    if($output_indiv_blr_peaks)
      {
	#Open the output file
	if(!open(OIBPF,">$output_indiv_blr_peak_file"))
	  {
	    #Report an error and iterate if there was an error
	    error("Unable to open output file: ",
		  "[$output_indiv_blr_peak_file].\n$!");
	  }
	else
	  {
	    verbose("[$output_indiv_blr_peak_file] Opened output file.");

	    #Store info. about the run as a comment at the top of the output
	    #file
	    print OIBPF ('#',join("$nl#",split(/\n|\r/,getVersion())),$nl,
			 '#',scalar(localtime($^T)),$nl,
			 '#',$ENV{CWD} || $ENV{PWD},$nl,
			 '#',getCommand(1),$nl) unless($noheader);

	    print OIBPF
	      (map {"$mzs[$_]\t$preprocessed->[$sample_index]->{$mzs[$_]}$nl"}
	       (0..(scalar(@mzs) - 1)));

	    #Close the output file handle
	    close(OIBPF);
	    verbose("[$output_indiv_blr_peak_file] Output file done.");
	  }
      }

    $sample_index++;
  }



#Create some flags for telling which files are being output to make the code
#cleaner
my $output_norm_peak_table  = (defined($peaks_file) && $peaks_file ne '' ?
			       1 : 0);
my $output_blr_peak_table   = (defined($preprocessed_file) &&
			       $preprocessed_file ne '' ?
			       1 : 0);
my $output_gnuplot_commands = (defined($preprocessed_file) &&
			       $preprocessed_file ne '' ?
			       1 : 0);

my $output_norm_peak_table_file  = "$peaks_file$table_suffix";
my $output_blr_peak_table_file   = "$preprocessed_file$table_suffix";
my $output_gnuplot_commands_file = "$preprocessed_file$gnuplotcommand_suffix";

if($output_norm_peak_table)
  {
    #Open the output file
    if(!open(ONPTF,">$output_norm_peak_table_file"))
      {
	#Report an error and iterate if there was an error
	error("Unable to open output file: [$output_norm_peak_table_file].\n",
	      $!);
      }
    else
      {
	verbose("[$output_norm_peak_table_file] Opened output file.");

	  #Store info. about the run as a comment at the top of the output file
	  print ONPTF ('#',join("$nl#",split(/\n|\r/,getVersion())),$nl,
		       '#',scalar(localtime($^T)),$nl,
		       '#',$ENV{CWD} || $ENV{PWD},$nl,
		       '#',getCommand(1),$nl) unless($noheader);

	print ONPTF ("\t",join("\t",@mzs),$nl);
	my $file_index = 0;
	foreach my $sample_array (@$peaks)
	  {
	    print ONPTF ("$input_files[$file_index]\t",
			 join("\t",@$sample_array),
			 $nl);
	    $file_index++;
	  }

	#Close the output file handle
	close(ONPTF);
	verbose("[$output_norm_peak_table_file] Output file done.");
      }
  }

if($output_blr_peak_table)
  {
    #Open the output file
    if(!open(OBPTF,">$output_blr_peak_table_file"))
      {
	#Report an error and iterate if there was an error
	error("Unable to open output file: [$output_blr_peak_table_file].\n",
	      $!);
      }
    else
      {
	verbose("[$output_blr_peak_table_file] Opened output file.");

	#Store info. about the run as a comment at the top of the output file
	print OBPTF ('#',join("$nl#",split(/\n|\r/,getVersion())),$nl,
		     '#',scalar(localtime($^T)),$nl,
		     '#',$ENV{CWD} || $ENV{PWD},$nl,
		     '#',getCommand(1),$nl) unless($noheader);

	print OBPTF ("\t",join("\t",@mzs),$nl);
	my $file_index = 0;
	foreach my $sample_hash (@$preprocessed)
	  {
	    print OBPTF ("$input_files[$file_index]\t",
			 join("\t",map {$sample_hash->{$_}} @mzs),
			 $nl);
	    $file_index++;
	  }

	#Close the output file handle
	close(OBPTF);
	verbose("[$output_blr_peak_table_file] Output file done.");
      }
  }

if($output_gnuplot_commands)
  {
    #Open the output file
    if(!open(OGCF,">$output_gnuplot_commands_file"))
      {
	#Report an error and iterate if there was an error
	error("Unable to open output file: [$output_gnuplot_commands_file].\n",
	      $!);
	next;
      }
    else
      {verbose("[$output_gnuplot_commands_file] Opened output file.")}

    #Store info. about the run as a comment at the top of the output file
    print OGCF ('#',join("$nl#",split(/\n|\r/,getVersion())),$nl,
		'#',scalar(localtime($^T)),$nl,
		'#',$ENV{CWD} || $ENV{PWD},$nl,
		'#',getCommand(1),$nl) unless($noheader);

    print OGCF ("plot ",
		join(", ",
		     map {"\"$preprocessed_file\" using 1:$_ with lines 1"}
		     (2..($num_samps + 1))),
	        $nl);

    #Close the output file handle
    close(OGCF);
    verbose("[$output_gnuplot_commands_file] Output file done.");
  }


#Report the number of errors, warnings, and debugs on STDERR
if(!$quiet && ($verbose                     ||
	       $DEBUG                       ||
	       defined($main::error_number) ||
	       defined($main::warning_number)))
  {
    print STDERR ("\n",'Done.  EXIT STATUS: [',
		  'ERRORS: ',
		  ($main::error_number ? $main::error_number : 0),' ',
		  'WARNINGS: ',
		  ($main::warning_number ? $main::warning_number : 0),
		  ($DEBUG ?
		   ' DEBUGS: ' .
		   ($main::debug_number ? $main::debug_number : 0) : ''),' ',
		  'TIME: ',scalar(markTime(0)),"s]\n");

    if($main::error_number || $main::warning_number)
      {print STDERR ("Scroll up to inspect errors and warnings.\n")}
  }

##
## End Main
##






























##
## Subroutines
##

##
## This subroutine prints a description of the script and it's input and output
## files.
##
sub help
  {
    my $script = $0;
    my $lmd = localtime((stat($script))[9]);
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #$software_version_number  - global
    #$created_on_date          - global
    $created_on_date = 'UNKNOWN' if($created_on_date eq 'DATE HERE');

    #Print a description of this program
    print << "end_print";

$script version $software_version_number
Copyright 2008
Robert W. Leach
Created: $created_on_date
Last Modified: $lmd
Center for Computational Research
701 Ellicott Street
Buffalo, NY 14203
rwleach\@ccr.buffalo.edu

* WHAT IS THIS: This program takes files produced by PrepMS, version 1.0 and
                produces a series of output files for use in plotting &
                analysis or as inputs to other MS processing tools.

* INPUT FORMAT: This script takes 3 files produced by PrepMS, version 1.0 as-is
                - no editing, plus a series of sample names (used for naming
                per-sample output files).  Note that the order of the file
                names on the command line must be the same as the order
                represented in the PrepMS files.

                The mz.txt file has a m/z value on each line, e.g.:

		5035.94502223
		5036.46441033
		5036.77605605
		5037.39937641
		5039.06175257
		5040.10087695
		5046.23389138
		...

                The peaks.txt file contains a normalized intensity of each
                identified peak, separated by tabs, one sample per line, e.g.:

		  0.00000024      0.00000224      0.00000196      ...
		  0.00000182      0.00000205      0.00000140      ...
		  0.00000123      0.00000189      0.00000134      ...
		  0.00000031      0.00000039      0.00000004      ...
		...

                Note there is leading white space after each tab, but this is
                not important to this script.

                preprocessed.txt is a tab-delimited file where the first column
                is an m/z value and each subsequent column is a set of baseline
                removed intensity values of each individual sample.  E.g.:

		5035.42566092     0.00000000      0.00000000     ...
		5035.52953104     0.00000000     16.04232200     ...
		5035.63340223    16.38040172     50.11538790     ...
		5035.73727449     0.00000000     77.18796281     ...
		...

* OUTPUT FORMATS: A series of tab-delimited files are output (depending on
                  which PrepMS files were input).  Per-sample files are
                  generated containing mz and peak intensities (one file with
                  normalized intensities and one with un-normalized (but
                  baseline-removed) intensities).  Another set of per-sample
                  files is output containing the entire baseline-removed mz and
                  intensity values.

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

    #Grab the first version of each option from the global GetOptHash
    my $options = '[' .
      join('] [',
	   grep {$_ ne '-i'}           #Remove REQUIRED params
	   map {my $key=$_;            #Save the key
		$key=~s/\|.*//;        #Remove other versions
		$key=~s/(\!|=.|:.)$//; #Remove trailing getopt stuff
		$key = (length($key) > 1 ? '--' : '-') . $key;} #Add dashes
	   grep {$_ ne '<>'}           #Remove the no-flag parameters
	   keys(%$GetOptHash)) .
	     ']';

    print << "end_print";
USAGE: $script -i "input file(s)" $options
       $script $options < input_file
end_print

    if($no_descriptions)
      {print("`$script` for expanded usage.\n")}
    else
      {
        print << 'end_print';

     -r|--preprocessed-   REQUIRED Optional if -p is supplied.  This is the
        file                       preprocessed.txt file output by PrepMS,
                                   version 1.0.  See --help for a description
                                   of the file format and contents.
     -p|--peaks-file      REQUIRED Optional if -r is supplied.  This is the
                                   peaks.txt file output by PrepMS, version
                                   1.0.  See --help for a description
                                   of the file format and contents.  Note this
                                   option must be supplied with -m.
     -m|--mz-file         OPTIONAL [none] Required if -p is supplied.  This is
                                   the mz.txt file output by PrepMS, version
                                   1.0.  See --help for a description of the
                                   file format and contents.
     -s|--spectra-files*  OPTIONAL This is a series of space-separated sample
                                   file names which will be used as strings in
                                   this script.  The files will not be opened
                                   or read - only the names will be used.  In
                                   fact, the files don't even have to exist.
                                   If this option is not supplied, samples will
                                   be numbered starting from 1.  The names must
                                   be submitted in the same order as they are
                                   represented in the PrepMS files above.  Perl
                                   glob characters (e.g. '*') are acceptable
                                   inside quotes (e.g. -i "*.txt *.text") if
                                   you want to make the sample names using an
                                   existing directory of original spectra files
                                   used as input to PrepMS.
                                   *No flag required.
     -o|--outfile-suffix  OPTIONAL [.preprocessed] This suffix is added to the
                                   spectra file names to use as output files.
                                   Each file will contain an mz and un-
                                   normalized intensity column of data.  See
                                   --help for a description of the output file
                                   format.  These files will not be output if
                                   -r is not supplied.  See --help for a
                                   description of the file format and contents.
     --on|--normal-suffix OPTIONAL [.normalized] This suffix is added to the
                                   spectra file names to use as output files.
                                   Each file will contain an mz and normalized
                                   intensity column of data.  See --help for a
                                   description of the output file format.
                                   These files will not be output if -p is not
                                   supplied.  See --help for a description of
                                   the file format and contents.
     --op|--peaks-suffix  OPTIONAL [.peaks] If -o is supplied, an additional
                                   file will be output for each sample in the
                                   same format as output using the -o
                                   parameter, except it will contain only peak
                                   values.  This suffix is appended to the -o
                                   suffix.  Additionally, if --on is supplied,
                                   another file will be output for each sample
                                   in the same format as output using the --on
                                   parameter, except it will contain only peak
                                   values.  This suffix is appended to the --on
                                   suffix.  See --help for a description of the
                                   file format and contents.
     --ot|--table-suffix  OPTIONAL [.peaktable] If -o is supplied, this suffix
                                   will be appended to the -r file to output a
                                   file containing a table of peak data where
                                   each row is a sample and each column is an
                                   m/z value.  Each data point will be an un-
                                   normalized intensity.  If --on is supplied,
                                   this suffix will be appended to the -p file
                                   to output a file containing a table of peak
                                   data where each row is a sample and each
                                   column is an m/z value.  Each data point
                                   will be a normalized intensity.  See --help
                                   for a description of the file format and
                                   contents.
     --og|--gnuplot-      OPTIONAL [.gnuplotcommands] If -r is supplied, this
          suffix                   suffix will be appended to the -r file and
                                   will contain gnuplot commands to plot all
                                   the un-normalized data.  You will have to
                                   edit the file manually to tweak line colors.
                                   See gnuplot help for instructions on how to
                                   use it.  See --help for a description of the
                                   file format and contents.
     --force|--overwrite  OPTIONAL Force overwrite of existing output files.
                                   Only used when the -o option is supplied.
     --ignore             OPTIONAL Ignore critical errors & continue
                                   processing.  (Errors will still be
                                   reported.)  See --force to not exit when
                                   existing output files are found.
     --verbose            OPTIONAL Verbose mode.  Cannot be used with the quiet
                                   flag.  Verbosity level can be increased by
                                   supplying a number (e.g. --verbose 2) or by
                                   supplying the --verbose flag multiple times.
     --quiet              OPTIONAL Quiet mode.  Suppresses warnings and errors.
                                   Cannot be used with the verbose or debug
                                   flags.
     --help|-?            OPTIONAL Help.  Print an explanation of the script
                                   and its input/output files.
     --version            OPTIONAL Print software version number.  If verbose
                                   mode is on, it also prints the template
                                   version used to standard error.
     --debug              OPTIONAL Debug mode.  Adds debug output to STDERR and
                                   prepends trace information to warning and
                                   error messages.  Cannot be used with the
                                   --quiet flag.  Debug level can be increased
                                   by supplying a number (e.g. --debug 2) or by
                                   supplying the --debug flag multiple times.
     --noheader           OPTIONAL Suppress commented header output.  Without
                                   this option, the script version, date/time,
                                   and command-line information will be printed
                                   at the top of all output files commented
                                   with '#' characters.
     --windows-compatible OPTIONAL [Off] This flag will cause the script to
                                   output carriage returns instead of newline
                                   characters.

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

#    #Ignore the overwrite flag if STDOUT will be mixed in
#    $overwrite_flag = 0 if(isStandardOutputToTerminal());

    #Read in the message
    my $verbose_message = join('',grep {defined($_)} @_);

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
	    warning('Hard returns and tabs cause overwrite mode to not work ',
		    'properly.');
	    $main::verbose_warning = 1;
	  }
      }
    else
      {chomp($verbose_message)}

    #If this message is not going to be over-written (i.e. we will be printing
    #a \n after this verbose message), we can reset verbose_length to 0 which
    #will cause $main::last_verbose_size to be 0 the next time this is called
    if(!$overwrite_flag)
      {$verbose_length = 0}
    #If there were \r's in the verbose message submitted (after the last \n)
    #Calculate the verbose length as the largest \r-split string
    elsif($verbose_message =~ /\r[^\n]*$/)
      {
	my $tmp_message = $verbose_message;
	$tmp_message =~ s/.*\n//;
	($verbose_length) = sort {length($b) <=> length($a)}
	  split(/\r/,$tmp_message);
      }
    #Otherwise, the verbose_length is the size of the string after the last \n
    elsif($verbose_message =~ /([^\n]*)$/)
      {$verbose_length = length($1)}

    #If the buffer is not being flushed, the verbose output doesn't start with
    #a \n, and output is to the terminal, make sure we don't over-write any
    #STDOUT output
    #NOTE: This will not clean up verbose output over which STDOUT was written.
    #It will only ensure verbose output does not over-write STDOUT output
    #NOTE: This will also break up STDOUT output that would otherwise be on one
    #line, but it's better than over-writing STDOUT output.  If STDOUT is going
    #to the terminal, it's best to turn verbose off.
    if(!$| && $verbose_message !~ /^\n/ && isStandardOutputToTerminal())
      {
	#The number of characters since the last flush (i.e. since the last \n)
	#is the current cursor position minus the cursor position after the
	#last flush (thwarted if user prints \r's in STDOUT)
	my $num_chars = tell(STDOUT) - sysseek(STDOUT,0,1);

	#If there have been characters printed since the last \n, prepend a \n
	#to the verbose message so that we do not over-write the user's STDOUT
	#output
	if($num_chars > 0)
	  {$verbose_message = "\n$verbose_message"}
      }

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
    my @error_message = split(/\n/,join('',grep {defined($_)} @_));
    push(@error_message,'') unless(scalar(@error_message));
    pop(@error_message) if(scalar(@error_message) > 1 &&
			   $error_message[-1] !~ /\S/);

    $main::error_number++;
    my $leader_string = "ERROR$main::error_number:";

    #Assign the values from the calling subroutines/main
    my(@caller_info,$line_num,$caller_string,$stack_level,$script);
    if($DEBUG)
      {
	$script = $0;
	$script =~ s/^.*\/([^\/]+)$/$1/;
	@caller_info = caller(0);
	$line_num = $caller_info[2];
	$caller_string = '';
	$stack_level = 1;
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
	$leader_string .= "$script:$caller_string";
      }

    $leader_string .= ' ';

    #Figure out the length of the first line of the error
    my $error_length = length(($error_message[0] =~ /\S/ ?
			       $leader_string : '') .
			      $error_message[0]);

    #Put location information at the beginning of the first line of the message
    #and indent each subsequent line by the length of the leader string
    print STDERR ($leader_string,
		  shift(@error_message),
		  ($verbose &&
		   defined($main::last_verbose_state) &&
		   $main::last_verbose_state ?
		   ' ' x ($main::last_verbose_size - $error_length) : ''),
		  "\n");
    my $leader_length = length($leader_string);
    foreach my $line (@error_message)
      {print STDERR (' ' x $leader_length,
		     $line,
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size  = 0;
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
    my @warning_message = split(/\n/,join('',grep {defined($_)} @_));
    push(@warning_message,'') unless(scalar(@warning_message));
    pop(@warning_message) if(scalar(@warning_message) > 1 &&
			     $warning_message[-1] !~ /\S/);

    my $leader_string = "WARNING$main::warning_number:";

    #Assign the values from the calling subroutines/main
    my(@caller_info,$line_num,$caller_string,$stack_level,$script);
    if($DEBUG)
      {
	$script = $0;
	$script =~ s/^.*\/([^\/]+)$/$1/;
	@caller_info = caller(0);
	$line_num = $caller_info[2];
	$caller_string = '';
	$stack_level = 1;
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
	$leader_string .= "$script:$caller_string";
      }

    $leader_string .= ' ';

    #Figure out the length of the first line of the error
    my $warning_length = length(($warning_message[0] =~ /\S/ ?
				 $leader_string : '') .
				$warning_message[0]);

    #Put leader string at the beginning of each line of the message
    #and indent each subsequent line by the length of the leader string
    print STDERR ($leader_string,
		  shift(@warning_message),
		  ($verbose &&
		   defined($main::last_verbose_state) &&
		   $main::last_verbose_state ?
		   ' ' x ($main::last_verbose_size - $warning_length) : ''),
		  "\n");
    my $leader_length = length($leader_string);
    foreach my $line (@warning_message)
      {print STDERR (' ' x $leader_length,
		     $line,
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size  = 0;
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
			 warning('Carriage returns were found in your file ',
				 'and replaced with hard returns.');
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
		     warning('Carriage returns were found in your file ',
			     'and replaced with hard returns.');
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
		warning('Carriage returns were found in your file and ',
			'replaced with hard returns.');
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
    my @debug_message = split(/\n/,join('',grep {defined($_)} @_));
    push(@debug_message,'') unless(scalar(@debug_message));
    pop(@debug_message) if(scalar(@debug_message) > 1 &&
			   $debug_message[-1] !~ /\S/);

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
    print STDERR ($leader_string,
		  shift(@debug_message),
		  ($verbose &&
		   defined($main::last_verbose_state) &&
		   $main::last_verbose_state ?
		   ' ' x ($main::last_verbose_size - $debug_length) : ''),
		  "\n");
    my $leader_length = length($leader_string);
    foreach my $line (@debug_message)
      {print STDERR (' ' x $leader_length,
		     $line,
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
	error('Supplied time mark index is larger than the size of the ',
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


sub getVersion
  {
    my $full_version_flag = $_[0];
    my $template_version_number = '1.37';
    my $version_message = '';

    #$software_version_number  - global
    #$created_on_date          - global
    #$verbose                  - global

    my $script = $0;
    my $lmd = localtime((stat($script))[9]);
    $script =~ s/^.*\/([^\/]+)$/$1/;

    if($created_on_date eq 'DATE HERE')
      {$created_on_date = 'UNKNOWN'}

    $version_message  = join((isStandardOutputToTerminal() ? "\n" : ' '),
			     ("$script Version $software_version_number",
			      " Created: $created_on_date",
			      " Last modified: $lmd"));

    if($full_version_flag)
      {
	$version_message .= (isStandardOutputToTerminal() ? "\n" : ' - ') .
	  join((isStandardOutputToTerminal() ? "\n" : ' '),
	       ('Generated using perl_script_template.pl ' .
		"Version $template_version_number",
		' Created: 5/8/2006',
		' Author:  Robert W. Leach',
		' Contact: robleach@ccr.buffalo.edu',
		' Company: Center for Computational Research',
		' Copyright 2008'));
      }

    return($version_message);
  }

#This subroutine is a check to see if input is user-entered via a TTY (result
#is non-zero) or directed in (result is zero)
sub isStandardInputFromTerminal
  {return(-t STDIN || eof(STDIN))}

#This subroutine is a check to see if prints are going to a TTY.  Note,
#explicit prints to STDOUT when another output handle is selected are not
#considered and may defeat this subroutine.
sub isStandardOutputToTerminal
  {return(-t STDOUT && select() eq 'main::STDOUT')}

#This subroutine exits the current process.  Note, you must clean up after
#yourself before calling this.  Does not exit is $ignore_errors is true.  Takes
#the error number to supply to exit().
sub quit
  {
    my $errno = $_[0];
    if(!defined($errno))
      {$errno = -1}
    elsif($errno !~ /^[+\-]?\d+$/)
      {
	error("Invalid argument: [$errno].  Only integers are accepted.  Use ",
	      "error() or warn() to supply a message, then call quit() with ",
	      "an error number.");
	$errno = -1;
      }

    debug("Exit status: [$errno].");

    exit($errno) if(!$ignore_errors || $errno == 0);
  }
