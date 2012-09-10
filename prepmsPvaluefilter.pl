#!/usr/bin/perl -w

#Generated using perl_script_template.pl 1.37
#Robert W. Leach
#rwleach@ccr.buffalo.edu
#Center for Computational Research
#Copyright 2008

#These variables (in main) are used by getVersion() and usage()
my $software_version_number = '1.0';
my $created_on_date         = '3/31/2009';

##
## Start Main
##

use strict;
use Getopt::Long;
use Statistics::TTest;

#Declare & initialize variables.  Provide default values here.
my($outfile_suffix,$report_suffix,$group_suffix); #Not defined so a user can overwrite the input file
my @input_files         = ();
my $current_output_file = '';
my $report_output_file  = '';
my $help                = 0;
my $version             = 0;
my $overwrite           = 0;
my $noheader            = 0;
my $confidence_cutoff   = 95; #This is a percent value
my $group1_hash         = {};
my $group2_hash         = {};

#These variables (in main) are used by the following subroutines:
#verbose, error, warning, debug, getCommand, quit, and usage
my $preserve_args = [@ARGV];  #Preserve the agruments for getCommand
my $verbose       = 0;
my $quiet         = 0;
my $DEBUG         = 0;
my $ignore_errors = 0;

my $GetOptHash =
  {# ENTER YOUR COMMAND LINE PARAMETERS HERE AS BELOW

   'i|input-file=s'        => sub {push(@input_files,   #REQUIRED unless <> is
				        sglob($_[1]))}, #         supplied
   '<>'                    => sub {push(@input_files,   #REQUIRED unless -i is
				        sglob($_[0]))}, #         supplied
   'r|report-suffix=s'     => \$report_suffix,          #OPTIONAL [undef]
   'c|confidence-cutoff=s' => \$confidence_cutoff,      #OPTIONAL [95]
   'g1|clinical-group-1=s' => sub {$group1_hash->{$_}=0 #OPTIONAL [first half
				     foreach(sglob($_[1]))},      #of samples]
   'g2|clinical-group-2=s' => sub {$group2_hash->{$_}=0 #OPTIONAL [second half
				     foreach(sglob($_[1]))},      #of samples]
   'g|group-suffix=s'      => \$group_suffix,           #OPTIONAL [undef]
   'o|outfile-suffix=s'    => \$outfile_suffix,         #OPTIONAL [undef]
   'force|overwrite'       => \$overwrite,              #OPTIONAL [Off]
   'ignore'                => \$ignore_errors,          #OPTIONAL [Off]
   'verbose:+'             => \$verbose,                #OPTIONAL [Off]
   'quiet'                 => \$quiet,                  #OPTIONAL [Off]
   'debug:+'               => \$DEBUG,                  #OPTIONAL [Off]
   'help|?'                => \$help,                   #OPTIONAL [Off]
   'version'               => \$version,                #OPTIONAL [Off]
   'noheader'              => \$noheader,               #OPTIONAL [Off]
  };

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
    print(getVersion($verbose),"\n");
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

#Put standard input into the input_files array if standard input has been redirected in
if(!isStandardInputFromTerminal())
  {
    push(@input_files,'-');

    #Warn the user about the naming of the outfile when using STDIN
    if(defined($outfile_suffix))
      {warning('Input on STDIN detected along with an outfile suffix.  Your ',
	       'output file will be named STDIN',$outfile_suffix)}
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

#Make sure there is input
if(scalar(@input_files) == 0)
  {
    error('No input files detected.');
    usage(1);
    quit(3);
  }

#Check to make sure previously generated output files won't be over-written
#Note, this does not account for output redirected on the command line
if(!$overwrite && defined($outfile_suffix))
  {
    my $existing_outfiles = [];
    foreach my $output_file (map {($_ eq '-' ? 'STDIN' : $_) . $outfile_suffix}
			     @input_files)
      {push(@$existing_outfiles,$output_file) if(-e $output_file)}

    if(scalar(@$existing_outfiles))
      {
	error("The output files: [@$existing_outfiles] already exist.  ",
	      'Use --overwrite to force an overwrite of existing files.  ',
	      "E.g.:\n",getCommand(1),' --overwrite');
	exit(4);
      }
  }

#Allow them to put a single percent sign
$confidence_cutoff =~ s/\%//;
if($confidence_cutoff !~ /^(\d+\.?\d*|\d*\.?\d+)$/)
  {
    error("Invalid confidence cutoff: [$confidence_cutoff].");
    usage();
    quit(-1);
  }

verbose('Run conditions: ',getCommand(1));

foreach my $sample_id (keys(%$group1_hash))
  {
    if(-e $sample_id)
      {
	delete($group1_hash->{$sample_id});
	my $tmp_group1_hash = getGroup($sample_id);
	$group1_hash->{$_} = $tmp_group1_hash->{$_}
	  foreach(keys(%$tmp_group1_hash));
      }
  }

foreach my $sample_id (keys(%$group2_hash))
  {
    if(-e $sample_id)
      {
	delete($group2_hash->{$sample_id});
	my $tmp_group2_hash = getGroup($sample_id);
	$group2_hash->{$_} = $tmp_group2_hash->{$_}
	  foreach(keys(%$tmp_group2_hash));
      }
  }

if((scalar(keys(%$group1_hash)) < 2 && scalar(keys(%$group1_hash)) != 0) ||
   (scalar(keys(%$group2_hash)) < 2 && scalar(keys(%$group2_hash)) != 0))
  {
    error("Too few members in one or more of your clinical groups.  Each ",
	  "group must contain greater than 2 members (i.e. sample IDs from ",
	  "your input file).");
    quit(-2);
  }

if(scalar(grep {exists($group1_hash->{$_})} keys(%$group2_hash)))
  {
    error("Your clinical groups have some members in common: [",
	  join(',',grep {exists($group1_hash->{$_})} keys(%$group2_hash)),
	  "].  Each group must contain different members.");
    quit(-3);
  }

#If output is going to STDOUT instead of output files with different extensions
#or if STDOUT was redirected, output run info once
verbose('[STDOUT] Opened for all output.') if(!defined($outfile_suffix));

#Store info. about the run as a comment at the top of the output file if
#STDOUT has been redirected to a file
if(!isStandardOutputToTerminal() && !$noheader)
  {print('#',getVersion(),"\n",
	 '#',scalar(localtime($^T)),"\n",
	 '#',getCommand(1),"\n");}

#For each input file
foreach my $input_file (@input_files)
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
	    error("Unable to open output file: [$current_output_file].\n$!");
	    next;
	  }
	else
	  {verbose("[$current_output_file] Opened output file.")}

	#Select the output file handle
	select(OUTPUT);

	#Store info. about the run as a comment at the top of the output file
	print('#',getVersion(),"\n",
	      '#',scalar(localtime($^T)),"\n",
	      '#',getCommand(1),"\n") unless($noheader);
      }

    #Open the input file
    if(!open(INPUT,$input_file))
      {
	#Report an error and iterate if there was an error
	error("Unable to open input file: [$input_file].\n$!");
	next;
      }
    else
      {verbose('[',($input_file eq '-' ? 'STDIN' : $input_file),'] ',
	       'Opened input file.')}

    #If a report file name suffix has been defined
    if(defined($report_suffix))
      {
	##
	## Open the next report file
	##

	#Set the report output file name
	$report_output_file = ($input_file eq '-' ? 'STDIN' : $input_file)
	  . $report_suffix;

	#Open the report file
	if(!open(REPORT,">$report_output_file"))
	  {
	    #Report an error if there was an error
	    error("Unable to open report file: [$report_output_file].\n$!");
	  }
	else
	  {verbose("[$report_output_file] Opened report file.")}

	#Store info. about the run as a comment at the top of the report file
	print REPORT ('#',getVersion(),"\n",
		      '#',scalar(localtime($^T)),"\n",
		      '#',getCommand(1),"\n") unless($noheader);
      }

    my $line_num     = 0;
    my $verbose_freq = 100;
    my $group1       = [];
    my $group2       = [];
    my $mzs_parsed   = 0;
    my $group1_sample_ids = [];
    my $group2_sample_ids = [];

    #For each line in the current input file
    while(getLine(*INPUT))
      {
	$line_num++;
	verboseOverMe('[',($input_file eq '-' ? 'STDIN' : $input_file),'] ',
		      "Reading line: [$line_num].") unless($line_num %
							   $verbose_freq);

	next if(/^\s*#/ || /^\s*$/);

	if(/^\s+([\d\s\.]+)$/)
	  {
	    #Header line with mz's
	    my $mzs = $1;
	    $mzs =~ s/\s+$//;
	    foreach my $mz (split(/\s+/,$mzs))
	      {
		push(@$group1,[$mz]);
		push(@$group2,[$mz]);
		$mzs_parsed++;
	      }
	  }
	elsif(/^([^\t]*\S)\s+([\d\s\.]+)$/)
	  {
	    #Sample line
	    my $id   = $1;
	    my $sigs = $2;

	    #Keep track of the sample IDs in case the user wants to output
	    #group files. (-g)
	    if(exists($group1_hash->{$id}))
	      {push(@$group1_sample_ids,$id)}
	    elsif(scalar(keys(%$group2_hash)) == 0 ||
		  exists($group2_hash->{$id}))
	      {push(@$group2_sample_ids,$id)}

	    if($mzs_parsed == 0)
	      {
		error("The m/z column headers appear to be missing or are ",
		      "malformed for file: [$input_file].  Unable to ",
		      "proceed.  Skipping to next file.");
		last;
	      }

	    my $cnt      = 0;
	    my $num_sigs = 0;
	    foreach my $sig (split(/\s+/,$sigs))
	      {
	        if(exists($group1_hash->{$id}))
		  {
		    push(@{$group1->[$cnt]},$sig);
		    $num_sigs++;
		  }
		elsif(scalar(keys(%$group2_hash)) == 0 ||
		      exists($group2_hash->{$id}))
		  {
		    push(@{$group2->[$cnt]},$sig);
		    $num_sigs++;
		  }
		else
		  {error("The sample ID: [$id] was not found in group 1 or ",
			 "group 2.  Skipping.")}
		$cnt++;
	      }

	    if($num_sigs != $mzs_parsed)
	      {
		error("Line [$line_num] of file [$input_file] had ",
		      "[$num_sigs] signal intensities successfully parsed, ",
		      "but there are [$mzs_parsed] m/z's in the column ",
		      "header row.  There should be the same number.");
	      }
	  }
	else
	  {error("Unrecognized line: [$_].  Skipping.")}
      }

    close(INPUT);

    verbose('[',($input_file eq '-' ? 'STDIN' : $input_file),'] ',
	    'Input file done.  Time taken: [',scalar(markTime()),' Seconds].');

    my $filter_hash = {};

    if($mzs_parsed == 0)
      {next}
    else
      {
	if(defined($report_suffix))
	  {print REPORT ("#M/Z\tP-Value\t\%Confidence\tFilter-Result\n")}

	foreach my $mzarrayindex (0..$#{$group1})
	  {
	    my $values1 = $group1->[$mzarrayindex];
	    my $mz1 = shift(@$values1);
	    my $values2 = $group2->[$mzarrayindex];
	    my $mz2 = shift(@$values2);

	    #Split group2 in half if there were no groups supplied on the
	    #command line
	    if(scalar(keys(%$group1_hash)) == 0 || scalar(@$values1) == 0)
	      {
		warning("Sample IDs for clinical group 1 were either not ",
			"supplied or were not the same as in the mass spec ",
			"data file: [$input_file].  The clinical groups will ",
			"be arbitrarily divided in half (default behavior).");
		$values1 = [splice(@$values2,0,int(scalar(@$values2)/2))];
		$group1_sample_ids =
		  [splice(@$group2_sample_ids,
			  0,
			  int(scalar(@$group2_sample_ids)/2))];
	      }

	    if(scalar(@$values1) < 2 || scalar(@$values2) < 2)
	      {
		error("There are not enough samples in either clinical group ",
		      "1: [",scalar(@$values1),"] or clinical group 2: [",
		      scalar(@$values2),"] to procede.  There must be at ",
		      "least 2 samples in each.");
		last;
	      }

	    if($mz1 ne $mz2)
	      {error("These m/z's don't match: [$mz1 $mz2].  Make sure your ",
		     "first row in the input file [$input_file] is a list of ",
		     "m/z values.  The first column (over the sample IDs) ",
		     "must be empty.\n")}

	    my $ttest = new Statistics::TTest;
	    $ttest->set_significance($confidence_cutoff);
	    $ttest->load_data($values1,$values2);

	    my $significance = $ttest->significance();
	    my $result = $ttest->null_hypothesis();
	    my $passed_ttest = ($result =~ /not/ ? 0 : 1);

	    if($passed_ttest)
	      {$filter_hash->{$mz1} = 1}

	    if(defined($report_suffix))
	      {print REPORT ("$mz1\t",$ttest->t_prob(),
			     "\t",(100 - $ttest->t_prob()*100),
			     "\t",($passed_ttest ? "Passed" : "Failed"),"\n")}
	  }

	#Open the input file again
	if(!open(INPUT,$input_file))
	  {
	    #Report an error and iterate if there was an error
	    error("Unable to open input file: [$input_file].\n$!");
	    next;
	  }
	else
	  {verbose('[',($input_file eq '-' ? 'STDIN' : $input_file),'] ',
		   'Opened input file.')}

	my $column_hash = {};

	#For each line in the current input file
	while(getLine(*INPUT))
	  {
	    $line_num++;
	    verboseOverMe('[',($input_file eq '-' ? 'STDIN' : $input_file),
			  "] Reading line: [$line_num].")
	      unless($line_num % $verbose_freq);

	    if(/^\s*#/ || /^\s*$/)
	      {
		unless($noheader)
		  {print}
		next;
	      }

	    if(/^(\s+)([\d\s\.]+)$/)
	      {
		#Header line with mz's
		my $spacer = $1;
		my $mzs    = $2;
		$mzs =~ s/\s+$//;
		my $c = 0;
		print($spacer,
		      join("\t",grep {$c++;
				      $column_hash->{$c} = 1
					if(exists($filter_hash->{$_}));
				      exists($filter_hash->{$_})}
			   split(/\s+/,$mzs)),"\n");
	      }
	    elsif(/^([^\t]*\S\s+)([\d\s\.]+)$/)
	      {
		#Sample line
		my $id   = $1;
		my $sigs = $2;

		my $num_sigs = 0;
		my $c        = 0;
		print($id,join("\t",grep {$c++;exists($column_hash->{$c})}
			       split(/\s+/,$sigs)),"\n")
	      }
	    else
	      {error("Unrecognized line: [$_].  Skipping.")}
	  }

	close(INPUT);

	verbose('[',($input_file eq '-' ? 'STDIN' : $input_file),'] ',
		'Input file done.  Time taken: [',scalar(markTime()),
		' Seconds].');
      }

    #If an output file name suffix is set
    if(defined($report_suffix))
      {
	#Close the report file handle
	close(REPORT);

	verbose("[$report_output_file] Output file done.");
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

    #Note this will create the same two output files multiple times (once per
    #input file), but I don't care.
    if(defined($group_suffix) && $group_suffix ne '')
      {
	my $group1_file = $input_files[0] . '.1' . $group_suffix;
	#Open the output group file
	if(!open(GOUT,">$group1_file"))
	  {error("Unable to open output group file: [$group1_file].\n$!")}
	else
	  {
	    verbose("[$group1_file] Opened group 1 output file.");

	    print GOUT (join("\n",@$group1_sample_ids),"\n");

	    close(GOUT);

	    verbose("[$group1_file] Output group 1 file done.");
	  }
	
	my $group2_file = $input_files[0] . '.2' . $group_suffix;
	#Open the output group file
	if(!open(GOUT,">$group2_file"))
	  {error("Unable to open output group file: [$group2_file].\n$!")}
	else
	  {
	    verbose("[$group2_file] Opened group 2 output file.");

	    print GOUT (join("\n",@$group2_sample_ids),"\n");

	    close(GOUT);

	    verbose("[$group2_file] Output group 2 file done.");
	  }
      }
  }

verbose("[STDOUT] Output done.") if(!defined($outfile_suffix));

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

sub getGroup
  {
    my $input_file = $_[0];

    #Open the input file
    if(!open(GROUP,$input_file))
      {
	#Report an error and iterate if there was an error
	error("Unable to open Group file: [$input_file].\n$!");
	return({});
      }
    else
      {verbose('[',($input_file eq '-' ? 'STDIN' : $input_file),'] ',
	       'Opened group file.')}

    my $line_num     = 0;
    my $verbose_freq = 100;
    my $hash = {};

    #For each line in the current input file
    while(getLine(*GROUP))
      {
	$line_num++;
	verboseOverMe('[',($input_file eq '-' ? 'STDIN' : $input_file),'] ',
		      "Reading line: [$line_num].") unless($line_num %
							   $verbose_freq);

	next if(/^\s*#/ || /^\s*$/);

	if(/^\s*(\S.*?)\s*$/)
	  {$hash->{$1} = 1}
      }

    close(GROUP);

    verbose('[',($input_file eq '-' ? 'STDIN' : $input_file),'] ',
	    'Group file done.  Time taken: [',scalar(markTime()),' Seconds].');

    return($hash);
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

* WHAT IS THIS: This script takes a tab-delimited file as output by either
                prepms_tabler.pl or reBinPrepMS.pl along with a list of sample
                IDs which belong to one of two clinical groups* in the data and
                outputs a report file of each m/z's (i.e. column's) P-Value and
                a percent confidence that the data has a statistically
                significant separation.  It uses a Student's T-Test to generate
                the P-Values and confidence values.  The script also outputs
                the same input table, except all columns that fall below the
                optionally supplied confidence cutoff are removed/filtered.
                Here's are examples of 3 runs using 3 different files as output
                by prepms_tabler.pl and reBinPrepMS.pl:

                prepmsPvaluefilter.pl peaks.txt.peaktable --g1 "4 5 6 7 8 9 10"
                prepmsPvaluefilter.pl preprocessed.txt.peaktable file --g1 "1 2 3 4 5"
                prepmsPvaluefilter.pl peaks.txt.peaktable.rebinned --g1 "6 7 8"

                * The second group is assumed since only two groups are
                  supported, but can also be supplied.  It is recommended to
                  supply both groups for error-checking.

* INPUT FORMAT: Tab-delimited file.  Comments are allowed as long as comment
                lines start with the '#' character.  The first row must start
                with a tab (or other white space) and contain the M/Z values.
                Each subsequent row must have a sample ID followed by intensity
                values.  The formats output by prepms_tabler.pl and
                reBinPrepMS.pl are compatible.  The compatible output files
                have extensions of either '.peaktable' or '.rebinned' by
                default.  Here is an example of a portion of a file:

                        5037.62458      5048.23586      5052.89092
                1       0.00000290      0.00000566      0.00000653
                2       0.00000238      0.00000520      0.00000555
                3       0.00000320      0.00000413      0.00000413
                4       0.00000307      0.00000658      0.00000685
                5       0.00000386      0.00000755      0.00000698

* GROUP FORMAT: The --g1 and --g2 flags can take either a set of space-
                separated sample IDs on the command line or a group file
                containing the sample IDs belonging to a single group.  The
                format of the group file is simply a sample ID on each line.
                For example:

                1
                2
                3
                4
                5

* OUTPUT FORMAT: The output format is the same as the input format except some
                 columns are removed based on the confidence cutoff.  The
                 format of the report file is a 4 column, tab-delimited file.
                 The columns are m/z, P-value, confidence, and filter status.

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

     -i|--input-file*     REQUIRED Space-separated input file(s inside quotes).
                                   Standard input via redirection is
                                   acceptable.  Perl glob characters (e.g. '*')
                                   are acceptable inside quotes (e.g.
                                   -i "*.txt *.text").  See --help for a
                                   description of the input file format.
                                   *No flag required.
     -c|--confidence-     OPTIONAL [95] This is the cutoff given to the T-Test,
        cutoff                     below which columns of intensity values at a
                                   particular m/z are filtered out.  It
                                   represents the percent probability that the
                                   clinical sample separation (see --g1 and
                                   --g2) is statistically significant, meaning
                                   that a 95% confidence level for a particular
                                   m/z means that we must be at least 95% sure
                                   that the values are statistically different
                                   between the two clinical groups and is not
                                   likely to have happened randomly.  Note, be
                                   aware that you must ensure that you've
                                   removed all biases from your data before
                                   using this filter.
     --g1|--clinical-     OPTIONAL [first half of samples] Space-separated list
          group-1                  of sample IDs (the first column in your
                                   input file) which belong to clinical group
                                   1.  For example, this may represent the
                                   control/normal samples.  Note, this scipt is
                                   only intended to work on 2 clinical groups,
                                   no more, no less.  You must supply 2 or more
                                   samples for each group.  If there are an odd
                                   number of samples, the middle sample goes in
                                   clinical group 2.  You can also submit a
                                   group file containing a sample ID on each
                                   line belonging to group 1.  See --help for a
                                   description of the file format.
     --g2|--clinical-     OPTIONAL [second half of samples] Space-separated
          group-2                  list of sample IDs (the first column in your
                                   input file) which belong to clinical group
                                   2.  For example, this may represent the
                                   treated samples.  Note, this scipt is only
                                   intended to work on 2 clinical groups, no
                                   more, no less.  You must supply 2 or more
                                   samples for each group.  This group 2 is
                                   assumed to be everything else that's not in
                                   group 1 if --g2 is not supplied.  However,
                                   it is recommended you supply both groups for
                                   error-checking.  You can also submit a
                                   group file containing a sample ID on each
                                   line belonging to group 1.  See --help for a
                                   description of the file format.
     -o|--outfile-suffix  OPTIONAL [nothing] This suffix is added to the input
                                   file names to use as output files.
                                   Redirecting a file into this script will
                                   result in the output file name to be "STDIN"
                                   with your suffix appended.  See --help for a
                                   description of the output file format.
     -r|--report-suffix   OPTIONAL [nothing] This suffix is added to the input
                                   file names to use as report files.  If this
                                   suffix is not supplied, the report is output
                                   on STDOUT before the filtered table is
                                   output..  See --help for a description of
                                   the report file format.
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
