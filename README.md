
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SimcypConsultancy

<!-- badges: start -->
<!-- badges: end -->

The SimcypConsultancy package provides tools for analyzing and reporting
PBPK data from the Simcyp Simulator. Our goals in making this package
are to:

1.  Increase accuracy and efficiency
2.  Easily and accurately make informative tables from Simulator output
3.  Create informative, professional-quality graphs
4.  Perform additional analyses of PBPK data

## Installation

You can install the SimcypConsultancy package from GitHub like this:

    devtools::install_github(repo = "shirewoman2/Consultancy", 
                             upgrade = "never")

Current version: 3.6.1

# An overview of what SimcypConsultancy functions do

Below are descriptions of all the functions in the package, and the
functions most users will want to use most frequently are in bold and
listed first in each section.

## Get help on using the SimcypConsultancy package

- **launch_package_index()** - Launch the shiny app for showing an
  annotated index of what the SimcypConsultancy package can do
- **make_example_PK_input()** - Examples for supplying PK parameters to
  the pk_table or calc_PK_ratios functions for the argument PKparameters
- show_code_example() - IN PROGRESS - Show some examples for the code to
  use for making PK tables or concentration-time plots

## Get information about your simulations

- **extractExpDetails_mult()** - Extract experimental details for
  multiple files at once
- **annotateDetails()** - Annotate Simcyp Simulator experimental details
- qc_sims() - Make an Excel file for QCing simulations
- extractExpDetails() - Extract details about the experimental design
- extractExpDetails_DB() - Extract simulation experimental details from
  a database file - UNDER CONSTRUCTION!!!!!
- extractExpDetails_XML() - Extract experimental details for multiple
  Simcyp Simulator workspace files at once

## PK tables and PK calculations

### Make typical PK tables

- **pk_table()** - Make tables of PK values from Simulator output Excel
  files

### Make PK comparisons not natively available in the Simulator outputs

- **calc_PK_ratios_mult()** - Calculate the ratio of PK parameters
  between multiple pairs of simulations
- calc_PK_ratios() - Calculate the ratio of PK parameters between two
  simulations

#### Helper functions for PK tables

- convert_units() - Convert concentration and time units as needed
- extractPK() - Extract PK data for specific parameters from a simulator
  output Excel file
- extractPK_DB() - Extract PK from a Simcyp Simulator database file –
  UNDER CONSTRUCTION!!!!

### Calculate PK

- calc_PK() - Calculate basic PK parameters for concentration-time data
- recalc_PK() - Recalculate the PK for specific concentration-time
  profiles after running calc_PK  
- elimFit() - Calculate the elimination rate(s) for a concentration-time
  curve
- trapRule() - Calculate the AUC using the trapezoidal rule
- noncompAUC() - Calculate the AUC using the trapezoidal rule

## Formatting and saving tables

- **formatTable_Simcyp()** - Format tables according to Simcyp
  Consultancy Team specifications, e.g.,
- format_table_simple() - Format a table rather simply to look nice in a
  Word file
- save_table_to_Word() - Save a bespoke PK table to Word using the
  pksummary_mult rmarkdown template

## Other tables you can make

- **make_simulation_directory()** - Make a directory of simulations
  indicating which simulation file is located in which folder
- insert_copyright() - Insert the standard Certara UK Limited copyright
  box that appears in compound summary files - Yes, this is actually a
  table, even though it just looks like a box.
- make_Simcyp_inputs_table() - Summarize a PBPK model using parameters
  pulled from simulations - UNDER CONSTRUCTION
- make_trial_design_table() - Create a table describing a simulation
  trial design.

## Data visualization

### Make concentration-time plots or variations thereof

- **ct_plot()** - Concentration-time plots to match Consultancy
  template  
- **ct_plot_overlay()** - Overlay multiple data sets onto a single
  concentration-time graph
- **enz_plot()** - Plots of enzyme abundance changes over time to match
  Consultancy template
- **enz_plot_overlay()** - Overlay multiple data sets onto a single
  enzyme-abundance graph
- **runShiny()** - Launch the shiny app for making PK summary tables and
  concentration-time plots
- ct_plot3() - Concentration-time plots of the full time range, first
  dose, and last dose
- ct_plot_1stlast() - Make concentration-time plots of the 1st and last
  doses only
- ct_plot_mult() - Make graphs for multiple Simulator output files at
  once
- ct_plot_obs() - Plot observed concentration-time data
- fm_plot() - Make a plot of dynamic fm and fe values

#### Helper functions for concentration-time plots

- **extractConcTime_mult()** - Pull concentration-time data from
  multiple Simcyp Simulator output files
- extractConcTime() - Extract concentration-time data from a simulator
  output Excel file
- extractConcTime_DB() - Extract concentration-time data from a Simcyp
  Simulator database file – UNDER CONSTRUCTION!!!
- extractEnzAbund() - Extract enzyme abundance data from a simulator
  output Excel file
- extractEnzAbund_mult() - Pull enzyme-abundance data from multiple
  Simcyp Simulator output files
- extractFmFe() - Extract fm and fe valuesthat change with time from a
  Simulator output Excel file
- match_obs_to_sim() - Match observed concentration-time data to the
  correct simulated concentration-time data
- extract_fu() - Extract fu,plasma values that change with time from a
  Simulator output Excel file. UNDER CONSTRUCTION!

### Make observed concentration-time plots

- ct_plot_obs() - Plot observed concentration-time data

#### Observed concentration-time plot helper functions

- create_doses() - Create a csv file of dosing rows for XML files
- create_doses_from_file() - Create a csv file of dosing rows for XML
  files using a study file to get subject information
- clean_obs_data() - Tidy up messy observed concentration-time data.
  UNDER CONSTRUCTION!!!
- extractObsConcTime() - Extract observed concentration-time data from
  an Excel file
- extractObsConcTime_mult() - Extract observed concentration-time data
  from multiple Excel files
- extractObsConcTime_xlsx() - INTERNAL - Extract observed conc-time data
  from an xlsx file
- extractObsConcTime_XML() - INTERNAL - Extract observed conc-time data
  from an xml file
- format_obs_for_XML() - Format generic observed data for making an XML
  overlay file

### Other data visualizations that can help you analyze your data

#### Comparing demographics

- **demog_table()** - Make a table of demographics for a set of
  simulations
- **demog_plot_sim()** - Make plots for comparing populations across
  simulations
- demog_plot_SO() - Make a set of plots showing the demographics used in
  a simulation and optionally compare them to observed demographics.
  UNDER CONSTRUCTION.
- extractDemog() - Extract simulated demographic data from the
  Demographic Data tab of a Simcyp Simulator ouptut Excel file

#### Forest plots

- **forest_plot()** - Create a forest plot
- extractForestData() - Extract pertinent data from Simulator output
  files for creating forest plots

#### Misc. data visualization functions

- **so_graph()** - Graph of simulated vs. observed PK
- **sensitivity_plot()** - Make graphs of sensitivity analysis results
- **trial_means_plot()** - Make graphs comparing center statistic and
  variability in PK across trials and, optionally, against observed PK
  data as well
- **checkSS()** - Create a graph of simulated concentrations to check
  for whether a perpetrator drug is at steady-state when the victim drug
  is dosed
- fm_treemap() - For comparing fm values, make a treemap as opposed to
  the standard and arguably inferior pie chart
- dissolution_profile_plot() - Make a graph of the dissolution profiles
  of a compound
- graph_boxplot() - Make boxplots or boxplots overlaid with individual
  points
- ontogeny_plot() - Graph the ontogenies of drug-metabolizing enzymes
  and transporters
- release_profile_plot() - Make a graph of the release profiles of a
  compound

### Helper functions for graphing

- blues() - Create a vector of blues
- blueGreens()- Create a vector of blues fading into greens
- purples() - Create a vector of purples
- chartreuse() - Create a vector of yellow-greens
- rainbow() - Create a vector of a rainbow of colors
- SimcypColors() - Simcyp colors used in the PowerPoint template
- log10_breaks() - Create nice breaks for graphs of log-transformed data
- make_log_breaks() - Make pretty breaks and labels for a log axis
- theme_consultancy() - Apply the standard Consultancy Team graphing
  styles to your graph
- scale_x_time() - Automatically scale a ggplot graph x axis for time

## Helper functions for manipulating typical SimcypConsultancy objects

- add_sims() - Add simulations to an R object
- filter_sims() - Selectively include or omit specific simulations from
  an R object
- sort_sims() - Sort the simulation file order in an object

## Miscellany

### Fit induction models to *in vitro* data

- **inductFit()** - Fit induction data to calculate EC50, Emax, and/or
  slope

### Getting data from a pdf table

- pdf_to_csv() - Convert a page from a pdf file to a csv file

### Examining DDIs

- **list_interactions()** - Find all the possible drug-drug interactions
  between compounds included in a single simulation

#### Tidbits for making quick calculations

- calcKi() - Calculate the Ki of an inhibitor with the Cheng-Prusoff
  equation
- centerBin() - Cut a set of numeric data into bins based on the middle
  value of the bin
- check_doseint() - Check whether the AUC interval matches the dosing
  interval and create warnings if not
- check_file_name() - Check whether the simulation file names comply
  with regulatory requirements for submitting PBPK data
- countWords() - Useful for guessing at good widths for columns in Excel
  output
- cutNumeric() - Cut numeric data into bins with vector output
- dateConv() - Convert dates from numbers to Date
- difftime_sim() - Calculate the time difference between two times
  formatted as in simulator output
- getReferences() - Pull references from a DataRec folder
- gm_mean() - Calculate the geometric mean. Related: gm_conf, gm_CV,
  gm_sd, confInt.
- rounding
  - Several functions for rounding, including round_consultancy,
    round_up, round_down, round_down_unit, etc.
- timeConv() - Convert times from numbers to POSIXct
- str_comma() - Collapse a vector of character strings into one readable
  string, separated by commas and by “and” and “or”

## Interact with the Simulator or with workspaces

#### Read or change workspaces

*NB: Most functions that actually change workspaces are only available
in the beta version of the package.*

- **make_xml_path_mine()** - Make the user name in the path of observed
  data overlay XML files and fixed trial design XML files match that of
  the current user
- **remove_file_timestamp()** - Remove date/time stamps from Excel
  results files created by the Autorunner
- change_wksz_interactions() - Change interaction parameters in Simcyp
  Simulator workspace files
- change_wksz_trial_design() - Change a limited set of trial-design
  parameters in Simcyp Simulator workspace files. UNDER CONSTRUCTION.
- change_xml_path() - Set the user name in the path of observed data
  overlay XML files and fixed trial design XML files
- detect_file_timestamp() - Figure out what date/time stamps on Excel
  results files would be removed by the function remove_file_timestamp

#### Helper functions that call on the Simcyp package

- check_simulator_initialized() - check whether the Simcyp Simulator has
  been initialized

## Internal functions you’ll probably never need to use but listed just in case you’ve seen them and are curious about what they do

- addObsPoints() - INTERNAL PACKAGE USE: Add observed data points to a
  concentration-time plot
- calc_dosenumber() - Calculate the dose number for each time in a set
  of concentration-time data given the experimental details for that
  simulation
- check_include_dose_num() - INTERNAL PACKAGE USE ONLY. Check on whether
  to include the dose number in the PK table headings
- ct_x_axis() - Set up x axis in a conc-time plot
- ct_y_axis() - Set up y axis in a conc-time plot
- deannotateDetails() - De-annotate a data.frame of experimental details
  that was previously annotated
- determine_myperpetrator() - Determine the (optionally pretty) name of
  all perpetrators included in a simulation, all concatenated nicely
- eCT_harmonize() - Harmonize the compound names on sim_data_xl to made
  finding the correct rows easier later.
- eCT_pulldata() - Pull the appropriate data from the harmonized Excel
  tab
- eCT_readxl() - Read into memory an Excel sheet for extracting
  conc-time data.
- extractAUCXtab() - FOR INTERNAL USE ONLY: Extract PK data from a
  generic AUC tab, e.g., NOT one formatted like the AUC tab from Simcyp
  V21 and earlier
- format_scripts() - Format common PBPK model parameters to have
  appropriate subscripts and superscripts. UNDER CONSTRUCTION.
- guess_col_widths() - INTERNAL guess appropriate column widths
- harmonize_details() - FOR INTERNAL PACKAGE USE. Take as input an
  existing data.frame or list of experimental details and make it be a
  list with standard items
- harmonize_PK_names() - Harmonize user input for PK parameters
- make_color_set() - Create a set of colors for use in graphs
- make_ct_caption() - INTERNAL - make caption for conc-time plots
- make_table_annotations() - Make table headings and captions for use
  internally when saving PK tables to Word
- make_text_legos() - INTERNAL - Assemble pieces of text for headings,
  captions and trial design descriptions
- move_asterisks() - INTERNAL - Move asterisks around so that you can
  put asterisks around that string and things will be italicized
  appropriately
- pk_table_subfun() - Subfunction for pk_table. FOR INTERNAL PACKAGE
  USE.
- prettify_column_names() - Convert the column names in a table of PK
  parameters into more human-readable names
- prettify_compound_name() - Make a compound name prettier and more
  human readable
- renameStats() - Rename the statistics listed in Simulator output to
  R-friendly versions
- round_opt() - INTERNAL: Rounding function
- scale_x_time_setup() - Get the information needed to automatically
  scale a ggplot graph x axis for time
- set_aesthet() - Set up certain aesthetics a conc-time plot
- set_boundary_colors() - INTERNAL: Create a set of boundary colors
  based on the color set and the boundaries the user needs.
- tidy_input_PK() - Tidy input PK data as needed
- tidy_PKparameters_names() - INTERNAL USE. tidy the column names of
  PKparameters supplied to, e.g., pk_table.
- tidyPop() - Tidying Simcyp simulator population names for reports,
  etc.
- wrapn() - INTERNAL PACKAGE USE. Wrap warning text and add a carriage
  return at the end.
