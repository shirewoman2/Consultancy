
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

<!-- badges: start -->
<!-- badges: end -->

The SimcypConsultancy package provides tools for analyzing and reporting
PBPK data from the Simcyp Simulator. Our goals in making this package
are to:

1.  Increase accuracy and efficiency overall
2.  Easily and accurately make informative tables from Simulator output
3.  Create scientifically informative, professional-quality graphs
4.  Perform additional analyses of PBPK data

## Installation

You can install the SimcypConsultancy package from GitHub like this:

    devtools::install_github(repo = "shirewoman2/Consultancy", 
                             upgrade = "never")

Current version: 3.8.3

**NOTE:** The SimcypConsultancy package requires that you have tidyverse
loaded.

A great place to start for getting help:

    launch_package_index()

# How to use this document

You do *not* need to read this document from start to finish to follow
it. Instead, please do skip around to only the parts that interest you.

- [Time savers for setting up
  workspaces](#timesavers-for-setting-up-workspaces)  
- [Check how simulations were set up, e.g., for
  QCing](#check-how-simulations-were-set-up-eg-for-qcing)  
- [Make concentration-time plots](#make-concentration-time-plots)
- [Make overlaid concentration-time
  plots](#make-overlaid-concentration-time-plots)  
- [Make graphs of enzyme abundance](#make-graphs-of-enzyme-abundance)  
- [Make nice-looking PK summary tables
  automatically](#make-nice-looking-pk-summary-tables-automatically)  
- [Automatically extract and graph sensitivity analysis
  data](#automatically-extract-and-graph-sensitivity-analysis-data)  
- [Fit induction data](#fit-induction-data)  
- [A list of what SimcypConsultancy functions
  do](#a-list-of-what-simcypconsultancy-functions-do)

We’re using example files in this document, but you generally can
substitute whatever Simcyp Simulator files you’re working with instead
of the examples. For help on any functions, type a question mark
followed by the name of the function. For example, here’s how to get
help on the ct_plot function:

`?ct_plot`

Let’s dive into some examples!

# Timesavers for setting up workspaces

If someone else set up some workspaces that they would like you to run
but those workspaces included observed data XML files on OneDrive, the
Simulator won’t be able to find that file unless you change the path to
include your user name instead of the original person’s user name.
Here’s how to quickly change all the paths for all the workspaces in a
folder so that the Simulator can find the observed data files with
*your* name in the path:

    make_xml_path_mine()

That will permanently change the workspaces, so please make sure that’s
what you want.

If you have run some simulations using the workflow tool in the
Simulator, it will include a date/time stamp on the output file name. To
remove those quickly, try this to remove that stamp from all the files
in your current folder:

    remove_file_timestamp()

The R Working Group has a few other tricks up our sleeves for changing
workspace parameters quickly, but they do require some care since they
permanently change workspace files and since only some parameters work
well when changed in an automated fashion. If you find yourself needing
to change something like the dose, the inhibition parameters, the
induction parameters, etc. for a bunch of workspaces and don’t want to
spend hours tediously opening, changing, saving, and closing them all
manually, please talk to Laura Shireman.

[Return to TOC](#how-to-use-this-document)

# Check how simulations were set up, e.g., for QCing

As we mentioned, one of our goals was to increase accuracy. For this
reason, several functions in the SimcypConsultancy package are set up
for checking or reporting data in an automated fashion.

## extractExpDetails example 1: Check on how all the simulations in a given folder were set up

First, we’ll collect some simulation experimental details and then save
that to an Excel file where we can easily compare parameters across
multiple simulations.

data.frame as a csv file. We’ll set “sim_data_files” in the code below
to NA to run this on *all* the simulator output Excel files in this
folder. Originally (and still by default), this function would create a
really wide table with one column for every experimental detail. Nikunj
had the excellent suggestion to allow for shaping the output to be a
long table instead of a really wide one, which can be cumbersome to
read, so let’s use that here. Setting “annotate_output = TRUE” will make
it long *and* will provide some context about what that detail is and
where it was found. This will take a moment to run.


    Details <- extractExpDetails_mult(sim_data_files = NA, 
                                      exp_details = "all")

## extractExpDetails example 2: Get specific details about how the simulations were set up overall

Now that we’ve got all the experimental details pulled into memory,
let’s ask for some specific information. Say you want to know some
details about how the experiment was set up such as the start and end
times of the simulation, what the substrates and inhibitors were and the
dosing regimens. You can use the function “annotateDetails” to do this
sort of task. Here’s how you’d get the first few rows with that
information:


    annotateDetails(Details, simulator_section = "Trial Design") %>%
       head() %>% formatTable_Simcyp(fontsize = 8)

![](inst/images/overviewtable1.png?raw=TRUE)

How about some information on what parameters were used for setting up
absorption but only for the substrate? We’re again only going to show
the first few rows of this, which is what “head() %\>%” does in the code
below.


    annotateDetails(Details, 
                    simulator_section = "Absorption",
                    compoundID = "substrate") %>% 
       head() %>% formatTable_Simcyp(fontsize = 8)

![](inst/images/overviewtable2.png?raw=TRUE)

To see all the possible experimental details, please see the file
“**[All possible experimental details to use with
extractExpDetails.csv](https://certaragbr.sharepoint.com/:x:/r/sites/SimcypPBPKConsultRFiles/Simcyp%20PBPKConsult%20R%20Files/SimcypConsultancy%20function%20examples%20and%20instructions/All%20possible%20experimental%20details%20to%20use%20with%20extractExpDetails.csv?d=wac18447b1ef557789cb09d8487781223&csf=1&web=1&e=yu3dId)**”
(Bold text is a link) or type this into the console:

     view(ExpDetailDefinitions)

For more options and examples, please see the help files for
extractExpDetails, extractExpDetails_mult, and annotateDetails and
please see
“**[Checking-simulation-experimental-details.docx](https://certaragbr.sharepoint.com/:w:/r/sites/SimcypPBPKConsultRFiles/Simcyp%20PBPKConsult%20R%20Files/SimcypConsultancy%20function%20examples%20and%20instructions/Checking%20simulation%20experimental%20details/Checking-simulation-experimental-details.docx?d=w63d17d07c1824c10ae55f18d78ecd94f&csf=1&web=1&e=J9hnEE)**”
(bold text is a link)

[Return to TOC](#how-to-use-this-document)

# Make concentration-time plots

The function “ct_plot” will allow you to automatically graph almost any
concentration-time data present in Simulator output Excel files. While
this function has many options, the default settings will generally
create decent graphs that comply with Consultancy Team report templates.

## ct_plot example 1: Substrate data showing simulated percentiles

Let’s make a graph of substrate concentration-time data in plasma,
including some observed data, and let’s save the output as “My conc-time
graph 1.png”.

First, extract the data from the Simulator output file and also from
either the observed-data XML overlay file or the Excel PE template file.
We’re using the function “extractConcTime” to extract the data and the
function “ct_plot” to make the graphs.


    LMV <- extractConcTime(sim_data_file = "letermovir MD.xlsx", 
                           obs_data_file = "Observed data files/letermovir obs data.xlsx", 
                           existing_exp_details = Details)

    ct_plot(ct_dataframe = LMV, 
            figure_type = "trial means", 
            save_graph = "My conc-time graph example 1.png")

![](inst/images/ct_plot5.png?raw=TRUE)

## ct_plot example 2: Zoom in on the last dose

Let’s zoom in on the last dose only and save this file, too, and this
time, let’s save it as a Word file so that we can get figure heading and
caption data already filled in and ready for copying and pasting into a
report. If you supply the output from “extractExpDetails_mult” here,
you’ll get a more informative figure heading and caption. Any text that
you’ll want to edit when you paste this into a report will be in bold.


    ct_plot(ct_dataframe = LMV, time_range = "last dose", 
            existing_exp_details = Details, 
            save_graph = "My conc-time graph example 2.docx")

![](inst/images/ct_plot2.png?raw=TRUE)

## ct_plot example 3: Substrate data with vs. without an inhibitor

Next, let’s make a graph of substrate concentration-time data in plasma
with and without the presence of an inhibitor. Let’s change the figure
type to show only the mean predicted concentrations.

Let’s include a legend, label it for the fact that our perpetrator
molecule is an inducer and not an inhibitor (the default), let’s only
make the semi-log plot here, and let’s save the file so that it is 3
inches high and 5 inches wide. (All of these options are described in
more detail in the example files in the folder “Concentration-time plots
1 - one sim at a time”.)


    MDZ <- extractConcTime(sim_data_file = "QD MDZ QD RIF.xlsx", 
                           obs_data_file = "Observed data files/QD MDZ QD RIF fake observed data.xlsx", 
                           existing_exp_details = Details)

    ct_plot(ct_dataframe = MDZ, time_range = "last dose", 
            figure_type = "means only", 
            linear_or_log = "log",
            legend_position = "right",
            legend_label = "Inducer",
            save_graph = "My conc-time graph example 3.docx", 
            fig_height = 3, fig_width = 5)

![](inst/images/ctplot_ex3.png?raw=TRUE)

For more options and examples, please see the help file for ct_plot
(type `?ct_plot` into the console) and see
**[“Concentration-time-plot-examples-1.docx”](https://certaragbr.sharepoint.com/:w:/r/sites/SimcypPBPKConsultRFiles/Simcyp%20PBPKConsult%20R%20Files/SimcypConsultancy%20function%20examples%20and%20instructions/Concentration-time%20plots%201%20-%20one%20sim%20at%20a%20time/Concentration-time-plot-examples-1.docx?d=wd31ccbfe966c4d6e938fc4be465b1602&csf=1&web=1&e=DsXp9L)**.

[Return to TOC](#how-to-use-this-document)

# Make overlaid concentration-time plots

You can also make concentration-time plots with multiple data sets
overlaid for easy comparisons.

## ct_plot_overlay example: Compare substrate concentrations in multiple tissues

First, let’s get some data. We’re using a variation on the function
“extractConcTime” called “extractConcTime_mult” that allows us to pull
data for multiple files, tissues, and compounds all at once.


    CT <- extractConcTime_mult(
       sim_data_files = c(
          "Example simulator output SD MDZ plus BID RTV.xlsx"), 
       tissues = c("plasma", "blood", "unbound plasma"), 
       ct_dataframe = "CT",
       compoundsToExtract = c("substrate", "inhibitor 1"), 
       existing_exp_details = Details)

Next, graph those data, coloring the lines by whether the perpetrator
was present and breaking up the graph into small multiples by the tissue
and by what compound we’re plotting. Let’s give a sense of the
variability of the data by showing transparent bands for the 5th to 95th
percentiles. We’ll save this graph as “My overlaid conc-time graph
example 1.docx”. Since we’re saving this as a Word file, we’ll again get
some figure heading and caption text filled in.


    ct_plot_overlay(ct_dataframe = CT, colorBy_column = Inhibitor,
                    facet1_column = Tissue, 
                    facet2_column = Compound, 
                    figure_type = "percentile ribbon",
                    color_set = "Set 1",
                    save_graph = "My overlaid conc-time graph example 1.docx", 
                    fig_width = 6, fig_height = 6)

![](inst/images/ctplotoverlay_ex1.png?raw=TRUE)

For more options and examples, please the help file for ct_plot_overlay
(type `?ct_plot_overlay` into the console) and see
**[“Concentration-time-plot-examples-3.docx”](https://certaragbr.sharepoint.com/:w:/r/sites/SimcypPBPKConsultRFiles/Simcyp%20PBPKConsult%20R%20Files/SimcypConsultancy%20function%20examples%20and%20instructions/Concentration-time%20plots%203%20-%20overlaying%20plots/Concentration-time-plot-examples-3.docx?d=w3059572742a740cf914cc28d97c15ceb&csf=1&web=1&e=2rdIei)**.

[Return to TOC](#how-to-use-this-document)

# Make graphs of enzyme abundance

You can apply most of the settings from the “ct_plot” function to
another function, “enz_plot”, which will automatically make enzyme
abundance plots for you. Just as with the ct_plot function, we’re first
going to extract the data using a separate function, this one called
“extractEnzAbund”.

For this graph, let’s include a legend, color the lines according to
whether the perpetrator is present using the colors we want, note in the
legend that the perpetrator is an inducer rather than the default
inhibitor, and let’s save the output, too.


    CYP3A4_liver <- extractEnzAbund(sim_data_file = "Example simulator output - MD MDZ MD EFV.xlsx", 
                                    existing_exp_details = Details)

    enz_plot(CYP3A4_liver,
             figure_type = "percentile ribbon", 
             line_type = "solid", 
             legend_position = "right", legend_label = "Inducer", 
             line_color = c("dodgerblue3", "darkorchid4"), 
             save_graph = "Enzyme abundance graph example.png", 
             fig_height = 3.5, fig_width = 6)

![](inst/images/enzplot_ex9.png?raw=TRUE)

For more options and examples, please the help file for enz_plot (type
`?enz_plot` into the console) and see
**[“Enzyme-abundance-plot-examples.docx”](https://certaragbr.sharepoint.com/:w:/r/sites/SimcypPBPKConsultRFiles/Simcyp%20PBPKConsult%20R%20Files/SimcypConsultancy%20function%20examples%20and%20instructions/Enzyme%20abundance%20plots/Enzyme-abundance-plot-examples.docx?d=w52b883659d5f460f989f6ba6bf09c990&csf=1&web=1&e=zG1dvd)**.

[Return to TOC](#how-to-use-this-document)

# Make nice-looking PK summary tables automatically

One of the tasks we would like to automate and make less prone to
copy/paste errors is creating tables for reports that summarize
simulated PK parameters. The function pk_table does that.

## pk_table example 1: Create a table of standard PK parameters for a simulation of a substrate alone

For this, we’ll use a file from a hands-on workshop demonstration with
letermovir. (We’ll return to the “checkDataSource” argument you see
inside the function call in a moment.)


    pk_table(
       sim_data_file = "letermovir MD.xlsx", 
       existing_exp_details = Details, 
       checkDataSource = FALSE) %>%
       formatTable_Simcyp()

![](inst/images/overview_pktable_ex1.png?raw=TRUE)

If you save the output to a Word file, which is what we recommend, you
will automatically get a table heading and caption, which will include
information about which compound, tissue, and simulation was used to
obtain the data, so the table will only include the columns “Statistic”,
“AUCtau” and “Cmax”.

## pk_table example 2: Adjust which summary stats are included and how they’re formatted

The default setting is to list geometric means and CVs, but you can see
arithmetic instead by specifying that with “mean_type”. Additionally,
let’s see the range of trial means (includeTrialMeans = TRUE) but not
the CVs (includeCV = FALSE).

    pk_table(
       sim_data_file = "letermovir MD.xlsx",
       mean_type = "arithmetic",
       includeTrialMeans = TRUE,
       includeCV = FALSE, 
       existing_exp_details = Details, 
       checkDataSource = FALSE) %>% formatTable_Simcyp()

![](inst/images/overview_pktable_ex2.png?raw=TRUE)

## pk_table example 3: Create a table of standard PK parameters for a DDI simulation

If a perpetrator molecule was included, the table will automatically
pull parameters sensible for that scenario. This time, let’s do ask to
check the data source. First, here’s the table:


    MyTable <- pk_table(
       sim_data_file = "QD MDZ QD RIF.xlsx", 
       existing_exp_details = Details, 
       checkDataSource = TRUE)

    MyTable$Table %>% formatTable_Simcyp() 

![](inst/images/overview_pktable_ex3.png?raw=TRUE)

Now, to see where those parameters came from, let’s look at the other
item that got included in our output when we said “checkDataSource =
TRUE”.


    MyTable$QC %>% formatTable_Simcyp(fontsize = 8)

![](inst/images/overview_pktable_ex4.png?raw=TRUE)

This is a data.frame listing the files, tabs, columns, and rows where
the data were found for the purposes of QCing.

For more options and examples, please see the help files for pk_table
and see
**[“PK-tables.docx”](https://certaragbr.sharepoint.com/:w:/r/sites/SimcypPBPKConsultRFiles/Simcyp%20PBPKConsult%20R%20Files/SimcypConsultancy%20function%20examples%20and%20instructions/Making%20PK%20tables/PK-tables.docx?d=weea77b5756754f949b38a3ebccb6518b&csf=1&web=1&e=yQ6xiD)**.

[Return to TOC](#how-to-use-this-document)

# Automatically extract and graph sensitivity analysis data

You can make sensitivity-analysis graphs using the function
sensitivity_plot, although one caveat is that this function does not
include any data for a 3rd graphical axis. Why? There isn’t really a
great way to make 3D graphs that would be interpretable here and,
perhaps more importantly, there is not, in our humble opinions, a good R
package for making 3D graphs.

You can save the output with the same options as those for the other
graphing functions. Let’s save this as a png file.

## sensitivity_plot example 1: Make an SA graph showing the effect of your independent variable on Cmax

We’ll supply the Excel file with the SA data to the argument “SA_file”,
we’ll tell R which dependent variable we want to see, give the graph a
title (the “\n” bit is how we can insert a manual carriage return), and
then specify whether to save the graph by supplying a file name to
“save_graph”.


    sensitivity_plot(SA_file = "SA on fa - mdz 5mg sd.xlsx",
                     dependent_variable = "Cmax", 
                     graph_title = "Sensitivity analysis:\nfa effect on Cmax", 
                     save_graph = "SA graph.png")

![](inst/images/overview_sa_ex1.png?raw=TRUE)

Other dependent variables are also possible; please see the help file
for all options.

## sensitivity_plot example 2: Make an SA graph with CL instead and also set the x-axis title manually

Here’s the same sensitivity analysis file but looking at oral clearance
and setting something manually for the independent variable label on the
x axis.


    sensitivity_plot(SA_file = "SA on fa - mdz 5mg sd.xlsx",
                     dependent_variable = "CLpo", 
                     ind_var_label = "fa for the substrate")

![](inst/images/overview_sa_ex2.png?raw=TRUE)

## sensitivity_plot example 3: Make an SA graph of plasma concentrations

And here is an example of plotting plasma concentrations from a
sensitivity-analysis file:


    sensitivity_plot(SA_file = "SA on fa - mdz 5mg sd.xlsx",
                     dependent_variable = "plasma", 
                     linear_or_log = "both vertical", 
                     time_range = c(0, 12))

![](inst/images/overview_sa_ex3.png?raw=TRUE)

Please see the help file for more information by typing
`?sensitivity_plot` into the console.

[Return to TOC](#how-to-use-this-document)

# Fit induction data

## inductFit example 1: Fit 4 models to induction data using the default settings

Let’s fit some induction data to each of 4 possible models: Indmax,
Indmax slope, slope, or sigmoid 3 parameter. For details on what these
are, please see the instructions **[fitting induction
data](https://certaragbr.sharepoint.com/:w:/r/sites/SimcypPBPKConsultRFiles/Simcyp%20PBPKConsult%20R%20Files/SimcypConsultancy%20function%20examples%20and%20instructions/Fitting%20induction%20data/Fitting-induction-data.docx?d=wf8cf2b1886c240abafdf6c6241563b3d&csf=1&web=1&e=v3AJRV)**.

For now, we’ll call on some pre-existing dummy data we made up for the
SimcypConsultancy package called `IndData`.

We’ll make an object “MyIndFits” to store the output and then look at
that output one piece at a time.

    MyIndFits <- inductFit(IndData, 
                           conc_column = Concentration_uM, 
                           fold_change_column = FoldInduction, 
                           model = "all", 
                           include_fit_stats = FALSE, 
                           rounding = "significant 3")

Now that we have run the function, let’s see the graphs.


    MyIndFits$Graph

![](inst/images/overview_indfit_ex1.png?raw=TRUE)

You can adjust the look of the graphs; please see the help file or the
example Word document for examples.

Let’s see the fitted parameters:


    MyIndFits$Fit %>% formatTable_Simcyp()

![](inst/images/overview_indfit_ex2.png?raw=TRUE)

[Return to TOC](#how-to-use-this-document)

# A list of what SimcypConsultancy functions do

Below are descriptions of all the functions in the package, and the
functions most users will want to use most frequently are in bold and
listed first in each section.

- [Get help on using the SimcypConsultancy
  package](#get-help-on-using-the-simcypconsultancy-package)  
- [Get information about your
  simulations](#get-information-about-your-simulations)  
- [PK tables and PK calculations](#pk-tables-and-pk-calculations)  
- [Other tables you can make](#other-tables-you-can-make)  
- [Concentration-time plots](#concentration-time-plots)  
- [Forest plots](#forest-plots)  
- [Fit induction models to *in vitro*
  data](#fit-induction-models-to-in-vitro-data)  
- [Evaluating model fidelity](#evaluating-model-fidelity)  
- [Examining variability between
  trials](#examining-variability-between-trials)  
- [Comparing demographics](#comparing-demographics)  
- [Other data visualizations that can help you analyze your
  data](#other-data-visualizations-that-can-help-you-analyze-your-data)  
- [Interact with the Simulator or with
  workspaces](#interact-with-the-simulator-or-with-workspaces)  
- [Helper functions for manipulating typical SimcypConsultancy
  objects](#helper-functions-for-manipulating-typical-simcypconsultancy-objects)  
- [Miscellany](#miscellany)

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

- **formatTable_Simcyp()** - Format tables according to Simcyp
  Consultancy Team specifications, e.g.,
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

## Concentration-time plots

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

## Forest plots

- **forest_plot()** - Create a forest plot
- extractForestData() - Extract pertinent data from Simulator output
  files for creating forest plots

## Fit induction models to *in vitro* data

- **inductFit()** - Fit induction data to calculate EC50, Emax, and/or
  slope

## Evaluating model fidelity

- **so_graph()** - Graph of simulated vs. observed PK

## Examining variability between trials

- **trial_means_plot()** - Make graphs comparing center statistic and
  variability in PK across trials and, optionally, against observed PK
  data as well

## Comparing demographics

- **demog_table()** - Make a table of demographics for a set of
  simulations
- **demog_plot_sim()** - Make plots for comparing populations across
  simulations
- demog_plot_SO() - Make a set of plots showing the demographics used in
  a simulation and optionally compare them to observed demographics.
  UNDER CONSTRUCTION.
- extractDemog() - Extract simulated demographic data from the
  Demographic Data tab of a Simcyp Simulator ouptut Excel file

## Other data visualizations that can help you analyze your data

- **sensitivity_plot()** - Make graphs of sensitivity analysis results
- **checkSS()** - Create a graph of simulated concentrations to check
  for whether a perpetrator drug is at steady-state when the victim drug
  is dosed
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

## Helper functions for manipulating typical SimcypConsultancy objects

- add_sims() - Add simulations to an R object
- filter_sims() - Selectively include or omit specific simulations from
  an R object
- sort_sims() - Sort the simulation file order in an object

## Miscellany

### Formatting and saving tables

- **formatTable_Simcyp()** - Format tables according to Simcyp
  Consultancy Team specifications, e.g.,
- format_table_simple() - Format a table rather simply to look nice in a
  Word file
- save_table_to_Word() - Save a bespoke PK table to Word using the
  pksummary_mult rmarkdown template

### Helper functions that call on the Simcyp package

- check_simulator_initialized() - check whether the Simcyp Simulator has
  been initialized

### Getting data from a pdf table

- pdf_to_csv() - Convert a page from a pdf file to a csv file

### Examining DDIs

- **list_interactions()** - Find all the possible drug-drug interactions
  between compounds included in a single simulation

### Tidbits for making quick calculations

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

### Internal functions you’ll probably never need to use but listed just in case you’ve seen them and are curious about what they do

  overlay XML files and fixed trial design XML files
- detect_file_timestamp() - Figure out what date/time stamps on Excel
  results files would be removed by the function remove_file_timestamp
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

# Final notes

If you encounter a bug, if you would like help using any of these
functions, or if there’s an additional feature you would like to have,
please contact a member of the R Working Group.
