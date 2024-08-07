% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/release_profile_plot.R
\name{release_profile_plot}
\alias{release_profile_plot}
\title{Make a graph of the release profiles of a compound}
\usage{
release_profile_plot(
  existing_exp_details,
  sims_to_include = NA,
  compoundsToExtract = NA,
  figure_type = "percentile ribbon",
  linear_or_log = "linear",
  colorBy_column,
  color_labels = NA,
  legend_label_color = NA,
  color_set = "default",
  include_errorbars = FALSE,
  errorbar_width = 0.5,
  linetype_column,
  linetype_labels = NA,
  linetypes = c("solid", "dashed"),
  line_width = NA,
  line_transparency = NA,
  legend_label_linetype = NA,
  facet1_column,
  facet2_column,
  facet_ncol = NA,
  facet_nrow = NA,
  floating_facet_scale = FALSE,
  facet_spacing = NA,
  time_range = NA,
  x_axis_interval = NA,
  x_axis_label = NA,
  pad_x_axis = TRUE,
  pad_y_axis = TRUE,
  y_axis_limits_lin = NA,
  y_axis_limits_log = NA,
  y_axis_interval = NA,
  y_axis_label = "Percent released",
  hline_position = NA,
  hline_style = "red dotted",
  vline_position = NA,
  vline_style = "red dotted",
  graph_labels = TRUE,
  graph_title = NA,
  graph_title_size = 14,
  legend_position = NA,
  prettify_compound_names = TRUE,
  qc_graph = FALSE,
  save_graph = NA,
  fig_height = 6,
  fig_width = 5
)
}
\arguments{
\item{existing_exp_details}{output from \code{\link{extractExpDetails}} or
\code{\link{extractExpDetails_mult}}. This must be from package version >=
2.8.0.}

\item{figure_type}{the type of figure to plot. \describe{

  \item{"means only"}{(default) show only the mean release profile}

  \item{"percentile ribbon"}{show an opaque line for the mean data and
  transparent shading for the standard deviation.}}}

\item{linear_or_log}{the type of graph to be returned. Options: \describe{
  \item{"semi-log"}{y axis is log transformed; this is the default}

  \item{"linear"}{no axis transformation}

  \item{"both vertical"}{both the linear and the semi-log graphs will be
  returned, and graphs are stacked vertically}

  \item{"both horizontal"}{both the linear and the semi-log graphs will be
  returned, and graphs are stacked horizontally}}}

\item{colorBy_column}{(optional) the column in
\code{existing_exp_details$ReleaseProfiles} that should be used for
determining which color lines and/or points will be. This should be
unquoted, e.g., \code{colorBy_column = Tissue}.}

\item{color_labels}{optionally specify a character vector for how you'd like
the labels for whatever you choose for \code{colorBy_column} to show up in
the legend. For example, use \code{color_labels = c("file 1.xlsx" = "fa
0.5", "file 2.xlsx" = "fa 0.2")} to indicate that "file 1.xlsx" is for an
fa of 0.5 and "file 2.xlsx" is for an fa of 0.2. The order in the legend
will match the order designated here.}

\item{legend_label_color}{optionally indicate on the legend something
explanatory about what the colors represent. For example, if
\code{colorBy_column = File} and \code{legend_label_color = "Simulations
with various fa values"}, that will make the label above the file names in
the legend more explanatory than just "File". The default is to use
whatever the column name is for \code{colorBy_column}. If you don't want a
label for this legend item, set this to "none".}

\item{color_set}{the set of colors to use. Options: \describe{

  \item{"default"}{a set of colors from Cynthia Brewer et al. from Penn State
  that are friendly to those with red-green colorblindness. The first three
  colors are green, orange, and purple. This can also be referred to as
  "Brewer set 2". If there are only two unique values in the colorBy_column,
  then Brewer set 1 will be used since red and blue are still easily
  distinguishable but also more aesthetically pleasing than green and
  orange.}

  \item{"Brewer set 1"}{colors selected from the Brewer palette "set 1". The
  first three colors are red, blue, and green.}

  \item{"ggplot2 default"}{the default set of colors used in ggplot2 graphs
  (ggplot2 is an R package for graphing.)}

  \item{"rainbow"}{colors selected from a rainbow palette. The default
  palette is limited to something like 6 colors, so if you have more than
  that, that's when this palette is most useful. It's \emph{not} very useful
  when you only need a couple of colors.}

  \item{"blue-green"}{a set of blues fading into greens. This palette can be
  especially useful if you are comparing a systematic change in some
  continuous variable -- for example, increasing dose or predicting how a
  change in intrinsic solubility will affect concentration-time profiles --
  because the direction of the trend will be clear.}

  \item{"blues"}{a set of blues fading light blue to dark blue. Like
  "blue-green", this palette can be especially useful if you are comparing a
  systematic change in some continuous variable.}

  \item{"Tableau"}{uses the standard Tableau palette; requires the "ggthemes"
  package}

  \item{"viridis"}{from the eponymous package by Simon Garnier and ranges
  colors from purple to blue to green to yellow in a manner that is
  "printer-friendly, perceptually uniform and easy to read by those with
  colorblindness", according to the package author}

  \item{a character vector of colors}{If you'd prefer to set all the colors
  yourself to \emph{exactly} the colors you want, you can specify those
  colors here. An example of how the syntax should look: \code{color_set =
  c("dodgerblue3", "purple", "#D8212D")} or, if you want to specify exactly
  which item in \code{colorBy_column} gets which color, you can supply a
  named vector. For example, if you're coloring the lines by the compound ID,
  you could do this: \code{color_set = c("substrate" = "dodgerblue3",
  "inhibitor 1" = "purple", "primary metabolite 1" = "#D8212D")}. If you'd
  like help creating a specific gradation of colors, please talk to a member
  of the R Working Group about how to do that using
  \link{colorRampPalette}.}}}

\item{include_errorbars}{TRUE or FALSE (default) for whether to include error
bars for the standard deviation.}

\item{errorbar_width}{width of error bars to use in hours (or, if you've used
some other time unit, in whatever units are in your data). Default is 0.5.}

\item{linetype_column}{the column in
\code{existing_exp_details$ReleaseProfiles} that should be used for
determining the line types}

\item{linetype_labels}{optionally specify a character vector for how you'd
like the labels for whatever you choose for \code{linetype_column} to show
up in the legend. For example, use \code{linetype_labels = c("file 1.xlsx"
= "fa 0.5", "file 2.xlsx" = "fa 0.2")} to indicate that "file 1.xlsx" is
for an fa of 0.5 and "file 2.xlsx" is for an fa of 0.2. The order in the
legend will match the order designated here.}

\item{linetypes}{the line types to use. Default is "solid" for all lines.
You'll need one line type for each possible value in the column you
specified for \code{linetype_column}.}

\item{line_width}{optionally specify how thick to make the lines. Acceptable
input is a number; the default is 1 for most lines and 0.8 for some, to
give you an idea of where to start.}

\item{legend_label_linetype}{optionally indicate on the legend something
explanatory about what the line types represent. For example, if
\code{linetype_column = Inhibitor} and \code{legend_label_linetype =
"Inhibitor present"}, that will make the label in the legend above, e.g.,
"none", and whatever perpetrator was present more explanatory than just
"Inhibitor". The default is to use whatever the column name is for
\code{linetype_column}. If you don't want a label for this legend item, set
this to "none".}

\item{facet1_column}{optionally break up the graph into small multiples; this
specifies the first of up to two columns to break up the data by, and the
designated column name should be unquoted, e.g., \code{facet1_column =
Tissue}. If \code{floating_facet_scale} is FALSE and you haven't specified
\code{facet_ncol} or  \code{facet_nrow}, then \code{facet1_column} will
designate the rows of the output graphs.}

\item{facet2_column}{optionally break up the graph into small multiples; this
specifies the second of up to two columns to break up the data by, and the
designated column name should be unquoted, e.g., \code{facet2_column =
CompoundID}. If \code{floating_facet_scale} is FALSE and you haven't
specified \code{facet_ncol} or  \code{facet_nrow}, then
\code{facet2_column} will designate the columns of the output graphs.}

\item{facet_ncol}{optionally specify the number of columns of facetted graphs
you would like to have. This only applies when you have specified a column
for \code{facet1_column} and/or \code{facet2_column}.}

\item{facet_nrow}{optionally specify the number of rows of facetted graphs
you would like to have. This only applies when you have specified a column
for \code{facet1_column} and/or \code{facet2_column}.}

\item{floating_facet_scale}{TRUE or FALSE (default) for whether to allow the
axes for each facet of a multi-facetted graph to scale freely to best fit
whatever data are present. Default is FALSE, which means that all data will
be on the same scale for easy comparison. However, this could mean that
some graphs have lines that are hard to see, so you can set this to TRUE to
allow the axes to shrink or expand according to what data are present for
that facet. Floating axes comes with a trade-off for the looks of the
graphs, though: Setting this to TRUE does mean that your x axis won't
automatically have pretty breaks that are sensible for times in hours.}

\item{facet_spacing}{Optionally set the spacing between facets. If left as
NA, a best-guess as to a reasonable amount of space will be used. Units are
"lines", so try, e.g. \code{facet_spacing = 2}. (Reminder: Numeric data
should not be in quotes.)}

\item{time_range}{time range to display. Options: \describe{

  \item{NA}{entire time range of data; default}

  \item{a start time and end time in hours}{only data in that time range,
  e.g. \code{c(24, 48)}. Note that there are no quotes around numeric data.}}}

\item{x_axis_interval}{set the x-axis major tick-mark interval. Acceptable
input: any number or leave as NA to accept default values, which are
generally reasonable guesses as to aesthetically pleasing and PK-relevant
intervals.}

\item{x_axis_label}{optionally supply a character vector or an expression to
use for the x axis label}

\item{pad_x_axis}{optionally add a smidge of padding to the x axis (default
is TRUE, which includes some generally reasonable padding). If changed to
FALSE, the y axis will be placed right at the beginning of your time range
and all data will end \emph{exactly} at the end of the time range
specified. If you want a \emph{specific} amount of x-axis padding, set this
to a number; the default is \code{c(0.02, 0.04)}, which adds 2\% more space
to the left side and 4\% more space to the right side of the x axis. If you
only specify one number, padding is added to the left side.}

\item{pad_y_axis}{optionally add a smidge of padding to the y axis (default
is TRUE, which includes some generally reasonable padding). As with
\code{pad_x_axis}, if changed to FALSE, the x axis will be placed right at
the bottom of your data, possibly cutting a point in half. If you want a
\emph{specific} amount of y-axis padding, set this to a number; the default
is \code{c(0.02, 0)}, which adds 2\% more space to the bottom and nothing
to the top of the y axis. If you only specify one number, padding is added
to the bottom.}

\item{y_axis_limits_lin}{Optionally set the Y axis limits for the linear
plot, e.g., \code{c(10, 1000)}. If left as NA, the Y axis limits for the
linear plot will be automatically selected. This only applies when you have
requested a linear plot with \code{linear_or_log}.}

\item{y_axis_limits_log}{Optionally set the Y axis limits for the semi-log
plot, e.g., \code{c(10, 1000)}. Values will be rounded down and up,
respectively, to the nearest order of magnitude. If left as NA, the Y axis
limits for the semi-log plot will be automatically selected. This only
applies when you have requested a semi-log plot with \code{linear_or_log}.}

\item{y_axis_interval}{set the y-axis major tick-mark interval. Acceptable
input: any number or leave as NA to accept default values, which are
generally reasonable guesses as to aesthetically pleasing intervals.}

\item{y_axis_label}{optionally supply a character vector or an expression to
use for the y axis label}

\item{hline_position}{numerical position(s) of any horizontal lines to add to
the graph. The default is NA to have no lines, and good syntax if you
\emph{do} want lines would be, for example, \code{hline_position = 10} to
have a horizontal line at 10 ng/mL (or whatever your concentration units
are) or \code{hline_position = c(10, 100, 1000)} to have horizontal lines
at each of those y values. Examples of where this might be useful would be
to indicate a toxicity threshold, a target Cmin, or the lower limit of
quantification for the assay used to generate the concentration-time data.}

\item{hline_style}{the line color and type to use for any horizontal lines
that you add to the graph with \code{hline_position}. Default is "red
dotted", but any combination of 1) a color in R and 2) a named linetype is
acceptable. Examples: "red dotted", "blue dashed", or "#FFBE33 longdash".
To see all the possible linetypes, type \code{ggpubr::show_line_types()}
into the console.}

\item{vline_position}{numerical position(s) of any vertical lines to add to
the graph. The default is NA to have no lines, and good syntax if you
\emph{do} want lines would be, for example, \code{vline_position = 12} to
have a vertical line at 12 h or \code{vline_position = seq(from = 0, to =
168, by = 24)} to have horizontal lines every 24 hours for one week.
Examples of where this might be useful would be indicating dosing times or
the time at which some other drug was started or stopped.}

\item{vline_style}{the line color and type to use for any vertical lines that
you add to the graph with \code{vline_position}. Default is "red dotted",
but any combination of 1) a color in R and 2) a named linetype is
acceptable. Examples: "red dotted", "blue dashed", or "#FFBE33 longdash".
To see all the possible linetypes, type \code{ggpubr::show_line_types()}
into the console.}

\item{graph_labels}{TRUE or FALSE for whether to include labels (A, B, C,
etc.) for each of the small graphs. (Not applicable if only outputting
linear or only semi-log graphs.)}

\item{graph_title}{optionally specify a title that will be centered across
your graph or set of graphs}

\item{graph_title_size}{the font size for the graph title if it's included;
default is 14. This also determines the font size of the graph labels.}

\item{legend_position}{Specify where you want the legend to be. Options are
"left", "right" (default in most scenarios), "bottom", "top", or "none" if
you don't want one at all.}

\item{prettify_compound_names}{set this to a) TRUE (default) or FALSE for
whether to make the compound names in the legend prettier or b) supply a
named character vector to set it to the exact name you'd prefer to see in
your legend. For example, \code{prettify_compound_names =
c("Sim-Ketoconazole-400 mg QD" = "ketoconazole", "Wks-Drug ABC-low_ka" =
"Drug ABC")} will make those compounds "ketoconazole" and "Drug ABC" in a
legend, and \code{prettify_compound_names = TRUE} will make some reasonable
guesses about what a prettier compound name should be. An example of
setting this to TRUE: "SV-Rifampicin-MD" would become "rifampicin", and
"Sim-Ketoconazole-200 mg BID" would become "ketoconazole".}

\item{save_graph}{optionally save the output graph by supplying a file name
in quotes here, e.g., "My conc time graph.png"or "My conc time graph.docx".
The nice thing about saving to Word is that the figure title and caption
text will be partly filled in automatically, although you should check that
the text makes sense in light of your exact graph. If you leave off ".png"
or ".docx", it will be saved as a png file, but if you specify a different
graphical file extension, it will be saved as that file format. Acceptable
graphical file extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png",
"bmp", or "svg". Do not include any slashes, dollar signs, or periods in
the file name. Leaving this as NA means the file will not be automatically
saved to disk.}

\item{fig_height}{figure height in inches; default is 6}

\item{fig_width}{figure width in inches; default is 5}
}
\value{
a ggplot2 graphs or a set of arranged ggplot2 graphs
}
\description{
\code{release_profile_plot} is meant to be used in conjunction with
\code{\link{extractExpDetails}} to create graphs with release-profile data,
possibly for multiple simulations or for multiple compounds.
}
\examples{
Details <- extractExpDetails_mult(sim_data_files = NA)

release_profile_plot(existing_exp_details = Details)

# If you have multiple simulations or multiple compounds and you only 
# want to graph one, here is an example of how to filter your data to do
# that. 

 Details_subset <- Details
 Details_subset$ReleaseProfiles <- Details_subset$ReleaseProfiles \%>\% 
      filter(CompoundID == "substrate" & 
             File == "simulation A.xlsx")

release_profile_plot(existing_exp_details = Details_subset)


}
