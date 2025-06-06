% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fm_treemap.R
\name{fm_treemap}
\alias{fm_treemap}
\title{For comparing fm values, make a treemap as opposed to the standard and
arguably inferior pie chart}
\usage{
fm_treemap(
  fm_dataframe,
  pathway_column,
  fm_column,
  mean_type = NA,
  DDI_option = "baseline only",
  show_numbers_on_graph = TRUE,
  rounding = NA,
  color_set = "default",
  label_fm_cutoff = 0.05,
  legend_nrow = NA,
  biggest_box_position = "top left",
  graph_title = NA,
  graph_title_size = 14,
  save_graph = NA,
  fig_height = 3,
  fig_width = 4
)
}
\arguments{
\item{fm_dataframe}{a data.frame containing columns for the drug-metabolizing
enzyme and the fm, the fraction metabolized. If you ran
\code{\link{extractFmFe}} to get these data, be sure to \emph{only} get the
data you want. For example, if this was a DDI simulation, you would want to
use only the data in the absense of a perpetrator or only in the presence
of one; summing up both numbers wouldn't make sense.}

\item{pathway_column}{the name of the column that contains the names of the
pathways. If you ran \code{\link{extractFmFe}} to get these data, set this
to \code{pathway_column = Enzyme}}

\item{fm_column}{the name of the column that contains the fm values. If you
ran \code{\link{extractFmFe}} to get these data, set this to
\code{fm_column = Max} if you want the maximum fm value for each. Note that
this should be the values as decimals, e.g., 0.8 for 80\%. The total area
occupied by boxes in your treemap will sum to 100\%, even if these numbers
do not, so pay attention to what data you're graphing and make sure things
sum to 1.}

\item{mean_type}{If there is a column called "statistic" in your data, which
mean type would you like to display? Acceptable values are any statistics
present in that column, but you can only show one.}

\item{DDI_option}{If the fm_dataframe included fm and fe values for when any
DDI perpetrator drugs are present, then you've got some options for how to
display this. Acceptable options are: "baseline only" (default) to show
only fm values at baseline, "DDI only", to show only fm values in the
presence of the perpetrator drug(s), or "facet by DDI" to show two graphs,
broken up by whether there is a DDI.}

\item{show_numbers_on_graph}{TRUE (default) or FALSE for whether to list the
numerical fm value for each pathway on the graph}

\item{rounding}{option for what rounding to perform, if any. Options are:
  \describe{

  \item{"none"}{No rounding will be performed.}

  \item{"significant X" where "X" is a number}{Output will be rounded to X
  significant figures. \code{rounding = "signif X"} also works fine. If you
  leave rounding as NA, the default is "significant 3".}

  \item{"round X" where "X" is a number}{Output will be rounded to X digits}}}

\item{color_set}{the set of colors to use. Options are "default", which
matches Simcyp colors in PowerPoint presentations and is rather nice, "set
1" for Brewer set 1, "set 2" for Brewer set 2 (colorblind friendly),
"rainbow", which works well when there are a LOT of fm's to visualize,
"blue-green", which makes a pretty gradient of blues and greens, "blues",
which makes a graident of blues, or "viridis", which is from the eponymous
package by Simon Garnier and ranges colors from purple to blue to green to
yellow in a manner that is "printer-friendly, perceptually uniform and easy
to read by those with colorblindness", according to the package author.}

\item{label_fm_cutoff}{cutoff to use for having a label show up underneath
the graph; default is 0.05. You may want to play around with this a bit in
the final graphic file because how this graph looks is extremely dependent
on its dimensions.}

\item{legend_nrow}{optionally specify the number of rows to use for the
legend. If left as NA, there will be 1 row.}

\item{biggest_box_position}{where to put the biggest box. Defaults to "top
left", and other options are "bottom left", "bottom right", and "top
right".}

\item{graph_title}{optionally specify a title that will be centered across
your graph}

\item{graph_title_size}{the font size for the graph title if it's included}

\item{save_graph}{optionally save the output graph by supplying a file name
in quotes here, e.g., "My conc time graph.png". If you do not designate a
file extension, it will be saved as a png file, but if you specify a
different file extension, it will be saved as that file format. Acceptable
extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg".
Do not include any slashes, dollar signs, or periods in the file name.
Leaving this as NA means the file will not be automatically saved to disk.}

\item{fig_height}{figure height in inches; default is 3}

\item{fig_width}{figure width in inches; default is 4}
}
\value{
a ggplot2 graph
}
\description{
Create a treemap -- filled rectangles -- to indicate the
  fraction of metabolism due to various pathways. Rectangles are proportional
  in size to the fm values and include labels for the drug-metabolizing
  enzyme involved and the percentage that enzyme is responsible for. Any
  rectangles that would be smaller than some user-specified cutoff (see
  argument "label_fm_cutoff") will be pulled out underneath the graph for
  readability.

  \strong{Why bother with a treemap as opposed to a pie chart?} Rectangles
  are easier to compare visually than the sizes of pie slices, making a
  treemap easier to understand and interpret (reference: "The Visual Display
  of Quantitative Information" by Edward Tufte, Professor Emeritus of
  Political Science, Statistics, and Computer Science at Yale University).
}
\examples{

Lenv_fm <- data.frame(DME = c("CYP3A4", "other CYPs", "aldehyde oxidase"),
                      fm = c(0.79, 0.20, 0.01))
fm_treemap(fm_dataframe = Lenv_fm,
           pathway_column = DME,
           fm_column = fm)

fm_treemap(fm_dataframe = Lenv_fm,
           pathway_column = DME,
           fm_column = fm,
           color_set = "blue-green",
           label_fm_cutoff = 0.01)

}
