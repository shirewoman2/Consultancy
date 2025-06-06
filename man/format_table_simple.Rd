% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_table_simple.R
\name{format_table_simple}
\alias{format_table_simple}
\title{Format a table rather simply to look nice in a Word file}
\usage{
format_table_simple(
  DF,
  shading_column,
  merge_shaded_cells = TRUE,
  merge_columns = NA,
  sort_column,
  bold_cells = list(c(0, NA)),
  highlight_gmr_colors = NA,
  highlight_so_cutoffs = NA,
  highlight_so_colors = "yellow to red",
  font = "Palatino Linotype",
  fontsize = 11,
  column_widths = NA,
  include_header = TRUE,
  alignment = "left",
  save_table = NA,
  page_orientation = "portrait",
  title_document = NA,
  table_caption = NA
)
}
\arguments{
\item{DF}{a data.frame}

\item{shading_column}{If you would like to alternate the shading of the rows
in the output table, supply here the unquoted name of the column to check
for when to change the shading; every time that column's value changes, the
shading will alternate between white and light gray. For example, if you
have a table with PK values for multiple files and you have more than one
row per file (an example of this would be the output from the function
\code{\link{pksummary_mult}}), setting \code{shading_column = File} will
cause the shading of the rows to alternate between white and light gray
whenever the file changes. Please see the examples at the bottom of this
help file.}

\item{merge_shaded_cells}{TRUE (default) or FALSE for whether to merge the
cells that have the same shade. This only applies when one of the columns
in the input data.frame is used for deciding when to alternate shading,
that is, \code{shading_column} has a value.}

\item{merge_columns}{a vector of quoted column names or of numeric column
positions that should be merged vertically whenever the values are the
same. For example, \code{merge_columns = c("File", "Tissue")} will cause
the cells in the columns "File" and "Tissue" to merge vertically whenever
the same value shows up in consecutive rows. Similarly, \code{merge_columns
= c(1, 3, 5)} will merge vertically the 1st, 3rd, and 5th columns whenever
the values are the same. Note: This is different from most other functions
in the SimcypConsultancy package, which require unquoted column names.
Honestly, we just don't know how code things for you to supply a variable
number of unquoted column names for a single argument; we've just hit a
coding knowledge limitation here!}

\item{bold_cells}{optionally specify cells in the table to be in bold-face
text with a numeric vector where the 1st number is the row number and the
2nd number is the column number (just like regular row and column
specifications in R). For example, \code{bold_cells = c(1, 2)} will make
the cell in row 1 and column 2 bold face. Use "0" for the row number if you
want to use bold face for something in the header row, and use NA in place
of a row or column number to make everything in that row or column bold
face. If you want to specify multiple places to use bold face, use a list
of numeric vectors. By default, the header row will be bold. Set
\code{bold_cells = NA} to make \emph{nothing} bold. Please see the examples
at the bottom of the help file.}

\item{highlight_gmr_colors}{optionally specify a set of colors to use for
highlighting geometric mean ratios for DDIs. Options are "yellow to red",
"green to red", "traffic" (a more vivid version of "green to red"), or a
vector of 4 colors of your choosing. If left as NA, no highlighting for GMR
level will be done.}

\item{highlight_so_cutoffs}{optionally specify cutoffs for highlighting any
simulated-to-observed ratios. Anything that is above those values or below
the inverse of those values will be highlighted. To figure out what cells
to highlight, this looks for a column titled "Statistic" or "Stat", then
looks for what row contains "S/O" or "simulated (something something)
observed" (as in, we'll use some wildcards to try to match your specific
text). Next, it looks for any values in that same row that are above those
cutoffs. This overrides anything else you specified for highlighting. The
default is NA, for \emph{not} highlighting based on S/O value. Acceptable
input for, say, highlighting values that are > 125\% or < 80\% of the
observed and also, with a second color, values that are > 150\% or < 66\%
would be: \code{highlight_so_cutoffs = c(1.25, 1.5)}. If you would like the
middle range of values to be highlighted, include 1 in your cutoffs. For
example, say you would like everything that's < 80\% or > 125\% to be
highlighted red but you'd like the "good" values from 80\% to 125\% to be
green, you can get that by specifying
\code{highlight_so_cutoffs = c(1, 1.25)} and \code{highlight_so_colors =
c("green", "red")}}

\item{highlight_so_colors}{optionally specify a set of colors to use for
  highlighting S/O values outside the limits you specified with
  \code{highlight_so_cutoffs}. Options: \describe{

  \item{"yellow to red" (default)}{A range of light yellow to light orange to
  light red. If you have included 1 in your cutoffs and you leave
  \code{highlight_so_colors} with the default setting, values in the middle,
  "good" range of S/O values will be highlighted a light green.}

  \item{"traffic"}{light green, yellow, and red designed to display values
  outside 1.25, 1.5, and 2 fold of unity, respectively. If you include 1 in
  \code{highlight_so_cutoffs}, you'll get a darker green for "good" S/O
  values. This color scheme was borrowed from Lisa, so if you've seen her
  slides, these will look familiar.}

  \item{a character vector of specific colors}{Any R-acceptable colors, will
  work here, e.g., \code{highlight_so_colors = c("yellow", "orange", "red")}}.
  If you do specify your own bespoke colors, you'll need to make sure that
  you supply one color for every value in \code{highlight_so_cutoffs}.}}

\item{font}{font to use. Default is "Palatino Linotype" and any fonts
available on your machine in either Word or PowerPoint should be
acceptable. If you get Times New Roman in your table when you asked for
something else, it means that that font isn't available or maybe wasn't
spelled the way R is expecting it. For example, "Calibri" works but
"Calibri (Body)" doesn't even though the latter is listed in PowerPoint and
Word.}

\item{fontsize}{the numeric font size for the output table. Default is 11
point.}

\item{column_widths}{optionally specify what the widths of the columns should
be with a numeric vector of the widths in inches, e.g., \code{column_widths
= c(1.5, 2, 0.5, 3)}}

\item{include_header}{TRUE (default) or FALSE for whether to include the
header row}

\item{alignment}{alignment of text throughout table. Options are "left"
(default), "right", "center", or "justify".}

\item{save_table}{optionally save the output table by supplying a file name
in quotes here, e.g., "My nicely formatted table.docx".  Do not include any
slashes, dollar signs, or periods in the file name. If you leave off the
file extension, we'll assume you want it to be ".docx". If there is a
column titled "File" in your table, we'll add a caption listing which files
were included.}

\item{page_orientation}{set the page orientation for the Word file output to
"portrait" (default) or "landscape"}

\item{title_document}{optionally specify a title for the Word document
output. If you don't save the table, this will be ignored.}

\item{table_caption}{optionally add some text for a table caption. If the
table you supply contains a column titled "File", there will already be a
caption listing the source files; this would add some additional text
before that.}
}
\value{
a formatted table
}
\description{
\code{format_table_simple} is meant to for use with writing compound summary
files. It is a wrapper function for formatTable_Simcyp that creates a
somewhat simpler style of table.
}
\examples{
# None yet

}
