% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_boundary_colors.R
\name{make_so_highlight_key}
\alias{make_so_highlight_key}
\title{Make a key for highlighted S/O values in a table or graph}
\usage{
make_so_highlight_key(
  highlight_so_cutoffs,
  highlight_so_colors,
  orientation = "long"
)
}
\arguments{
\item{highlight_so_cutoffs}{cutoffs for highlighting simulated-to-observed
ratios. Anything that is above those values or below the inverse of those
values will be highlighted. Acceptable input for, say, highlighting values
that are > 125\% or < 80\% of the observed and also, with a second color,
values that are > 150\% or < 66\% would be: \code{highlight_so_cutoffs =
c(1.25, 1.5)}. If you would like the middle range of values to be
highlighted, include 1 in your cutoffs. For example, say you would like
everything that's < 80\% or > 125\% to be highlighted red but you'd like
the "good" values from 80\% to 125\% to be green, you can get that by
specifying
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

\item{orientation}{orientation of the key; options are "long" (default) or
"wide"}
}
\value{
a flextable for use as a key for which colors mean what S/O cutoff
}
\description{
Make a key for highlighted S/O values in a table or graph
}
\examples{
make_so_highlight_key(highlight_so_colors = "green to red",
                      highlight_so_cutoffs = c(1, 1.25, 1.5, 2))

}
