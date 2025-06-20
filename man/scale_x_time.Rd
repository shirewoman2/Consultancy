% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_x_time.R
\name{scale_x_time}
\alias{scale_x_time}
\title{Automatically scale a ggplot graph x axis for time}
\usage{
scale_x_time(
  time_range = NA,
  impose_limits = TRUE,
  time_units = "hours",
  x_axis_interval = NA,
  x_breaks = NA,
  pad_x_axis = TRUE
)
}
\arguments{
\item{time_range}{time range to show for the graph. Options: \describe{

  \item{NA}{(default) entire time range of data}

  \item{a start time and end time in hours}{only data in that range, e.g.
  \code{time_range = c(24, 48)}. Note that there are no quotes around numeric
  data.}}}

\item{impose_limits}{TRUE (default) or FALSE to actually \emph{set} the
limits listed in \code{time_range}. Why on earth would you \emph{not}
impose limits on the graph here? Behind the scenes, \code{scale_x_time}
uses the ggplot2 function \code{\link{ggplot::scale_x_continuous}}, which
filters out all data beyond the limits you specify. That means that you
can, in some situations, get graphs where lines are cut off abruptly. This
is set up with the idea that you can set the time range here to get
reasonable x axis intervals, but then you can add a call to
\code{\link{ggplot::coord_cartesian}} to limit the range without the abrupt
cut off.}

\item{time_units}{the units of time in the graph. Options are "hours"
(default), "minutes", "days", or "weeks".}

\item{x_axis_interval}{optionally set the x-axis major tick-mark interval.
Acceptable input: any number or leave as NA to accept default values, which
are generally reasonable guesses as to aesthetically pleasing and
time-relevant intervals.}

\item{x_breaks}{x-axis breaks to use if you want more control than the
default values. Most of the time, you want this to be NA, but if the breaks
the function is coming up with just are not working for your scenario, you
can specify \emph{exactly} what you want here. Either specify the exact
numbers you want, as in, \code{x_breaks = c(0, 4, 6, 10)} or specify the
start, end, and interval you want as in \code{x_breaks = seq(from = 0, to =
336, by = 10)}. As an additional convenience, you can let the function
calculate a reasonable time interval and specify that with a named list of
"from", "to", and "by" (analogous to the base R "seq" function), e.g.,
\code{x_breaks = list(from = 0, to = 336, by = "default")} and the minimum
and maximum values will be set manually but the interval will be
calculated. This differs from the argument \code{time_range} in that it
does NOT set the limits of the actual data -- only the way things are
labeled on the x axis.}

\item{pad_x_axis}{optionally add a smidge of padding to the the x axis
(default is TRUE, which includes some generally reasonable padding). If
changed to FALSE, the y axis will be placed right at the beginning of your
time range and all data will end \emph{exactly} at the end of the time
range specified. If you want a \emph{specific} amount of x-axis padding,
set this to a number; the default is \code{c(0.02, 0.04)}, which adds 2\%
more space to the left side and 4\% more to the right side of the x axis.
If you only specify one number, we'll assume that's the percent you want
added to the left side.}
}
\value{
a ggplot2 graph scale for the x axis (replaces scale_x_continuous in
  a graph)
}
\description{
\code{scale_x_time} formats a ggplot graph x axis to have breaks
  that make sense for the user-specified time units and adds a minor tick
  between the labels. It optionally adds padding to the left and right sides
  of the x axis.
}
\details{
\strong{Notes for ggplot2 users:} Like other ggplot2
  \code{scale_...} functions, \code{scale_x_time} clips the underlying data
  rather than zooming in on a specific x-axis range as
  \code{coord_cartesian(...)} does. If you do not want that data-clipping
  behavior, you can set the time range with \code{scale_x_time} but set the
  argument "impose_limits" to FALSE and then follow the call on
  \code{scale_x_time} with a call to \code{coord_cartesian(xlim = ...)}. This
  will avoid the clipping behavior while still getting you good time
  intervals on your x axis.
}
\examples{
# scale_x_time will make nice time intervals on your x axis
MyData <- data.frame(Time = 0:168,
                     Conc = rnorm(n = 169, mean = 100))
ggplot(MyData, aes(x = Time, y = Conc)) +
       geom_line() +
       scale_x_time()

# scale_x_time works the same way that other ggplot2 scale_... functions do,
# though, in that it will clip your data. Here, we're intentionally creating
# some data with breaks across typical time intervals to demonstrate:
MyData <- data.frame(Time = seq(0, 168, by = 5),
                     Conc = rnorm(n = 34, mean = 100))

ggplot(MyData, aes(x = Time, y = Conc)) +
    geom_point() +
    scale_x_time(time_range = c(24, 48))
# Data are clipped.

# To avoid clipping while still getting good time intervals, set
# impose_limits to F and add a call to coord_cartesian.
ggplot(MyData, aes(x = Time, y = Conc)) +
    geom_point() +
    scale_x_time(time_range = c(24, 48),
                 impose_limits = FALSE) +
    coord_cartesian(xlim = c(24, 48))


}
