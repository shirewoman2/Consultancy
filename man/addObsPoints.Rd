% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addObsPoints.R
\name{addObsPoints}
\alias{addObsPoints}
\title{INTERNAL PACKAGE USE: Add observed data points to a concentration-time plot}
\usage{
addObsPoints(
  obs_dataframe,
  A,
  AES,
  obs_shape,
  obs_shape_user,
  obs_size,
  obs_color,
  obs_color_user,
  obs_line_trans,
  obs_line_trans_user,
  obs_fill_trans,
  obs_fill_trans_user,
  figure_type,
  MapObsData,
  connect_obs_points,
  line_width,
  LegCheck
)
}
\arguments{
\item{obs_dataframe}{observed data as a data.frame}

\item{A}{the existing ggplot2 graph to which the observed data will be added}

\item{MapObsData}{TRUE or FALSE for whether to map the observed data to
specific columns. Originally from ct_plot_overlay.}

\item{connect_obs_points}{TRUE or FALSE (default) for whether to add
connecting lines between observed data points from the same individual}

\item{LegCheck}{TRUE or FALSE for whether to include the legend. From
ct_plot_overlay originally and should always be TRUE for ct_plot.}
}
\value{
a ggplot2 layer with observed data
}
\description{
\code{addObsPoints} adds observed data to \code{\link{ct_plot}} or
\code{\link{ct_plot_overlay}} and sets the color and shape of those points
appropriately. This is NOT meant to be used as a stand-alone function.
}
