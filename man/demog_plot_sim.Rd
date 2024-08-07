% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demog_plot_sim.R
\name{demog_plot_sim}
\alias{demog_plot_sim}
\title{Make plots for comparing populations across simulations}
\usage{
demog_plot_sim(
  demog_dataframe,
  demog_parameters = NA,
  variability_display = "kernel density",
  colorBy_column,
  color_labels = NA,
  legend_label_color = NA,
  color_set = "default",
  graph_title = "Demographics",
  alpha = 0.8,
  ncol = NULL,
  nrow = NULL,
  facet_by_sex = TRUE,
  border_facets = TRUE,
  graph_labels = TRUE
)
}
\arguments{
\item{demog_dataframe}{the output from running \code{\link{extractDemog}}.}

\item{demog_parameters}{demographic parameters to include. We're starting
  with a limited set: \itemize{\item{Individual parameters, which will be
  displayed as either a kernel density plot or a boxplot depending on your 
  choice for \code{variability_display}: \itemize{
  
  \item{"Age" (age in years)}

  \item{"AGP_gL" (alpha-1-acid glycoprotein in g/L; "AGP" is fine)}
  
  \item{"BMI_kgm2" ("BMI" is fine)}

  \item{"BrainWt_g" (brain weight; "Brain" is fine)}

  \item{"BSA_m2" (body surface area in m2; "BSA" is fine)}

  \item{"CardiacOut" (cardiac output in L/h; "Cardiac" is fine)}

  \item{"Creatinine_umolL" (creatinine in umol/L; "Creatinine" is fine)}

  \item{"GFR_mLminm2" (glomerular filtration rate in mL/min/m2; "GFR" is fine)}

  \item{"Haematocrit" (haematocrit)}

  \item{"Height_cm" (height in cm; "Height" is fine)}

  \item{"HSA_gL" (human serum albumin in g/L; "HSA" is fine)}

  \item{"KidneyWt_g" (kidney weight; "Kidney" is fine)}

  \item{"LiverWt_g" (liver weight; "Liver" is fine)}

  \item{"Sex" (graph shows the percent female by population)}

  \item{"Weight_kg" (weight in kg; "Weight" is fine)}

  \item{"RenalFunction" (renal function as calculated by the GFR in 
  mL/min/m squared body surface area divided by the reference GFR for that 
  sex: 120 for female subjects and 130 for male subjects as of V23 of the 
  Simcyp Simulator)}}}

  \itemize{Comparisons of two parameters, which will create a scatter
  plot: \itemize{
  
  \item{"Weight vs Height"}

  \item{"Height vs Age"}

  \item{"Weight vs Age"}

  \item{"Sex vs Age"}}}}

  If you want only a subset
  of those, list them in a character vector, e.g., \code{demog_parameters = c("Age",
  "Height_cm", "Weight_kg")}. Plots will be in the order you list.}

\item{variability_display}{How should the variability be shown? Options are
"kernel density" (default, a type of smoothed histogram) or "boxplot". Any
demographic parameters requested in the form of "X vs Y", e.g., "weight vs
height", will always be shown as scatter plots.}

\item{colorBy_column}{the column in \code{demog_dataframe} that should be
used for determining which color lines and/or points will be. This should
be unquoted, e.g., \code{colorBy_column = File}. If left blank, we will
color by the simulation file name.}

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

\item{graph_title}{title to use on the plots}

\item{alpha}{how transparent to make the points, with 0 being completely
transparent and invisible so I don't know why you'd want that but, hey, you
do you, to 1, which is fully opaque.}

\item{ncol}{optionally specify the number of columns. If left as NULL, a
reasonable guess will be used.}

\item{nrow}{optionally specify the number of rows. If left as NULL, a
reasonable guess will be used.}

\item{facet_by_sex}{TRUE or FALSE (default) for whether to break up the
graphs into facets based on the sex of the subjects}

\item{border_facets}{TRUE (default) or FALSE for whether to include a border
around the facets if the graphs are broken up by the sex of the subjects}

\item{graph_labels}{TRUE or FALSE for whether to include labels (A, B, C,
etc.) for each of the small graphs.}
}
\value{
a set of graphs. This does not yet save the graphs for you, so you'll
  need to run ggsave(...) to do that.
}
\description{
UNDER CONSTRUCTION.
}
\examples{
# none yet
}
