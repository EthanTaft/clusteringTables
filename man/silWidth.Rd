% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/silWidth.R
\name{silWidth}
\alias{silWidth}
\title{Find Silhouette widths and plot for many k in R}
\usage{
silWidth(df, gowerDiss, tableSize = NULL)
}
\arguments{
\item{df}{original dataframe}

\item{gowerDiss}{gower distance from using computeGowerDiss()}

\item{tableSize}{If you want a specific table size, what is it?}
}
\description{
This function allows user to calculate silhouette widths and plot them
to find the appropriate cluster size for later use.
}
\keyword{plot}
\keyword{silhouette}
\keyword{width,}
