% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatialise.R
\name{em38_decode}
\alias{em38_decode}
\title{Process EM38 data}
\usage{
em38_decode(
  n38_decoded = NULL,
  hdop_filter = 3,
  time_filter = 5,
  fix_filter = NULL
)
}
\arguments{
\item{n38_decoded}{Nested list output by \code{\link{n38_decode}}.}

\item{hdop_filter}{Numeric, discard GPS data where the Horizontal Dilution of
Precision is greater than this number. Defaults to 3 metres. Set to NULL to
keep all readings.}

\item{time_filter}{Numeric, discard readings taken more than \code{n} seconds
after the last acceptable GPS reading. Set to 2-3x GPS aquisition
frequency. Defaults to 5 seconds. Set to NULL to keep all readings.}

\item{fix_filter}{Select readings with a minimum quality of GPS fix.
  Options are:
 \enumerate{
  \item Autonomous GPS fix.
  \item DGPS fix, using a local DGPS base station or correction service
  such as WAAS or EGNOS.
  \item Pulse per second (PPS) fix.
  \item real-time kinematic (RTK) fix.
  \item RTK float fix.
  \item estimated (dead reckoning)
  \item manual input mode
  \item simulation mode
  \item WAAS fix.
  }
To filter on multiple options, supply a vector e.g. \code{c(2, 9)}. Defaults
to NULL.}
}
\value{
A two element list containing file header data, and a list of
  processed survey lines. For each survey line, an `sf` data frame with
  sfc_POINT geometry is returned where valid GPS data exists. If instrument
  readings without valid GPS data are present, a data frame is returned.
  Otherwise, a string is returned explaining the failure to process.
}
\description{
This function processes the outputs of \code{\link{n38_decode}} into a
useable end product.
}
\examples{
data('n38_demo')
n38_chunks  <- n38_chunk(n38_demo)
n38_decoded <- n38_decode(n38_chunks)
demo_survey <- em38_decode(n38_decoded, 3)
}
