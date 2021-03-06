#' Data: imported N38 track file
#'
#' A dataset representing a track walked by a worker carrying an EM38-MK2
#' instrument connected to an Allegra-CX datalogger and GPS.
#'
#' @format A matrix of raw bytes, 25 columns by 37249 rows, created by calling
#'   \code{\link{n38_import}} on \code{system.file("extdata", "em38_demo.n38",
#'   package = "em38")}.
#' @source This dataset was recorded on a farm near Jondaryan, South East
#'   Queensland in March 2018 during a training exercise. The track mostly
#'   covered fallow ground but ventured into a mature sorghum crop for a few
#'   meters on the eastern edge. The logger recorded an EM reading every 0.2
#'   seconds and a GPS reading every second. The EM38 instrument was properly
#'   calibrated for quad-phase readings but may not have been for in-phase. The
#'   intent of the exercise was to measure soil water content. The soil type at
#'   this location is a Black Vertosol under the Australian Soils Classification
#'   (Isbell, 2002).
#'
"n38_demo"
