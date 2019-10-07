#' Translate GPS data
#'
#' This function decodes GPS data for use in em38_decode and returns it along
#' with its timestamp data.
#' @param block Data frame holding GPS message data, usually a subset of
#'   $location_data in a decoded n38 object
#' @return data frame with a single row
#' @keywords internal
#' @examples
#' data('n38_demo')
#' n38_chunks  <- n38_chunk(n38_demo)
#' n38_decoded <- n38_decode(n38_chunks)
#' loc_1 <- em38:::get_loc_data(n38_decoded$survey_line_1$location_data[1:7, ])
#'
get_loc_data <- function(block = NULL) {

  # all the interesting stuff is in the GPGGA message
  gpgga  <- block[block$TYPE == 'GPGGA', ]
  gpgga  <- process_gpgga(toString(paste0(gpgga[[1]][1], ',', gpgga[[2]][1])))

  ## keeping these for later but no need to decode the entire block just yet
  #gpvtg <- em38:::process_gpvtg(paste0('$GPVTG,', x$MESSAGE[x$TYPE == 'GPVTG']))
  #gprmc <- em38:::process_gprmc(paste0('$GPRMC,', x[x$TYPE == 'GPRMC', 'MESSAGE' ]))
  #gpgsa <- em38:::process_gpgsa(paste0('$GPGSA,', x[x$TYPE == 'GPGSA', 'MESSAGE' ]))
  #gpgsv <- split(x[x$TYPE == 'GPGSV', 'MESSAGE' ], 1:nrow(x[x$TYPE == 'GPGSV', ]))
  #gpgsv <- lapply(gpgsv, function(row) { em38:::process_gpgsv(paste0('$GPGSV,', row)) })

  # what actually needs to be output?
  data.frame('LATITUDE'     = gpgga[['latitude']],
             'LONGITUDE'    = gpgga[['longitude']],
             'FIX'          = gpgga[['fix_quality']],
             'HDOP'         = gpgga[['HDOP']],
             'CHKSUM'       = block$CHKSUM[block$TYPE == 'GPGGA'],
             'timestamp_ms' = block$timestamp_ms[block$TYPE == 'GPGGA'])
  # if this fails it'll likely be that messages are coming from
  # a GPS device that doesn't return GPGGA (e.g. GLGPA, or GPRMC)
  # will defo need to add handlers for GLONASS and other systems but need test
  # data

  }

#' Process EM38 Survey line
#'
#' This function processes an EM38 survey line into a data frame, which may be
#' spatial.
#' @param survey_line a decoded survey line from \code{\link{n38_decode}}.
#' @param hdop_filter Numeric, discard GPS data where the Horizontal Dilution of
#'   Precision is greater than this number. Defaults to 3 metres. Set to NULL to
#'   keep all readings.
#' @param time_filter Numeric, discard readings taken more than \code{n} seconds
#'   after the last acceptable GPS reading. Usually best to set this to 2-3x GPS
#'   aquisition frequency.
#' @param fix_filter Select readings with a minimum quality of GPS fix.
#'   Options are:
#'  \enumerate{
#'   \item Autonomous GPS fix.
#'   \item DGPS fix, using a local DGPS base station or correction service
#'   such as WAAS or EGNOS.
#'   \item Pulse per second (PPS) fix.
#'   \item real-time kinematic (RTK) fix.
#'   \item RTK float fix.
#'   \item estimated (dead reckoning)
#'   \item manual input mode
#'   \item simulation mode
#'   \item WAAS fix.
#'   }
#' To filter on multiple options, supply a vector e.g. \code{c(2, 9)}.
#' @return If valid GPS data is present, an `sf` data frame with sfc_POINT
#'   geometry. Otherwise, if valid instrument data is present, a data frame.
#'   Otherwise, a string explaining failure to process.
#' @note Inclusion of comments is yet to be implemented.
#' @examples
#' data('n38_demo')
#' n38_chunks  <- n38_chunk(n38_demo)
#' n38_decoded <- n38_decode(n38_chunks)
#' demo_line_1 <- em38_surveyline(n38_decoded[[2]], 3)
#' demo_line_2 <- em38_surveyline(n38_decoded[[2]], 3, 0.2)
#'
#' @importFrom dplyr bind_rows group_by lead lag mutate ungroup
#' @importFrom sf st_as_sf
#' @importFrom tidyr fill
#' @importFrom rlang .data
#' @export
#'
em38_surveyline <- function(survey_line = NULL,
                            hdop_filter = NULL,
                            time_filter = NULL,
                            fix_filter  = NULL) {

  hdr <- survey_line[['sl_header']]
  cal <- survey_line[['cal_data']]
  tim <- survey_line[['timer_data']]
  rdg <- survey_line[['reading_data']]
  loc <- survey_line[['location_data']]

  # quality checks
  if(all(dim(rdg)[1] == 0, any(dim(loc)[1] == 0, is.null(dim(loc))))) {
    return('This survey line contains no readings or location data.')
  }
  if(any(dim(rdg)[1] == 0, is.null(dim(rdg)))) {
    return('This survey line contains no readings.')
    }
  # note that calibration data, if present, is applied by n38_decode()
  if(any(dim(cal)[1] == 0, is.null(dim(cal)))) {
    warning("Survey line is uncalibrated")
    }

  # if input had readings but no GPS data recorded, process as non-spatial
  if(any(dim(loc)[1] == 0, is.null(dim(loc)))) {
    return(cbind('ID' = seq(nrow(rdg)), rdg[, !names(rdg) %in% 'timestamp_ms'],
                 "date_time" = conv_stamp(tim$computer_time, tim$timestamp_ms,
                                          rdg$timestamp_ms)
                 ))
  }

  ### location data processing
  # group loc data by repeating sequence of records
  loc$lag_chk <- ifelse(loc$TYPE == loc$TYPE[1], TRUE, FALSE)
  loc$group   <- cumsum(loc$lag_chk) # can't use CHKSUM bc NA stuffs grouping
  loc_s <- split(loc, loc$group)
  # drop any chunks that don't have a GPGGA message (usually a start/pause
  # error)
  keep <- sapply(loc_s, function(x) {
    if(any(x$TYPE %in% 'GPGGA')) { TRUE } else { FALSE }
  })
  loc_s <- loc_s[keep]
  # decode messages and keep only the essential data
  loc_s <- lapply(loc_s, function(x) {
    get_loc_data(x)
  })
  loc_f <- do.call('rbind', loc_s)

  # remove checksum failures
  loc_f <- loc_f[loc_f$CHKSUM == TRUE, ]

  if(dim(loc_f)[1] == 0) { # no unscrambled gps data (unlikely)
    return(cbind('ID' = seq(nrow(rdg)), rdg[, !names(rdg) %in% 'timestamp_ms'],
                 "date_time" = conv_stamp(tim$computer_time, tim$timestamp_ms,
                                          rdg$timestamp_ms)
    ))
  }

  # remove no-fix failures
  loc_f <- loc_f[loc_f$FIX != 0, ]

  if(dim(loc_f)[1] == 0) { # gps data but no signal fix achieved
    return(cbind('ID' = seq(nrow(rdg)), rdg[, !names(rdg) %in% 'timestamp_ms'],
                 "date_time" = conv_stamp(tim$computer_time, tim$timestamp_ms,
                                          rdg$timestamp_ms))
    )
  }

  # filter on fix type if supplied
  if(!is.null(fix_filter)) {
    loc_f <- loc_f[loc_f$FIX %in% fix_filter, ]
  }

  if(dim(loc_f)[1] == 0) {
    return('No readings with the supplied fix type could be retrieved from this survey line.')
  }

  # filter out low-precision locations
  # note that this is not done by the nmea parser on purpose - prefer record
  # sequence intact for grouping above
  loc_f <- if(!is.null(hdop_filter)) {
    loc_f[loc_f$HDOP < hdop_filter, ]
  } else {
    loc_f[!is.na(loc_f$HDOP), ]
  }

  if(dim(loc_f)[1] == 0) {
    return('No readings with acceptable HDOP could be retrieved from this survey line.')
  }

  # add lead(lat) and lead(long) for interpolation, plus time lag between
  # readings. Timestamps are proxying for distance fractions later
  loc_f$LEAD_LAT  <- dplyr::lead(loc_f$LATITUDE)
  loc_f$LEAD_LONG <- dplyr::lead(loc_f$LONGITUDE)
  loc_f$TS_LAG    <- dplyr::lead(loc_f$timestamp_ms) - loc_f$timestamp_ms

  # remove readings outside first and last gps reading
  readings_in <- rdg[rdg$timestamp_ms > loc_f$timestamp_ms[1] &
                       rdg$timestamp_ms < loc_f$timestamp_ms[dim(loc_f)[1]], ]

  # nb might be able to add them back in later (not yet impemented)
  #readings_out <-
  #  readings[!(readings$timestamp_ms %in% readings_in$timestamp_ms), ]

  all_data <- dplyr::bind_rows(loc_f, readings_in)
  all_data <-
    all_data[with(all_data, order(all_data$timestamp_ms)), ]
  all_data$TS_LAG_AD <-
    all_data$timestamp_ms - dplyr::lag(all_data$timestamp_ms)
  all_data$TS_LAG_AD[is.na(all_data$TS_LAG_AD)] <- 0

  # fill a few values in so all instrument readings have a 'from' and 'to'
  # for interpolation
  all_data <-
    tidyr::fill(all_data,
                .data$LATITUDE, .data$LONGITUDE,
                .data$LEAD_LAT, .data$LEAD_LONG,
                .data$TS_LAG,
                .direction = 'down'
    )
  # group recombined data so that GPS reading(s) and following instrument
  # reading(s) are together

  # grouping by sequence is hard and this seems awful but whateverrrrr
  grp <-
    data.frame('rle' = rle(ifelse(is.na(all_data$HDOP), 1, 0))$lengths)
  grp$grp <- c(rep(seq.int(dim(grp)[1] / 2), each = 2),
               ceiling(dim(grp)[1] / 2))
  grp     <- split(grp, grp$grp)
  grp     <- sapply(grp, function(x) { sum(x$rle) })

  all_data$GROUP <- unlist(mapply(function(x, y) { rep(x, times = y) },
                                  x = as.integer(names(grp)), y = grp,
                                  SIMPLIFY = FALSE))

  # get cumulative time lag within each group (effectively distance
  # between last gps reading and current instrument reading)
  all_data$ind3 = ifelse(is.na(all_data$HDOP), 1, 0)
  all_data <- dplyr::group_by(all_data, .data$GROUP, .data$ind3)
  all_data <- dplyr::mutate(all_data, TS_NOW = cumsum(.data$TS_LAG_AD))
  all_data <- dplyr::ungroup(all_data)
  all_data$TS_NOW <- ifelse(!is.na(all_data$HDOP), 0 , all_data$TS_NOW)

  # filter readings more than time_filter seconds after the last acceptable
  # GPS reading
  if(!is.null(time_filter)) {
    all_data <- all_data[all_data$TS_NOW < time_filter * 1000, ]
  }

  # use 2D linear interpolation to get locations for instrument readings
  # no point getting geodetic here, we're generally working at very short
  # distances
  # https://math.stackexchange.com/questions/1918743/how-to-interpolate-points-between-2-points#1918765
  all_data$NEW_LAT <- all_data$LATITUDE  + (
    all_data$TS_NOW / all_data$TS_LAG  *
      (all_data$LEAD_LAT  - all_data$LATITUDE)
  )
  all_data$NEW_LONG <- all_data$LONGITUDE  + (
    all_data$TS_NOW / all_data$TS_LAG  *
      (all_data$LEAD_LONG  - all_data$LONGITUDE)
  )
  # 7 dp = 1cm
  all_data$NEW_LAT <- round(all_data$NEW_LAT, 7)
  all_data$NEW_LONG <- round(all_data$NEW_LONG, 7)

  # convert timestamp to real time
  all_data$date_time <- conv_stamp(tim$computer_time, tim$timestamp_ms,
                                   all_data$timestamp_ms)

  # filter to just keep instrument readings and interpolated locations
  out_data <-
    all_data[, c('indicator', 'marker', 'mode', 'cond_05', 'cond_1', 'IP_05',
                 'IP_1', 'temp_05', 'temp_1', 'date_time', 'NEW_LAT', 'NEW_LONG')]
  out_data <- out_data[complete.cases(out_data), ]
  out_data <- cbind('ID' = seq(nrow(out_data)), out_data)

  # spatialise output and return
  sf::st_as_sf(out_data,
               coords = c('NEW_LONG', 'NEW_LAT'),
               crs = 4326) # may need to epoch at some point??
  }


#' Process EM38 data
#'
#' This function processes the outputs of \code{\link{n38_decode}} into a
#' useable end product.
#' @param n38_decoded Nested list output by \code{\link{n38_decode}}.
#' @param hdop_filter Numeric, discard GPS data where the Horizontal Dilution of
#'   Precision is greater than this number. Defaults to 3 metres. Set to NULL to
#'   keep all readings.
#' @param time_filter Numeric, discard readings taken more than \code{n} seconds
#'   after the last acceptable GPS reading. Set to 2-3x GPS aquisition
#'   frequency. Defaults to 5 seconds. Set to NULL to keep all readings.
#' @param fix_filter Select readings with a minimum quality of GPS fix.
#'   Options are:
#'  \enumerate{
#'   \item Autonomous GPS fix.
#'   \item DGPS fix, using a local DGPS base station or correction service
#'   such as WAAS or EGNOS.
#'   \item Pulse per second (PPS) fix.
#'   \item real-time kinematic (RTK) fix.
#'   \item RTK float fix.
#'   \item estimated (dead reckoning)
#'   \item manual input mode
#'   \item simulation mode
#'   \item WAAS fix.
#'   }
#' To filter on multiple options, supply a vector e.g. \code{c(2, 9)}. Defaults
#' to NULL.
#' @return A two element list containing file header data, and a list of
#'   processed survey lines. For each survey line, an `sf` data frame with
#'   sfc_POINT geometry is returned where valid GPS data exists. If instrument
#'   readings without valid GPS data are present, a data frame is returned.
#'   Otherwise, a string is returned explaining the failure to process.
#' @examples
#' data('n38_demo')
#' n38_chunks  <- n38_chunk(n38_demo)
#' n38_decoded <- n38_decode(n38_chunks)
#' demo_survey <- em38_decode(n38_decoded, 3)
#' @export
#'
em38_decode <- function(n38_decoded = NULL, hdop_filter = 3,
                        time_filter = 5, fix_filter = NULL) {

  # process each survey line
  survey_lines <-
    lapply(2:length(n38_decoded), function(x) {
      em38_surveyline(n38_decoded[[x]], hdop_filter = hdop_filter,
                      time_filter = time_filter, fix_filter = fix_filter)
    })
  names(survey_lines) <- seq(length(survey_lines))

  list('file_header' = n38_decoded[[1]], # file header,
       'survey_lines' = survey_lines)

}

#' Import and convert N38 logfiles
#'
#' This is a wrapper function that processes a raw on-disk N38 file into useable
#' data.
#' @param path A file path pointing to a valid *.N38 file, produced by a
#'   Geonics EM38-MK2 conductivity sensor connected to an Allegra CX or Archer
#'   datalogger (and optionally, a GPS device).
#' @param hdop_filter Numeric, discard GPS data where the Horizontal Dilution of
#'   Precision is greater than this number. Defaults to 3 metres. Set to NULL to
#'   keep all readings.
#' @param time_filter Numeric, discard readings taken more than \code{n} seconds
#'   after the last acceptable GPS reading. Set to 2-3x GPS aquisition
#'   frequency. Defaults to 5 seconds. Set to NULL to keep all readings.
#' @param fix_filter Select readings with a minimum quality of GPS fix.
#'   Options are:
#'  \enumerate{
#'   \item Autonomous GPS fix.
#'   \item DGPS fix, using a local DGPS base station or correction service
#'   such as WAAS or EGNOS.
#'   \item Pulse per second (PPS) fix.
#'   \item real-time kinematic (RTK) fix.
#'   \item RTK float fix.
#'   \item estimated (dead reckoning)
#'   \item manual input mode
#'   \item simulation mode
#'   \item WAAS fix.
#'   }
#' To filter on multiple options, supply a vector e.g. \code{c(2, 9)}. Defaults
#' to NULL.
#' @return A two element list containing file header data, and a list of
#'   processed survey lines. For each survey line, an `sf` data frame with
#'   sfc_POINT geometry is returned where valid GPS data exists. If instrument
#'   readings without valid GPS data are present, a data frame is returned.
#'   Otherwise, a string is returned explaining the failure to process.
#' @examples
#' demo_survey <-
#' em38_from_file(path = system.file("extdata", "em38_demo.N38", package = "em38"),
#'               hdop_filter = 3)
#' @export
#'
em38_from_file <- function(path = NULL, hdop_filter = 3,
                           time_filter = 5, fix_filter = NULL) {
  mat  <- n38_import(path)
  chnk <- n38_chunk(mat)
  dec  <- n38_decode(chnk)

  em38_decode(n38_decoded = dec, hdop_filter = hdop_filter,
              time_filter = time_filter, fix_filter = fix_filter)

}

#' Reconcile locations of paired data
#'
#' Where paired horizontal and vertical readings have been taken during a
#' 'manual' mode survey, the first and second readings at each station should
#' have the same location. The nature of the device logging generally precludes
#' this from happening by default, especially with high-frequency GPS recording.
#' This function reconciles the locations of such paired datasets after they
#' have been generated using \code{\link{em38_decode}} or
#' \code{\link{em38_from_file}}.
#' @param decode spatial point dataframe for a survey line produced by
#'   \code{\link{em38_decode}} or \code{\link{em38_from_file}}.
#' @param time_filter removes point pairs that are close together but that were
#'   sampled more than n seconds apart.
#' @return An sf data frame with sfc_POINT geometry. WGS84 projection. Output
#'   locations are averages of input horizontal/vertical paired locations.
#' @note Input survey should be of survey type 'GPS' and record type 'manual'.
#'   Both input datasets should ideally have the same number of rows, with row 1
#'   of horizontal_data paired with row 1 of vertical_data.
#' @importFrom purrr map2
#' @import sf
#' @export
#'
em38_pair <- function(decode = NULL, time_filter = NULL) {

  # which orientation has the fewest readings?
  n_v <- length(which(decode[['mode']] == 'Vertical'))
  n_h <- length(which(decode[['mode']] == 'Horizontal'))
  min_mode <- which(c(n_v, n_h) == min(c(n_v, n_h)))

  if(min_mode == 1) {
    anchor <- decode[which(decode[['mode']] == 'Vertical'), ]
    target <- decode[which(decode[['mode']] == 'Horizontal'), ]
  } else {
    anchor <- decode[which(decode[['mode']] == 'Horizontal'), ]
    target <- decode[which(decode[['mode']] == 'Vertical'), ]
  }

  # find the nearest target to each anchor
  closest <- sf::st_nearest_feature(anchor, target)
  target <- target[closest, ]

  # take paired horizontal and vertical em38 readings and reconcile their
  # locations
  pts <- purrr::map2(.x = sf::st_geometry(anchor),
                     .y = sf::st_geometry(target), function(.x, .y) {
    out_long <- mean(c(as.vector(.x)[1],
                     as.vector(.y)[1]))
    out_lat  <- mean(c(as.vector(.x)[2],
                     as.vector(.y)[2]))

    sf::st_point(c(out_long, out_lat))
  })

  # interleave filtered data
  anchor$ID <- seq(nrow(anchor) * 2)[c(TRUE, FALSE)]
  anchor$GRP <- seq(nrow(anchor))
  target$ID <- seq(nrow(target) * 2)[c(FALSE, TRUE)]
  target$GRP <- seq(nrow(target))
  both <- rbind(sf::st_set_geometry(anchor, NULL),
                sf::st_set_geometry(target, NULL))
  both <- both[order(both$ID), ]

  out <- sf::st_as_sf(both, geometry = pts, crs = 4326)

  if(!is.null(time_filter)) {
    del <- split(out$date_time, out$GRP)
    del <- sapply(del, function(x) {
      abs(as.numeric(difftime(x[2], x[1], units = 'secs')))
    })
    keep <- which(del <= time_filter)
    out <- out[out$GRP %in% keep, ]
  }

  out

}
