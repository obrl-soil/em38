#' Process file header
#'
#' This function pulls out file header data from *.N38 binaries and dumps them in
#' a list. This is an internal function with no wider use case.
#' @param file_header A matrix with 2 rows and 25 columns, produced by
#'   \code{\link{n38_import}}.
#' @return A list containing 9 information elements recorded in N38 file
#'   headers.
#' @keywords internal
#' @examples
#' data('n38_demo')
#' n38_chunked <- n38_chunk(n38_demo)
#' n38_fh <- em38:::process_fheader(n38_chunked[['file_header']])
#'
process_fheader <- function(file_header = NULL) {

  out        <- vector('list', length = 9)
  names(out) <- c(   'prog_file_id',  'version_no',  'survey_type',
                        'unit_type', 'dipole_mode',  'survey_mode',
                  'instrument_type',   'file_name', 'time_samples')

  out[['prog_file_id']]    <- rawToChar(file_header[1, 1:7])
  out[['version_no']]      <- as.numeric(rawToChar(file_header[1, 10:12]))/100
  out[['survey_type']]     <- rawToChar(file_header[1, 13:15])
  out[['unit_type']]       <- switch(rawToChar(file_header[1, 16]),
                                     '0' = 'meters',
                                     '1' = 'feet')
  out[['dipole_mode']]     <- switch(rawToChar(file_header[1, 17]),
                                     '0' = 'Vertical',
                                     '1' = 'Horizontal',
                                     '2' = 'Both')
  out[['survey_mode']]     <- switch(rawToChar(file_header[1, 18]),
                                     '0' = 'Auto',
                                     '2' = 'Manual')
  out[['instrument_type']] <- switch(rawToChar(file_header[1, 20]),
                                     '1' = 'EM38-MK2-1',
                                     '2' = 'EM38-MK2-2')
  out[['file_name']]       <- trimws(rawToChar(file_header[2, 3:10]))
  out[['time_samples']]    <- if(out[['survey_mode']] == 'Auto') {
    x <- as.numeric(rawToChar(file_header[2, 12:18]))
    attr(x, 'measurement_unit') <- 'Sample frequency in seconds'
    x
  } else {
    x <- as.numeric(rawToChar(file_header[2, 12:18]))
    attr(x, 'measurement_unit') <- 'Samples per reading'
    x
  }

  return(out)
}

#' Process survey line header
#'
#' This function pulls out survey line header data from *.N38 binaries and dumps
#' them into a list. This is an internal function with no wider use case.
#' @param survline_header A matrix with 4 rows and 25 columns, produced by
#'   \code{\link{n38_import}}.
#' @return A list containing 5 information elements recorded in N38 survey line
#'   headers.
#' @keywords internal
#' @examples
#' data('n38_demo')
#' n38_chunked <- n38_chunk(n38_demo)
#' n38_slh <- em38:::process_slheader(n38_chunked[['survey_line_1']][['sl_header']])
#'
process_slheader <- function(survline_header = NULL) {

  out        <- vector('list', length = 5)
  names(out) <- c('line_name', 'start_station', 'direction',
                  'station_increment', 'timestamp')

  out[['line_name']]         <-  trimws(rawToChar(survline_header[1, 2:9]))
  out[['start_station']]     <-  as.numeric(rawToChar(survline_header[2, 2:12]))
  out[['direction']]         <-  rawToChar(survline_header[3, 2]) # consider switch
  out[['station_increment']] <-  as.numeric(rawToChar(survline_header[3, 9:18]))
  # note that no timezone info is embedded in the N38 data file
  # time is currently assumed to be 24h as no AM/PM info is available
  out[['timestamp']]         <-  as.POSIXlt(rawToChar(survline_header[4, 2:18]),
                                            format = c('%d%m%Y %H:%M:%OS'))

  return(out)

}

#' Process calibration data
#'
#' This function pulls out calibration data from *.N38 binaries and dumps it into
#' a list. This is an internal function with no wider use case.
#' @param cal_row A matrix with 1 row and 25 columns, produced by
#'   \code{\link{n38_import}}.
#' @return A list containing 3 information elements recorded in N38 calibration
#'   rows.
#' @keywords internal
#' @examples
#' data('n38_demo')
#' n38_chunked <- n38_chunk(n38_demo)
#' n38_cal1 <- em38:::process_cal(n38_chunked[['survey_line_1']][['cal_data']][1, ])
#'
process_cal <- function(cal_row = NULL) {

  out        <- vector('list', length = 3)
  names(out) <- c('channel', 'cal_current', 'cal_former')

  out[['channel']]     <- rawToChar(cal_row[2])
  out[['cal_current']] <- as.numeric(rawToChar(cal_row[3:12]))
  out[['cal_former']]  <- as.numeric(rawToChar(cal_row[14:23]))

  return(out)
}

#' Process timer relation data
#'
#' This function pulls out timer relation data from *.N38 binaries and dumps it
#' into a list. This is an internal function with no wider use case.
#' @param timer_rel A matrix with 1 row and 25 columns, produced by
#'   \code{\link{n38_import}}.
#' @return A list containing 2 information elements recorded in N38 timer
#'   relation rows.
#' @note Time will be returned in the local timezone.
#' @keywords internal
#' @examples
#' data('n38_demo')
#' n38_chunked <- n38_chunk(n38_demo)
#' n38_t <- em38:::process_timer(n38_chunked[['survey_line_1']][['timer_data']])
#'
process_timer <- function(timer_rel = NULL) {
        out  <- vector('list', length = 2)
  names(out) <- c('computer_time', 'timestamp_ms')
  # may combine these later, not sure how best to handle yet
  out[['computer_time']] <- as.POSIXlt(rawToChar(timer_rel[2:13]), format = '%H:%M:%OS')
  # sometimes timestamps are too big for integer format (2023-09-24)
  out[['timestamp_ms']]  <- as.numeric(rawToChar(timer_rel[16:25]))

  return(out)

}

#' Process instrument reading data
#'
#' This function pulls out instrument reading data from *.N38 binaries and dumps
#' it into a list. This is an internal function with no wider use case.
#' @param reading A matrix with 1 row and 25 columns, produced by
#'   \code{\link{n38_import}}.
#' @return A list containing 10 data elements recorded in N38 instrument reading
#'   rows.
#' @keywords internal
#' @examples
#' data('n38_demo')
#' n38_chunked <- n38_chunk(n38_demo)
#' n38_r1 <- em38:::process_reading(n38_chunked[['survey_line_1']][['reading_data']][1, ])
#'
process_reading <- function(reading = NULL) {

  out        <- vector('list', length = 10)
  names(out) <- c('indicator',  'marker', 'mode', 'cond_05', 'cond_1',
                  'IP_05', 'IP_1', 'temp_05', 'temp_1', 'timestamp_ms')

  out[['indicator']] <- switch(rawToChar(reading[1]),
                               'T' = 'first reading at EM38-MK2-2 station',
                               't' = 'first reading at EM38-MK2-1 station',
                               '2' = 'second reading at station')

  info_byte <- rawToBits(reading[2]) # littendian on win - os proof???
  # look into soft and ext markers
  out[['marker']] <- if(info_byte[2] == 1) {
    'no marker'
  } else {
    'trigger pressed'
  }

  out[['mode']] <- if(info_byte[3] == 1) {
    'Vertical'
  } else {
    'Horizontal'
  }

  # uncalibrated at this stage
  out[['cond_05']] <- get_cond(as.integer(reading[3]) * 256 +
                               as.integer(reading[4]))
  out[['cond_1']]  <- get_cond(as.integer(reading[7]) * 256 +
                               as.integer(reading[8]))
  out[['IP_05']]   <- get_cond(as.integer(reading[5]) * 256 +
                               as.integer(reading[6])) * 0.00720475
  out[['IP_1']]    <- get_cond(as.integer(reading[9]) * 256 +
                               as.integer(reading[10])) * 0.028819
  out[['temp_05']] <- get_temp(as.integer(reading[11]) * 256 +
                               as.integer(reading[12]))
  out[['temp_1']]  <- get_temp(as.integer(reading[13]) * 256 +
                               as.integer(reading[14]))

  out[['timestamp_ms']]  <- as.numeric(rawToChar(reading[16:25]))

  out
}

#' Process comment data
#'
#' This function pulls out comment data from *.N38 binaries and dumps it into a
#' list. This is an internal function with no wider use case.
#' @param comment A matrix with 1 row and 25 columns, produced by
#'   \code{\link{n38_import}}.
#' @return A list containing 2 data elements recorded in N38 comment rows.
#' @keywords internal
#' @examples \dontrun{
#' data('n38_demo')
#' n38_chunked <- n38_chunk(n38_demo)
#' # no comments in this file, soz. sub in yr own data
#' n38_c1 <- em38:::process_comment(n38_chunked[['survey_line_1']][['comments']][1, ])
#' }
#'
process_comment <- function(comment = NULL) {

  out        <- vector('list', length = 2)
  names(out) <- c('comment', 'timestamp_ms')

  out[['comment']]       <- trimws(rawToChar(comment[2:12]))
  out[['timestamp_ms']]  <- as.numeric(rawToChar(comment[16:25]))

  out
}

#' Process new station data
#'
#' This function pulls out new station data from *.N38 binaries and dumps it into
#' a list. This is an internal function with no wider use case.
#' @param nstat A matrix with 1 row and 25 columns, produced by
#'   \code{\link{n38_import}}.
#' @return A list containing 2 data elements recorded in N38 new station rows.
#' @keywords internal
#' @examples \dontrun{
#' data('n38_demo')
#' n38_chunked <- n38_chunk(n38_demo)
#' # no new station data in this file, soz. sub in yr own data
#' n38_ns1 <- em38:::process_nstat(n38_chunked[['survey_line_1']][['new_stations']][1, ])
#' }
#'
process_nstat <- function(nstat = NULL) {

  out        <- vector('list', length = 2)
  names(out) <- c('new_station', 'timestamp_ms')

  out[['new_station']]  <- as.integer(rawToChar(nstat[2:12]))
  out[['timestamp_ms']] <- as.numeric(rawToChar(nstat[16:25]))

  out
}
