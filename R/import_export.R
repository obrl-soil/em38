#' Import EM38 data
#'
#' This function reads N38 binary files into R and does some minimal
#' pre-processing.
#' @param path A file path pointing to a valid *.N38 file, produced by a Geonics
#'   EM38-MK2 conductivity sensor connected to an Allegra CX or Archer
#'   datalogger (and optionally, a GPS device).
#' @return A matrix with n rows and 25 columns, containing raw bytes.
#' @examples
#' n38_mat <-
#'   n38_import(system.file("extdata", "em38_demo.N38", package = "em38"))
#' @export
#'
n38_import <- function(path = NULL) {

  n38_con <- file(path, open = 'rb')
  n38_raw <- readBin(n38_con, what = raw(), n = file.size(path))
  close(n38_con)

  n38_mat <- matrix(n38_raw, ncol = 26, byrow = TRUE)
  # chuck the line end column
  n38_mat[, -26]
}

#' Separate message types
#'
#' This function chunks imported n38 binary matrices into a list, grouping file
#' header and survey line information for further processing.
#' @param n38_mat The binary matrix output by  \code{\link{n38_import}}.
#' @return A nested list containing binary matrices encoding a) file header
#'   information and b) each survey line in the input file. For each survey
#'   line, a list of matrices with survey line data separated by type - header
#'   and calibration data, instrument readings, position information, and
#'   comments.
#' @examples
#' data('n38_demo')
#' n38_chunks <- n38_chunk(n38_demo)
#' @export
#'
n38_chunk <- function(n38_mat = NULL) {

  # pull all the start-of-row characters, they ID what each row is about
  rids <- rawToChar(n38_mat[ , 1], multiple = TRUE)
  # count total number of survey lines in file
  slines <- length(which(rids == 'L'))

  # establish a list with one file header entry and n survey line entries
  out_list <- vector('list', length = 1 + slines)
  names(out_list) <- c('file_header', paste0('survey_line_', 1:slines))

  # populate said list with parts of the input matrix as appropriate

  # file header
  fh_rns <- match('E', rids)
  out_list[['file_header']] <- n38_mat[fh_rns:(fh_rns + 1), ]

  # get each survey line start and end row numbers into two vectors
  sl_starts <- which(rids == 'L')
  sl_ends   <- if(length(sl_starts) == 1) {
    dim(n38_mat)[1]
  } else {
    c(sl_starts[2:length(sl_starts)] - 1, dim(n38_mat)[1])
  }

  out_list[2:length(out_list)] <- lapply(2:length(out_list), function(i) {
    # ugh but w/e
    sl_x <- n38_mat[sl_starts[i - 1]:sl_ends[i - 1], ]

    # separate the record types within each survey line
    sline_list <- vector('list', length = 7)
    names(sline_list) <- c('sl_header', 'cal_data', 'timer_data',
                           'reading_data', 'location_data', 'new_station',
                           'comments')

    # get the row indices for each category in sline_list
    sl_rids <- rids[sl_starts[i - 1]:sl_ends[i - 1]]

    # survey line header
    sline_list[['sl_header']] <-
      sl_x[which(sl_rids %in% c('L', 'B', 'A', 'Z')), , drop = FALSE]

    # calibration data
    sline_list[['cal_data']] <- sl_x[which(sl_rids == 'O'), , drop = FALSE]

    # timer data
    sline_list[['timer_data']]   <- sl_x[which(sl_rids == '*'), ]

    # instrument readings
    sline_list[['reading_data']] <-
      sl_x[which(sl_rids %in% c('T', 't', '2')), , drop = FALSE]

    # location data (note readings that fail the checksum still come through
    # here and are handled later)
    gps <- which(!(sl_rids %in% c('L', 'B', 'A', 'Z','O', '*', 'T', 't', '2',
                                  'C', 'S', 'X')))
    sline_list[['location_data']] <- sl_x[gps, , drop = FALSE]

    # new station (nb often null, for when no GPS in use???)
    sline_list[['new_station']] <- sl_x[which(sl_rids == 'S'), , drop = FALSE]

    # comments (nb often null bc who has the time really)
    sline_list[['comments']] <- sl_x[which(sl_rids == 'C'), , drop = FALSE]

    sline_list
  })

  out_list
}

#' process chunked n38 data
#'
#' This function decodes n38 data according to the specifications provided by
#' the EM38-MKII manual.
#' @param chunks The nested list of raw matrices output by
#'   \code{\link{n38_chunk}}.
#' @return A nested list containing decoded n38 data with the following
#'   structure:
#'   \itemize{
#'   \item file header information (list)
#'   \item survey line
#'   \item header information (list)
#'   \item calibration data (data frame)
#'   \item timer data (list)
#'   \item instrument readings (timestamped data frame, calibrated)
#'   \item location data (timestamped data frame of NMEA-0138 message strings)
#'   \item new station data (timestamped data frame)
#'   \item comments (timestamped data frame)
#'   }
#'
#' @examples
#' data('n38_demo')
#' n38_chunks <- n38_chunk(n38_demo)
#' n38_decoded <- n38_decode(n38_chunks)
#' @importFrom purrr map map_lgl transpose
#' @export
#'
n38_decode <- function(chunks = NULL) {

  chunks[['file_header']] <- process_fheader(chunks[['file_header']])

  chunks[2:length(chunks)] <- lapply(2:length(chunks), function(i) {

    chunks[[i]][['sl_header']] <- process_slheader(chunks[[i]][['sl_header']])

    chunks[[i]][['cal_data']] <- apply(chunks[[i]][['cal_data']],
                                       MARGIN = 1,
                                       FUN = function(x) process_cal(x))
    chunks[[i]][['cal_data']] <- purrr::transpose(chunks[[i]][['cal_data']])
    chunks[[i]][['cal_data']] <-
      as.data.frame(lapply(chunks[[i]][['cal_data']], unlist),
                    stringsAsFactors = FALSE)

    chunks[[i]][['timer_data']] <- process_timer(chunks[[i]][['timer_data']])
    # adjust date to that given by survey line header
    chunks[[i]][['timer_data']][['computer_time']]$mday <-
      chunks[[i]][['sl_header']][['timestamp']]$mday
    chunks[[i]][['timer_data']][['computer_time']]$mon  <-
      chunks[[i]][['sl_header']][['timestamp']]$mon
    chunks[[i]][['timer_data']][['computer_time']]$year <-
      chunks[[i]][['sl_header']][['timestamp']]$year
    chunks[[i]][['timer_data']][['computer_time']]$wday <-
      chunks[[i]][['sl_header']][['timestamp']]$wday
    chunks[[i]][['timer_data']][['computer_time']]$yday <-
      chunks[[i]][['sl_header']][['timestamp']]$yday

    # process instrument readings
    chunks[[i]][['reading_data']] <- apply(chunks[[i]][['reading_data']],
                                           MARGIN = 1,
                                           FUN = function(x) process_reading(x))
    chunks[[i]][['reading_data']] <-
      purrr::transpose(chunks[[i]][['reading_data']])
    chunks[[i]][['reading_data']] <-
      lapply(chunks[[i]][['reading_data']], function(x) {
      unlist(x, recursive = FALSE)
    })
    chunks[[i]][['reading_data']] <-
      as.data.frame(chunks[[i]][['reading_data']],
                    col.names = names(chunks[[i]][['reading_data']]),
                    stringsAsFactors = FALSE)

    # apply calibration factors to instrument readings
    chunks[[i]][['reading_data']]$cond_05 <-
      chunks[[i]][['reading_data']]$cond_05 +
      chunks[[i]][['cal_data']]$cal_current[2]

    chunks[[i]][['reading_data']]$cond_1 <-
      chunks[[i]][['reading_data']]$cond_1 +
      chunks[[i]][['cal_data']]$cal_current[1]

    chunks[[i]][['reading_data']]$IP_05 <- mapply(function(mode, reading) {
      if(mode == 'Vertical') {
        reading - chunks[[i]][['cal_data']]$cal_current[4]
      } else {
        reading - chunks[[i]][['cal_data']]$cal_current[6]
      }
    },
    mode    = chunks[[i]][['reading_data']]$mode,
    reading = chunks[[i]][['reading_data']]$IP_05)

    chunks[[i]][['reading_data']]$IP_1 <- mapply(function(mode, reading) {
      if(mode == 'Vertical') {
        reading - chunks[[i]][['cal_data']]$cal_current[3]
      } else {
        reading - chunks[[i]][['cal_data']]$cal_current[5]
      }
    },
    mode    = chunks[[i]][['reading_data']]$mode,
    reading = chunks[[i]][['reading_data']]$IP_1)

    # location data
    # Each reading starts with @, different types are different
    # lengths, and there are variable stretches of whitespace plus multiline
    # signifiers to deal with, what a fun time this was
    chunks[[i]][['location_data']] <-
      if(dim(chunks[[i]][['location_data']])[1] == 0) {
        NA
        } else {
          loc  <- rawToChar(unlist(t(chunks[[i]][['location_data']])),
                            multiple = TRUE)
          locs <- split(loc, cumsum(loc %in% c('@', '?')))

    # ditch the `#` newline signifiers and whitespace, convert to string
    locs <- lapply(locs, function(x) {
      paste0(x[- which(x %in% c('#', ' '))], collapse = '')
    })

    # The following ditches checksum fails that have already been flagged by GPS
    # software. These start with ? not @ and use " instead of # as an internal
    # newline signifier. Also handled are cases where GPS messages can
    # occasionally get cut off after the message comes through but before the
    # timestamp does (where someone hits pause at the wrong time)
    keep <- purrr::map_lgl(locs, function(x) grepl('^@.+\\!', x))
    locs <- locs[keep]

    # split up string into main components (decode happens later)
    # note that only TYPE = 'GPGGA' is used in final output
    # will probs have to change to %in% c('GPGGA', 'GLGGA') soon
    bang <- as.integer(gregexpr('!', locs))
    type <- substr(locs, 3, 7)
    out  <- list('TYPE'         = type,
                 'MESSAGE'      = substr(locs, 9, bang - 1),
                 # NB this only works if a checksum failure does not involve
                 # scrambling/loss of the message type - check is skipped if
                 # GPGGA is scrambled to e.g. GPGA,. Such failures are still
                 # never decoded down the track so not a big deal
                 'CHKSUM'       = ifelse(type == 'GPGGA',
                                         nmea_check(substr(locs, 2, bang - 1)),
                                         NA),
                 'timestamp_ms' =
                   as.integer(substr(locs, bang + 1, nchar(locs))))
    as.data.frame(out, col.names = names(out), stringsAsFactors = FALSE)
    }

    chunks[[i]][['new_station']] <-
      if(dim(chunks[[i]][['new_station']])[1] == 0) {
        NA
        } else {
          process_nstat(chunks[[i]][['new_station']])
        }

    chunks[[i]][['comments']] <- if(dim(chunks[[i]][['comments']])[1] == 0) {
      NA
      } else {
        process_comment(chunks[[i]][['comments']])
      }

    chunks[[i]]
  })

  chunks

}

#' output m38
#'
#' This function processes decoded n38 data into the m38 format used by Geonics.
#' Its a bit useless from an R perspective, but users may wish to compare
#' outputs with Geonics' own conversion software or process it further with the
#' DAT38MK2 application.
#' @param n38_decoded Nested list output by n38_decode
#' @return Large character; combination of location data and calibrated
#'   instrument readings. Note that record timestamps won't always agree with an
#'   'official' m38 file at the microsecond level.
#' @note Write output to file in working directory with e.g. `write(m38_example,
#'   paste0('m38_from_R_', Sys.Date(), '.m38'))`
#' @examples
#' data('n38_demo')
#' n38_chunks <- n38_chunk(n38_demo)
#' n38_decoded <- n38_decode(n38_chunks)
#' m38_example <- n38_to_m38(n38_decoded)
#' @importFrom purrr flatten
#' @importFrom stats complete.cases
#' @export
#'
n38_to_m38 <- function(n38_decoded = NULL) {
  options(digits.secs = 3)

  # shrug emoji
  fn <- paste0(c(n38_decoded$file_header$file_name,
          rep.int(' ',
                  times = (8 - length(n38_decoded$file_header$file_name)))),
          collapse = '')
  file_header <- paste0(substr(n38_decoded$file_header$prog_file_id, 1, 4), '-',
                     substr(n38_decoded$file_header$prog_file_id, 5, 7), ' V',
                     n38_decoded$file_header$version_no, ' ', fn)

  survey_lines <- lapply(2:length(n38_decoded), function(i) {

    sl_mode <- if(length(unique(n38_decoded[[i]]$reading_data$mode)) > 1) {
      'B'
    } else if(unique(n38_decoded[[i]]$reading_data$mode) == 'Horizontal') {
      'H'
    } else {
      'V'
    }
    slunits <-
      if (n38_decoded$file_header$unit_type == 'meters') {
        0
      } else {
        1
      }
    slmode <-
      if (n38_decoded$file_header$survey_mode == 'Auto') {
        'A'
      } else {
        'M'
      }
    datetime <- as.character(n38_decoded[[i]]$sl_header$timestamp,
                             format = '%d/%m/%Y %H:%M:%S')

    sl_header <- paste0('L', paste0(rep.int(' ', times = 3), collapse = ''),
           n38_decoded[[i]]$sl_header$line_name, ' ',
            paste0(rep.int(' ', 8 - nchar(n38_decoded[[i]]$sl_header$line_name)),
                  collapse = ''),
           'B', sl_mode, 10, slunits, ' ', n38_decoded[[i]]$sl_header$direction, ' ',
                  slmode, sprintf('%.3f', n38_decoded$file_header$time_samples),
           ' ', fn, '   ', datetime)

    # add station data to readings
    # increment is -ve when GPS not in use and GRD dir is South or West
    # must make it easier to recombine back-and-forth tracks
    n38_decoded[[i]]$reading_data$station <-
      if(n38_decoded[[i]]$sl_header$station_increment > 0) {
     seq(from = n38_decoded[[i]]$sl_header$start_station,
         to = dim(n38_decoded[[i]]$reading_data)[1],
         by = n38_decoded[[i]]$sl_header$station_increment)
    } else {
      seq(from = n38_decoded[[i]]$sl_header$start_station,
          to = n38_decoded[[i]]$sl_header$start_station - (dim(n38_decoded[[i]]$reading_data)[1] - 1),
          by = n38_decoded[[i]]$sl_header$station_increment)
    }


    reading_split <- split(n38_decoded[[i]]$reading_data,
                           seq.int(dim(n38_decoded[[i]]$reading_data)[1]))
    # slow :/
    readings <- purrr::map_chr(reading_split, function(row) {

      param_1 <- if(grep('first', row['indicator']) == 1)  {'S'} else {'R'}
      param_2 <- if(row['mode'] == 'Vertical')             {'V'} else {'H'}
      param_3 <- if(row['marker'] == 'no marker')          { 0 } else { 1 }
      param_4 <- if(n38_decoded[[1]][[1]] == 'EM38MK2')    { 1 } else { 0 }


      paste0(param_1, param_2, param_3, param_4, ',',
             paste0(rep.int(' ', 13 - nchar(sprintf('%.3f', row['station']))),
                    collapse = ''),
             sprintf('%.3f', row['station']), ',',
             # cond 1m, - all numbers are formatted to F11.3
             paste0(rep.int(' ', 11 - nchar(sprintf('%.3f', row['cond_1']))), collapse = ''),
             sprintf('%.3f', row['cond_1']), ',',
             # IP 1m
             paste0(rep.int(' ', 11 - nchar(sprintf('%.3f', row['IP_1']))), collapse = ''),
             sprintf('%.3f', row['IP_1']), ',',
             # cond 0.5m
             paste0(rep.int(' ', 11 - nchar(sprintf('%.3f', row['cond_05']))), collapse = ''),
             sprintf('%.3f', row['cond_05']), ',',
             # IP 0.5m
             paste0(rep.int(' ', 11 - nchar(sprintf('%.3f', row['IP_05']))), collapse = ''),
             sprintf('%.3f', row['IP_05']), ',',
             # temp 1m
             paste0(rep.int(' ', 11 - nchar(sprintf('%.3f', row['temp_1']))), collapse = ''),
             sprintf('%.3f', row['temp_1']), ',',
             # temp 0.5m
             paste0(rep.int(' ', 11 - nchar(sprintf('%.3f', row['temp_05']))), collapse = ''),
             sprintf('%.3f', row['temp_05']), ',',
             # timestamp - should be ok but doesn't quite match offical version -
             # some kind of rounding at 3rd dec pl, difference is never more than a couple of ms
             # main thing is that order of records is not affected so all good
             as.character(n38_decoded[[i]]$timer_data$computer_time +
                            (row$timestamp_ms - n38_decoded[[i]]$timer_data$timestamp_ms) / 1000,
                          format = '%H:%M:%OS') # note options set above
      )
    })

  new_stations <- if(!is.na(n38_decoded[[i]]$new_station)) {
      apply(n38_decoded[[i]]$new_station, MARGIN = 1, FUN = function(row) {
        # haven't seen one of these yet so not sure if this is correct
        paste0('S', row$new_station,
               as.character(n38_decoded[[i]]$timer_data$computer_time +
                              (row$timestamp_ms - n38_decoded[[i]]$timer_data$timestamp_ms) / 1000,
                            format = '%H:%M:%OS'))
      })} else {NA}

  comments <- if(!is.na(n38_decoded[[i]]$comments)) {
    apply(n38_decoded[[i]]$comments, MARGIN = 1, FUN = function(row) {
      # haven't seen one of these yet so not sure if this is correct
      paste0('C', row$comment,
             as.character(n38_decoded[[i]]$timer_data$computer_time +
                            (row$timestamp_ms - n38_decoded[[i]]$timer_data$timestamp_ms) / 1000,
                          format = '%H:%M:%OS'))
    })
  } else {
      NA
  }

  # only some location messages are output
  loc_subset <-
    n38_decoded[[i]]$location_data[n38_decoded[[i]]$location_data$TYPE %in%
                                     c('GPGGA', 'GPGSA') , ]

  locations <- apply(loc_subset, MARGIN = 1, FUN = function(row) {
      sentence <- paste0('$', row['TYPE'], ',', row['MESSAGE'], ',')
      timestamp <-
        as.character(n38_decoded[[i]]$timer_data$computer_time +
                       (as.numeric(row['timestamp_ms']) -
                          n38_decoded[[i]]$timer_data$timestamp_ms) / 1000,
                     format = '%H:%M:%OS')
      padding <-
        paste0(rep.int(' ', times = 103 - (nchar(sentence) + nchar(timestamp))),
               collapse = '')
      paste0(sentence, padding, timestamp)
    })

  readings_done <-
    data.frame('DATA' = readings,
               'TIME' = n38_decoded[[i]]$reading_data$timestamp_ms,
               stringsAsFactors = FALSE)

  nstat_done  <- if(!is.na(n38_decoded[[i]]$new_station)) {
    data.frame('DATA' = new_stations,
               'TIME' = n38_decoded[[i]]$new_station$timestamp_ms,
               stringsAsFactors = FALSE)
  } else {
    # ughhhh
    data.frame('DATA' = NA_character_,
               'TIME' = NA_integer_,
               stringsAsFactors = FALSE)
  }

  comments_done  <- if(!is.na(n38_decoded[[i]]$comments)) {
    data.frame('DATA' = comments,
               'TIME' = n38_decoded[[i]]$comments$timestamp_ms,
               stringsAsFactors = FALSE)
  } else {
    data.frame('DATA' = NA_character_,
               'TIME' = NA_integer_,
               stringsAsFactors = FALSE)
  }

  locations_done <- data.frame('DATA' = locations,
                               'TIME' = loc_subset$timestamp_ms,
                               stringsAsFactors = FALSE)

  all_sl_readings <-
    do.call('rbind',
            list(readings_done, nstat_done, comments_done, locations_done))
  all_sl_readings <- all_sl_readings[complete.cases(all_sl_readings), ]

  # order by timestamp asc
  all_sl_readings <- all_sl_readings[with(all_sl_readings, order(TIME)), ]

  append(sl_header, all_sl_readings$DATA)
  })

  out <- append(file_header, unlist(survey_lines))
  # write(out, paste0('m38_from_R_', Sys.Date(), '.m38'))
  out

  }

