#' Translate GPS data
#'
#' This function decodes GPS data for use in em38_spatial and returns it along with its timestamp
#' data.
#' @param block Data frame holding GPS message data, usually a subset of $location_data in a
#' decoded n38 object
#' @return data frame with a single row
#'
get_loc_data <- function(block = NULL) {
  # all the interesting stuff is in the first one
  gpgga <- process_gpgga(paste0('$GPGGA,', block$MESSAGE[block$TYPE == 'GPGGA']))

  # keeping these for later but no need to decode the entire block just yet
  #gpvtg <- em38:::process_gpvtg(paste0('$GPVTG,', x$MESSAGE[x$TYPE == 'GPVTG']))
  #gprmc <- em38:::process_gprmc(paste0('$GPRMC,', x[x$TYPE == 'GPRMC', 'MESSAGE' ]))
  #gpgsa <- em38:::process_gpgsa(paste0('$GPGSA,', x[x$TYPE == 'GPGSA', 'MESSAGE' ]))
  #gpgsv <- split(x[x$TYPE == 'GPGSV', 'MESSAGE' ], 1:nrow(x[x$TYPE == 'GPGSV', ]))
  #gpgsv <- lapply(gpgsv, function(row) { em38:::process_gpgsv(paste0('$GPGSV,', row)) })

  # what actually needs to be output?
  data.frame('LATITUDE'     = gpgga[['latitude']],
             'LONGITUDE'    = gpgga[['longitude']],
             'HDOP'         = gpgga[['HDOP']],
             'timestamp_ms' = block$timestamp_ms[block$TYPE == 'GPGGA'])
  # if this fails, suspect #1 is multiple GPGGA messages in block
  # suspect #2 is a GPS device that doesn't return GPGGA (e.g. GLGPA, or GPRMC)
  # will defo need to add handlers for GLONASS and other systems but need test data

}

#' Spatialise data
#'
#' This function processes a decoded N38 record into a point spatial dataset.
#' @param n38_decoded Nested list output by n38_decode
#' @param hdop_filter Numeric, discard GPS data where the Horizontal Dilution of Precision is
#' greater than this number. Defaults to 3 metres. Set to NULL to keep all readings.
#' @param out_mode Character, em38 dipole mode. Output dataset can only contain Vertical or
#' Horizontal data, never both.
#' @return An sf data frame with sfc_POINT geometry. WGS84 projection. If the n38_decoded object
#'  contains more than one survey line, a list of sf objects is returned - one for each line.
#' @note Input n38_decoded object must be of survey type 'GPS' and record type 'auto'.
#' @examples
#' data('n38_demo')
#' n38_chunks  <- n38_chunk(n38_demo)
#' n38_decoded <- n38_decode(n38_chunks)
#' em38_points <- em38_spatial(n38_decoded, 3, 'Vertical')
#'
#' @importFrom dplyr bind_rows group_by lead lag mutate ungroup
#' @importFrom sf st_as_sf
#' @importFrom tidyr fill
#' @importFrom rlang .data
#' @export
em38_spatial <- function(n38_decoded = NULL,
                         hdop_filter = 3,
                         out_mode = c('Vertical', 'Horizontal')) {
  out <- lapply(2:length(n38_decoded), function(i) {
    # stop if input had no embedded GPS data
    if(all(is.na(n38_decoded[[i]][['location_data']]))) {
      # better way? this will handle where some survey lines are ok and others not at least
      return('This survey line contains no embedded location data.')
    } else {

      ### get reading data and tidy it up
      readings <- n38_decoded[[i]][['reading_data']]

      # filter readings to chosen dipole mode
      readings <- readings[readings$mode == out_mode, ]

      # if no readings of the chosen out_mode exist in this sl,
      if(nrow(readings) == 0) {
        return(paste0('No readings were recorded with ', out_mode,
                      ' dipole mode on this survey line.'))
      } else {

        # pull out location data and group it by repeating sequence of records
        loc <- n38_decoded[[i]][['location_data']]
        loc <- dplyr::mutate(loc,
                             lag_chk = ifelse(.data$TYPE == .data$TYPE[1], T, F),
                             group   = cumsum(.data$lag_chk)
                             )
        loc_s <- split(loc, loc$group)
        # drop any chunks that don't have a GPGGA message (usually a start/pause error)
        keep <-
          sapply(loc_s, function(x)
            if (any(x$TYPE %in% 'GPGGA')) {
              TRUE
            } else {
              FALSE
            })
        loc_s <- loc_s[keep]
        # decode messages and keep only the essential data
        loc_s <- lapply(loc_s, function(x) {
          get_loc_data(x)
        })
        loc_f <- do.call('rbind', loc_s)

        # filter out low-precision locations and also some dud readings (checksum passed
        # but message still missing essential data)
        # note that this is not done by the nmea parser on purpose - prefer record sequence
        # intact for grouping above
        loc_f <- if (!is.null(hdop_filter)) {
          x <- loc_f[loc_f$HDOP < hdop_filter,]
          x[!is.na(x$HDOP), ]
        } else {
          loc_f[!is.na(loc_f$HDOP), ]
        }

        # if all the locations were still borked,
        if(nrow(loc_f) == 0) {
          return('No usable location data could be retrieved from this survey line')
        } else {

        # add lead(lat) and lead(long) for interpolation, plus time lag between readings
        # timestamps are proxying for distance fractions later
        loc_f$LEAD_LAT  <- dplyr::lead(loc_f$LATITUDE)
        loc_f$LEAD_LONG <- dplyr::lead(loc_f$LONGITUDE)
        loc_f$TS_LAG    <-
          dplyr::lead(loc_f$timestamp_ms) - loc_f$timestamp_ms

        # remove readings outside first and last gps reading
        readings_in <-
          readings[readings$timestamp_ms > loc_f$timestamp_ms[1] &
                     readings$timestamp_ms < loc_f$timestamp_ms[nrow(loc_f)], ]

        # nb might be able to add them back in later (not yet impemented)
        #readings_out <-
        #  readings[!(readings$timestamp_ms %in% readings_in$timestamp_ms), ]

        all_data <- dplyr::bind_rows(loc_f, readings_in)
        all_data <-
          all_data[with(all_data, order(all_data$timestamp_ms)), ]
        all_data$TS_LAG_AD <-
          all_data$timestamp_ms - dplyr::lag(all_data$timestamp_ms)
        all_data$TS_LAG_AD[is.na(all_data$TS_LAG_AD)] <- 0

        # group recombined data so that GPS reading(s) and following instrument reading(s) are
        # together

        # grouping by sequence is hard and this seems awful but whateverrrrr
        grp     <- rle(ifelse(is.na(all_data$LATITUDE), 1, 0))$lengths
        grp     <- data.frame('rle' = grp)
        grp$grp <- c(rep(1:(nrow(grp) / 2), each = 2), ceiling(nrow(grp) / 2))
        grp     <- split(grp, grp$grp)
        grp     <-
          sapply(grp, function(x) {
            sum(x$rle)
          })
        all_data$GROUP <- unlist(mapply(
          function(x, y)
            rep(x, times = y),
          x = as.integer(names(grp)),
          y = grp
        ))

        # fill a few values in so all instrument readings have a 'from' and 'to' for interpolation
        all_data <-   tidyr::fill(
          all_data,
          .data$LATITUDE,
          .data$LONGITUDE,
          .data$LEAD_LAT,
          .data$LEAD_LONG,
          .data$TS_LAG,
          .direction = 'down'
        )

        # get cumulative time lag within each group (effectively distance between last gps reading
        # and current instrument reading)
        all_data$ind3 = ifelse(is.na(all_data$HDOP), 1, 0)
        all_data <- dplyr::group_by(all_data, .data$GROUP, .data$ind3)
        all_data <-
          dplyr::mutate(all_data, TS_NOW = cumsum(.data$TS_LAG_AD))
        all_data <- dplyr::ungroup(all_data)
        all_data$TS_NOW <-
          ifelse(!is.na(all_data$HDOP), 0 , all_data$TS_NOW)

        # use 2D linear interpolation to get locations for instrument readings
        # no point getting geodetic here, we're generally working at < 1m distances
        # https://math.stackexchange.com/questions/1918743/how-to-interpolate-points-between-2-points#1918765
        all_data <- dplyr::mutate(
          all_data,
          NEW_LAT  = .data$LATITUDE  + (
            .data$TS_NOW / .data$TS_LAG  *
              (.data$LEAD_LAT  - .data$LATITUDE)
          ),
          NEW_LONG = .data$LONGITUDE + (
            .data$TS_NOW / .data$TS_LAG  *
              (.data$LEAD_LONG - .data$LONGITUDE)
          )
        )

        # filter to just keep instrument readings and interpolated locations
        out_data <-
          all_data[, c('cond_05',
                       'cond_1',
                       'IP_05',
                       'IP_1',
                       'temp_05',
                       'temp_1',
                       'NEW_LAT',
                       'NEW_LONG')]
        out_data <- out_data[complete.cases(out_data),]

        # handler to average location for paired readings should go here i think (tbd)

        # spatialise output and return
        sf::st_as_sf(out_data,
                     coords = c('NEW_LONG', 'NEW_LAT'),
                     crs = 4326)
  }}}})

  # return each survey line separately, let the user combine later
  # consider a helper function for this - can build in point filtering etc
  if (length(out) == 1) {
    out[[1]]
  } else {
    out
  }

}
