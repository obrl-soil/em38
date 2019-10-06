#' Translate GPS data
#'
#' This function decodes GPS data for use in em38_spatial and returns it along
#' with its timestamp data.
#' @param block Data frame holding GPS message data, usually a subset of
#'   $location_data in a decoded n38 object
#' @return data frame with a single row
#' @keywords Internal
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
             'HDOP'         = gpgga[['HDOP']],
             'CHKSUM'       = block$CHKSUM[block$TYPE == 'GPGGA'],
             'timestamp_ms' = block$timestamp_ms[block$TYPE == 'GPGGA'])
  # if this fails it'll likely be that messages are coming from
  # a GPS device that doesn't return GPGGA (e.g. GLGPA, or GPRMC)
  # will defo need to add handlers for GLONASS and other systems but need test
  # data

  }

#' Spatialise EM38 data
#'
#' This function processes a decoded N38 record into a point spatial dataset.
#' @param n38_decoded Nested list output by n38_decode
#' @param hdop_filter Numeric, discard GPS data where the Horizontal Dilution of
#'   Precision is greater than this number. Defaults to 3 metres. Set to NULL to
#'   keep all readings.
#' @return An sf data frame with sfc_POINT geometry. WGS84 projection. If the
#'   n38_decoded object contains more than one survey line, a list of sf objects
#'   is returned - one for each line.
#' @note Input n38_decoded object should be of survey type 'GPS'. If not, the
#'   function will fail gracefully by returning a list of reasons why the data
#'   could not be converted to points.
#' @examples
#' data('n38_demo')
#' n38_chunks  <- n38_chunk(n38_demo)
#' n38_decoded <- n38_decode(n38_chunks)
#' em38_points <- em38_spatial(n38_decoded, 3)
#'
#' @importFrom dplyr bind_rows group_by lead lag mutate ungroup
#' @importFrom sf st_as_sf
#' @importFrom tidyr fill
#' @importFrom rlang .data
#' @export
em38_spatial <- function(n38_decoded = NULL,
                         hdop_filter = 3) {
  out <- lapply(2:length(n38_decoded), function(i) {

    # if input had no embedded GPS data
    if(all(is.na(n38_decoded[[i]][['location_data']]))) {
      # better way? this will handle where some survey lines are ok and others
      # not at least
      return('This survey line contains no embedded location data.')
    }

    # if input had no readings recorded
    if(all(is.na(n38_decoded[[i]][['reading_data']]))) {
      return('This survey line contains no data.')
    }

    ### get reading data and tidy it up
    readings <- n38_decoded[[i]][['reading_data']]

    # pull out location data and group it by repeating sequence of records
    loc         <- n38_decoded[[i]][['location_data']]
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

    # filter out low-precision locations and also some dud readings
    # (checksum passed but message still missing essential data)
    # note that this is not done by the nmea parser on purpose - prefer
    # record sequence intact for grouping above
    loc_f <- if(!is.null(hdop_filter)) {
      x <- loc_f[loc_f$HDOP < hdop_filter,]
      x[!is.na(x$HDOP), ]
      } else {
        loc_f[!is.na(loc_f$HDOP), ]
      }

    # if all the locations were still borked,
    if(dim(loc_f)[1] == 0) {
      return('No readings with acceptable HDOP could be retrieved from this survey line.')
      }

    # add lead(lat) and lead(long) for interpolation, plus time lag between
    # readings. Timestamps are proxying for distance fractions later
    loc_f$LEAD_LAT  <- dplyr::lead(loc_f$LATITUDE)
    loc_f$LEAD_LONG <- dplyr::lead(loc_f$LONGITUDE)
    loc_f$TS_LAG    <- dplyr::lead(loc_f$timestamp_ms) - loc_f$timestamp_ms

    # remove readings outside first and last gps reading
    readings_in <-
      readings[readings$timestamp_ms > loc_f$timestamp_ms[1] &
                 readings$timestamp_ms < loc_f$timestamp_ms[dim(loc_f)[1]], ]

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
                                    x = as.integer(names(grp)), y = grp
                                    )
                             )

    # get cumulative time lag within each group (effectively distance
    # between last gps reading and current instrument reading)
    all_data$ind3 = ifelse(is.na(all_data$HDOP), 1, 0)
    all_data <- dplyr::group_by(all_data, .data$GROUP, .data$ind3)
    all_data <- dplyr::mutate(all_data, TS_NOW = cumsum(.data$TS_LAG_AD))
    all_data <- dplyr::ungroup(all_data)
    all_data$TS_NOW <- ifelse(!is.na(all_data$HDOP), 0 , all_data$TS_NOW)

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

        # filter to just keep instrument readings and interpolated locations
    out_data <-
        all_data[, c('cond_05', 'cond_1', 'IP_05', 'IP_1', 'temp_05', 'temp_1',
                     'mode', 'NEW_LAT', 'NEW_LONG')]
        out_data <- out_data[complete.cases(out_data),]
        out_data <- cbind('ID' = seq(nrow(out_data)), out_data)

        # spatialise output and return
        sf::st_as_sf(out_data,
                     coords = c('NEW_LONG', 'NEW_LAT'),
                     crs = 4326)
        })

  # return each survey line separately, let the user combine later
  # consider a helper function for this - can build in point filtering etc
  if (length(out) == 1) {
    out[[1]]
  } else {
    out
  }

}

#' Import and convert N38 logfiles to points
#'
#' This is a wrapper function that processes a raw on-disk N38 file into an sf
#' point spatial dataset.
#' @param path A file path pointing to a valid *.N38 file, produced by a Geonics
#'   EM38-MK2 conductivity sensor connected to an Allegra CX or Archer
#'   datalogger (and optionally, a GPS device).
#' @param hdop_filter Numeric, discard GPS data where the Horizontal Dilution of
#'   Precision is greater than this number. Defaults to 3 metres. Set to NULL to
#'   keep all readings.
#' @return An sf data frame with sfc_POINT geometry. WGS84 projection. If the
#'   n38_decoded object contains more than one survey line, a list of sf objects
#'   is returned - one for each line.
#' @note Input file should be of survey type 'GPS'. If not, the function will
#'   fail gracefully by returning reasons why the data could not be converted to
#'   points.
#' @examples
#' vert_points <-
#' n38_to_points(path = system.file("extdata", "em38_demo.N38", package = "em38"),
#'               hdop_filter = 3)
#' @export
n38_to_points <- function(path = NULL, hdop_filter = 3) {
  mat  <- n38_import(path)
  chnk <- n38_chunk(mat)
  dec  <- n38_decode(chnk)

  em38_spatial(n38_decoded = dec, hdop_filter = hdop_filter)

}

#' Reconcile locations of paired data
#'
#' Where paired horizontal and vertical readings have been taken during a
#' 'manual' mode survey, the first and second readings at each station should
#' have the same location. The nature of the device logging generally precludes
#' this from happening by default, especially with high-frequency GPS recording.
#' This function reconciles the locations of such paired datasets after they
#' have been generated using \code{\link{em38_spatial}} or
#' \code{\link{n38_to_points}}.
#' @param horizontal_data spatial point dataframe produced by
#'   \code{\link{em38_spatial}}  or \code{\link{n38_to_points}} with `out_mode =
#'   'Horizontal`.
#' @param vertical_data spatial point dataframe produced by
#'   \code{\link{em38_spatial}}  or \code{\link{n38_to_points}} with `out_mode =
#'   'Vertical`.
#' @return An sf data frame with sfc_POINT geometry. WGS84 projection. Output
#'   locations are averages of input locations. Data columns are labelled as
#'   horizontal or vertical.
#' @note Input data should be of survey type 'GPS' and record type 'manual'.
#'   Both input datasets should have the same number of rows, with row 1 of
#'   horizontal_data paired with row_1 of vertical_data.
#' @importFrom purrr map2
#' @importFrom sf st_crs st_geometry st_point st_set_geometry st_sfc st_sf
#' @export
#'
em38_pair <- function(horizontal_data = NULL, vertical_data = NULL) {
  # take paired horizontal and vertical em38 readings and reconcile their
  # locations
  geom_h <- st_geometry(horizontal_data)
  geom_v <- st_geometry(vertical_data)

  pts <- purrr::map2(.x = geom_h, .y = geom_v, function(.x, .y) {
    out_long <- mean(c(as.vector(.x)[1],
                     as.vector(.y)[1]))
    out_lat  <- mean(c(as.vector(.x)[2],
                     as.vector(.y)[2]))

    st_point(c(out_long, out_lat))
  })

  new_geom <- st_sfc(pts, crs = st_crs(geom_h)$proj4string)

  # combine in output
  hdata <- st_set_geometry(horizontal_data, NULL)
  names(hdata) <- paste0('H_', names(hdata))
  vdata <- st_set_geometry(vertical_data, NULL)
  names(vdata) <- paste0('V_', names(vdata))
  all_data <- cbind(hdata, vdata)

  st_sf(all_data, 'geometry' = new_geom)

}
