#' Calculate NMEA-0183 XOR checksum
#'
#' This function calculates the checksum for NMEA-0183 G*GGA strings.
#' @param string A string with valid NMEA-0183 G*GGA structure.
#' @return Logical; TRUE if checksum is correct.
#' @examples
#' # first G*GGA msg from data('n38_demo')
#' msg_1 <- "$GPGGA,015808.00,2726.53758,S,15126.05255,E,1,08,1.0,365.1,M,39.5,M,,*79"
#' msg_2 <- "$GPG5808.00,2726.53758,S,15126.05255,E,1,08,1.0,365.1,M,39.5,M,,*79"
#' chk_1 <- em38:::nmea_check(string = msg_1)
#' chk_2 <- em38:::nmea_check(string = msg_2)
#'
nmea_check <- function(string = NULL) {
  prs <- unlist(strsplit(string, '\\*'))[1]
  # lower case required for comparison
  chk <- tolower(unlist(strsplit(string, '\\*'))[2])
  # if the checksum signifier * is missing, that's a fail anyway
  if(is.na(chk)) { return(FALSE) }
  # fn still works if starting $ missing already
  prs <- gsub('^\\$', '', prs)
  prs <- sapply(unlist(strsplit(prs, '*')), charToRaw)

  prs_chksum <- base::Reduce(xor, prs)

  # good ol' coercion rules, see ?base::Comparison
  if(prs_chksum == chk) { TRUE } else { FALSE }
}

#' Process NMEA-0183 G*GGA messages
#'
#' This function pulls out position fix data from NMEA-0183 G*GGA strings and
#' dumps it into a list.
#' @param string A string with valid NMEA-0183 G*GGA structure.
#' @return A list containing the data elements recorded in NMEA-0183 G*GGA data
#'   chunks. Elements are given appropriate R data types.
#' @examples
#' # first GPGGA msg from data('n38_demo')
#' msg_1   <- "$GPGGA,015808.00,2726.53758,S,15126.05255,E,1,08,1.0,365.1,M,39.5,M,,*79"
#' gpgga_1 <- em38:::process_gga(string = msg_1)
#' @importFrom units as_units
#'
process_gga <- function(string = NULL) {
  gga_reading <- unlist(strsplit(string, split = c(',')))

  out        <- vector('list', length = 11)
  names(out) <- c('UTC_time', 'latitude', 'longitude', 'fix_quality', 'n_sats',
                  'HDOP', 'antenna_alt', 'geoid_sep', 'difcor_age_s',
                  'base_stn', 'checksum')

  # ignore date, time itself is fine. don't need lt either
  out[['UTC_time']]  <- as.POSIXct(gga_reading[2], format = '%H%M%OS',
                                   tz = 'UTC')

  # see signal_conversion.R for lat/long retrieval. inputs shld be WGS84
  out[['latitude']]  <- gga_lat(gga_reading[3],gga_reading[4])
  out[['longitude']] <- gga_long(gga_reading[5],gga_reading[6])
  out[['fix_quality']]   <- gga_reading[7] # allowed: 0-9
  out[['n_sats']]    <- as.integer(gga_reading[8])
  out[['HDOP']]      <- as.numeric(gga_reading[9])
  out[['antenna_alt']] <- as.numeric(gga_reading[10])
  # antenna_alt is always in metres afaict
  units(out[['antenna_alt']]) <- units::as_units('m')
  out[['geoid_sep']] <- as.numeric(gga_reading[12])
  # same here
  units(out[['geoid_sep']]) <- units::as_units('m')
  # only in use if differential gps is (fix_quality == '2', possibly some other
  # vals as well)
  out[['difcor_age_s']] <-  as.numeric(gga_reading[14])
  # only in use if differential GPS is
  # and it isn't properly comma delimited jfc whyyyyyy >:-(
  out[['base_stn']] <- if(substr(gga_reading[15], 1, 1) == '*') {
    NA
  } else {
    # grab whatever is before the *
    gsub('\\*.*', '', gga_reading[15])
  }
  # checksum
  # "It is the representation of two hexadecimal characters of an XOR of all
  # characters in the sentence between – but not including – the $ and the *
  # character."
  # https://rietman.wordpress.com/2008/09/25/how-to-calculate-the-nmea-checksum/
  out[['checksum']] <- gsub('^[^\\*]*', '', gga_reading[15])
  # validated later
  out
}

#' Process NMEA-0183 G*VTG messages
#'
#' This function pulls out track made good and speed over ground data from
#' NMEA-0183 G*VTG strings and dumps it into a list.
#' @param string A string with valid NMEA-0183 G*VTG structure.
#' @return A list containing 6 data elements recorded in NMEA-0183 G*VTG data
#'   chunks. Elements are given appropriate data types.
#' @examples
#' # first GPVTG msg from data('n38_demo')
#' msg_1   <- "$GPVTG,208.02,T,,M,0.32,N,0.59,K,A*38"
#' gpvtg_1 <- em38:::process_vtg(string = msg_1)
#' @importFrom units as_units
#'
process_vtg <- function(string = NULL) {
  vtg_reading <- unlist(strsplit(string, split = c(',')))

  out        <- vector('list', length = 6)
  names(out) <- c('TMG_Mag', 'TMG_True', 'speed_knots', 'speed_kmh', 'fix_type',
                  'checksum')

  # not sure how to best units the first three
  out[['TMG_Mag']] <- as.numeric(vtg_reading[2])
  out[['TMG_True']] <- as.numeric(vtg_reading[4])
  out[['speed_knots']] <- as.numeric(vtg_reading[6])
  out[['speed_kmh']] <- as.numeric(vtg_reading[8])
  units(out[['speed_kmh']]) <- units::as_units('km/h')
  # http://www.gpsinformation.org/dale/nmea.htm#2.3
  # fix type: A = autonomous, D = differential, E = Estimated, N = not valid,
  # S = Simulator
  out[['fix_type']] <- gsub('\\*.*', '', vtg_reading[10])
  out[['checksum']] <- gsub('^[^\\*]*', '', vtg_reading[10])
  out
}

#' Process NMEA-0183 G*RMC messages
#'
#' This function pulls out reccommended minimum sentence data from NMEA-0183
#' G*RMC strings and dumps it into a list.
#' @param string A string with valid NMEA-0183 G*RMC structure.
#' @return A list containing 9 data elements recorded in NMEA-0183 G*RMC data
#'   chunks. Elements are given appropriate data types.
#' @examples
#' # first GPRMC msg from data('n38_demo')
#' msg_1   <- "$GPRMC,015808.00,A,2726.53758,S,15126.05255,E,0.32,208.02,160318,,,A*48"
#' gprmc_1 <- em38:::process_rmc(string = msg_1)
#'
process_rmc <- function(string = NULL) {
  rmc_reading <- unlist(strsplit(string, split = c(',')))

  out        <- vector('list', length = 10)
  # http://aprs.gids.nl/nmea/#rmc
  names(out) <- c('UTC_date_time', 'validity', 'latitude', 'longitude',
                  'speed_knots', 'TMG_Mag', 'mag_var', 'mag_var_dir',
                  'fix_type', 'checksum')

  out[['UTC_date_time']] <- as.POSIXlt(rmc_reading[2], format = '%H%M%OS',
                                       tz = 'UTC')
  # amend date, WARNING not sure about timezone effects here, not sure if date
  # is supplied in UTC as time is, or if its locale-dependant.
  date <- as.POSIXlt(rmc_reading[10], format = '%d%m%y')
  out[['UTC_date_time']]$mday <- date$mday
  out[['UTC_date_time']]$mon  <- date$mon
  out[['UTC_date_time']]$year <- date$year
  out[['UTC_date_time']]$wday <- date$wday
  out[['UTC_date_time']]$yday <- date$yday
  out[['UTC_date_time']] <- as.POSIXct(out[['UTC_date_time']])

  out[['validity']] <- switch(rmc_reading[3],
                              "A" = "ok",
                              "V" = "invalid")
  # see signal_conversion.R for lat/long retrieval. inputs shld be WGS84
  out[['latitude']]  <- gga_lat(rmc_reading[4],rmc_reading[5])
  out[['longitude']] <- gga_long(rmc_reading[6],rmc_reading[7])
  out[['speed_knots']] <- as.numeric(rmc_reading[8])
  out[['TMG_Mag']] <- as.numeric(rmc_reading[9])
  out[['mag_var']] <- as.numeric(rmc_reading[11])
  out[['mag_var_dir']] <- as.character(rmc_reading[12])
  out[['fix_type']] <- gsub('\\*.*', '', rmc_reading[13])
  out[['checksum']] <- gsub('^[^\\*]*', '', rmc_reading[13])
  out
}

#' Process NMEA-0183 G*GSA messages
#'
#' This function pulls out GPS DOP and active satellites data from NMEA-0183
#' G*GSA strings and dumps it into a list.
#' @param string A string with valid NMEA-0183 G*GSA structure.
#' @return A list containing n data elements recorded in NMEA-0183 G*GSA data
#'   chunks. Elements are given appropriate data types.
#' @examples
#' # first GPGSA msg from data('n38_demo')
#' msg_1   <- "$GPGSA,M,3,05,10,15,16,20,21,26,29,,,,,1.6,1.0,1.2*32"
#' gpgsa_1 <- em38:::process_gsa(string = msg_1)
#'
process_gsa <- function(string = NULL) {
  gsa_reading <- unlist(strsplit(string, split = c(',')))

  out        <- vector('list', length = 8)
  names(out) <- c('fix_selection_mode', 'fix_type', 'sats_used', 'n_sats',
                  'PDOP', 'HDOP', 'VDOP', 'checksum')

  out[['fix_selection_mode']] <- switch(gsa_reading[2],
                                        'M' = 'manual',
                                        'A' = 'automatic')
  out[['fix_type']] <- switch(gsa_reading[3],
                              '1' = 'no fix',
                              '2' = '2D',
                              '3' = '3D')
  # https://www.trimble.com/OEM_ReceiverHelp/V4.44/en/NMEA-0183messages_GSA.html
  # PRN number, 01 through 32 for GPS, 33 through 64 for SBAS, 64+ for GLONASS
  out[['sats_used']] <- as.numeric(gsa_reading[4:15])[!is.na(as.numeric(gsa_reading[4:15]))]
  # saves having to check other message types
  out[['n_sats']] <- length(out[['sats_used']])
  # smaller is better. TODO: rate DOPs
  out[['PDOP']] <- as.numeric(gsa_reading[16])
  out[['HDOP']] <- as.numeric(gsa_reading[17])
  out[['VDOP']] <- as.numeric(gsub('\\*.*', '', gsa_reading[18]))
  out[['checksum']] <- gsub('^[^\\*]*', '', gsa_reading[18])
  out
}


#' Process NMEA-0183 G*GSV messages
#'
#' This function pulls out satellites in view data from NMEA-0183 G*GSV strings
#' and dumps it into a list.
#' @param string A string with valid NMEA-0183 G*GSV structure.
#' @return A list containing 15 data elements recorded in NMEA-0183 G*GSV data
#'   chunks. Elements are given appropriate data types. Note that depending on
#'   the number of satellites in view, up to three of these messages may exist
#'   for every GPS reading. Up to four satellites are reported on per string.
#'   Note also that SNR is receiver-dependant and should only be considered
#'   relative to other readings in the same dataset.
#' @examples
#' # first GPGSV msg from data('n38_demo')
#' msg_1   <- "$GPGSV,3,1,11,05,14,138,46,10,14,316,37,12,04,012,,13,24,100,*76"
#' gpgsv_1 <- em38:::process_gsv(string = msg_1)
#'
process_gsv <- function(string = NULL) {
  gsv_reading <- unlist(strsplit(string, split = c(',')))

  out        <- vector('list', length = 19)
  names(out) <- c('sentence_x_of_y', 'n_sats_in_view',
                  'sat_1_PRN', 'sat_1_elevation', 'sat_1_azimuth', 'sat_1_SNR',
                  'sat_2_PRN', 'sat_2_elevation', 'sat_2_azimuth', 'sat_2_SNR',
                  'sat_3_PRN', 'sat_3_elevation', 'sat_3_azimuth', 'sat_3_SNR',
                  'sat_4_PRN', 'sat_4_elevation', 'sat_4_azimuth', 'sat_4_SNR',
                  'checksum')

  out[['sentence_x_of_y']] <- paste0(gsv_reading[3], ' of ', gsv_reading[2])
  # nb not the same as sats used!
  out[['n_sats_in_view']]  <- as.numeric(gsv_reading[4])
  out[['sat_1_PRN']]       <- if(gsv_reading[ 5] == '') { NA } else { gsv_reading[ 5] }
  out[['sat_2_PRN']]       <- if(gsv_reading[ 9] == '') { NA } else { gsv_reading[ 9] }
  out[['sat_3_PRN']]       <- if(gsv_reading[13] == '') { NA } else { gsv_reading[13] }
  out[['sat_4_PRN']]       <- if(gsv_reading[17] == '') { NA } else { gsv_reading[17] }
  out[['sat_1_elevation']] <- as.numeric(gsv_reading[ 6])
  out[['sat_2_elevation']] <- as.numeric(gsv_reading[10])
  out[['sat_3_elevation']] <- as.numeric(gsv_reading[14])
  out[['sat_4_elevation']] <- as.numeric(gsv_reading[18])
  out[['sat_1_azimuth']]   <- as.numeric(gsv_reading[ 7])
  out[['sat_2_azimuth']]   <- as.numeric(gsv_reading[11])
  out[['sat_3_azimuth']]   <- as.numeric(gsv_reading[15])
  out[['sat_4_azimuth']]   <- as.numeric(gsv_reading[19])
  out[['sat_1_SNR']]       <- as.numeric(gsv_reading[ 8])
  out[['sat_2_SNR']]       <- as.numeric(gsv_reading[12])
  out[['sat_3_SNR']]       <- as.numeric(gsv_reading[16])
  out[['sat_4_SNR']]       <- as.numeric(gsub('\\*.*', '', gsv_reading[20]))
  out[['checksum']]        <- gsub('^[^\\*]*', '', gsv_reading[20])
  out
}
