#' Signal to conductivity/inphase reading
#'
#' This function converts an EM38-MK2 signal recieved on any of Channels 1 to 4. Additional
#' conversion is required for inphase signals, and further application of a calibration factor is
#'  required to derive the final conductivity or resistivity value. Do not use this function in
#'  isolation.
#' @param signal Integer.
#' @return Numeric; uncalibrated instrument reading.
#' @keywords Internal
#' @examples
#' channel_1_or_3 <- em38:::get_cond(30456)
#' channel_2 <- em38:::get_cond(30456) * 0.00720475
#' channel_4 <- em38:::get_cond(30456) * 0.028819
#'
get_cond <- function(signal = NULL) {
  (signal * 5 / 1024 - 160) * 8
}

#' Signal to temperature reading
#'
#' This function calculates temperature from an EM38-MK2 signal recieved on Channel 5 or 6.
#' @param signal Integer.
#' @return Temperature in degrees C
#' @keywords Internal
#' @examples
#' channel_5 <- em38:::get_temp(30456)
#'
get_temp <- function(signal = NULL) {
  # J. Pawlowski (Geonics, Geomar), pers. comm. 2018-03-28
  signal / 3.108 - 50
}

#' GPGGA latitude
#'
#' This function retrieves latitude position information from a GPGGA string.
#' @param lat String containing latitude information. GPGGA strings are comma-separated, latitude
#' is in the second delimited position
#' @param dir String determining whether latitude is N or S of the equator. GPGGA strings are
#' comma-separated, NS status is in the third delimited position
#' @return Numeric, latitude in decimal degrees.
#' @note Inputting a numeric to lat will give incorrect results for latitude -10 < x < 10 due to
#' loss of leading zero(s).
#' @keywords Internal
#' @examples
#' lat <- em38:::gpgga_lat('2729.10198', 'S')
#'
gpgga_lat <- function(lat = NULL, dir = NULL) {
  # this is potentially stupid depending on how the numbers are stored
  # - need more raw datasets for testing
  # its safe if leading zeros are used correctly
  # https://www.labsat.co.uk/index.php/en/free-gps-nmea-simulator-software maybe
  degrees <- as.integer(substr(lat, 1, 2))
  dec_minutes <- as.numeric(substr(lat, 3, nchar(lat)))

  # 7 dec pl is about a centimetre on-ground
  if(dir == 'S') {
    round(0 - (degrees + dec_minutes/60), 7)
  } else {
    round(degrees + dec_minutes/60, 7)
  }
}

#' GPGGA longitude
#'
#' This function retrieves longitude position information from a GPGGA string.
#' @param long String containing longitude information. GPGGA strings are comma-separated, longitude
#' is in the fourth delimited position
#' @param dir String determining whether longitude is E or W of 0 degrees. GPGGA strings
#'  are comma-separated, EW status is in the fifth delimited position
#' @return Numeric, longitude in decimal degrees.
#' @note Inputting a numeric to long will give incorrect results for longitude -100 < x < 100 due to
#' loss of leading zero(s).
#' @keywords Internal
#' @examples
#' lat <- em38:::gpgga_long('15257.5556', 'E')
#'
gpgga_long <- function(long = NULL, dir = NULL) {

  degrees <- as.integer(substr(long, 1, 3))
  dec_minutes <- as.numeric(substr(long, 4, nchar(long)))

  if(dir == 'W') {
    round(0 - (degrees + dec_minutes/60), 7)
  } else {
    round(degrees + dec_minutes/60, 7)
  }
}
