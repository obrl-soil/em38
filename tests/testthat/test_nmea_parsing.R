context('NMEA message parsing')

# these functions are all in GPS_data.R

# 1. nmea_check
test_that(
  'nmea checksum validator works correctly',
  c(
    msg <- "$GPGGA,015808.00,2726.53758,S,15126.05255,E,1,08,1.0,365.1,M,39.5,M,,*79",
    expect_true(nmea_check(string = msg)),
    expect_false(nmea_check(string = gsub('015', '009', msg))),
    expect_false(nmea_check(string = gsub('\\*', '', msg))),
    expect_length(nmea_check(string = msg), 1L),
    expect_type(nmea_check(string = msg), 'logical')
  )
)

# 2. process_gga
test_that(
  'process_gga works correctly',
  c(
    msg <- "$GPGGA,015808.00,2726.53758,S,15126.05255,E,1,08,1.0,365.1,M,39.5,M,,*79",
    expect_type(process_gga(msg), 'list'),
    expect_length(process_gga(msg), 11L),
    expect_is(process_gga(msg)[[1]], class = 'POSIXct'),
    expect_equal(process_gga(msg)$base_stn, NA)
  )
)

# 3. process_vtg
test_that(
  'process_vtg works correctly',
  c(
    msg <- "$GPVTG,208.02,T,,M,0.32,N,0.59,K,A*38",
    expect_type(process_vtg(msg), 'list'),
    expect_length(process_vtg(msg), 6L),
    expect_is(process_vtg(msg)$speed_kmh, 'units'),
    expect_equal(process_vtg(msg)$fix_type, 'A')
  )
)

# 4. process_rmc
test_that(
  'process_rmc works correctly',
  c(
    msg <- "$GPRMC,015808.00,A,2726.53758,S,15126.05255,E,0.32,208.02,160318,,,A*48",
    expect_type(process_rmc(msg), 'list'),
    expect_length(process_rmc(msg), 10L),
    expect_is(process_rmc(msg)$UTC_date_time, 'POSIXct'),
    expect_equal(format(process_rmc(msg)$UTC_date_time), '2018-03-16 01:58:08'),
    expect_equal(process_rmc(msg)$fix_type, 'A')
  )
)

# 5. process_gsa
test_that(
  'process_gsa works correctly',
  c(
    msg <- "$GPGSA,M,3,05,10,15,16,20,21,26,29,,,,,1.6,1.0,1.2*32",
    expect_type(process_gsa(msg), 'list'),
    expect_length(process_gsa(msg), 8L),
    expect_equal(process_gsa(msg)$VDOP, 1.2)
  )
)

# 6. process_gsv
test_that(
  'process_gsv works correctly',
  c(
    msg <- "$GPGSV,3,1,11,05,14,138,46,10,14,316,37,12,04,012,,13,24,100,*76",
    expect_type(process_gsv(msg), 'list'),
    expect_length(process_gsv(msg), 19L),
    expect_equal(process_gsv(msg)$sat_4_SNR, NA_real_)
  )
)
