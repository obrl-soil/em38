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

# 2. process_gpgga
test_that(
  'process_gpgga works correctly',
  c(
    msg <- "$GPGGA,015808.00,2726.53758,S,15126.05255,E,1,08,1.0,365.1,M,39.5,M,,*79",
    expect_type(process_gpgga(msg), 'list'),
    expect_length(process_gpgga(msg), 11L),
    expect_is(process_gpgga(msg)[[1]], class = 'POSIXlt'),
    expect_equal(process_gpgga(msg)$base_stn, NA)
  )
)

# 3. process_gpvtg
test_that(
  'process_gpvtg works correctly',
  c(
    msg <- "$GPVTG,208.02,T,,M,0.32,N,0.59,K,A*38",
    expect_type(process_gpvtg(msg), 'list'),
    expect_length(process_gpvtg(msg), 6L),
    expect_is(process_gpvtg(msg)$speed_kmh, 'units'),
    expect_equal(process_gpvtg(msg)$fix_type, 'A')
  )
)

# 4. process_gprmc
test_that(
  'process_gprmc works correctly',
  c(
    msg <- "$GPRMC,015808.00,A,2726.53758,S,15126.05255,E,0.32,208.02,160318,,,A*48",
    expect_type(process_gprmc(msg), 'list'),
    expect_length(process_gprmc(msg), 10L),
    expect_is(process_gprmc(msg)$UTC_date_time, 'POSIXlt'),
    expect_equal(format(process_gprmc(msg)$UTC_date_time), '2018-03-16 01:58:08'),
    expect_equal(process_gprmc(msg)$fix_type, 'A')
  )
)

# 5. process_gpgsa
test_that(
  'process_gpgsa works correctly',
  c(
    msg <- "$GPGSA,M,3,05,10,15,16,20,21,26,29,,,,,1.6,1.0,1.2*32",
    expect_type(process_gpgsa(msg), 'list'),
    expect_length(process_gpgsa(msg), 8L),
    expect_equal(process_gpgsa(msg)$VDOP, 1.2)
  )
)

# 6. process_gpgsa
test_that(
  'process_gpgsv works correctly',
  c(
    msg <- "$GPGSV,3,1,11,05,14,138,46,10,14,316,37,12,04,012,,13,24,100,*76",
    expect_type(process_gpgsv(msg), 'list'),
    expect_length(process_gpgsv(msg), 19L),
    expect_equal(process_gpgsv(msg)$sat_4_SNR, NA_real_)
  )
)
