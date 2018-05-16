context('NMEA message parsing')

test_that(
  'latitude is converted to numeric correctly',
  c(
    # verified against https://data.aad.gov.au/aadc/calc/dms_decimal.cfm
    expect_equal(gpgga_lat('2726.53758', 'S'), -27.442293),
    expect_equal(gpgga_lat('2726.53758', 'N'),  27.442293),
    expect_equal(gpgga_lat('2706.53758', 'N'),  27.1089597),
    expect_equal(gpgga_lat('0700.01666', 'S'),  -7.0002777),
    expect_equal(gpgga_lat('0001.10000', 'N'),   0.0183333),
    expect_equal(gpgga_lat('1000.01000', 'N'),  10.0001667),
    expect_type(gpgga_lat('2726.53758', 'S'), 'double'),
    expect_type(gpgga_lat('2726.53758', 'N'), 'double')
  )
)


test_that(
  'longitude is converted to numeric correctly',
  c(
    # verified against https://data.aad.gov.au/aadc/calc/dms_decimal.cfm
    expect_equal(gpgga_long('15200.00000', 'E'), 152.0000000),
    expect_equal(gpgga_long('01520.00000', 'E'),  15.3333333),
    expect_equal(gpgga_long('00152.00000', 'W'),  -1.8666667),
    expect_equal(gpgga_long('00015.20000', 'W'),  -0.2533333),
    expect_equal(gpgga_long('00001.52000', 'E'),   0.0253333),
    expect_equal(gpgga_long('00000.15200', 'E'),   0.0025333),
    expect_type(gpgga_long('15200.00000', 'E'), 'double'),
    expect_type(gpgga_long('15200.00000', 'W'), 'double')
  )

)

test_that(
  'nmea checksum validator works correctly',
  c(
    msg <- "$GPGGA,015808.00,2726.53758,S,15126.05255,E,1,08,1.0,365.1,M,39.5,M,,*79",
    expect_true(nmea_check(string = msg)),
    expect_false(nmea_check(string = gsub('015', '009', msg))),
    expect_false(nmea_check(string = gsub('\\*', '', msg))),
    expect_length(nmea_check(string = msg), 1),
    expect_type(nmea_check(string = msg), 'logical')
  )
)

test_that(
  'process_gpgga works correctly',
  c(
    msg <- "$GPGGA,015808.00,2726.53758,S,15126.05255,E,1,08,1.0,365.1,M,39.5,M,,*79",
    expect_type(process_gpgga(msg), 'list'),
    expect_length(process_gpgga(msg), 11),
    expect_is(process_gpgga(msg)[[1]], class = 'POSIXlt')
  )
)
