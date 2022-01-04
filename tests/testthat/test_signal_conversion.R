context('signal conversions')

test_that(
  'raw sensor data is converted to (uncalibrated) conductivity correctly',
  c(
    expect_equal(get_cond(signal = 30456), -90.3125),
    expect_type(get_cond(signal = 30456), 'double'),
    # can handle vectors
    expect_equal(round(get_cond(signal = c(200, 201, 202)), 2), # don't @ me
                 c(-1272.19, -1272.15, -1272.11))
  )
)

test_that(
  'raw sensor data is converted to temperature correctly',
  c(
    # not too cold, not too hot, all you need is a light jacket
    expect_equal(get_temp(signal = 227), 23.037323037323),
    expect_type(get_temp(signal = 227), 'double'),
    # can handle vectors
    expect_equal(round(get_temp(signal = c(200, 201, 202)), 2),
                 c(14.35, 14.67, 14.99))
  )
)

test_that(
  'latitude is converted to numeric correctly',
  c(
    # verified against https://data.aad.gov.au/aadc/calc/dms_decimal.cfm
    expect_equal(gga_lat('2726.53758', 'S'), -27.442293),
    expect_equal(gga_lat('2726.53758', 'N'),  27.442293),
    expect_equal(gga_lat('2706.53758', 'N'),  27.1089597),
    expect_equal(gga_lat('0700.01666', 'S'),  -7.0002777),
    expect_equal(gga_lat('0001.10000', 'N'),   0.0183333),
    expect_equal(gga_lat('1000.01000', 'N'),  10.0001667),
    expect_type(gga_lat('2726.53758', 'S'), 'double'),
    expect_type(gga_lat('2726.53758', 'N'), 'double')
  )
)

test_that(
  'longitude is converted to numeric correctly',
  c(
    # verified against https://data.aad.gov.au/aadc/calc/dms_decimal.cfm
    expect_equal(gga_long('15200.00000', 'E'), 152.0000000),
    expect_equal(gga_long('01520.00000', 'E'),  15.3333333),
    expect_equal(gga_long('00152.00000', 'W'),  -1.8666667),
    expect_equal(gga_long('00015.20000', 'W'),  -0.2533333),
    expect_equal(gga_long('00001.52000', 'E'),   0.0253333),
    expect_equal(gga_long('00000.15200', 'E'),   0.0025333),
    expect_type(gga_long('15200.00000', 'E'), 'double'),
    expect_type(gga_long('15200.00000', 'W'), 'double')
  )
)

test_that(
  'time stamp converted correctly',
  c(
    data('n38_demo'),
    n38_chunks  <- n38_chunk(n38_demo),
    n38_decoded <- n38_decode(n38_chunks),
    sl1 <- n38_decoded[[2]],
    time <- sl1$timer_data,
    rd1 <- sl1$reading_data$timestamp[1],
    val1 <- conv_stamp(time$computer_time, time$timestamp_ms, rd1),
    expect_is(val1, 'POSIXct'),
    expect_equal(as.character(val1), "2018-03-16 13:00:23.073")
  )
)
