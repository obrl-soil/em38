context('signal conversions')

# these functions are all in signal_conversion.R

# 1. get_cond
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

# 2. get_temp
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

# 3. gpgga_lat
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

# 4. gpgga_long
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

