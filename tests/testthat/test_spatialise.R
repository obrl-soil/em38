context('spatialising decoded data')

# these functions are all in spatialise.R

# 1. get_loc_data
test_that(
  'essential location data is extracted correctly',
  c(
    data('n38_demo'),
    n38_chunks  <- n38_chunk(n38_demo),
    n38_decoded <- n38_decode(n38_chunks),
    loc_1 <- n38_decoded$survey_line_1$location_data[1:7, ],
    expect_is(get_loc_data(loc_1), 'data.frame'),
    expect_equal(names(get_loc_data(loc_1)),
                 c('LATITUDE', 'LONGITUDE', 'HDOP', 'CHKSUM', 'timestamp_ms')),
    expect_equal(dim(get_loc_data(loc_1))[1], 1),
    expect_equal(dim(get_loc_data(loc_1))[2], 5),
    expect_is(get_loc_data(loc_1)$LATITUDE, 'numeric'),
    expect_is(get_loc_data(loc_1)$LONGITUDE, 'numeric'),
    expect_is(get_loc_data(loc_1)$HDOP, 'numeric'),
    expect_is(get_loc_data(loc_1)$CHKSUM, 'logical'),
    expect_is(get_loc_data(loc_1)$timestamp_ms, 'integer')
    )
)

# 2. em38_spatial
test_that(
  'em38 data spatialises correctly',
  c(
    data('n38_demo'),
    n38_chunks  <- n38_chunk(n38_demo),
    n38_decoded <- n38_decode(n38_chunks),
    n38_sf <- em38_spatial(n38_decoded, 3),
    expect_is(n38_sf, 'sf'),
    expect_equal(dim(n38_sf)[1], 3164L),
    expect_equal(dim(n38_sf)[2], 9L),
    expect_equal(sf::st_crs(n38_sf)$proj4string,
                 "+proj=longlat +datum=WGS84 +no_defs"),
    expect_equal(sf::st_bbox(n38_sf)[[1]], 151.4341589),
    # test missing loc data
    n38_noloc <- n38_decoded,
    n38_noloc$survey_line_1$location_data <- NA,
    expect_equal(em38_spatial(n38_noloc),
                 'This survey line contains no embedded location data.'),
    # test missing reading data
    n38_nodat <- n38_decoded,
    n38_nodat$survey_line_1$reading_data <- NA,
    expect_equal(em38_spatial(n38_nodat),
                 'This survey line contains no data.'),
    # test no good HDOP
    expect_equal(em38_spatial(n38_decoded, 0.1),
                 'No readings with acceptable HDOP could be retrieved from this survey line.')
  )
)

# 3. n38_to_points
test_that(
  'em38 data spatialises correctly with wrapper',
  c(
    n38_sf <-
      n38_to_points(path = system.file("extdata", "em38_demo.N38",
                                       package = "em38"), hdop_filter = 3),
    expect_is(n38_sf, 'sf'),
    expect_equal(dim(n38_sf)[1], 3164),
    expect_equal(dim(n38_sf)[2], 9),
    expect_equal(sf::st_crs(n38_sf)$proj4string,
                 "+proj=longlat +datum=WGS84 +no_defs"),
    expect_equal(sf::st_bbox(n38_sf)[[1]], 151.4341589),
    # test no good HDOP
    expect_equal(n38_to_points(path = system.file("extdata", "em38_demo.N38",
                                                  package = "em38"),
                               hdop_filter = 0.1),
                 'No readings with acceptable HDOP could be retrieved from this survey line.')
  )
)
