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
    expect_equal(nrow(get_loc_data(loc_1)), 1),
    expect_equal(ncol(get_loc_data(loc_1)), 5),
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
    n38_sf <- em38_spatial(n38_decoded, 3, 'Vertical'),
    expect_is(n38_sf, 'sf'),
    expect_equal(nrow(n38_sf), 3162),
    expect_equal(ncol(n38_sf), 7),
    expect_equal(sf::st_crs(n38_sf)$proj4string,
                 "+proj=longlat +datum=WGS84 +no_defs"),
    expect_equal(as.numeric(sf::st_bbox(n38_sf)[1]), 151.434158912587),
    # test missing loc data
    n38_noloc <- n38_decoded,
    n38_noloc$survey_line_1$location_data <- NA,
    expect_equal(em38_spatial(n38_noloc),
                 'This survey line contains no embedded location data.'),
    # test poorly defined out_mode
    expect_equal(em38_spatial(n38_decoded, 3, 'vertical'),
                 "Please check the value of `out_mode`."),
    expect_equal(em38_spatial(n38_decoded, 3),
                 "Please specify `out_mode`."),
    # test no readings of out_mode in this dataset
    n38_nov <- n38_decoded,
    n38_nov$survey_line_1$reading_data <-
      n38_nov$survey_line_1$reading_data[
        n38_nov$survey_line_1$reading_data$mode == 'Horizontal', ],
    expect_equal(em38_spatial(n38_nov, 3, 'Vertical'),
                 'No readings were recorded with Vertical dipole mode on this survey line.'),
    # test no good HDOP
    expect_equal(em38_spatial(n38_decoded, 0.1, 'Vertical'),
                 'No readings with acceptable HDOP could be retrieved from this survey line.')
  )
)

# 3. n38_to_points
test_that(
  'em38 data spatialises correctly with wrapper',
  c(
    n38_sf <-
      n38_to_points(path = system.file("extdata", "em38_demo.N38",
                                       package = "em38"),
                    hdop_filter = 3, out_mode = 'Vertical'),
    expect_is(n38_sf, 'sf'),
    expect_equal(nrow(n38_sf), 3162),
    expect_equal(ncol(n38_sf), 7),
    expect_equal(sf::st_crs(n38_sf)$proj4string,
                 "+proj=longlat +datum=WGS84 +no_defs"),
    expect_equal(as.numeric(sf::st_bbox(n38_sf)[1]), 151.434158912587),
    expect_equal(n38_to_points(path = system.file("extdata", "em38_demo.N38",
                                                  package = "em38"),
                               hdop_filter = 3, out_mode = 'vertical'),
                 "Please check the value of `out_mode`."),
    expect_equal(n38_to_points(path = system.file("extdata", "em38_demo.N38",
                                                  package = "em38"),
                               hdop_filter = 3),
                 "Please specify `out_mode`."),
    # test no good HDOP
    expect_equal(n38_to_points(path = system.file("extdata", "em38_demo.N38",
                                                  package = "em38"),
                               hdop_filter = 0.1, out_mode = 'Vertical'),
                 'No readings with acceptable HDOP could be retrieved from this survey line.')
  )
)
