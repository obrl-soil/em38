context('final processing for decoded data')

test_that(
  'get_loc_data works correctly',
  c(
    data('n38_demo'),
    n38_chunks  <- n38_chunk(n38_demo),
    n38_decoded <- n38_decode(n38_chunks),
    loc_1 <- n38_decoded$survey_line_1$location_data[1:7, ],
    expect_is(get_loc_data(loc_1), 'data.frame'),
    expect_equal(names(get_loc_data(loc_1)),
                 c('LATITUDE', 'LONGITUDE', 'FIX', 'HDOP', 'CHKSUM',
                   'timestamp_ms')),
    expect_equal(dim(get_loc_data(loc_1))[1], 1),
    expect_equal(dim(get_loc_data(loc_1))[2], 6),
    expect_is(get_loc_data(loc_1)$LATITUDE, 'numeric'),
    expect_is(get_loc_data(loc_1)$LONGITUDE, 'numeric'),
    expect_is(get_loc_data(loc_1)$HDOP, 'numeric'),
    expect_is(get_loc_data(loc_1)$CHKSUM, 'logical'),
    expect_is(get_loc_data(loc_1)$timestamp_ms, 'integer')
    )
)

test_that(
  'em38_surveyline works correctly',
  c(
    data('n38_demo'),
    n38_chunks  <- n38_chunk(n38_demo),
    n38_decoded <- n38_decode(n38_chunks),
    sl1 <- em38_surveyline(n38_decoded[[2]], 3),
    expect_is(sl1, 'sf'),
    expect_equal(dim(sl1)[1], 3164L),
    expect_equal(dim(sl1)[2], 12L),
    expect_equal(sf::st_crs(sl1)$proj4string,
                 "+proj=longlat +datum=WGS84 +no_defs"),
    expect_equal(sf::st_bbox(sl1)[[1]], 151.4341589),
    # no loc data
    sl2 <- n38_decoded[[2]],
    sl2[['location_data']] <- sl2[['location_data']][0, ],
    sl2 <- em38_surveyline(sl2, 3),
    expect_is(sl2, 'data.frame'),
    expect_equal(dim(sl2)[1], 3164L),
    expect_equal(dim(sl2)[2], 11L),
    # no reading_data
    sl3 <- n38_decoded[[2]],
    sl3[['reading_data']] <- sl3[['reading_data']][0, ],
    sl3 <- em38_surveyline(sl3, 3),
    expect_is(sl3, 'character'),
    expect_equal(sl3, "This survey line contains no readings."),
    # nuffin
    sl4 <- n38_decoded[[2]],
    sl4[['reading_data']] <- sl4[['reading_data']][0, ],
    sl4[['location_data']] <- sl4[['location_data']][0, ],
    sl4 <- em38_surveyline(sl4, 3),
    expect_is(sl4, 'character'),
    expect_equal(sl4, "This survey line contains no readings or location data."),
    # warning on no cal,
    sl5 <- n38_decoded[[2]],
    sl5[['cal_data']] <- sl5[['cal_data']][0, ],
    expect_warning(em38_surveyline(sl5, 3)),
    # test no good HDOP
    expect_equal(em38_surveyline(n38_decoded[[2]], 0.1),
                 'No readings with acceptable HDOP could be retrieved from this survey line.')
  )
)

test_that(
  'em38_decode works correctly',
  c(
    data('n38_demo'),
    n38_chunks  <- n38_chunk(n38_demo),
    n38_decoded <- n38_decode(n38_chunks),
    survey <- em38_decode(n38_decoded),
    expect_is(survey, 'list'),
    expect_length(survey, 2),
    expect_equal(names(survey), c('file_header', 'survey_lines')),
    expect_is(survey[[2]], 'list'),
    expect_equal(names(survey[[2]]), c('1'))
  )
)

test_that(
  'em38_from_file works',
  c(
    from_file <-
      em38_from_file(path = system.file("extdata", "em38_demo.N38",
                                       package = "em38"), hdop_filter = 3),
    data('n38_demo'),
    n38_chunks  <- n38_chunk(n38_demo),
    n38_decoded <- n38_decode(n38_chunks),
    survey <- em38_decode(n38_decoded),
    expect_equal(from_file, survey)
    )
)
