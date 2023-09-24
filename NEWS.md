# Version 0.0.6

  * some under-the-hood corrections to work with recent R changes
  * some improvements in handling data from certain GNSS receivers, many thanks to the person who supplied a test dataset :pray:
  * updated vignette, various methods of spatially interpolating the raw data are now demonstrated

# Version 0.0.5

  * package now reports orthometric height of each spatialised reading, calculated from GPS data, per user request (thanks dukarvat!). Note that elevation data may still need further correction to a local vertical datum before use.

# Version 0.0.4

  * package can now handle GPS messages from GLONASS, GALILEO, and multi-GNSS
  systems

# Version 0.0.3

  * Update for compatibility with newer versions of `units`

# Version 0.0.2

  * Bugfix for GH-1 comment processing, many thanks to sanjaykmenon for 
    providing test data  

# Version 0.0.1

  * Major refactor - process survey lines separately, cope with missing/bad GPS
    data, better paired data handling
  * Added `em38_surveyline()` which will process data from an individual survey 
    line output by `n38_decode()`.
  * Survey line decodes can now be `sf` or `data.frame` and include both 
    timestamps and sequential ID.
  * renamed `em38_spatial()` to `em38_decode()`, the function now outputs a list 
    containing file header data and survey lines.
  * renamed `n38_to_points()` to `em38_from_file()`, the output is identical to 
    `em38_decode()`  
  * Survey lines are no longer filtered by dipole mode
  * Added filters for GPS time delay and signal quality.
  * Various small bugfixes

# Version 0.0.0.9003

  * `n38_decode()` better living through proper vectorisation - speed 
    improvement.
  * More unit tests.
  * Package-level metadata.
  * Travis-CI integration

# Version 0.0.0.9002

  * New `em38_pair()` - combines data from manual-mode surveys where 
    horizontal and vertical readings have been taken at each station.
  * Checksums on NMEA-0183 sentences are evaluated for $GPGGA type, failures 
    are dropped.
  * Who's got two thumbs and forgot about `@keywords Internal`? \*gestures at 
    self\*. The help menu is less cluttered now.

# Version 0.0.0.9001

  * New `n38_to_points()` wrapper function - on-disk file to spatial points 
    in one hit.
  * `n38_import()` variable name fix; `x` is now `path`.
  * `em38_spatialise()` now constrains `out_mode` values and fails gracefully 
    when allowable values aren't supplied.
  * Rebuilt demo data to match demo extdata.

# Version 0.0.0.9000

  * Basic functions: import N38 files, convert them to a nested list of raw() 
    matrices, decode those, output an M38 file, output a spatial point dataset.
  * Minimal testing for NMEA-0183 sentence parsing (just checks on lat and 
    long extraction from string).
  
