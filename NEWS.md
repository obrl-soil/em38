# v 0.0.1

  * Refactor `em38_spatial()` 
  * `n38_to_points()` and `em38_spatial()` no longer filter on dipole mode
  * Columns containing dipole mode and a sequential ID added to outputs of `n38_to_points()` and `em38_spatial()`


# v 0.0.0.9003

  * `n38_decode()` better living through proper vectorisation - speed 
    improvement.
  * More unit tests.
  * Package-level metadata.
  * Travis-CI integration

# v 0.0.0.9002

  * New `em38_pair()` - combines data from manual-mode surveys where 
    horizontal and vertical readings have been taken at each station.
  * Checksums on NMEA-0183 sentences are evaluated for $GPGGA type, failures 
    are dropped.
  * Who's got two thumbs and forgot about `@keywords Internal`? \*gestures at 
    self\*. The help menu is less cluttered now.

# v. 0.0.0.9001

  * New `n38_to_points()` wrapper function - on-disk file to spatial points 
    in one hit.
  * `n38_import()` variable name fix; `x` is now `path`.
  * `em38_spatialise()` now constrains `out_mode` values and fails gracefully 
    when allowable values aren't supplied.
  * Rebuilt demo data to match demo extdata.

# v. 0.0.0.9000

  * Basic functions: import N38 files, convert them to a nested list of raw() 
    matrices, decode those, output an M38 file, output a spatial point dataset.
  * Minimal testing for NMEA-0183 sentence parsing (just checks on lat and 
    long extraction from string).
  
