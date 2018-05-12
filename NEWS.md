# v 0.0.0.9002

  * Who's got two thumbs and forgot about `@keywords Internal`? \*gestures at self\*
  * Function `em38_pair()` added - combines data from manual-mode surveys where horizontal and vertical readings have been taken at each station
  * Checksums on NMEA-0183 sentences are evaluated for $GPGGA type, failures are dropped.

# v. 0.0.0.9001

  * Wrapper function added - `n38_to_points()` goes from on-disk file to spatial points in one hit.
  * Rrebuilt demo data to match demo extdata
  * Variable name fix in `n38_import()`
  * Constrain out_mode better in `em38_spatialise()`

# v. 0.0.0.9000

  * Basic functions: import N38 files, convert them to a nested list of raw() matrices, decode those, output an M38 file,
    output a spatial point dataset
  * Minimal testing for NMEA-0183 sentence parsing (just checks on lat and long extraction from string)
  
