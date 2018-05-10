# v. 0.0.0.9001

  * wrapper function added - `n38_to_points()` goes from on-disk file to spatial points in one line.
  * rebuilt demo data to match demo extdata
  * variable name fix in `n38_import()`

# v. 0.0.0.9000

  * Basic functions: import N38 files, convert them to a nested list of raw() matrices, decode those, output an M38 file,
    output a spatial point dataset
  * Minimal testing for NMEA-0183 sentence parsing (just checks on lat and long extraction from string)
  
