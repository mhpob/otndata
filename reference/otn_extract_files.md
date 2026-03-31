# List OTN extract files

This function recursively lists files in the *Detection Extracts*
section of your project.

## Usage

``` r
otn_extract_files(project, server = NULL, since = NULL, batch_size = 25)
```

## Arguments

- project:

  Character. The project code.

- server:

  Character. URL of the OTN-style Plone server. Defaults to the main OTN
  server at <https://members.oceantrack.org>.

- since:

  Character. Filter for files modified since this date (YYYY-MM-DD
  format).

- batch_size:

  Numeric. The number of results to return. Defaults to 25.
