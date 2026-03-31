# Return files associated with an OTN project

Return files associated with an OTN project

## Usage

``` r
.otn_files(project, server = NULL, since = NULL, batch_size = NULL, type)
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

- type:

  Character. Portion of the URL representing the data type you wish to
  return.
