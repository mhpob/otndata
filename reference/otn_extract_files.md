# List OTN extract files.

This function recursively lists files in the *Detection Extracts*
section of your project.

## Usage

``` r
otn_extract_files(project, since = NULL, batch_size = 25)
```

## Arguments

- project:

  Character. The project code.

- since:

  Character. Filter for files modified since this date (YYYY-MM-DD
  format).

- batch_size:

  Numeric. The number of results to return. Defaults to 25.

## See also

[.otn_files](https://otndata.obrien.page/reference/dot-otn_files.md)
