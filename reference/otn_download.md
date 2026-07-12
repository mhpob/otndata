# Download file(s) from an OTN-style Plone server.

Either `files` or `url` must be provided. If `files` is provided, files
in the data.frame are downloaded in parallel.

## Usage

``` r
otn_download(files = NULL, url = NULL, outdir = ".")
```

## Arguments

- files:

  a data.frame returned by by `otn_project_files` or
  `otn_extract_files`. Can have been filtered or edited, but must have
  columns of "url" and "name".

- url:

  The URL of the file as returned by `otn_project_files` or
  `otn_extract_files`

- outdir:

  The output directory (where you want the file to be saved). Defaults
  to the current working directory.

## Value

Silently returns a vector of file locations.
