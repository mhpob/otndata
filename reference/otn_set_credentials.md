# Install your OTN username and password in your `.Renviron` File for repeated use

This code was adapted from
[`tidycensus::census_api_key`](https://github.com/walkerke/tidycensus/blob/ddb33b5f72734a4ff14332bd55cbac4850688600/R/helpers.R).
Note that this saves your credentials in your .Renviron, meaning that
anyone who is using your computer can theoretically access what your
MATOS username and password are. So... use this carefully!

## Usage

``` r
otn_set_credentials(temporary = TRUE, overwrite = FALSE)
```

## Arguments

- temporary:

  Logical. Scrub credentials after the current session?

- overwrite:

  Logical. Overwrite previously-stored MATOS credentials?

## Examples

``` r
if (FALSE) { # \dontrun{
otn_set_credentials()
} # }
# Yup, that's it!
```
