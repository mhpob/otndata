# Installs your OTN username and password in your `.Renviron` file for repeated use.

This code was adapted from
[`tidycensus::census_api_key`](https://github.com/walkerke/tidycensus/blob/ddb33b5f72734a4ff14332bd55cbac4850688600/R/helpers.R).
Note that this saves your credentials in your .Renviron, meaning that
anyone who is using your computer can theoretically access what your
MATOS username and password are. So... use this carefully!

## Usage

``` r
otn_set_credentials(network, temporary = FALSE, overwrite = FALSE)
```

## Arguments

- network:

  Character. Lowercase code of the desired telemetry network. One of
  "otn", "act", "atap", "itag", "migramar", "nep", "npact", "pirat", or
  "otn_devel" (the OTN development server). Note that "etn", "fact",
  "glatos", "path" and "raft" are accepted, but only to produce an error
  and redirect you.

- temporary:

  Logical. Scrub credentials after the current session?

- overwrite:

  Logical. Overwrite previously-stored credentials?

## Examples

``` r
if (FALSE) { # \dontrun{
otn_set_credentials("act")
} # }
# Yup, that's it!
```
