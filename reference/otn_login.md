# Authenticate to an Ocean Tracking Network server

This function prompts you for the username and password associated with
your OTN account. This is necessary so that you may interface with any
project-specific files.

## Usage

``` r
otn_login(server = NULL, temporary = FALSE)
```

## Arguments

- server:

  Character. URL of the OTN-style Plone server. Defaults to the main OTN
  server at <https://members.oceantrack.org>.

- temporary:

  Logical. Provide credentials for the current session only?

## Details

A pop up will appear asking for your username and password. If
everything works out, a token is written to the "OTN_SESSION_TOKEN"
system variable. Your username/password will not be saved – this was
done intentionally so that you don't accidentally save credentials in a
public script.
