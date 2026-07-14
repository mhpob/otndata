# Authenticate to an Ocean Tracking Network server.

This function prompts you for the username and password associated with
your OTN account. This is necessary so that you may interface with any
project-specific files.

## Usage

``` r
otn_login(network = NULL)
```

## Arguments

- network:

  Character. Lowercase code of the desired telemetry network. One of
  "otn", "act", "atap", "itag", "migramar", "nep", "npact", "pirat", or
  "otn_devel" (the OTN development server). Note that "etn", "fact",
  "glatos", "path" and "raft" are accepted, but only to produce an error
  and redirect you.

## Details

A pop up will appear asking for your username and password. If
everything works out, a token is written to the "SESSION_TOKEN" variable
in the `otn_global` environment. Your username/password will not be
saved – this was done intentionally so that you don't accidentally save
credentials in a public script.
