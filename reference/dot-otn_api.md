# Call OTN API endpoints.

Call OTN API endpoints.

## Usage

``` r
.otn_api(endpoint, server = otn_global$server)
```

## Arguments

- endpoint:

  Character. API endpoint to call.

- server:

  Character. Lowercase network code of the desired server. One of "otn",
  "act", "npact", or "devel" (the OTN development server). Note that
  "etn", "fact", and "glatos" are accepted, but only to produce an error
  and redirect you.
