# Internal function to list small OTN databases.

Provides access to the data that is on the main OTN members page. Login
is not required.

## Usage

``` r
.otn_list(server, type)
```

## Arguments

- server:

  Character. Lowercase network code of the desired server. One of "otn",
  "act", "npact", or "devel" (the OTN development server). Note that
  "etn", "fact", and "glatos" are accepted, but only to produce an error
  and redirect you.

- type:

  Character. Internal data base to retrieve.
