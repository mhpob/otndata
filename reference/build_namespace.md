# Build project namespace

Most project files are saved under `/data/repository/{project}`. Some
networks, namely ATAP, MigraMar and NEP, are directly hosted by OTN and
so have their files saved under `/data/repository/{network}/{project}`.
This function does some light cleaning of the project code and adjusts
for these OTN-hosted networks.

## Usage

``` r
build_namespace(project)
```

## Arguments

- project:

  Character. The project code.
