# Return files associated with an OTN project.

Return files associated with an OTN project.

## Usage

``` r
.otn_files(project, since = NULL, batch_size = NULL, type)
```

## Arguments

- project:

  Character. The project code.

- since:

  Character. Filter for files modified since this date (YYYY-MM-DD
  format).

- batch_size:

  Numeric. The number of results to return. Defaults to 25.

- type:

  Character. Portion of the URL representing the data type you wish to
  return.

## See also

- Plone REST API documentation:

  - [Querystring
    Search](https://6.docs.plone.org/plone.restapi/docs/source/endpoints/querystringsearch.html),
    triggered when using the "`since`" argument. [Query operations are
    listed
    here.](https://6.docs.plone.org/plone.restapi/docs/source/endpoints/querystring.html).

  - [Search](https://6.docs.plone.org/plone.restapi/docs/source/endpoints/searching.html),
    a simpler search used when the "`since`" argument is NULL.
