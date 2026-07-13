# Package index

## Authentication

- [`otn_login()`](https://otndata.obrien.page/reference/otn_login.md) :
  Authenticate to an Ocean Tracking Network server.

- [`otn_set_credentials()`](https://otndata.obrien.page/reference/otn_set_credentials.md)
  :

  Installs your OTN username and password in your `.Renviron` file for
  repeated use.

## Listing Project and Extract Files

- [`otn_project_files()`](https://otndata.obrien.page/reference/otn_project_files.md)
  : List OTN project files.
- [`otn_extract_files()`](https://otndata.obrien.page/reference/otn_extract_files.md)
  : List OTN extract files.

## Downloading files

- [`otn_download()`](https://otndata.obrien.page/reference/otn_download.md)
  : Download file(s) from an OTN-style Plone server.

## Summarize data extracts

Functions to download and prepare data for
[otndo](https://otndo.obrien.page) reports

- [`otn_receiver_summary()`](https://otndata.obrien.page/reference/otn_receiver_summary.md)
  : Create summary reports of receiver project data from the OTN data
  push.
- [`otn_tag_summary()`](https://otndata.obrien.page/reference/otn_tag_summary.md)
  : Create summary reports of receiver project data from the OTN data
  push.

## Data from the summary widget

Polls the data from the OTN summary widget. Not all nodes have this
add-on installed, so be aware that they may not work.

- [`otn_search_code()`](https://otndata.obrien.page/reference/otn_search_code.md)
  : Search OTN database by project code.
- [`otn_search_contact()`](https://otndata.obrien.page/reference/otn_search_contact.md)
  : Search OTN database by project contact.
- [`otn_search_country()`](https://otndata.obrien.page/reference/otn_search_country.md)
  : Search OTN database by project country.
- [`otn_search_institution()`](https://otndata.obrien.page/reference/otn_search_institution.md)
  : Search OTN database by institution.
- [`otn_search_node()`](https://otndata.obrien.page/reference/otn_search_node.md)
  : Search OTN database by local node.
- [`otn_search_species()`](https://otndata.obrien.page/reference/otn_search_species.md)
  : Search OTN database by project species
- [`otn_list_contacts()`](https://otndata.obrien.page/reference/otn_list_contacts.md)
  : Retrieve OTN contact list.
- [`otn_list_countries()`](https://otndata.obrien.page/reference/otn_list_countries.md)
  : Retrieve OTN country list.
- [`otn_list_institutions()`](https://otndata.obrien.page/reference/otn_list_institutions.md)
  : Retrieve OTN institution list.
- [`otn_list_projects()`](https://otndata.obrien.page/reference/otn_list_projects.md)
  : Retrieve OTN project list.
- [`otn_list_species()`](https://otndata.obrien.page/reference/otn_list_species.md)
  : Retrieve OTN species list.
- [`otn_list_stats()`](https://otndata.obrien.page/reference/otn_list_stats.md)
  : Retrieve OTN server statistics.

## Internals

- [`.otn_server_url()`](https://otndata.obrien.page/reference/dot-otn_server_url.md)
  : Set the OTN Plone instance with which you wish to interact.
- [`.otn_api()`](https://otndata.obrien.page/reference/dot-otn_api.md) :
  Call OTN API endpoints.
- [`.otn_files()`](https://otndata.obrien.page/reference/dot-otn_files.md)
  : Return files associated with an OTN project.
- [`.otn_list()`](https://otndata.obrien.page/reference/dot-otn_list.md)
  : Internal function to list small OTN databases.
- [`.otn_search()`](https://otndata.obrien.page/reference/dot-otn_search.md)
  : Internal function for searching OTN projects.
