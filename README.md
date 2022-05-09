# koboAPI

<!-- badges: start -->

[![R-CMD-check](https://github.com/ElliottMess/koboAPI/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ElliottMess/koboAPI/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ElliottMess/koboAPI/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ElliottMess/koboAPI?branch=main)
<!-- badges: end -->

koboAPI is a package to access and interact with [KoboToolbox](https://www.kobotoolbox.org/)-based APIs via R. The package technically supports any API that is based on KoboToolbox. It is however made simpler for [KoboToolbox](https://www.kobotoolbox.org/) and [humanitarianresponse.info](https://kobo.humanitarianresponse.info/) servers.

KoboAPI provides simple functions to retrieve forms and data set from KoboToolbox.

The [documentation of the kobo APIs](https://support.kobotoolbox.org/api.html) is incomplete. Some community-based examples on how to access and interact with the API can be found [here](https://community.kobotoolbox.org/t/kobo-api-examples-using-new-kpi-endpoints/2742).

## Installation

```r
devtools::install_github("ElliottMess/koboAPI")
```
## Authentication

Two type of authentication is available:

- `client`: client authentication by providing a valid `auth_key` whick follows the format '{user}:{password}'
- `token`: authentication through a token. [See here to get a token](https://support.kobotoolbox.org/api.html#getting-your-api-token)

Eventually, all authentication will result in retrieving token.

### Setting your token in your environement

The easiest way to use the package is to set the API token in the R environment:

1. Get your API token: [see how here](https://support.kobotoolbox.org/api.html#getting-your-api-token).
2. Run `usethis::edit_r_environ()`
3. Add a line to the now opened `'.Renviron'` file with either:
   - KOBOTOOLBOX\_TOKEN = “yourtoken here”
   - HUMANITARIAN\_RESPONSE\_TOKEN = “yourtoken here”
4.  Make sure `'.Renviron'` ends in a new line, save it, and restart R

## kobo_api
The core function of the package is `kobo_api`. It makes a call to the API and return the response in a helpful object.

For instance, if you want to retrieve all the assets linked to your access token, you can use:
```r
kobo_api(path = "/assets", auth_type = "token", auth_key = "HERE_A_TOKEN", api = "kobotoolbox")
```

Any valid GET call to the API can passed to `kobo_api()`.

## get functions
A few convenience wrappers around kobo_api are provided for convienence. They return objects that are close to what can be found downloaded from the kobotoolbox-based web interfaces. All of them are based on passing a valid `asset_id`. Assets IDs available to you can be found through `get_asset_id_list` for a named list of IDs or `get_asset_id_df` for a dataframe with a bit more information.

- `get_form`: retrieves the form for the `asset_id`. The output is a list of three dataframes that are the three sheets of a [XLSform](https://xlsform.org/en/).
- `get_data`: retrieves the data with the `asset_id` responses.

## Roadmap
The package provides basic functions, and a lot more could be done:

- Support of `exports`
- Support of POST functions
- Coverage of all API functions
