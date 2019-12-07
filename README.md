# koboAPI
koboAPI is a package to facilitate the interaction between R users and [KoBoToolbox API](https://www.kobotoolbox.org/). koboAPI provides simple functions to retrieve forms and datasets. The default output format is as close to the format provided by the website interface as possible.

As of November 2019, KoBoToolbox has two version of its API: KC and KPI, the later being the most recen.t. If KoBoToolbox developpers say that KPI should be used, the documentation is only partial, especially on how to retrieve data.

## Installation

```r
devtools:install_github("EliottMess/koboAPI")
```

## Main Functions

### Form
- dowload_form: download the questionnaire for a specific deployment.

- download_form_all: download basic informations about all the deployment linked to the account.

### Data

- download_data: download all the dataset linked to a specific deployment. 
