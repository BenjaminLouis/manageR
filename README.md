[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.org/BenjaminLouis/manageR.svg?branch=master)](https://travis-ci.org/BenjaminLouis/manageR)

# {manageR} : a shiny app to help freelancers

The package {manageR} contains a dashboard shiny app that give tools to freelancers to help with day to day management. So far, the app allows to organise data about clients, billing addresses, quotes and invoices. Users are able to print personalised pdf of quotes and invoices based on saved data.

## Motivation

As a freelance data analyst, I needed a tool to organise data about my clients and the services I offer. No softwares found on the web convinced me enough so I decided to build my own.

At the same time, I wanted to learn more about shiny app. As a big [R](https://www.r-project.org/) user, I decided to give a try and here we are with this shiny app. 

Be aware that this is an experimental project that matches my needs. Feel free to open [issues](https://github.com/BenjaminLouis/xmlprocessor/issues) and give constructive critisms on the app and the code itself.

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BenjaminLouis/manageR")
```
## Features

### Home



### Quotes



### Invoices



### Clients



### Billing addresses



## Future features



## Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## Credits

Quotes and invoices are printed in `PDF` from a [R Markdown](https://rmarkdown.rstudio.com/) file using *CSS fo Paged Media*. This was possible through the utilisation of two packages still in development :

* [{sassr}](https://github.com/rstudio/sass) from [@RStudio](https://github.com/rstudio) : a CSS preprocessor based on [LibSass](https://github.com/sass/libsass)

* [{weasydoc}](https://github.com/RLesur/weasydoc) from [@RLesur](https://github.com/RLesur) : a package using *CSS for Paged Media* converters ([WeasyPrint](https://weasyprint.org/), [Prince](https://www.princexml.com/)) to convert [R Markdown](https://rmarkdown.rstudio.com/) using *CSS fo Paged Media* to `PDF`

Thanks to [@RLesur](https://github.com/RLesur) for the inspiring and helping discussions !
