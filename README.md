# BEIB <img src="figures\BBdois alternative.svg" align="right" height="197" alt="Hex Sticker Placeholder" />

<!-- badges: start -->
<!-- badges: end -->

**BEIB** (Basic Education Indicators of Brazil) is an R package developed with the goal of facilitating access to and retrieving data from educational indicators calculated and made available by [INEP](https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/indicadores-educacionais) regarding Basic Education in Brazil.

## Installation

You can install the development version of BEIB from [GitHub](https://github.com/) by running:

```r
# install.packages("devtools")
devtools::install_github("your-username/BEIB")
```

## Available Indicators

The package provides functions to easily access the following datasets and Basic Education indicators:

* Adequacy of Teacher Training
* School Management Complexity
* Teaching Effort
* Ideb (Basic Education Development Index)
* Educational Financial Indicators
* Average Number of Students per Class
* Average Daily Class Hours
* Socioeconomic Level (Inse)
* Percentage of Teachers with Higher Education Degree
* Regularity of the Teaching Staff
* Average Teacher Remuneration
* Age-Grade Distortion Rates
* Non-Response Rates (TNR)
* Transition Rates
* Pass/Fail/Dropout Rates (Taxas de Rendimento)

## Usage Example

This is a basic example of how to use the package to import data from one of the indicators:

```r
library(BEIB)

# Hypothetical example: Downloading Average Number of Students per Class data for a specific year
class_data <- get_media_alunos_turma(ano = 2023)

head(class_data)
```

## Data Source

All data accessed by this package comes from the Federal Government's Open Data portal and the Anísio Teixeira National Institute of Educational Studies and Research (Inep).

For more details on the calculation methodologies of each indicator, visit:
[Educational Indicators - INEP](https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/indicadores-educacionais)

---

**Note:** This package is under development. Contributions, suggestions, and bug reports are very welcome!
