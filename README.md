
<!-- README.md is generated from README.Rmd. Please edit that file -->

# evolovr

<!-- badges: start -->

<!-- badges: end -->

Le but d’*evolovr* est d’offrir des fonctions pour manipuler des données
de biodiversité automatiquement pour le projet de blogue
[Évologie](https://evologie.netlify.app).

## Installation

Pour installer evolovr depuir
[GitHub](https://github.com/beausoleilmo/evolovr) avec:

``` r
# install.packages("pak")
pak::pak("beausoleilmo/evolovr")
```

## Example

``` r
library(evolovr)
# Partir une connection duckdb
con <- setup_duckdb()
## Faire des opérations 

discon_duckdb(con = con)
```
