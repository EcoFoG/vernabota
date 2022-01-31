![stability-wip](https://img.shields.io/badge/stability-work_in_progress-lightgrey.svg)
![R-CMD-check](https://github.com/EcoFoG/vernabota/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/github/EcoFoG/vernabota/branch/main/graphs/badge.svg)](https://codecov.io/github/EcoFoG/vernabota)

# Gapfilling missing botanical names using vernacular names

The R package **vernabota** allows gapfilling missing botanical names using vernacular names, in the case of Guyafor census data.
The objective is to obtain a chosen number of simulated communities for which individuals only identified with a vernacular name are given a botanical name based on probabilities of association of vernacular and botanical names.

## Installing and loading the package

Before the first use, the package needs to be installed from GitHub:

```{r install, eval=FALSE}
devtools::install_github("EcoFoG/vernabota", build_vignettes = TRUE)
```

It can then be loaded.

```{r setup}
library(vernabota)
```

A quick [introduction](https://ecofog.github.io/vernabota/articles/vernabota.html) is in `vignette("vernabota")`.
