# Droplets analysis in R

This repository contains R code for generating set of plots for droplet diameter distribution, volume, and generating rate with, IMHO, visually beautiful plots and palette.

The code has many possible configurations, not all of them are high-level and require moderate understating R, `tidyverse` (`ggplot2`) packages and non-parametric statistics.

## How to use

To run code is recommended to source the code in R directory from your own R script and make high-level operation via `process_runs` functions.

## Dependencies:

The following R packages are required:

```R

library(fs)            # file system helpers
library(readr)         # read_csv(), parse_number()
library(stringr)       # str_detect(), str_split_fixed()
library(dplyr)         # data wrangling verbs
library(tidyr)         # pivot_longer()
library(purrr)         # map_dfr()
library(lubridate)     # now()
library(ggplot2)       # plotting
library(patchwork)     # plot composition
library(colorspace)    # lighten()
library(ggridges)      # if you want ridgeline variants later
library(ggsci)         # pal_npg()
library(scales)        # comma(), hue_pal()
library(glue)          # glue()
library(ragg)          # agg_png() device
library(WRS2)          # t1waybt function
library(moments)       # skewness and kurtosis
```

To import functions from the source one may use the following code:

```R

source("path/to/droplets_analysis/R/droplets_analysis.R")

groups = list(
    group1 = list(...),
    ...,
    groupn = list(...)
)

process_runs(
    root = "path/to/dir/with/csv",
    groups = groups,
    ...,
    compare = list(
        "group1 | group2 | group3",
        "group1 - group5",
        "group2 & group4"
    )
)

```

For more detailed customization see source code.

## License

Distributed under MIT License, see `LICENSE`
