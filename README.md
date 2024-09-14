<!-- badges: start -->
[![R-CMD-check](https://github.com/gibbona1/NCAExtent/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gibbona1/NCAExtent/actions/workflows/R-CMD-check.yaml)
[![EcoEvoRxiv](http://img.shields.io/badge/DOI-10.32942/X2K332-red.svg)](https://ecoevorxiv.org/repository/view/7315/)
<!-- badges: end -->

# ExActR: A Shiny App for Creating Ecosystem Extent Accounts

`ExActR` (Extent Accounts in R) is an open-source R Shiny application for generating ecosystem extent accounts using shapefiles, a geospatial vector data format. The app is designed to assist in integrating nature into sustainable decision-making by implementing the first pillar of the System of Environmental Economic Accounting–Ecosystem Accounting (SEEA-EA) framework.

## Features

- Multiple Timepoints Support: Generate extent accounts for consecutive pairs of timepoints to accommodate dynamic ecosystem assessments over multiple periods.
- Interactive Visualizations: Create both interactive (Leaflet) and static maps for each timepoint.
- Land Cover Change Analysis: Produce bar plots illustrating land type composition and changes over time.
- Data Export: Copy tables into multiple formats, including LaTeX, for seamless integration into reports.
- Versatility: Compatible with any spatial grouping variable and adaptable to various land cover classification systems.
- User-Friendly Interface: Built with R Shiny for an interactive and responsive user experience.

## Prerequisites

To install and use the app locally

- R (version 4.0 or higher)
- RStudio (recommended)
- Git (if cloning the repository)

## Installation

```{R}
# Install devtools if not already installed
install.packages("devtools")

# Install ExActR from GitHub
devtools::install_github("gibbona1/ExActR")
```

## Usage

```
library(ExActR)

# Run the Shiny app
run_app()
```

A demo deployment of the Extent Account Shiny App is available [here](https://gibbona1.shinyapps.io/extent_app/)

TODO:
General documentation
Instructions
