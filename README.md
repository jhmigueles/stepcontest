
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stepcontest

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/stepcontest)](https://CRAN.R-project.org/package=stepcontest)
<!-- badges: end -->

The package `stepcontest` is a Shiny app proposing a simple contest on
accumulating steps over a period. This will be used in a BSc Thesis to
challenge high-school students to accumulate more steps.

## Specifics

`stepcontest` uses GGIR and the verisense step count function in the
background, meaning that it can read data from ActiGraph, GENEActiv,
Axivity, Movisense devices and other brands.

## Live Demo

[View the app on
shinyapps.io](https://jhmigueles.shinyapps.io/stepcontest/)

## How It Works

- Upload accelerometer files.
- Process the data and compare step counts.
