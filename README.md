# OpenSciThesis

To run, source the [make.R file](make.R)

### Code for my Honours thesis on open science practice

This repository contains the R code for my honours thesis analysis. It uses the [drake package](https://github.com/ropensci/drake) and Miles McBain's [dflow](https://github.com/MilesMcBain/dflow) for structuring the file base.

Please see [here](R/plan.R) for the drake code plan. Each file in the R folder is [used as a function](https://books.ropensci.org/drake/scripts.html#function-oriented-workflows) in the plan.

### This code cannot be run on its own!
As I do not have ownership over the data used, the data/ folder is not part of this repository. If permission is given for the data to be uploaded, it will likely be uploaded to the [Open Science Framework](https://osf.io/cxus9/).

The data files that are missing are:

- TOP factor data
  - This can be obtained from: https://osf.io/qatkz/
- Sherpa/ROMEO data
  - This can be obtained from: https://v2.sherpa.ac.uk/romeo/
  - I use an API call  in fetch_sherpa to obtain the journals for analysis. An api key is needed for this to work (see get_key).
- Journal ranking data
  - This can be obtained from Scopus: https://www.scopus.com/sources
  - Have to export the first sheet of the xlsb as 'CiteScore.csv', as R cannot parse xlsb files well.

- Survey data
  - This cannot currently be obtained without permission. If uploaded, will be on the OSF.

### Dependency Graph

![dependency graph](plot.png)
