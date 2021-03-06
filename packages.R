## library() calls go here
## Workflow Packages ##
library(conflicted)
library(dotenv)
library(drake)

## Data Analysis ##
library(withr)
library(readxl)
library(magrittr)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(fuzzyjoin)
library(gtools)
library(rlist)
library(psych)
library(coxed)

# Data Viz
library(ggplot2)
library(cowplot)
library(diagram)
library(ggridges)
library(ggpubr)

## Parallel Processing ##
library(doSNOW)
library(parallel)

## R Markdown ##
library(rmarkdown)
library(papaja)
library(knitr)
library(kableExtra)