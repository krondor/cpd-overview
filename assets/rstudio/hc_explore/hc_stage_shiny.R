#install.packages(c("flexdashboard", "plotly", "remotes"))
#remotes::install_github("laderast/burro")

library(flexdashboard)
library(burro)
library(plotly)
library(readr)

PimaIndians <- read_csv("~/project_git_repo/cpd-overview/assets/data_asset/PimaIndians.csv")

## specify outcome variable here
outcome <- c("test")

## specify covariate columns
covars <- colnames(diamonds)

burro::build_shiny_app(dataset = PimaIndians, covariates = covars, outcome_var = outcome)