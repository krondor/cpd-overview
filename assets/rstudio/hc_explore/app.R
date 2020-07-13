library(burro)
library(shiny)
library(here)
#set option so explore_data returns a list instead of a shinyApp()
#object
options(app_list=TRUE)

PimaIndians <- readRDS(here('data','dataset.rds'))
  outcome_var <- 'test'

data_dict <- NULL

data_dict_path <- here('data', 'data_dictionary.rds')
if(file.exists(data_dict_path)) {

    data_dict <- readRDS(data_dict_path)

}

#edit your covariates here
covars <- c('carat','cut','color','clarity','depth','table','price','x','y','z')

#build the burro app and run it
app_list <- burro::explore_data(dataset=PimaIndians,
      covariates=covars, outcome_var=outcome_var,
      data_dictionary=data_dict)

ui <- app_list[['ui']]
server <- app_list[['server']]

app <- shiny::shinyApp(ui, server)
app
