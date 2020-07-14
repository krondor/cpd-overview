# Sample Materials, provided under license.
# Licensed Materials - Property of IBM
# Â© Copyright IBM Corp. 2019, 2020. All Rights Reserved.
# US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.

## Buttons to click on, each button has name, product, last offer
clientButton <- function(id, name, last_offer_status) {
  
  
  tags$p(
    actionButton(paste0('client-btn-', id),
                 fluidRow(
                   
                   column(9, align="center", style="padding:16px;",
                          strong(name) #,
                          
                   ),

                   column(3, align="center", style="padding:16px;",
                          strong(last_offer_status)
                          
                   )
                 ),
                 style="width:100%"
    )
  )
}

homePanel <- function(){
  drop_choice=c(unique(AllPatients$PRINC_DIAG_DESC))
  tabPanel(
    "Dashboard",
    tags$head(
      tags$style(HTML("
                      .datatables {
                      width: 100% !important;
                      }
                      "))
      ),
    shinyjs::useShinyjs(),
    
    fluidRow(
      column(5, panel(
        tags$h2("All Hospitals"),
        tags$br(),
        column(9, align="center", ("Hospital")),
       # column(3, align="center", ("Hospital ID")),
        column(3, align="center", ("Patients Count")),
        lapply(clientIds, function(id){
          
          client <- clients[[toString(id)]]
          
          clientButton(id, client$name, client$Patient_count)
        })
      )
      
      ),
      
      
      column(7, 
             #panel(
             #  h2("Location of the hospitals and patients"),
             #  leafletOutput("LocationPlot", width = "700px", height = "400px")
             #),
             panel(
               div(style="float:left",prettyRadioButtons("dropdiagInput", "Diagnosis Code",
                                                         choices = drop_choice,
                                                         selected = drop_choice[1])),
               br(),br(),br(),br(),
               h2("Nearest Hospital vs Visited Hospital"),
               plotlyOutput("LocationPlotly", width = "800px", height = "500px")
             ),
             
             panel(
               
               
               h2("Average cost in the hospitals"),
               plotlyOutput("coststaylengthplot", width = "800px", height = "400px"),
               
               h2("Patients Age"),
               plotlyOutput("PatientsPlot", width = "600px", height = "400px")
             )
      )
      
    )
      )
  
  }


AllPatient=AllPatients[!(is.na(AllPatients$PVDR_LAT) | AllPatients$PVDR_LAT==""),]


a <- list(
  autotick = FALSE,
  ticks = "outside",
  tick0 = 0,
  dtick = 1,
  ticklen = 5,
  tickwidth = 2,
  tickcolor = toRGB("blue")
)




homeServer <- function(input, output, session, sessionVars) {
  

    # Observation events for client buttons
    lapply(paste0('client-btn-', clientIds),
           function(x){
             observeEvent(
               input[[x]],
               {
                 id <- sub("client-btn-", "", x)
                 sessionVars$selectedClientId <- id
                 updateTabsetPanel(session, "proNav", selected = "clientPanel")
               }
             )
           })
    
    
  observeEvent( input$dropdiagInput,{    
    AllPatient <- AllPatient[AllPatient$PRINC_DIAG_DESC==input$dropdiagInput,]
    
    df1 <- AllPatient %>% group_by(PROVIDER_NAME,NEAREST_HOSPITAL) %>% summarise(
      Patient_Count = n(),
      Trade_off = round(mean(TRADEOFF),1)
    )
    
    df2=as.data.frame(df1)
    
    # Plot 1
    hospitalcharge<-AllPatient%>% group_by(PROVIDER_NAME) %>% summarise(
      
      AVG_CHARGES_PER_DAY = round(mean(CHARGES_PER_DAY),2),
      AVG_LENGTH_OF_STAY = round(mean(LENGTH_OF_STAY),2),
      AVG_Distance_travelled = round(mean(DISTANCE_TO_HOSPITAL),2)
    )
    hospitalcharge$AVG_CHARGES <-hospitalcharge$AVG_CHARGES_PER_DAY#round(hospitalcharge$AVG_CHARGES_PER_DAY/1000,2)
    hospitalcharge$AVG_CHARGES_TEXT <- hospitalcharge$AVG_CHARGES_PER_DAY#paste0(hospitalcharge$AVG_CHARGES,"k per day")
    
    hospitalchargePlot <- plot_ly(hospitalcharge, x = ~AVG_CHARGES, y = ~PROVIDER_NAME,text = ~paste("Average Charges($):",AVG_CHARGES_TEXT,"<br>","Average Length of stay: ",AVG_LENGTH_OF_STAY), textposition = 'middle right',orientation="h", type = 'bar', name = 'AVERAGE CHARGES PER DAY'  ) %>%
      #add_trace(x = ~AVG_LENGTH_OF_STAY, name = 'AVERAGE LENGTH OF STAY') %>%
      layout(margin = list(l = 200),xaxis = list(title = 'AVERAGE COST PER DAY ', barmode = 'group'),yaxis = list(title = 'Provider Name'))
    hospitalchargePlot$width=800
    hospitalchargePlot$elementId=NULL
    output$coststaylengthplot <- renderPlotly(hospitalchargePlot)
    
    #Plot 2
    
    Patage=AllPatient%>% 
      group_by(PAT_AGE) %>% tally()
    
    Patage=as.data.frame(Patage)
    
 
    
    PatientsPlot <- plot_ly(Patage, labels = ~PAT_AGE, values = ~n, type = 'pie') %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    


    PatientsPlot$elementId=NULL
    # Display plot 1
    output$PatientsPlot <- renderPlotly(PatientsPlot)
  
  
  #output$PatientsPlot <- renderggiraph(ggiraph(ggobj = PatientsPlot, width_svg=6, height_svg=4))
  
 # output$LocationPlot <- renderLeaflet(LocationPlot)
    
    output$LocationPlotly <-renderPlotly(
    
      ss <-ggplot(data = df2, aes(x=PROVIDER_NAME, y=NEAREST_HOSPITAL, fill=Patient_Count, text=paste0("Distance Between them- ",Trade_off)))  +
      
      geom_tile() +
      theme(
        axis.title.x = element_text(angle = 0, hjust = 1, size = 10, vjust=0.5, margin=margin(-15,0,0,0)),
        axis.title.y = element_text(angle = 90, hjust = 1, size = 10, vjust=0.5, margin=margin(-15,0,0,0)),
        axis.text.x = element_text(angle = 45, hjust=1, margin=margin(0,-30,0,0)),
        axis.text.y = element_text(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank() 
      )  + xlab("Visited Hospital") + ylab("Nearest Hospital") +
      #geom_point(aes(text="TEST"))+
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                   title.position = "top", title.hjust = 0.5))+
      scale_fill_gradient2(low="darkblue", high="darkgreen", guide="colorbar")
    
    #ggplotly(ss)
    
  )
  }) #observe event
}