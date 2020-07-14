# Sample Materials, provided under license.
# Licensed Materials - Property of IBM
# © Copyright IBM Corp. 2019, 2020. All Rights Reserved.
# US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.

# Load shap JavaScript
shapjs <- content(GET("https://github.com/slundberg/shap/raw/0849aa20551cf9825f9e294fcc29d7fbe7b9f932/shap/plots/resources/bundle.js"))

library(DT)

clientPanel <- function() {
  # Put diagnosis code into a vector
  cho=c(unique(AllPatients$PRINC_DIAG_DESC))
  
  # creates a tab
  tabPanel(
    "Hospital",
    value = "clientPanel",
    
    panel(
      # create a fluid row for the diagnosis code dropdown
      fluidRow(
        column(4,
        # diagnosis codes as Drop down inputs 
        div(style="float:right",selectInput("dropInput", "Diagnosis Code",
                           choices = cho,
                           selected = cho[1])) 

        
        )),
      # Fluid row for the map and a plot
        fluidRow(
        column(6, 
               
               # header for the map
               h4("Hospital Distance", class="text-center"),
               #leaflet plot for the map
               leafletOutput("HospLocPlot", width = "700px", height = "480px")
        ),
        column(5,
               
               h4("Source of payment",class="text-center"),
               hr(),
               plotlyOutput("severity_distance",width = "700px", height = "400px")
               
               ))


    ,
    
    panel(
      
      br(),
      fluidRow(

        column(3,
               h4("Number of patients",class="text-center"),
               hr(),

               plotlyOutput("nearest")
               
        ),
        column(3,
               h4("Average Distance travelled",class="text-center"),
               hr(),

               plotlyOutput("dist")
               
               
        ),
        column(6,
               #header for the plot3
               h4("Patients travelled to other hospitals"),
               hr(),
               DT::dataTableOutput("compete",width = "500px", height = "280px")
        )
        
        )
        )
      )
      )

  }






# Reactive server variables store (pervades across all sessions)
#serverVariables = reactiveValues(deployments = list())

clientServer <- function(input, output, session, sessionVars) {
  
  observe({
    
    client <- clients[[toString(sessionVars$selectedClientId)]]
    
    # Load customer data for customer sessionVars$selectedClientId
    selection <- AllPatients[AllPatients$PROVIDER_ID == sessionVars$selectedClientId,]#[1,]
 
    hospital_detail <- hospitals[hospitals$PROVIDER_ID == sessionVars$selectedClientId,]
    
    hospital_name <- hospital_detail$PROVIDER_NAME
    
    ##
    selection <- selection[selection$PRINC_DIAG_DESC==input$dropInput,]
    
    ############# Plot 1 #############  
    map_title <- c(hospital_name)
    my_title <- tags$p(tags$style("p {color: blue; font-size:23px}"),
                       tags$b(map_title))
    
    icons <- awesomeIcons(
      icon = 'hospital-o',
      iconColor = 'red',
      library = 'fa',
      markerColor = "green"
    )
    icon2 <- makeIcon(
      iconUrl = "https://img.icons8.com/flat_round/64/000000/home.png",
      iconWidth = 24, iconHeight = 24)
    
    HospLocPlot<-leaflet() %>%
      addTiles( group = "(default)") %>%  # Add default OpenStreetMap map tiles
      addAwesomeMarkers(lng=selection$PVDR_LONG, lat=selection$PVDR_LAT, popup = paste("Hospital ID:", selection$PROVIDER_ID, "<br>","Hospital Name:", selection$PROVIDER_NAME),icon=icons, group="Hospitals")  %>%
      addMarkers(lng=selection$PAT_LONG, lat=selection$PAT_LAT,popup = paste("PATIENTS DIAGNOSIS: ",selection$PRINC_DIAG_DESC,"<br>","Patient zipcode:", selection$PAT_ZIP, "<br>","Hospital Visited:", selection$PROVIDER_NAME, "<br>","Nearest Hospital:", selection$NEAREST_HOSPITAL, "<br>","Distance Travelled:", round(selection$DISTANCE_TO_HOSPITAL,2),"m"), icon=icon2, group="Patients") %>%
      addControl(my_title, className="map-title", position = "bottomleft")
    
    ## Render plot 1
    output$HospLocPlot <- renderLeaflet(HospLocPlot)
    
    
    
    ############# Plot 4 #############  
    charges_length <- selection%>% group_by(PRINC_DIAG_DESC,YEAR) %>% summarise(
      
      AVG_CHARGES_PER_DAY = round(mean(CHARGES_PER_DAY),2),
      AVG_LENGTH_OF_STAY = round(mean(LENGTH_OF_STAY),2),
      AVG_Distance_Travelled =round(mean(DISTANCE_TO_HOSPITAL),1),
      Num_OF_PATIENTS =n()
    )


    
    observeEvent( input$dropInput,{
      charges_length <- charges_length[charges_length$PRINC_DIAG_DESC==input$dropInput,]
      output$nearest= renderPlotly({
      
      phe<-plot_ly(charges_length, x = ~YEAR, y = ~Num_OF_PATIENTS, type = 'bar',text = ~Num_OF_PATIENTS, name = 'Number of Patients') %>%
        layout( barmode = 'group',yaxis = list(title = 'Number of Patients'))
      phe$elementId=NULL
      phe
      })
      
      output$dist =renderPlotly({
        
        distance<-plot_ly(charges_length, x = ~YEAR, y = ~AVG_Distance_Travelled, type = 'bar', name = 'Average Distance Travelled',text =~AVG_Distance_Travelled,marker = list(color = 'rgb(222,40,125)'))%>%
          layout( barmode = 'group',yaxis = list(title = 'Average Distance'))

        distance$elementId=NULL
        distance
      })
      

        
       }
    )
    
    
    ############# Plot 3 #############
    AllPatients2=AllPatients[AllPatients$NEAREST_HOSPITAL==hospital_name & AllPatients$PRINC_DIAG_DESC==input$dropInput,]
    compete_plt=AllPatients2[AllPatients2$PROVIDER_NAME!=hospital_name,]%>% 
      group_by(YEAR,PROVIDER_NAME) %>% summarise(
        
        PATIENT_COUNT=n(),
        AVG_TRADEOFF_DISTANCE=round(mean(TRADEOFF),1)
        
        
      )
    #compete_plt <- compete_plt[, c(2, 1, 3)]
    

    
    names(compete_plt) <- c("YEAR","Hospital Patient Visited","Number of Patients","Average Tradeoff Distance(m)")
    
    
    
    output$compete <- DT::renderDataTable({
      compete_plt2 <- datatable(compete_plt,options = list(autoWidth = FALSE)
      )

      
      
      
      compete_plt2
      
    },server = FALSE)
    
    selections=selection[!(is.na(selection$PVDR_LAT) | selection$PVDR_LAT==""),]
    
    ############# Plot 2 #############  

    
    # Plot2
    src_payment <- selections%>% group_by(FIRST_PAYMENT_SRC) %>% summarise(
      COUNT_PAT = n()
    )
    
    output$severity_distance= renderPlotly({
      
      plot_sev <- plot_ly(src_payment, labels = ~FIRST_PAYMENT_SRC, values = ~COUNT_PAT, type = 'pie') %>%
        layout(  margin=list(r=0),showlegend = T,legend = list(orientation="h", xanchor = "right",  # use center of legend as anchor
                                                                                            x = 1),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      plot_sev$elementId=NULL
      plot_sev
      
    })
    
  })
}

  
  


