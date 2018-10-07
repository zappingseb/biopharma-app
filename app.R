
library(shiny)
library(viridisLite)

source("./R/utils.R")

# -------- Inputs for Clustering
ui <- fluidPage(
  
  # Setup
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  HTML(author_notes()),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one custom input
    sidebarPanel(
      # Slider to choose # of patients
      sliderInput("patients","Nr of patients",10,100,50),
      # Slider to choos # clusters
      sliderInput("nc","Nr of clusters",1,10,2),
      
      # Selectinput to choose the variables / measurements
      uiOutput('variables'),
      
      # SelectInput to choose the clustering method
      selectInput("method","Clustering Method",choices=c(
        "ward.D2"="ward.D2",
        "complete"="complete",
        "ward.D"="ward.D",
        "centroid"="centroid"
      ))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 fluidRow(
                   
                   # A 1D Clustering heatmap clustering the
                   # patients
                   column(6,
                          
                          tags$h2("Heatmap of 1D clustering of patients"),
                          plotOutput(outputId = "plot1"),
                          tags$legend("The patients are clustered on the
                                      X-axis, the Y-Axis shows variables
                                      used for clustering.
                                      The color gives the value
                                      of each variable
                                      ")
                   ),
                   
                   # A Phylogenetic tree plot
                   # showing the relationships of the patients
                   # labeling shall be the "disease status"
                   column(6,
                          tags$h2("Phylogenetic tree of the patients"),
                          plotOutput(outputId = "plot2"),
                          tags$legend("A phylogenetic tree of the patients.
                                      Colors of the large circles show the clustering, colors of single circles
                                      show the 'disease status'. Labels also show the 'disease status'.
                                      ")
                   )#column
                 )#fluidRow
              ),#tabPanel,
              tabPanel("Table",
                       
                       # A table showing the clustering of the patients 
                       # Columns: ClusterID, PatientID, Disease status
                       tableOutput("cluster_table")
                       
              )
    
  )
)

)
)

server <- function(input, output, session) {
  
  # Cleaning the data to be clustered
  biopsy_numeric <- reactive({
    # Remove ID and disease status
    biopsy_numeric <- biopsy[,c(-1,-11)]
    
    # Choose columns by the "vars" input
    columns <- input$vars
    
    biopsy_numeric <- biopsy_numeric[c(1:input$patients),columns]
    
    # Overwrite NA values by the rowwise mean
    biopsy_numeric[is.na(biopsy_numeric)] <- 
      round(mean(apply(biopsy_numeric,1,mean,na.rm=T),na.rm=T))
    return(biopsy_numeric)
  }
  
  )
  
  #
  #
  #----------- Tutorial Part ---------------
  # 
  #
  
  # Provide a selectInput field with:
  #
  # the ID "vars"
  # the label as you wish
  # the choices being made by the columns of the
  # biopsy table exectp ID and disease status
  # The selections as the same as the choices
  # allowing multiple choices
  output$variables <- renderUI({
   NULL
  })
  
  # Provide a functionality that creates a provides a heatmap
  # with 
  # the "biopsy_numeric" data as input
  # the input$method as the clustering method
  # A color scale out of the viridisLite package
  # 1D clustering (just columns)
  # Column labels as disease status
  output$plot1 <- renderPlot({
    NULL
  })
  
  # Provide a plot of the phylogenetic tree using our
  # function 'phyltree'
  
  # with biopsy_numeric as the data
  # the method as the input method
  # viridis as the color palette
  # the disease status as the labels.
  # The cuttree level as input$nc
  output$plot2 <- renderPlot({
   NULL
  })
  
  # Provide a table that has three columns
  # 1) Clustering result using the hclust function
  #    with the input$method and the cuttree function
  #    with the input$nc
  # 2) The patient ID
  # 3) The disease status of the patient
  # Order the table by ClusterID
  output$cluster_table <- renderTable({
    
   NULL
    })
  
}
shinyApp(ui, server)

