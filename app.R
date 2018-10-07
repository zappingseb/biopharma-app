library(ape)
library(magrittr)
library(igraph)
library(viridisLite)
library(MASS)

source("./R/utils.R")

# -------- Inputs for Clustering
ui <- fluidPage(
		
		HTML("<div>Test</div>"),
		
		# Generate a row with a sidebar
		sidebarLayout(      
				
				# Define the sidebar with one custom input
				sidebarPanel(
						sliderInput("patients","Nr of patients",10,100,50),
						sliderInput("nc","Nr of clusters",1,10,2),
						uiOutput('Variables'),
						selectInput("method","Clustering Method",choices=c(
										"ward.D2"="ward.D2",
										"complete"="complete",
										"ward.D"="ward.D",
										"centroid"="centroid"
										))
				),
				
				mainPanel(
						fluidRow(
								column(6,plotOutput(outputId = "plot1")),
								column(6,plotOutput(outputId = "plot2"))
								)
				)
		
		)
)

server <- function(input, output, session) {
		# -------- Derive the data ------------
		data(biopsy)

		biopsy_numeric <- reactive({
			biopsy_numeric <- biopsy[,c(-1,-11)]
			print(input$vars)
				columns <- input$vars
			biopsy_numeric <- biopsy_numeric[c(1:input$patients),columns]
			biopsy_numeric[is.na(biopsy_numeric)] <- 
					round(mean(apply(biopsy_numeric,1,mean,na.rm=T),na.rm=T))
			return(biopsy_numeric)
		}

		
		
		)

	output$Variables <- renderUI({
							selectInput(inputId="vars", 
									label = "Variables to use", 
									choices = names(biopsy)[c(-1,-11)],
									multiple = TRUE,
									selected = names(biopsy)[c(-1,-11)]
									)
			})
	
	output$plot1 <- renderPlot({
			my_hclust <- function(...){
				hclust(method=my_method,...)
			}
			my_method <<- input$method
			heatmap(x = t(as.matrix(biopsy_numeric())), Rowv=NA, hclustfun=my_hclust,
					labCol = paste(biopsy[,1],biopsy[,11]),
					col=viridis(15)
			)
		})

  output$plot2 <- renderPlot({
	  phyltree( x = biopsy_numeric(),
			  method = input$method,
			  nc = input$nc,
			  labels = as.character(biopsy[1:input$patients,11]))
  })

}
shinyApp(ui, server)

