library("BiocManager")
options(repos = BiocManager::repositories())
getOption("repos")
library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(shinyjs)
library(tibble)
library(DESeq2)
options(shiny.maxRequestSize = 10000*1024^2)



# Define UI
ui <- shinyUI(navbarPage(title = "DESeq2-Shiny",
                         
                         
                         tabPanel(title = "Upload",
                                  sidebarLayout(
                                    sidebarPanel(
                                      fileInput("file", label = "Count Matrix (tab separated .txt file)"),
                                      fileInput("file2", label = "MetaData target file (tab separated .txt file)"),
                                      actionButton(inputId="run","RUN"),
                                      hr(),
                                      textOutput("log"),
                                      
                                      # plotOutput("hist"), 
                                      width = 3), 
                                    
                                    mainPanel(
                                      
                                      DT::dataTableOutput("mytable1"),
                                      DT::dataTableOutput("mytable2"), 
                                      width = 9
                                      
                                    )
                                  )
                         ),
                         
                         tabPanel(title = "Clust", plotOutput(outputId = "clust", width = 800, height = 600), ),
                         
                         tabPanel("3D-PCA", 
                                  sidebarLayout(
                                    sidebarPanel(
                                      uiOutput("groups"),width = 3, height =3), 
                                    
                                    mainPanel(
                                      plotlyOutput("plot"))
                                  )
                         ),
                         
                         tabPanel("2D-PCA", 
                                  sidebarLayout(
                                    sidebarPanel(
                                      uiOutput("groups2"),width = 4,
                                      plotOutput(outputId = "scree"),
                                      downloadButton("downloadData", "Download Eigenvals"),
                                      
                                      verbatimTextOutput("prop")), 
                                    
                                    mainPanel(
                                      plotlyOutput("plot2"), width = 6)
                                  )
                         ),
                         
                         tabPanel(title = "DGE",
                                  
                                  sidebarLayout(fluid = F,
                                    sidebarPanel(
                                      
                                      uiOutput("numerator"),
                                      uiOutput("denominator"), 
                                      plotOutput(outputId = "MAPlot"),
                                      downloadButton("normcounts", "Download Normalized Counts"),
                                      width = 4
                                    ),
                                    
                                    mainPanel(
                                      #plotOutput("group4")
                                      DT::dataTableOutput("contrast"),
                                      downloadButton("results", "Download DE results"),
                                      verbatimTextOutput("spit"), width = 7
                                    )
                                  )
                                  
                                  
                                  
                                  
                                  
                                  
                         ),

                         shinyjs::useShinyjs(),
                         div(class = "footer",
                             includeHTML("footer.html")
                         )
                         
)
)

