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
                                      actionButton(inputId="example","RUN with Example Data Set "),
                                      hr(),
                                      textOutput("log"),
                                      
                                      # plotOutput("hist"), 
                                      width = 3), 
                                    
                                    mainPanel(
                                      
                                      DT::dataTableOutput("mytable2"),
                                      hr(),
                                      DT::dataTableOutput("mytable1"), 
                                      width = 9
                                      
                                    )
                                  )
                         ),
                         
                         tabPanel(title = "Clust", plotOutput(outputId = "clust", width = 800, height = 600)),
                         
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
                                      uiOutput("xaxis"),
                                      uiOutput("yaxis"),
                                      plotlyOutput(outputId = "scree"),
                                      hr(),
                                      downloadButton("downloadData", "Download Eigenvals"),
                                      hr(),
                                      verbatimTextOutput("prop")), 
                                    
                                    mainPanel(
                                      plotlyOutput("plot2"), width = 8)
                                  )
                         ),
                         
                         tabPanel(title = "DGE",
                                  
                                  sidebarLayout(fluid = F,
                                                sidebarPanel(
                                                  
                                                  uiOutput("numerator"),
                                                  uiOutput("denominator"), 
                                                  plotOutput(outputId = "MAPlot"),
                                                  hr(),
                                                  # downloadButton("normcounts", "Download Normalized Counts"),
                                                  downloadButton("results", "Download DE results"),
                                                  width = 4
                                                ),
                                                
                                                mainPanel(
                                                  #plotOutput("group4")
                                                  DT::dataTableOutput("contrast"),
                                                  hr(),
                                                  
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

