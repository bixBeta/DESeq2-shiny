library("BiocManager")
options(repos = BiocManager::repositories())
getOption("repos")
library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(shinyjs)
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
                                      width = 4), 
                                    
                                    mainPanel(
                                      
                                      DT::dataTableOutput("mytable1"),
                                      DT::dataTableOutput("mytable2")
                                      
                                    )
                                  )
                         ),
                         
                         tabPanel(title = "Clust", plotOutput(outputId = "clust", width = 1080, height = 1080)),
                         
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
                                      plotlyOutput("plot2"))
                                  )
                         ),
                         
                         shinyjs::useShinyjs(),
                         hr(),
                         div(class = "footer",
                             includeHTML("footer.html")
                         )
                         
)
)

