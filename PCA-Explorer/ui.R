library("BiocManager")
options(repos = BiocManager::repositories())
getOption("repos")
library(shiny)
library(plotly)
library(DT)
library(dplyr)
options(shiny.maxRequestSize = 10000*1024^2)



# Define UI
ui <- shinyUI(navbarPage(title = "SARTools - Explorer",
                         
                         tabPanel(title = "Main",
                                  sidebarLayout(
                                      sidebarPanel(
                                          fileInput("file", label = ""),
                                          actionButton(inputId="run","RUN"),
                                          # plotOutput("hist"), 
                                          width = 3), 
                                      
                                      mainPanel(
                                          DT::dataTableOutput("mytable1")
                                      )
                                  )
                         ),
                         
                         tabPanel(title = "Clust", plotOutput(outputId = "clust", width = 1080, height = 1080)),
                         
                         tabPanel("3D-PCA", 
                                  sidebarLayout(
                                      sidebarPanel(
                                          uiOutput("groups"),width = 3), 
                                      
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
                         )
                         
                         
                         
                         
)
)

