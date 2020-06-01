# Define server logic
server <- shinyServer(function(input, output, session) {
  
  
  observeEvent(input$run,{
    
    
    req(input$file)
    
    
    counts <- read.table(input$file$datapath,
                         header = T,
                         sep = "\t",
                         row.names = 1)
    counts = counts[ , order(names(counts))]
    
    req(input$file2)
    
    
    renderUI(actionButton("rmv", "x"),)
    
    target <- read.table(input$file2$datapath,
                         header = T,
                         sep = "\t")
    
    rownames(target) <- target$label
    
    target = target[order(rownames(target)), ]
    
    
    
    # load the file into new environment and get it from there
    e = new.env()
    
    progress <- Progress$new(session, min=1, max=20)
    on.exit(progress$close())
    
    progress$set(message = 'Execution in progress',
                 detail = 'This may take a while...')
    for (i in 1:4) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    
    # Plot the data
    #target2 <- target %>% select(label, group)
    output$mytable1 <- DT::renderDataTable({
      DT::datatable(target, list(pageLength = 5, scrollX=T),rownames = F)
    })
    output$mytable2 <- DT::renderDataTable({
      DT::datatable(counts, list(pageLength = 5, scrollX=T))
    })
    
    output$prop <- renderPrint(summary(pca))
    
    
    
    # ################################
    # ################################
    # # DESEQ2
    #
    library("DESeq2")
    library("dplyr")
    library("tidyverse")
    library("plotly")
    #
    
    ## -------------------------------------------------------------------------------------------------------------------
    dds <- DESeqDataSetFromMatrix(countData = counts,
                                  colData = target,
                                  design = ~ group)
    
    #
    #
    #
    # ## -------------------------------------------------------------------------------------------------------------------
    for (i in 4:12) {
      progress$set(value = i)
      Sys.sleep(0.1)
    }
    
    
    withCallingHandlers({
      shinyjs::html("log", "DESeq2 Log: ")
      dds <- DESeq(dds)
    },
    message = function(m) {
      shinyjs::html(id = "log", html = m$message, add = TRUE)
      shinyjs::html(id = "log", html = "<br>", add = TRUE)
    }, collapse="<br>")
    
    
    
    #resultsNames(dds)
    #
    # ## -------------------------------------------------------------------------------------------------------------------
    vsd <- varianceStabilizingTransformation(dds, blind=T)
    
    for (i in 13:17) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    #
    # ## -------------------------------------------------------------------------------------------------------------------
    # calculate the variance for each gene
    rv <- rowVars(assay(vsd))
    
    # select the ntop genes by variance
    select <- order(rv, decreasing=TRUE)[seq_len(min(500, length(rv)))]
    
    # perform a PCA on the data in assay(x) for the selected genes
    pca <- prcomp(t(assay(vsd)[select,]))
    
    # the contribution to the total variance for each component
    percentVar <- pca$sdev^2 / sum( pca$sdev^2 )
    pVar.df <- as.data.frame(percentVar)
    pVar.df$x = as.factor(paste0("PC",rownames(pVar.df)))
    
    pVar.df = pVar.df[ , order(names(pVar.df))]
    pVar.df$percentVar = pVar.df$percentVar * 100
    pVar.df$percentVar = round(pVar.df$percentVar, digits = 2)
    
    group = target$group
    intgroup.df <- as.data.frame(colData(vsd)[, "group", drop=FALSE])
    
    # assembly the data for the plot
    
    
    d <- data.frame(pca$x, name=rownames(pca$x))
    d2 <- left_join(target, d, by= c("label"="name"))
    
    ### -------------------------------------------------------------------------------------------------------------------
    
    hc2 <- hclust(dist(t(assay(vsd))), method="ward.D")
    
    output$clust <- renderPlot(plot(hc2, hang=-1, ylab="Height", las=2,
                                    xlab="Method: Euclidean distance - Ward criterion",
                                    main="Cluster Dendrogram"))
    
    
    output$groups <- renderUI({
      
      
      tagList(
        selectInput(inputId = "choice", label = "Select Groupings",
                    choices = colnames(d2)[colnames(d2) %in% colnames(target)], selected = "group")
      )
      
      
      
    })
    
    output$groups2 <- renderUI({
      
      
      tagList(
        selectInput(inputId = "choice2", label = "Select Groupings",
                    choices = colnames(d2)[colnames(d2) %in% colnames(target)], selected = "group")
      )
      
      
      
    })
    
    
    output$xaxis <- renderUI({
      
      
      tagList(
        selectInput(inputId = "pcx", label = "Select x-axis PC",
                    choices = colnames(d2)[grep(pattern = "PC", colnames(d2))], selected = "PC1")
      )
      
      
      
    })
    
    
    output$yaxis <- renderUI({
      
      
      tagList(
        selectInput(inputId = "pcy", label = "Select y-axis PC",
                    choices = colnames(d2)[grep(pattern = "PC", colnames(d2))], selected = "PC2")
      )
      
      
      
    })
    
    
    
    m <- list(
      l = 50,
      r = 50,
      b = 100,
      t = 100,
      pad = 4
    )
    output$plot <- renderPlotly({
      
      if(is.null(input$choice)){return()}
      plot_ly(data = d2, x = ~ PC1, y= ~ PC2, z = ~ PC3, 
              width = 800, height = 800,
              color = ~ get(input$choice),
              #colors = color2,
              marker = list(size = 8,
                            line = list(color = ~ label , width = 1))) %>%
        add_markers() %>%
        layout(autosize = F, margin =m, title = "First 3 Principal Components",
               scene = list(xaxis = list(title = 'PC1'),
                            yaxis = list(title = 'PC2'),
                            zaxis = list(title = 'PC3')
               )
        )
    })
    
    
    
    x = reactive({input$pcx})
    y = reactive({input$pcy})
    
    
    plotdata <- reactive({
      
      dplyr::select(d2, !contains("PC"))
      
    })
    
    
    
    
    output$plot2 <- renderPlotly({
      
      if(is.null(input$choice2)){return()}
      
      d3   <- plotdata()
      d3$x <- d2[[input$pcx]]
      d3$y <- d2[[input$pcy]]
      
      plot_ly(data = d3 , x = ~ x, y =  ~ y, 
              width = 800, height = 600,
              color = ~ get(input$choice2),
              marker = list(size = 12,
                            line = list(color = ~ label, width = 1))) %>%
        add_markers() %>%
        layout(autosize = F, title = paste0(input$pcy, " vs ", input$pcx),
               xaxis = list(title = input$pcx),
               yaxis = list(title = input$pcy))
      
    })
    
    output$scree <- renderPlotly(
      
      plot_ly(data = pVar.df, x = ~x, y = ~ percentVar, type = "scatter", mode = "markers",
              width = 400, height = 400) %>%
        layout(autosize = F, margin = m,
               xaxis = list(categoryorder = "array",title = "nth PC", categoryarray = ~x),
               yaxis = list(title = "Percent Var", ticksuffix = "%"),
               title = "elbow plot")
    )
    
    
    
    
    for (i in 18:20) {
      progress$set(value = i)
      Sys.sleep(0.1)
    }
    
    
    
    # save eigenvals to a new object 
    datasetInput <- pca$x
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("eigenvalues.csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput, file, row.names = T)
      }
    )
    
    
    
    
    #----------------------------------------------------------
    #----------------------------------------------------------
    #----------------------------------------------------------
    #-- Differential Expression Analysis
    
    # Make Normalized Counts Downloadable
    
    normalized_counts <- counts(dds, normalized=TRUE)
    colnames(normalized_counts) = paste0("norm.", colnames(normalized_counts))
    normalized_counts <- round(normalized_counts, digits = 0)
    # Downloadable csv of normalized counts dataset ----
    
    output$normcounts <- downloadHandler(
      filename = function() {
        paste("normalizedCounts.csv", sep = "")
      },
      content = function(file) {
        write.csv(normalized_counts, file, row.names = T, quote = F, col.names = NA)
      }
    )
    
    output$numerator <- renderUI({
      
      
      tagList(
        selectInput(inputId = "choice4", label = "Select Group of interest for DE",
                    choices = dds$group)
      )
    })
    
    output$denominator <- renderUI({
      
      
      tagList(
        selectInput(inputId = "choice3", label = "Select Base-level for DE",
                    choices = dds$group)
      )
    })
    
    
    
    dds.results <- reactive({
      
      req(input$choice3)
      req(input$choice4)
      
      results(dds, contrast=c("group", as.character(input$choice4), 
                              as.character(input$choice3)), alpha = 0.05)
      
    })        
    
    de.data <- reactive({
      
      req(input$choice3)
      req(input$choice4)
      round(as.data.frame(dds.results()),3)
      
      
    })
    
    nc <- as.data.frame(normalized_counts)
    nc$features <- rownames(nc)
    
    de.merge <- reactive({
      
      dds.nc <- de.data()
      dds.nc$features <- rownames(dds.nc)
      nc2 <- left_join(nc, dds.nc, by = "features")
      nc2 <- nc2 %>% 
        select("features", everything())
    })
    
    
    output$contrast <- DT::renderDataTable({
      
      DT::datatable(de.merge(), list(pageLength = 10, scrollX=T), rownames = F)
      
    })
    
    ma.data <- reactive({
      
      req(input$choice3)
      req(input$choice4)
      plotMA(dds.results(), main = paste0("MAPlot ", input$choice4, "_vs_", input$choice3)) 
      
    })
    
    output$MAPlot <-
      renderPlot(ma.data(), width = 400, height = 400)
    
    
    
    output$results <- downloadHandler(
      filename = function() {
        paste0(input$choice4, "_vs_", input$choice3, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(as.data.frame(de.merge()), file, row.names = T, quote = F, col.names = NA)
      }
    )
    
    
    
    
    output$spit <-
      
      renderPrint(summary(dds.results()))
    
  })
  
  
  
  
  #------ Example Data Set ------------
  
  observeEvent(input$example,{
    
    
    #req(input$file)
    
    counts <- read.table("example/countMatrix.txt",
                         header = T,
                         sep = "\t",
                         row.names = 1)
    counts = counts[ , order(names(counts))]
    
    #req(input$file2)
    
    renderUI(actionButton("rmv", "x"),)
    
    target <- read.table("example/targetFile.txt",
                         header = T,
                         sep = "\t")
    
    rownames(target) <- target$label
    
    target = target[order(rownames(target)), ]
    
    
    
    # load the file into new environment and get it from there
    e = new.env()
    
    progress <- Progress$new(session, min=1, max=20)
    on.exit(progress$close())
    
    progress$set(message = 'Execution in progress',
                 detail = 'This may take a while...')
    for (i in 1:4) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    
    # Plot the data
    #target2 <- target %>% select(label, group)
    output$mytable1 <- DT::renderDataTable({
      DT::datatable(target, list(pageLength = 5, scrollX=T), rownames = F)
    })
    output$mytable2 <- DT::renderDataTable({
      DT::datatable(counts, list(pageLength = 5, scrollX=T))
    })
    
    output$prop <- renderPrint(summary(pca))
    
    
    
    # ################################
    # ################################
    # # DESEQ2
    #
    library("DESeq2")
    library("dplyr")
    library("tidyverse")
    library("plotly")
    #
    
    ## -------------------------------------------------------------------------------------------------------------------
    dds <- DESeqDataSetFromMatrix(countData = counts,
                                  colData = target,
                                  design = ~ group)
    
    #
    #
    #
    # ## -------------------------------------------------------------------------------------------------------------------
    for (i in 4:12) {
      progress$set(value = i)
      Sys.sleep(0.1)
    }
    
    
    withCallingHandlers({
      shinyjs::html("log", "DESeq2 Log: ")
      dds <- DESeq(dds)
    },
    message = function(m) {
      shinyjs::html(id = "log", html = m$message, add = TRUE)
      shinyjs::html(id = "log", html = "<br>", add = TRUE)
    }, collapse="<br>")
    
    
    
    #resultsNames(dds)
    #
    # ## -------------------------------------------------------------------------------------------------------------------
    vsd <- varianceStabilizingTransformation(dds, blind=T)
    
    for (i in 13:17) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    #
    # ## -------------------------------------------------------------------------------------------------------------------
    # calculate the variance for each gene
    rv <- rowVars(assay(vsd))
    
    # select the ntop genes by variance
    select <- order(rv, decreasing=TRUE)[seq_len(min(500, length(rv)))]
    
    # perform a PCA on the data in assay(x) for the selected genes
    pca <- prcomp(t(assay(vsd)[select,]))
    
    # the contribution to the total variance for each component
    percentVar <- pca$sdev^2 / sum( pca$sdev^2 )
    pVar.df <- as.data.frame(percentVar)
    pVar.df$x = as.factor(paste0("PC",rownames(pVar.df)))
    
    pVar.df = pVar.df[ , order(names(pVar.df))]
    pVar.df$percentVar = pVar.df$percentVar * 100
    pVar.df$percentVar = round(pVar.df$percentVar, digits = 2)
    group = target$group
    intgroup.df <- as.data.frame(colData(vsd)[, "group", drop=FALSE])
    
    # assembly the data for the plot
    
    #d <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2], PC3=pca$x[,3], name=colnames(vsd))
    
    d <- data.frame(pca$x, name=rownames(pca$x))
    
    d2 <- left_join(target, d, by= c("label"="name"))
    
    ### -------------------------------------------------------------------------------------------------------------------
    
    hc2 <- hclust(dist(t(assay(vsd))), method="ward.D")
    
    output$clust <- renderPlot(plot(hc2, hang=-1, ylab="Height", las=2,
                                    xlab="Method: Euclidean distance - Ward criterion",
                                    main="Cluster Dendrogram"))
    
    output$groups <- renderUI({
      
      
      tagList(
        selectInput(inputId = "choice", label = "Select Groupings",
                    choices = colnames(d2)[colnames(d2) %in% colnames(target)], selected = "group")
      )
      
      
      
    })
    
    output$groups2 <- renderUI({
      
      
      tagList(
        selectInput(inputId = "choice2", label = "Select Groupings",
                    choices = colnames(d2)[colnames(d2) %in% colnames(target)], selected = "group")
      )
      
      
      
    })
    
    
    output$xaxis <- renderUI({
      
      
      tagList(
        selectInput(inputId = "pcx", label = "Select x-axis PC",
                    choices = colnames(d2)[grep(pattern = "PC", colnames(d2))], selected = "PC1")
      )
      
      
      
    })
    
    
    output$yaxis <- renderUI({
      
      
      tagList(
        selectInput(inputId = "pcy", label = "Select y-axis PC",
                    choices = colnames(d2)[grep(pattern = "PC", colnames(d2))], selected = "PC2")
      )
      
      
      
    })
    
    
    m <- list(
      l = 50,
      r = 50,
      b = 100,
      t = 100,
      pad = 4
    )
    
    output$plot <- renderPlotly({
      
      if(is.null(input$choice)){return()}
      plot_ly(data = d2, x = ~ PC1, y= ~ PC2, z = ~ PC3, 
              width = 800, height = 800,
              color = ~ get(input$choice),
              #colors = color2,
              marker = list(size = 8,
                            line = list(color = ~ label , width = 1))) %>%
        add_markers() %>%
        layout(autosize = F, margin =m, title = "First 3 PC Dimensions",
               scene = list(xaxis = list(title = 'PC1'),
                            yaxis = list(title = 'PC2'),
                            zaxis = list(title = 'PC3')
               )
        )
    })
    
    
    x = reactive({input$pcx})
    y = reactive({input$pcy})
    
    
    plotdata <- reactive({
      
      dplyr::select(d2, !contains("PC"))
      
    })
    
    
    
    
    output$plot2 <- renderPlotly({
      
      if(is.null(input$choice2)){return()}
      
      d3   <- plotdata()
      d3$x <- d2[[input$pcx]]
      d3$y <- d2[[input$pcy]]
      
      plot_ly(data = d3 , x = ~ x, y =  ~ y, 
              width = 800, height = 600,
              color = ~ get(input$choice2),
              marker = list(size = 10,
                            line = list(color = ~ label, width = 1))) %>%
        add_markers() %>%
        layout(autosize = F, title = paste0(input$pcy, " vs ", input$pcx),
               xaxis = list(title = input$pcx),
               yaxis = list(title = input$pcy))
      
    })
    
    
    m2<- list(
      l = 70,
      r = 30,
      b = 100,
      t = 100,
      pad = 2
    )
    
    
    output$scree <- renderPlotly(
      
      plot_ly(data = pVar.df, x = ~x, y = ~ percentVar, type = "scatter", mode = "markers",
              width = 400, height = 400) %>%
        layout(autosize = F, margin = m2,
               xaxis = list(categoryorder = "array",title = "nth PC", categoryarray = ~x),
               yaxis = list(title = "Percent Var", ticksuffix = "%"),
               title = "elbow plot")
    )
    
    
    for (i in 18:20) {
      progress$set(value = i)
      Sys.sleep(0.1)
    }
    
    
    
    # save eigenvals to a new object 
    datasetInput <- pca$x
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("eigenvalues.csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput, file, row.names = T)
      }
    )
    
    
    
    
    #----------------------------------------------------------
    #----------------------------------------------------------
    #----------------------------------------------------------
    #-- Differential Expression Analysis
    
    # Make Normalized Counts Downloadable
    
    normalized_counts <- counts(dds, normalized=TRUE)
    colnames(normalized_counts) = paste0("norm.", colnames(normalized_counts))
    normalized_counts <- round(normalized_counts, digits = 0)
    
    # Downloadable csv of normalized counts dataset ----
    
    # output$normcounts <- downloadHandler(
    #     filename = function() {
    #         paste("normalizedCounts.csv", sep = "")
    #     },
    #     content = function(file) {
    #         write.csv(normalized_counts, file, row.names = T, quote = F, col.names = NA)
    #     }
    # )
    
    output$numerator <- renderUI({
      
      
      tagList(
        selectInput(inputId = "choice4", label = "Select Group of interest for DE",
                    choices = dds$group)
      )
    })
    
    output$denominator <- renderUI({
      
      
      tagList(
        selectInput(inputId = "choice3", label = "Select Base-level for DE",
                    choices = dds$group)
      )
    })
    
    
    
    dds.results <- reactive({
      
      req(input$choice3)
      req(input$choice4)
      
      results(dds, contrast=c("group", as.character(input$choice4), 
                              as.character(input$choice3)), alpha = 0.05)
      
    })        
    
    de.data <- reactive({
      
      req(input$choice3)
      req(input$choice4)
      round(as.data.frame(dds.results()),3) 
      
      
      
    })
    
    nc <- as.data.frame(normalized_counts)
    nc$features <- rownames(nc)
    
    de.merge <- reactive({
      
      dds.nc <- de.data()
      dds.nc$features <- rownames(dds.nc)
      nc2 <- left_join(nc, dds.nc, by = "features")
      nc2 <- nc2 %>% 
        select("features", everything())
    })
    
    
    output$contrast <- DT::renderDataTable({
      
      DT::datatable(de.merge(), list(pageLength = 10, scrollX=T), rownames = F)
      
    })
    
    
    ma.data <- reactive({
      
      req(input$choice3)
      req(input$choice4)
      plotMA(dds.results(), main = paste0("MAPlot ", input$choice4, "_vs_", input$choice3)) 
      
    })
    
    output$MAPlot <-
      renderPlot(ma.data(), width = 400, height = 400)
    
    
    
    output$results <- downloadHandler(
      filename = function() {
        paste0(input$choice4, "_vs_", input$choice3, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(as.data.frame(de.merge()), file, row.names = T, quote = F, col.names = NA)
      }
    )
    
    
    
    
    output$spit <-
      
      renderPrint(summary(dds.results()))
    
    
    
    
    
  })
  
  
  
  
  
  
  
})