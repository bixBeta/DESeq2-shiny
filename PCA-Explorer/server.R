# Define server logic
server <- shinyServer(function(input, output, session) {
    
    
    observeEvent(input$run,{
        if ( is.null(input$file)) return(NULL)
        inFile <- input$file
        file <- inFile$datapath
        # load the file into new environment and get it from there
        e = new.env()
        name <- load(file, envir = .GlobalEnv)
        
        progress <- Progress$new(session, min=1, max=20)
        on.exit(progress$close())
        
        progress$set(message = 'Execution in progress',
                     detail = 'This may take a while...')
        for (i in 1:4) {
            progress$set(value = i)
            Sys.sleep(0.5)
        }
        
        # Plot the data
        target2 <- target %>% select(label, group)
        output$mytable1 <- DT::renderDataTable({
            DT::datatable(target2)
        })
        
        output$prop <- renderPrint(summary(pca))
        
        
        # output$groups <- renderUI({
        #   tagList(
        #     selectInput(inputId = "choice", label = "Select Groupings", 
        #                 choices = colnames(target), selected = "group"))
        #   
        # })
        
        
        
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
        
        dds <- DESeq(dds)
        resultsNames(dds)
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
        
        group = target$group
        intgroup.df <- as.data.frame(colData(vsd)[, "group", drop=FALSE])
        
        # assembly the data for the plot
        #d <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2], PC3=pca$x[,3], Group=group, intgroup.df, name=colnames(vsd))
        d <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2], PC3=pca$x[,3], name=colnames(vsd))
        d2 <- left_join(d, target, by= c("name"="label"))
        
        ### -------------------------------------------------------------------------------------------------------------------
        
        hc2 <- hclust(dist(t(assay(vsd))), method="ward.D") 
        
        output$clust <- renderPlot(plot(hc2, hang=-1, ylab="Height", las=2, 
                                        xlab="Method: Euclidean distance - Ward criterion", 
                                        main="Cluster Dendrogram"))
        
        output$groups <- renderUI({
            #      if(is.null(colnames(pComp.df))){return()}
            
            tagList(
                selectInput(inputId = "choice", label = "Select Groupings",
                            choices = colnames(d2), selected = "group")
            )
            
            
            
        })
        
        output$groups2 <- renderUI({
            #      if(is.null(colnames(pComp.df))){return()}
            
            tagList(
                selectInput(inputId = "choice2", label = "Select Groupings",
                            choices = colnames(d2), selected = "group")
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
                    color = ~ get(input$choice),
                    #colors = color2,
                    marker = list(size = 8,
                                  line = list(color = ~ name , width = 1))) %>% 
                add_markers() %>%
                layout(autosize = F, width = 800, height = 800, margin =m,
                       scene = list(xaxis = list(title = 'PC1'),
                                    yaxis = list(title = 'PC2'),
                                    zaxis = list(title = 'PC3')
                       )
                )
        })
        
        
        output$plot2 <- renderPlotly({
            
            if(is.null(input$choice2)){return()}
            plot_ly(data = d2, x = ~ PC1, y = ~ PC2,
                    color = ~ get(input$choice2),
                    marker = list(size = 8,
                                  line = list(color = ~ name, width = 1))) %>%
                add_markers() %>%
                layout(autosize = F, width = 800, height = 800,
                       xaxis = list(title = 'PC1'),
                       yaxis = list(title = 'PC2'))
            
        })
        
        output$scree <- renderPlot({
            plot(pVar.df$percentVar *100,  main = "elbow plot",  xlab= "nth PC",  ylab = "Percent Var")
            
        })
        
        
        for (i in 18:20) {
            progress$set(value = i)
            Sys.sleep(0.1)
        }
        
        
        
        # Reactive value for selected dataset ----
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
        
        
        
        
        
        
    })
})