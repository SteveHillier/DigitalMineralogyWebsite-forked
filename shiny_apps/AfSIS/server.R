server <- function(input, output, session) {

observe({
  #CLUSTERING OUTPUTS

  #Convert the diffraction data list into a data frame
    diffrac_cluster <- AfSIS_sub_pt

    for (i in 1:length(diffrac_cluster)) {
      diffrac_cluster[[i]]$tth <- NULL
    }

    diffrac_cluster <- data.frame(diffrac_cluster)

    diffrac_cluster <- data.frame(t(diffrac_cluster))

    row.names(diffrac_cluster) <- names(AfSIS_sub_pt)

    #pca_clust <- prcomp(scale(diffrac_cluster))
    pca_clust <- prcomp(diffrac_cluster)

    vars_clust <- apply(pca_clust$x, 2, var)
    props_clust <- vars_clust / sum(vars_clust)

    #cs <- kmeans(pca_clust$x[, c(1:min(which(cumsum(round(props_clust*100, 2)) > 99)))],
    #       centers = as.numeric(input$selectclust))
    
    fcm <- cmeans(pca_clust$x[, c(1:min(which(cumsum(round(props_clust*100, 2)) > 99)))],
                  centers = as.numeric(input$selectclust), iter.max = 100, m = 2)

    #cs_df <- data.frame("SAMPLE_ID" = rownames(diffrac_cluster),
    #                 "CLUSTER" = as.factor(cs$cluster))

    fcm_df <- data.frame("SAMPLE_ID" = rownames(diffrac_cluster),
                         "CLUSTER" = fcm$cluster,
                         stringsAsFactors = FALSE)
    
    fcm_members <- data.frame(fcm$membership,
                              CLUSTER = fcm$cluster)
    
    remove_list <- list()
    
    for(i in 1:ncol(fcm_members[-1])) {
      remove_list[[i]] <- which(fcm_members$CLUSTER == i &
                          fcm_members[[i]] < quantile(
                            fcm_members[[i]][which(fcm_members$CLUSTER == i)])[as.numeric(input$selectquantile)])
      
    }
    
    remove_these <- unlist(remove_list)

    #Define a color palette based on clusters

    #extract all the colors from the brewer
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


    #Randomly sample a colour vector of the same length as the number of clusters
    my_pal <- col_vector[c(1:length(table(fcm_df$CLUSTER)))]

    #extract all the cluster types
    cluster_types <- names(table(fcm_df$CLUSTER))


    cluster_cols <- c()
    for (i in 1:length(cluster_types)) {

      cluster_index <- which(fcm_df$CLUSTER %in% cluster_types[i])
      cluster_cols[cluster_index] <- my_pal[i]

    }


    #pca <- prcomp(scale(diffrac_cluster))
    pca <- prcomp(diffrac_cluster)

    vars <- apply(pca$x, 2, var)
    props <- vars / sum(vars)

    topsoils_pca <- data.frame(fcm_df,
                               pca$x[,1:3])
    
    
    AfSIS_sf_sub <- AfSIS_sf
    if(length(remove_these) > 0) {
    topsoils_pca <- topsoils_pca[-remove_these, ]
    AfSIS_sf_sub <- AfSIS_sf_sub[-remove_these, ]
    }

    #Define the labels that can be used for PCA plots

    pc1_lab <- list(
      title = paste0("PC1 (", round(props[1] * 100, 2), " %)")
    )

    pc2_lab <- list(
      title = paste0("PC2 (", round(props[2] * 100, 2), " %)")
    )

    pc3_lab <- list(
      title = paste0("PC3 (", round(props[3] * 100, 2), " %)")
    )

    output$clusterplotly <- renderPlotly({

      #Create a smaller PCA data frame
      pca_df <- topsoils_pca

      pca_df <- pca_df[order(as.numeric(pca_df$CLUSTER)), ]


      if(input$select2 == "2D (PC1 vs PC2)") {

        plot_1 <- paste("plot_ly(pca_df, x = ~PC1, y = ~PC2, color = ~as.factor(CLUSTER), colors = my_pal,
                        text = ~paste('SAMPLE_ID:', SAMPLE_ID, '\n CLUSTER:', CLUSTER),
                        key = ~SAMPLE_ID,
                        marker = list(size = 15,
                        line = list(color = 'black',
                        width = 2))) %>% layout(xaxis = pc1_lab, yaxis = pc2_lab, dragmode = 'select')")
      }

      if(input$select2 == "2D (PC1 vs PC3)") {

        plot_1 <- paste("plot_ly(pca_df, x = ~PC1, y = ~PC3, color = ~as.factor(CLUSTER), colors = my_pal,
                        text = ~paste('SAMPLE_ID:', SAMPLE_ID, '\n CLUSTER:', CLUSTER),
                        key = ~SAMPLE_ID,
                        marker = list(size = 15,
                        line = list(color = 'black',
                        width = 2))) %>% layout(xaxis = pc1_lab, yaxis = pc3_lab, dragmode = 'select')")
      }

      if(input$select2 == "2D (PC2 vs PC3)") {

        plot_1 <- paste("plot_ly(pca_df, x = ~PC2, y = ~PC3, color = ~as.factor(CLUSTER), colors = my_pal,
                        text = ~paste('SAMPLE_ID:', SAMPLE_ID, '\n CLUSTER:', CLUSTER),
                        key = ~SAMPLE_ID,
                        marker = list(size = 15,
                        line = list(color = 'black',
                        width = 2))) %>% layout(xaxis = pc2_lab, yaxis = pc3_lab, dragmode = 'select')")
      }

      if(input$select2 == "3D") {

        plot_1 <- paste0("plot_ly(pca_df,
                         x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(CLUSTER), colors = my_pal,
                         text = ~paste('SAMPLE_ID:', SAMPLE_ID, '\n CLUSTER:', CLUSTER),
                         key = ~SAMPLE_ID,
                         opacity = 0.8,
                         marker = list(size = 10,
                         line = list(color = 'black',
                         width = 2)))")
      }

      eval(parse(text = plot_1))

      })


    output$clickplot <- renderPlotly({

      #Plot diffraction data if XRPD is selected

      if(input$select2 == "3D") {
        e <- event_data("plotly_click")
      } else {e <- event_data("plotly_selected")}

      if (length(e) == 0) {
        xrd_select <- data.frame(TTH = c(1:70), COUNTS = rep(1, 70), SAMPLE_ID = rep("Please select a sample", 70))
      } else {
        xrd_select <- AfSIS_sub_pt[which(names(AfSIS_sub_pt) %in% e$key)]

        for (i in 1:length(xrd_select)) {
          xrd_select[[i]]$SAMPLE_ID <- rep(names(xrd_select)[i], nrow(xrd_select[[i]]))
          names(xrd_select[[i]]) <- c("TTH", "COUNTS", "SAMPLE_ID")
        }

        xrd_select <- do.call(rbind, xrd_select)

      }

      p <- ggplot(data = xrd_select) +
        geom_line(aes(x = TTH, y = COUNTS, col = SAMPLE_ID), size = 0.2) +
        theme(legend.title = element_blank()) +
        xlab("2theta") +
        ylab("sqrt(counts)")

      ggplotly(p)


    })

    updateSelectInput(session, "selectmap",
                      label = paste('Select the cluster to plot'),
                      choices = 1:input$selectclust)
    
    output$ClusterSize <- renderText({
      
      cluster_size <- data.frame(AfSIS_sf_sub, "CLUSTER" = topsoils_pca$CLUSTER,
                                  stringsAsFactors = FALSE)
      
      cluster_size <- cluster_size[which(cluster_size$CLUSTER == input$selectmap), ]
      
      paste("There are", nrow(cluster_size), "soil samples in this cluster")
      
    })


    output$map1 <- renderLeaflet({

      AfSIS_sf_sub2 <- data.frame(AfSIS_sf_sub, "CLUSTER" = topsoils_pca$CLUSTER,
                                    stringsAsFactors = FALSE)
      
      AfSIS_sf_sub2 <- AfSIS_sf_sub2[which(AfSIS_sf_sub2$CLUSTER == input$selectmap), ]

      AfSIS_sf_sub2 <- st_as_sf(AfSIS_sf_sub2)

      #Define the popups

      AfSIS_pops <- paste0(
        "<strong>Sample ID: </strong>", AfSIS_sf_sub2$SSN,
        "<br>
        <strong>Cluster: </strong>", AfSIS_sf_sub2$CLUSTER)

      #without grouped markers
      leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addTiles(group = "OSM (default)") %>%
        addCircleMarkers(data = AfSIS_sf_sub2, color = "red",
                         popup = AfSIS_pops, opacity = 0.8, fillOpacity = 0) %>%
        addLayersControl(
          baseGroups = c("Imagery", "OSM (default)"))
    })



    # output$download_clusters <- downloadHandler(
    # 
    #   filename = function() {
    #     paste0("CLUSTERS-", Sys.Date(), ".csv", sep="")
    #   },
    #   content = function(file) {
    #     write.table(topsoils_pca[1:2], file, sep = ",", col.names = TRUE, row.names = FALSE)
    #   }
    # )
    
    
    #Prepare an sf dataframe for all downloads
    
    topsoils_pca_export <- topsoils_pca
    names(topsoils_pca_export)[1] <- "SSN"
      
    attributes_export <- join(x = topsoils_pca_export,
                            y = AfSIS_props_cont,
                            by = "SSN")
    
    #download_attrbutes
    # output$download_attributes <- downloadHandler(
    #   
    #   filename = "AfSIS_clustering_attributes.Rdata",
    #   content = function(con) {
    #     assign("cluster_attributes", attributes_export)
    #     
    #     save(list="cluster_attributes", file=con)
    #   }
    #   
    # )
    
    # output$download_spatial <- downloadHandler(
    #   
    #   filename = "AfSIS_clustering_spatial.Rdata",
    #   content = function(con) {
    #     assign("cluster_spatial", AfSIS_sf_sub)
    #     
    #     save(list="cluster_spatial", file=con)
    #   }
    #   
    # )
    
    

      

    output$property_bp <- renderPlotly({

      prop <- data.frame("SAMPLE_ID" = AfSIS_props_cont$SSN,
                         "PROPERTY" = AfSIS_props_cont[paste(as.character(input$select6))])

      pbp <- join(x = topsoils_pca[1:2],
                  y = prop,
                  by = "SAMPLE_ID")

      pbp$SAMPLE_ID <- NULL

      names(pbp) <- c("x", "y")

      pbp$x <- as.factor(pbp$x)

      #property box plot
      p3 <- ggplot(data = pbp, aes(x = x, y = y)) +
        geom_boxplot() +
        #stat_summary(fun.y=mean, geom="line", aes(group=1))  +
        #stat_summary(fun.y=mean, geom="point") +
        xlab("Cluster") +
        ylab(paste(input$select6)) +
        theme(axis.text.x = element_text(hjust = 1, vjust = 0.5))

      ggplotly(p3)


    })
    
    
    plotly_export <- reactive({
      
      prop_ex <- data.frame("SAMPLE_ID" = AfSIS_props_cont$SSN,
                         "PROPERTY" = AfSIS_props_cont[paste(as.character(input$select6))])
      
      pbp_ex <- join(x = topsoils_pca[1:2],
                  y = prop_ex,
                  by = "SAMPLE_ID")
      
      pbp_ex$SAMPLE_ID <- NULL
      
      names(pbp_ex) <- c("x", "y")
      
      pbp_ex$x <- as.factor(pbp_ex$x)
      
      #property box plot
      p_ex <- ggplot(data = pbp_ex, aes(x = x, y = y)) +
        geom_boxplot() +
        #stat_summary(fun.y=mean, geom="line", aes(group=1))  +
        #stat_summary(fun.y=mean, geom="point") +
        xlab("Cluster") +
        ylab(paste(input$select6)) +
        ggtitle(paste(input$select6)) +
        theme(axis.text.x = element_text(hjust = 1, vjust = 0.5))
      
      ggplotly(p_ex)
      
    })
    
    plot_export <- reactive({
      
      prop_ex <- data.frame("SAMPLE_ID" = AfSIS_props_cont$SSN,
                            "PROPERTY" = AfSIS_props_cont[paste(as.character(input$select6))])
      
      pbp_ex <- join(x = topsoils_pca[1:2],
                     y = prop_ex,
                     by = "SAMPLE_ID")
      
      pbp_ex$SAMPLE_ID <- NULL
      
      names(pbp_ex) <- c("x", "y")
      
      pbp_ex$x <- as.factor(pbp_ex$x)
      
      #property box plot
      p_ex <- ggplot(data = pbp_ex, aes(x = x, y = y)) +
        geom_boxplot() +
        #stat_summary(fun.y=mean, geom="line", aes(group=1))  +
        #stat_summary(fun.y=mean, geom="point") +
        xlab("Cluster") +
        ylab(paste(input$select6)) +
        ggtitle(paste(input$select6)) +
        theme(axis.text.x = element_text(hjust = 1, vjust = 0.5))
      
    })
    
    #Download an example reference library
    # output$DownloadPlotly <- downloadHandler(
    #   
    #   filename = paste0(input$select6, ".html"),
    #   content = function(con) {
    #     assign("export_plotly", plotly_export())
    #     
    #     #save(list="example_plot", file=con)
    #     htmlwidgets::saveWidget(as_widget(export_plotly), con)
    #   }
    # )
    
    
    # output$DownloadPlot = downloadHandler(
    #   filename = paste0(input$select6, ".png"),
    #   content = function(file) {
    #     device <- function(..., width, height) {
    #       grDevices::png(..., width = 11.69, height = 8.27,
    #                      res = 1200, units = "in")
    #     }
    #     ggsave(file, plot = plot_export(), device = device)
    #   })




      })


  session$onSessionEnded(function() {
    stopApp()
  })





      }
