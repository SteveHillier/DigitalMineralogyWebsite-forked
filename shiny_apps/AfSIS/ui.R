header <- dashboardHeader(title = "Cluster analysis of pre-treated AfSIS data",
                          titleWidth = 450
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("PCA", tabName = "clustering", icon = icon("line-chart")),
    menuItem("Property boxplots", tabName = "property_boxplots",
             icon = icon("bar-chart"))
  )
)

body <- dashboardBody(
  #This is the CSS code for changing the colors
  tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        '),
            tags$style(
              HTML('
                   /* logo */
                   .skin-blue .main-header .logo {
                   background-color: #74027a;
                   }

                   /* logo when hovered */
                   .skin-blue .main-header .logo:hover {
                   background-color: #74027a;
                   }

                   /* navbar (rest of the header) */
                   .skin-blue .main-header .navbar {
                   background-color: #74027a;
                   }

                   /* main sidebar */
                   .skin-blue .main-sidebar {
                   background-color: #0e8e02;
                   }

                   /* active selected tab in the sidebarmenu */
                   .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                   background-color: #33d0f7;
                   }

                   /* other links in the sidebarmenu */
                   .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                   background-color: #0e8e02;
                   color: #000000;
                   }

                   /* other links in the sidebarmenu when hovered */
                   .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                   background-color: #15e202;
                   }
                   /* toggle button when hovered  */
                   .skin-blue .main-header .navbar .sidebar-toggle:hover{
                   background-color: #038fba;
                   }
                   '))),

  #Now define the tab items
  tabItems(
    tabItem(tabName = "clustering",
            fluidRow(
              column(width = 9,
                     box(collapsible = TRUE, title = "PCA plot of soil XRPD data",
                         width = NULL,
                         solidHeader = TRUE, status = "info",
                         tags$hr(),
                         plotlyOutput("clusterplotly", width = "auto",
                                      height = "530px"))),
              column(width = 3,
                     box(collapsible = TRUE, title = "Clustering adjustments",
                         width = NULL,
                         solidHeader = TRUE,
                         status = "info",
                         selectInput('select2', 'Select whether the PCA plot is 2D or 3D.
                                     Only the 2D plots can be used to select more than 1
                                     sample.',
                                     choices = c("2D (PC1 vs PC2)", "2D (PC1 vs PC3)",
                                                 "2D (PC2 vs PC3)", "3D"),
                                     selected = "2D (PC1 vs PC2)",
                                     multiple = FALSE),
                         tags$hr(),
                         selectInput('selectclust', 'Select the number of clusters.',
                                     choices = 2:20,
                                     multiple = FALSE),
                         tags$hr(),
                         selectInput('selectquantile', 'Select the quantile used to exclude membership coefficients',
                                     choices = list("0 %" = 1,
                                                    "25 %" = 2,
                                                    "50 %" = 3,
                                                    "75 %" = 4),
                                     multiple = FALSE)
                     ))
            ),
            fluidRow(
              column(width = 12,
                     box(collapsible = TRUE, title = "Selected XRPD data",
                         width = NULL,
                         solidHeader = TRUE, status = "info",
                         plotlyOutput("clickplot", width = "auto",
                                      height = 400)),
                     box(collapsible = TRUE, title = "Sample locations within clusters",
                         width = NULL,
                         solidHeader = TRUE, status = "info",
                         selectInput("selectmap", label = "Select the cluster to plot",
                                     choices = c(1), multiple = FALSE),
                         textOutput("ClusterSize"),
                         tags$hr(),
                         leafletOutput("map1", height = "600px")))
            )
            #fluidRow(
            #  column(width = 12,
            #         box(collapsible = TRUE, title = "Export the clusters",
            #             width = NULL,
            #             solidHeader = TRUE, status = "warning",
            #             downloadButton(outputId = "download_clusters",
            #                            label = "Download these results as a .csv file"),
            #             downloadButton(outputId = "download_spatial",
            #                            label = "Download the spatial aspects of the clusters as an sf dataframe"),
            #             downloadButton(outputId = "download_attributes",
            #                            label = "Download all the attribute as a dataframe")))
            #)
            ),
    tabItem(tabName = "property_boxplots",
            column(width = 12,
                   box(collapsible = TRUE, title = "Box plots of the selected property by cluster",
                       width = NULL,
                       solidHeader = TRUE, status = "info",
                       selectInput('select6', 'Select a soil property to plot',
                                   choices = names(AfSIS_props_cont[-1]),
                                   multiple = FALSE),
                       plotlyOutput("property_bp", width = "auto",
                                    height = 500)
                       #downloadButton("DownloadPlotly", label = "Download as .html file"),
                       #downloadButton("DownloadPlot", label = "Download as .png file")
                   )))
    )
              )



ui <- dashboardPage(header, sidebar, body)
