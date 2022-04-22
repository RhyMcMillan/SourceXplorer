library(shiny)

SourceXplorer <- function (...) {

library(rsconnect)
library(tidyr)
library(ggplot2)
library(plotly)
library(ggfortify)
library(leaflet)
library(leaflet.extras)
library(scales)
library(lattice)
library(plyr)
library(dplyr)
library(DT)
library(MASS)
library(caret)
library(stringr)
library(htmltools)
library(RColorBrewer)
library(readxl)
library(svglite)
library(ggiraph)
library(ggrepel)
library(shinythemes)
library(mapview)
library(ggcorrplot)
library(rcompanion)
library(DescTools)
library(cowplot)
library(shinyWidgets)
library(janitor)
library(ggpubr)
library(sp)
library(openxlsx)
library(shinyalert)

####SETUP AND SOURCE DATA LOADING####


####UI####
ui <- fluidPage(
  theme = shinytheme("sandstone"), #https://rstudio.github.io/shinythemes/

                h1(strong("SourceXplorer"), style = "font-size:40px;"),

####sidebar####
  sidebarLayout(
    sidebarPanel(
      h3("Source Data"),
      div(style="margin-bottom:15px"),
      fileInput(inputId = "file2", label = "Upload CSV: Sources",
                multiple = FALSE,
                accept = c(".csv"),
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected"),
      selectInput("sourceGrpVar", "Source Grouping Variable", choices=c()),
      div(style="margin-bottom:15px"),
      h3("Unknown Data"),
      div(style="margin-bottom:15px"),
      fileInput(inputId = "file1", label = "Upload CSV: Unknowns",
                multiple = FALSE,
                accept = c(".csv"),
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected"),
      selectInput("artGrpVar", "Unknown Grouping Variable", choices=c()),
      selectInput("artIdVar", "Unknown ID Variable", choices=c()),
      div(style="margin-bottom:15px"),
      h3("Select Variables for Analysis"),
      checkboxGroupInput(inputId = "varSelect", "Available Variables (Select >1):", ""),
      div(style="margin-bottom:15px"),
      h3("Select Confidence Ellipse Characteristics"),
      div(style="margin-bottom:15px"),
      selectInput('confint', 'Confidence Interval (%)',
                  list(68, 95),
                  selected = "95"),
      selectInput('conf_type', 'Confidence Interval Type',
                  list('norm', 't', 'euclid'),
                  selected = 'norm'),
      div(style="margin-bottom:15px"),
      actionLink("conf_type_description", "Click here for confidence ellipse details."),
      div(style="margin-bottom:15px"),
      h3("Select LDA Thresholds for Post-Hoc Tests"),
      div(style="margin-bottom:15px"),
      selectInput('probthresh', 'LDA Posterior Probability Threshold (%)',
                  list(10, 20, 30, 40, 50, 60, 68, 70, 80, 90, 95),
                  selected = "70"),
      div(style="margin-bottom:15px"),
      selectInput('accthresh', 'LDA Model Accuracy Threshold (%)',
                  list(10, 20, 30, 40, 50, 60, 68, 70, 80, 90, 95),
                  selected = "80"),
    div(style="margin-bottom:15px"),
      h3("Tab-Specific Options"),
      conditionalPanel(
        condition = "input.tabselected == 4",
        selectInput('xcol', 'X Variable', choices = c()),
        selectInput('ycol', 'Y Variable', choices = c()),
        actionButton("loading_button_pca", 'Plot Most Heavily-Weighted Variables (PCA)'),
        div(style="margin-bottom:15px"),
        actionButton("loading_button_lda", 'Plot Most Heavily-Weighted Variables (LDA)')),
    div(style="margin-bottom:15px"),



      conditionalPanel(
        condition = "input.tabselected == 2",
        selectInput("saveFormatLDA", "Save file format", choices=c("svg","png","pdf")),
        downloadButton("download6", "Download LDA Plot"),
        downloadButton("download5", "Download LDA Plot Data")),

      conditionalPanel(
        condition = "input.tabselected == 3",
        selectInput("saveFormatPCA", "Save file format", choices=c("svg","png","pdf")),
        downloadButton("download3", "Download PCA Plot"),
        downloadButton("download4", "Download PCA Plot Data")),

      conditionalPanel(
        condition = "input.tabselected == 4",
        selectInput("saveFormatScatter", "Save file format", choices=c("svg","png","pdf")),
        downloadButton("download1", "Download Scatterplot")),

      conditionalPanel(
        condition = "input.tabselected == 8",
        selectInput("saveFormatBar", "Save file format", choices=c("svg","png","pdf")),
        selectInput('prov_level', 'Choose a sourcing summary to plot', choices = c("Provenance Summary (Basic)", "Provenance Summary (Standard)", "Provenance Summary (Robust)"),
                    selected = "Provenance Summary (Basic)"),
        downloadButton("download7", "Download Bar Chart"),
        downloadButton("download8", "Download Bar Chart Data")),

      conditionalPanel(
        condition = "input.tabselected == 7",
        selectInput('prov_level_summary', 'Choose a sourcing summary to tabulate', choices = c("Provenance Summary (Basic)", "Provenance Summary (Standard)", "Provenance Summary (Robust)"),
                    selected = "Provenance Summary (Basic)")),
      ),

####mainpanel####
mainPanel(

  ###Map Panel###
  leafletOutput(outputId = "map"),


  ###Tab Set Panels###
  tabsetPanel(
    type = "tabs",
    tabPanel("Home", htmlOutput("text.1"), value = 1, fluidRow(column(h3("Welcome to SourceXplorer Version 1.01:"),
                                                                            HTML("<b><i>A graphical user interface for guided archaeological lithic source investigations</b></i>"),
                                                                        div(style="margin-bottom:15px"),
                                                                      HTML("SourceXplorer allows researchers to apply sophisticated statistical procedures that generate robust and
                                                                           reproducible interpretations of relationships between sources and unknowns using any type of numerical data."),
                                                                      div(style="margin-bottom:15px"),
                                                                      HTML("Upload your data, select your variables, and navigate through the tabs, above, to continue."),
                                                                      div(style="margin-bottom:15px"),
                                                                      actionLink("quickstartHelp", "Click here for quick-start help and other helpful hints."),
                                                                      div(style="margin-bottom:15px"),
                                                                      HTML("SourceXplorer is free, transparent, flexible, and malleable. Check out www.sourcexplorer.org to find the most recent version of the
                                                                           application."),
                                                                      div(style="margin-bottom:15px"),
                                                                      HTML("No sourcing outcome is absolute, especially when determined using indirect proxies. Such results are always only as
                                                                           good as the models used to produce them. Considering this, SourceXplorer provides no guarantees nor warranties of any kind.
                                                                           We have made the summarized results produced by the application as transparent as possible, but we take no responsibility for
                                                                           misuse or misinterpretation of the generated outcomes."),
                                                                      div(style="margin-bottom:15px"),
                                                                        HTML("Upload your data, update the map, select your variables, and navigate through the tabs, above, to continue."),
                                                                        div(style="margin-bottom:15px"),
                                                                        actionLink("quickstartHelp", "Click here for quickstart help and other helpful hints."),
                                                                        div(style="margin-bottom:15px"),
                                                                        downloadLink("downloadtemplatesource", "Download Template: Sources"),
                                                                        div(style="margin-bottom:15px"),
                                                                        downloadLink("downloadtemplateunknown", "Download Template: Unknowns"),
                                                                        div(style="margin-bottom:15px"),

                                                                        width = 12))),
    tabPanel("Source Data (Live)", htmlOutput("text.9"), DT::dataTableOutput('source.table'), value = 10),
    tabPanel("Unknown Data (Live)", htmlOutput("text.10"), DT::dataTableOutput('unknown.table'), value = 11),
    tabPanel("LDA Diagram", htmlOutput("text.2"), value = 2, fluidRow(column(plotlyOutput("LDA"),width = 12))),
    tabPanel("PCA Diagram", htmlOutput("text.3"), value = 3, fluidRow(column(plotlyOutput("PCA"),width = 12))),
    tabPanel("Scatterplot", value = 4, id = 'scatter_tab', htmlOutput("text.4"), width = "100%",fluidRow(column(plotlyOutput("Scatter"),width = 12))),
    tabPanel("Summary (All)", htmlOutput("text.6"), DT::dataTableOutput('summary.table'), value = 6),
    tabPanel("Summary (Tabulated)", htmlOutput("text.7"), DT::dataTableOutput('summary.table.tabulated'), value = 7),
    tabPanel(value = 8,"Summary Bar Plot",plotOutput("barplot")),
    tabPanel("Case Study", htmlOutput("text.8"), value = 9, fluidRow(column(h3("Case Study Demo: Not Just A Graphing Tool"),
                                                                       HTML("<b>SourceXplorer</b> allows researchers to apply sophisticated statistical procedures that generate robust and reproducible interpretations of artifact provenance using trace element concentrations.
                                                                       Follow the steps below to test how SourceXplorer works using our case study data.<br><br>
                                                                            <b><i>Step 1.</i></b> <b>Download</b> the case study"),
                                                                       downloadLink("demo_sources", "source data."),
                                                                       HTML("<br><br><b><i>Step 2.</i></b> <b>Upload</b> the <b>source data</b> using the <b>'Upload CSV: User Sources'</b> tool in the <b>sidebar (left)</b>.<br><br>
                                                                             <b><i>Step 3. </i></b><b>Select the variables you'd like to include and navigate</b> through the <b>tabs (above)</b> to explore relationships among the uploaded sources. Sources included in the models can modified using the map FOV,
                                                                             clicking on the markers on the map (excludes sources), and using the buttons on the map interface (clears exclusions).<br><br>
                                                                             <b><i>Step 4.</i></b> <b>Download</b> the case study"),
                                                                       downloadLink("demo_unknowns", "artifact data."),
                                                                       HTML("<br><br><b><i>Step 5. </i></b><b>Upload</b> the <b>artifact data</b> using the <b>'Upload CSV: User Unknowns'</b> tool in the <b>sidebar (left)</b>.<br><br>
                                                                             <b><i>Step 6. </i></b>Use the <b>dropdown menus</b> in the <b>sidebar</b> to control the artifact and source material ID and grouping variables and update the selected numeric variables to those shared between the artifact and source data.
                                                                             We suggest making the <b>Grouping Variable</b> either <b>'Locality'</b> or <b>'Territory'</b> for the <b>sources</b>, and the <b>Grouping Variable</b> to either
                                                                             <b>'Type'</b> or <b>Type.2'</b> and the <b>ID Variable</b> to <b>'info'</b> for the <b>artifacts</b>. We also suggest selecting all shared variables between the datasets (Rb, Sr, Y, Zr, Nb). <br><br>
                                                                             <b><i>Step 7. </i></b><b>Explore</b> the relationships among sources and artifacts, including the results of a suite of post-hoc tests and sourcing summaries, using the <b>tabs (above)</b>. <br><br>
                                                                            <b><i>Step 8.</i></b><b> Download</b> diagrams, diagram data, and tables using either in the relevant tab or the <b>Tab-Specific Options</b> in the sidebar panel."),

                                                                       width = 12))),
    id = "tabselected"
    )
  )))

####SERVER####
server <- function(session, input, output) {




  ###Quickstart Help###
  observeEvent(input$conf_type_description, {
    showModal(modalDialog(
      title = "Confidence Ellipse Types",
      HTML("
        Confidence ellipses are used to characterize hypothetical distributions of sample populations in bivariate space. <br><br>
        However, unlike a convex hull, which is the smallest possible convex shape that can be used to enclose a population in bivariate space (similar to a set of
        datapoints being enclosed by a rubber band stretched around them), different ellipse shapes are used to describe different population types.
        They also can be set to different confidence levels, similar to standard deviations of a mean value.<br><br>
        <b>SourceXplorer allows users to choose between three ellipse types and confidence intervals that will be both displayed on diagrams and used in post-hoc tests. </b><br><br>
       <b>Ellipse Types: </b><br><br>
       <b> norm </b> (default) - assumes a multivariate normal distribution. As multivariate normality is an assumption of LDA, this is selected as the default. <br><br>
       <b> t </b> - assumes a multivariate t-distribution. <br><br>
       <b> euclid </b> - draws a circle with the radius equal to the confidence interval, representing the euclidean distance from the center of the population. <br><br>
       <b>Ellipse Levels: </b><br><br>
       <b> 95% </b> - (default) represents a predicted distribution equivalent to 2 standard deviations. <br><br>
       <b> 67.7% </b> - represents a predicted distribution equivalent to 1 standard deviation. <br><br>
      "),
      easyClose = TRUE
    ))
  })

  observeEvent(input$quickstartHelp, {
    showModal(modalDialog(
      title = "Quickstart Instructions and Outcome Descriptions",
      HTML("
        Load your geological source sample data using the 'Upload CSV: User sources' button. <br><br>
        <b> Make sure your source data has coordinates so it is included in the map view and the plots and models! (Decimal degrees format.)</b><br><br>
       Use the 'User Source Grouping Variable' pulldown menu to select the field with your geological source name(s) from any non-numeric column.
       Load your archaeological sample data using the 'Upload CSV: User Unknowns' button.<br><br>
       Use the 'User Unknown Grouping Variable' pulldown menu to select your grouping variable (i.e. site, artifact type, etc.) from any non-numeric column.<br><br>
       Use the 'User Unknown ID Variable' pulldown menu to select the unique identifier field for your data (e.g., sample ID or artifact number) from any non-numeric columnb.<br><br>
       Move the map view and zoom in or out to include/exclude sources from the plots and models.
       You can also exclude sources by clicking the markers on the map, and include them again using the buttons on the map interface (if the sources are still within the map FOV).
       <b> Only sources within the map field of view will be included in the plots and tests!</b><br><br>
        Click markers on the map to exclude that source from the tests and plots for step-wise source elimination (excluded sources will appear red).<br><br>
       Restore eliminated sources within the current FOV by <b>clicking</b> the 'Clear All Exclusions' button or just the most recently-excluded source by clicking the 'Clear Last Exclusion' button in the map window.<br><br>
       Then select the numeric variables shared by your unknown and source data to be included in the statistical analyses. </b><br><br>
       Browse through the tabs for plots, LDA, and PCA results as well as sourcing summaries based on results of both tests and a series of post-hoc assessments.<br><br>
       To post-hoc tests operate as follows:<br><br>
       <b> Basic</b>- unknowns must fall into either the convex hull or the confidence interval for their predicted source in either LDA or PCA space (so, at least one ‘match’ overall).<br><br>
       <b> Standard</b>- unknowns must fall into either the convex hull or the confidence interval for their predicted source in both LDA and PCA space (so, at least one ‘match' in both LDA and PCA space).<br><br>
       <b> Robust</b>- unknowns must fall into both the convex hull and the confidence interval for their predicted source in both LDA and PCA space (so, matches all distributions in both diagrams).<br><br>
       Export your tables and plots using the export buttons.
      "),
      easyClose = TRUE
    ))
  })

  ###Templates and Demo Data###
  output$downloadtemplateunknown <- downloadHandler(
    filename = function() {
      paste("SourceXplorer_Template_Unknowns", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(templateunknown, file, row.names = FALSE)
    }
  )

  output$downloadtemplatesource <- downloadHandler(
    filename = function() {
      paste("SourceXplorer_Template_Sources", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(templatesources, file, row.names = FALSE)
    }
  )

  output$demo_sources <- downloadHandler(
    filename = function() {
      paste("SourceXplorer_Demo_Stsailes_Sources", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(demo_sources, file, row.names = FALSE)
    }
  )

  output$demo_unknowns <- downloadHandler(
    filename = function() {
      paste("SourceXplorer_Demo_Stsailes_Artifacts", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(demo_unknowns, file, row.names = FALSE)
    }
  )

  ###Upload Data###
  ###Sources###
  sourceIn <- reactive({

    if (is.null(input$file2)) {
      return(NULL)
    }

    else if (!is.null(input$file2)) {

      sourceIn <- read.csv(input$file2$datapath, header=T, na.strings=c(""," ","NULL","na","NaN","Inf","NA"))
      sourceIn <- na.omit(sourceIn)

      return(sourceIn)
    }

  })




  observeEvent(input$file2, {
    shinyalert( html = TRUE, title = "Source Data Uploaded.",
                text = HTML( "Note that any incomplete rows will be removed prior to use in SourceXplorer. <br><br>
                              You can see your processed data in the 'Source Data (Live)' tab,
                              including all complete rows for sources within the map FOV.<br><br>
                                If your data contains no complete rows (e.g., if you have an empty column in your data),
                                you will need to modify and restart before proceeding. <br><br>
                                If you had already uploaded a file here, please restart before uploading another." )) })

  observe(if (!is.null(sourceIn())) {

    sourceInnames <- sourceIn()[, !(names(sourceIn()) %in% names(sourceIn() %>% select_if(is.numeric)))]

    updateSelectInput(session,
                      "sourceGrpVar",
                      choices = colnames(sourceInnames),
                      selected = c())
  })



  sitesInBounds1 <- eventReactive(input$sourceGrpVar,{

    sitesInBounds_base <- df

    if (is.null(input$file2) ){
      return(sitesInBounds_base)
    }

    else if (!is.null(input$file2) & !is.null(input$sourceGrpVar) & !is.null(sourceIn()$latitude) & !is.null(sourceIn()$longitude)){
      sourceIn <- as.data.frame(sourceIn())
      dfSrcTrace <-  sourceIn%>%
        select_if(is.numeric)%>%
        drop_na()

      dfSrcTrace$info <- sourceIn()[,input$sourceGrpVar] #place holder column
      dfSrcTrace$shortName <- dfSrcTrace$info
      dfSrcTrace$type <- "User Data"
      dfSrcTrace$data.source <- "User Upload"

      #add number of samples to shortname
      dfSrcTrace$n <- 1
      df_nSources <- aggregate( n ~ shortName, data = dfSrcTrace, FUN = "sum")
      dfSrcTrace$n <- NULL
      dfSrcTrace <- left_join(dfSrcTrace , df_nSources, by = "shortName")
      dfSrcTrace$shortName <- paste0( dfSrcTrace$shortName, " n = ", dfSrcTrace$n)
      dfSrcTrace <- subset(dfSrcTrace, dfSrcTrace$shortName %like% "% n =%")
      dfSrcTrace$n <- NULL
      dfSrcTrace <- na.omit(dfSrcTrace)

      return(dfSrcTrace)
    }

    else if (!is.null(input$file2) & !is.null(input$sourceGrpVar) & is.null(sourceIn()$latitude) & is.null(sourceIn()$longitude)){
      sourceIn <- as.data.frame(sourceIn())
      dfSrcTrace <-  sourceIn%>%
        select_if(is.numeric)%>%
        drop_na()

      dfSrcTrace$info <- sourceIn()[,input$sourceGrpVar] #place holder column
      dfSrcTrace$shortName <- dfSrcTrace$info
      dfSrcTrace$type <- "User Data"
      dfSrcTrace$data.source <- "User Upload"

      #add number of samples to shortname
      dfSrcTrace$n <- 1
      df_nSources <- aggregate( n ~ shortName, data = dfSrcTrace, FUN = "sum")
      dfSrcTrace$n <- NULL
      dfSrcTrace <- left_join(dfSrcTrace , df_nSources, by = "shortName")
      dfSrcTrace$shortName <- paste0( dfSrcTrace$shortName, " n = ", dfSrcTrace$n)
      dfSrcTrace <- subset(dfSrcTrace, dfSrcTrace$shortName %like% "% n =%")
      dfSrcTrace$n <- NULL
      dfSrcTrace$latitude <- as.numeric("49")
      dfSrcTrace$longitude <- as.numeric("-130")
      dfSrcTrace <- na.omit(dfSrcTrace)

      return(dfSrcTrace)
    }
  })

  ###Upload Data###
  ###Unknowns###
  dataIn0 <- reactive({
    if (is.null(input$file2) ){
      return(NULL)
    }

    else if (!is.null(input$file2)){
      validate(
        need(!is.na(input$file1), "Standby, I'm thinking... Have you uploaded unknown data? If so, are all your rows complete?"))

    dataIn <- read.csv(input$file1$datapath, header=T, na.strings=c(""," ","NULL","na","NaN","Inf","NA"))

    dataIn <- na.omit(dataIn)
    dataIn <- as.data.frame(dataIn)

    return(dataIn)
    }
  })

  output$unknown.table <- DT::renderDataTable(server = FALSE,{

    validate(
      need(!is.na(dataIn0()[1]), "Standby, I'm thinking... Have you uploaded unknown data?")
    )

    if (is.null(input$file1))
      return(NULL)

    else if (!is.null(input$file1)){

      return(dataIn0())

    }
  }, extensions = 'Buttons',
  options = list(dom = 'Bfrtip',
                 buttons = c('copy', 'csv', 'pdf', 'print'), pageLength = 25)
  )

  observeEvent(input$file1, {
    shinyalert( html = TRUE,  title = "Unknown Data Uploaded.",
                text = HTML( "Note that any incomplete rows will be removed prior to use in SourceXplorer. <br><br>
                              You can see your processed data in the 'Unknown Data (Live)' tab,
                              including all rows passed to the statistical assessments.<br><br>
                                If your data contains no complete rows (e.g., if you have an empty column in your data),
                                you will need to modify and restart before proceeding. <br><br>
                                If you had already uploaded a file here, please restart before uploading another." ) ) })

  observe(if (!is.null(dataIn0())) {

    dataIn <- as.data.frame(dataIn0())
    df2info <-  dataIn[, !(names(dataIn) %in% names(dataIn %>% select_if(is.numeric)))]

    updateSelectInput(session,
                      "artGrpVar",
                      choices = colnames(df2info),
                      selected = c())
    updateSelectInput(session,
                      "artIdVar",
                      choices = colnames(df2info),
                      selected = c())
  })



  map_clicks<-reactiveValues(Clicks=list())

  mapMaterials <- reactive({
    Sources <- sitesInBounds1()
    return(Sources)
  })


####map####

  #Create the Map
  foundational.map <- reactive({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
      addTiles("https://tiles.macrostrat.org/carto/{z}/{x}/{y}.png", group = "Macrostrat") %>%
      addLayersControl(baseGroups = c("OSM", "Macrostrat"), position ="topleft") %>%
      addControl(actionButton("dumper", "Clear all exclusions"),
                 position = "topright") %>%
      addControl(actionButton("miniDump", "Clear last exclusion"),
                 position = "topright") %>%
      setView(lng = -121.323326,
              lat = 	50.810810,
              zoom = 4) #setting the FOV
  })
  output$map <- leaflet::renderLeaflet({
    foundational.map()
  })

  ###Map and Source Data Interactions###

  #refresh the map with data type changes
  observeEvent(
    !is.null(mapMaterials()),
    {

      info <- as.character(mapMaterials()$info)

      labs <- lapply(seq(nrow(mapMaterials())), function(i) {
        paste0(mapMaterials()[i, "type"], '<br>',
               mapMaterials()[i, "shortName"])
      })

      leafletProxy("map")  %>%
        clearMarkers() %>%
        clearShapes() %>%
        addCircleMarkers(
          data = mapMaterials(),
          lng = ~ longitude,
          lat = ~ latitude,
          radius = 6,
          weight = 4,
          opacity = 1,
          color = "blue",
          layerId = info,
          group = "myMarkers",
          label = lapply(labs, htmltools::HTML)
        )
    }, ignoreNULL = FALSE)

  observeEvent(
    input$file1,
    {

      info <- as.character(mapMaterials()$info)

      labs <- lapply(seq(nrow(mapMaterials())), function(i) {
        paste0(mapMaterials()[i, "type"], '<br>',
               mapMaterials()[i, "shortName"])
      })

      leafletProxy("map")  %>%
        clearMarkers() %>%
        clearShapes() %>%
        addCircleMarkers(
          data = mapMaterials(),
          lng = ~ longitude,
          lat = ~ latitude,
          radius = 6,
          weight = 4,
          opacity = 1,
          color = "blue",
          layerId = info,
          group = "myMarkers",
          label = lapply(labs, htmltools::HTML)
        )
    }, ignoreNULL = FALSE)




  #Restrict source data to map FOV
  sitesInBounds2 <- reactive({

    df <- sitesInBounds1()


    if (is.null(input$map_bounds))
      return(df[FALSE, ])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subdf <- subset(df,
                    latitude >= latRng[1] & latitude <= latRng[2] &
                      longitude >= lngRng[1] &
                      longitude <= lngRng[2])

  })


  ###Modify models based on map clicks
  sitesInBounds <- reactive({
    sitesInBounds <- sitesInBounds2() %>%
      filter(!info  %in% map_clicks$Clicks)

    i <- sapply(sitesInBounds, is.factor)
    sitesInBounds[i] <- lapply(sitesInBounds[i], as.character)
    return(sitesInBounds)
  })

  vars <- reactive({

    if (is.null(input$file2)) {
      return(NULL)
    }

    else if (!is.null(input$file2) & is.null(input$file1)) {
      source_vars1 <- sourceIn()
      source_vars <- dplyr::select_if(source_vars1, is.numeric)
      names(source_vars)
    }
    else if (!is.null(input$file2) & !is.null(input$file1)) {

      combo_data1 <- dplyr::bind_rows(dataIn(), sitesInBounds())
      combo_data <- combo_data1[, colSums(is.na(combo_data1)) == 0]
      source_vars <- dplyr::select_if(combo_data, is.numeric)
      names(source_vars)
    }

  })

  output$source.table <- DT::renderDataTable(server = FALSE,{

    validate(
      need(!is.na(sitesInBounds()[1]), "Standby, I'm thinking... Have you uploaded source data? Is it within the map FOV?")
    )

    if (is.null(input$file2))
      return(NULL)

    else if (!is.null(input$file2)){

      return(sitesInBounds())

    }
  }, extensions = 'Buttons',
  options = list(dom = 'Bfrtip',
                 buttons = c('copy', 'csv', 'pdf', 'print'), pageLength = 25)
  )

  vars_selected <- reactive({
    vars_selected <- input$varSelect
    return(vars_selected)
  })

  var_list <- reactive({

    var_list <- str_flatten(vars_selected(), collapse = ", ")
    return(var_list)
  })


  ####Use map to filter source sites based on marker click####
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    map_clicks$Clicks <-
      c(map_clicks$Clicks, click$id) #add the new selection

    sites_clicked <- mapMaterials() %>%
      filter(info %in% map_clicks$Clicks)

    leafletProxy("map")  %>%
      removeMarker(layerId = sites_clicked$id) %>%
      addCircleMarkers(
        data = sites_clicked,
        lng = ~ longitude,
        lat = ~ latitude,
        radius = 6,
        weight = 3.85,
        color = "red",
        fill = "red",
        opacity = 1,
        layerId = sites_clicked$id
      )
  })

  #clear all map marker selections
  observeEvent(input$dumper,{
    map_clicks$Clicks <- NULL

    info <- as.character(mapMaterials()$info)

    labs <- lapply(seq(nrow(mapMaterials())), function(i) {
      paste0(mapMaterials()[i, "type"], '<br>',
             mapMaterials()[i, "shortName"])
    })

    leafletProxy("map")  %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(
        data = mapMaterials(),
        lng = ~ longitude,
        lat = ~ latitude,
        radius = 6,
        weight = 4,
        opacity = 1,
        color = "blue",
        layerId = info,
        group = "myMarkers",
        label = lapply(labs, htmltools::HTML)
      )
  })

  #clear the last map marker selection
  observeEvent(input$miniDump,{

    info <- as.character(mapMaterials()$info)

    labs <- lapply(seq(nrow(mapMaterials())), function(i) {
      paste0(mapMaterials()[i, "type"], '<br>',
             mapMaterials()[i, "shortName"])
    })

    leafletProxy("map")  %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(
        data = mapMaterials(),
        lng = ~ longitude,
        lat = ~ latitude,
        radius = 6,
        weight = 4,
        opacity = 1,
        color = "blue",
        layerId = info,
        group = "myMarkers",
        label = lapply(labs, htmltools::HTML)
      )
    map_clicks$Clicks <- head(map_clicks$Clicks,-1)

    sites_clicked <- mapMaterials() %>%
      filter(info %in% map_clicks$Clicks)

    leafletProxy("map")  %>%
      removeMarker(layerId = sites_clicked$id) %>%
      addCircleMarkers(data = sites_clicked,
                       lng = ~longitude,
                       lat = ~latitude,
                       radius = 6,
                       weight = 3.85,
                       color = "red",
                       fill = "red",
                       opacity = 1,
                       layerId = sites_clicked$id)
  })

  #create dataset for blocking markers
  sitesClicked <- reactive({
    sitesInBounds0() %>%
      filter(info  %in% map_clicks$Clicks)
  })

  ### Prepare data for PCA

  TESources.transformedPCA <-reactive({
    Sources <- sitesInBounds()[, input$varSelect]
    Sources <- as.data.frame(Sources)
    return(Sources)
  })

  TEArtRed.transformedPCA <-  reactive({
    TEArtRed.transformed1 <- dataIn0()[, input$varSelect]
    TEArtRed.transformed <- na.omit(TEArtRed.transformed1)
    TEArtRed.transformed <- as.data.frame(TEArtRed.transformed)
    return(TEArtRed.transformed)
  })


  ####LDA
  TESources.transformed1 <-reactive({
    Sources <- sitesInBounds()[, input$varSelect]
    Sources$shortName <- sitesInBounds()$shortName
    return(Sources)
  })

  TEArtRed.transformed1 <-  reactive({

    TEArtRed.transformed1 <- dataIn0()[, input$varSelect]

    TEArtRed.transformed1$shortName <- dataIn0()[, input$artIdVar]
    TEArtRed.transformed1$shortName <-
      factor(TEArtRed.transformed1$shortName)


    TEArtRed.transformed <- na.omit(TEArtRed.transformed1)

  })



  ##Transform data for LDA (caret)
  TESources.transformed <- reactive({
    TESources.transformed1 <-TESources.transformed1()
    preproc.param <- TESources.transformed1 %>%
      preProcess(method = c("center", "scale"))
    TESources.transformed <- na.omit(TESources.transformed1)
    preproc.param %>% predict(TESources.transformed)

  })

  TEArtRed.transformed <- reactive({
    TESources.transformed1 <-TESources.transformed1()
    preproc.param <- TESources.transformed1 %>%
      preProcess(method = c("center", "scale"))
    TEArtRed.transformed <- na.omit(TEArtRed.transformed1())
    preproc.param %>% predict(TEArtRed.transformed)

  })


  ##Source models
  lda.source.model <- reactive({
    lda.source.model <- lda(shortName ~
                              .,
                            data = TESources.transformed())
    return(lda.source.model)
  })

  lda.data <- reactive({
    lda.data <- cbind(TESources.transformed(), predict(lda.source.model())$x)
    rad <- 3
    lda.data$length <- with(lda.data, sqrt(LD1^2+LD2^2))
    lda.data$angle <- atan2(lda.data$LD1, lda.data$LD2)
    lda.data$x_start <- lda.data$y_start <- 0
    lda.data$x_end <- cos(lda.data$angle) * rad
    lda.data$y_end <- sin(lda.data$angle) * rad

    return(lda.data)
  })

  ##Artifact predictions
  prediction.data.lda <- reactive({
    # Make predictions
    predictionsldaartifact <-
      lda.source.model() %>% predict(TEArtRed.transformed())
    # Prepare Predictions for plotting
    prediction.data.lda <-
      cbind(TEArtRed.transformed(), predictionsldaartifact$x)
    prediction.data.lda <-
      cbind(prediction.data.lda, predictionsldaartifact$class)
    prediction.data.lda <-
      cbind(prediction.data.lda, predictionsldaartifact$posterior)
    prediction.data.lda$LDAClassification <-
      prediction.data.lda$'predictionsldaartifact$class'
    prediction.data.lda$'predictionsldaartifact$class' <- NULL
    prediction.data.lda <- as.data.frame(prediction.data.lda)
    return(prediction.data.lda)
  })

  dataIn <- reactive({
    dataIn <- dataIn0()
    return(dataIn)
  })

  ###Assess LDA accuracy
  lda_accuracy <- reactive({

    preproc.param <- TESources.transformed() %>%
      preProcess(method = c("center", "scale"))

    set.seed(123)
    training.samples <- TESources.transformed()$shortName %>%
      createDataPartition(p = 0.8, list = FALSE)
    train.data <- TESources.transformed()[training.samples,]
    test.data <- TESources.transformed()[-training.samples,]

    train.transformed <- preproc.param %>% predict(train.data)
    test.transformed <- preproc.param %>% predict(test.data)

    modelldasource <- lda(shortName ~ .,
                          data = train.transformed)
    # Make predictions
    predictionsldasource <-
      modelldasource %>% predict(test.transformed)
    # Model accuracy
    sourceacclda <-
      mean(predictionsldasource$class == test.transformed$shortName)

    return(sourceacclda)
  })


  ###Update variable list
  observeEvent(
    input$file2,
    {

      updateCheckboxGroupInput(session,
                               "varSelect",
                               choices = vars(),
                               selected=vars_selected())
    })

  observeEvent(
    input$file1,
    {

      updateCheckboxGroupInput(session,
                               "varSelect",
                               choices = vars(),
                               selected=vars_selected())
    })

  observe({

    updateSelectInput(session,
                      "xcol",
                      choices = vars_selected())


    updateSelectInput(session,
                      "ycol",
                      choices = vars_selected())
  })


  ###Pulling LDA and PCA Loadings/Scalings for Scatterplot Weighted Variables
  loading1LDA <- reactive({

      lda.data.out.scaling <- as.data.frame(lda.source.model()$scaling)
      lda.data.out.scaling$Variables <- rownames(lda.data.out.scaling)

      loading1 <-  lda.data.out.scaling %>%
        dplyr::slice_max(abs(LD1)) %>%
        pull(Variables)

      return(loading1)
  })



  loading2LDA <- reactive({

      lda.data.out.scaling <- as.data.frame(lda.source.model()$scaling)
      lda.data.out.scaling$Variables <- rownames(lda.data.out.scaling)

      loading2 <-  lda.data.out.scaling %>%
        dplyr::slice_max(abs(LD2)) %>%
        pull(Variables)

      return(loading2)

  })


  loading1PCA <- reactive({

      pca.data <- prcomp( ~ ., data = TESources.transformedPCA(), scale. = TRUE)
      PCAloadings <-
        data.frame(Variables = rownames(pca.data$rotation), pca.data$rotation)

      loading1 <-  PCAloadings %>%
        dplyr::slice_max(abs(PC1)) %>%
        pull(Variables)



      return(loading1)

  })



  loading2PCA <- reactive({

      pca.data <- prcomp( ~ ., data = TESources.transformedPCA(), scale. = TRUE)
      PCAloadings <-
        data.frame(Variables = rownames(pca.data$rotation), pca.data$rotation)

      loading2 <-  PCAloadings %>%
        dplyr::slice_max(abs(PC2)) %>%
        pull(Variables)



      return(loading2)

    # }

  })




  observeEvent(input$loading_button_lda, {

    lda.data.out.scaling <- as.data.frame(lda.source.model()$scaling)
    lda.data.out.scaling$Variables <- rownames(lda.data.out.scaling)

    loading1 <-  lda.data.out.scaling %>%
      dplyr::slice_max(abs(LD1)) %>%
      pull(Variables)

    loading2 <-  lda.data.out.scaling %>%
      dplyr::slice_max(abs(LD2)) %>%
      pull(Variables)

    updateSelectInput(session,
                      "xcol",
                      choices = vars_selected(),
                      selected = loading1)


    updateSelectInput(session,
                      "ycol",
                      choices = vars_selected(),
                      selected = loading2)
  })

  observeEvent(input$loading_button_pca, {

    pca.data <- prcomp( ~ ., data = TESources.transformedPCA(), scale. = TRUE)
    PCAloadings <-
      data.frame(Variables = rownames(pca.data$rotation), pca.data$rotation)

    loading1 <-  PCAloadings %>%
      dplyr::slice_max(abs(PC1)) %>%
      pull(Variables)

    loading2 <-  PCAloadings %>%
      dplyr::slice_max(abs(PC2)) %>%
      pull(Variables)

    updateSelectInput(session,
                      "xcol",
                      choices = vars_selected(),
                      selected = loading1)


    updateSelectInput(session,
                      "ycol",
                      choices = vars_selected(),
                      selected = loading2)
  })






  ########PLOTS#######

  observe({
    Scatter <- ggplot() +
      geom_point(
        data = sitesInBounds(),
        aes(
          x = sitesInBounds()[, input$xcol],
          y = sitesInBounds()[, input$ycol],
          color = shortName,
          text = sitesInBounds()$info
        ),
        shape = 3,
        size = 2.5
      ) +

      ggpubr::stat_chull(linetype = "dotdash",fill = NA, data = sitesInBounds(),
        aes( x = sitesInBounds()[, input$xcol],
             y = sitesInBounds()[, input$ycol],
             color = shortName),
        alpha = 1, geom = "polygon", show.legend = F) +

      stat_ellipse(
        data = sitesInBounds(),
        aes(x = sitesInBounds()[, input$xcol], y = sitesInBounds()[, input$ycol], color = shortName),
        type = input$conf_type,
        level = as.numeric(input$confint)/100,
        show.legend = F
      ) +

      {
        if (!is.null(input$file1))
          geom_point(
            data = dataIn(),
            aes(
              x = dataIn()[, input$xcol],
              y = dataIn()[, input$ycol],
              fill = if (is.numeric(dataIn()[, input$artGrpVar])) {
                paste(colnames(dataIn()[input$artGrpVar]) ,
                      dataIn()[, input$artGrpVar],
                      sep = " ")
              }
              else if (!is.numeric(dataIn()[, input$artGrpVar])) {
                dataIn()[, input$artGrpVar]
              },
              text = dataIn()[, input$artIdVar]
            ),
            shape = 21,
            size = 3.5
          )
      } +

      ylab(input$ycol) +
      xlab(input$xcol) +
      labs(color = "Sources",
           fill = "Samples",
           caption =
             str_wrap(paste0("Data source(s):\n",
                             toString(
                               unique(sitesInBounds()$data.source)
                             )))) +
      theme_classic()

    output$Scatter <- renderPlotly({

      validate(
        need(!is.na(sitesInBounds()[1]), "Standby, I'm thinking... Where's your source data?"),
        need(length(vars_selected())>1, "How many variables have you included? SourceXplorer requires at least two to make predictions.")
      )

      myplot <- ggplotly(Scatter,
               tooltip = "text",
               height = 800,
               width = 1200) %>%
        layout(
          autosize = FALSE,
          title = paste("Scatterplot: ", input$ycol, " / ", input$xcol),
          margin = list(b = 200, t = 120),
          annotations =
            list(
              x = 0,
              y = -0.2,
              text = str_wrap(paste0(
                "Data source(s):\n",
                toString(unique(sitesInBounds()$data.source))
              )),
              showarrow = F,
              xref = 'paper',
              yref = 'paper',
              xanchor = 'left',
              yanchor = 'auto',
              xshift = 0,
              yshift = 0,
              font = list(size = 12, color = "black")
            )
        )

      for (i in 1:length(myplot$x$data)){
        if (!is.null(myplot$x$data[[i]]$name)){
          myplot$x$data[[i]]$name =  gsub("\\(","",str_split(myplot$x$data[[i]]$name,",")[[1]][1])
        }
      }
      myplot

    })

    output$download1 <- downloadHandler(
      filename = function() {
        paste("Scatterplot", input$saveFormatScatter, sep = ".")
      },
      content = function(file) {
        Scatter_legend <- get_legend(Scatter)
        Scatter_grid <- ggdraw(plot_grid(
          Scatter + theme(legend.position = "none"),
          Scatter_legend,
          ncol = 2
        ))
        ggsave(
          file,
          plot = Scatter_grid ,
          device = input$saveFormatScatter,
          width = 10,
          height = 6
        )
      }
    )
  })

  ####PCA####


  output$PCA <- renderPlotly({

    validate(
      need(!is.na(sitesInBounds()[1]), "Standby, I'm thinking... Where's your source data?"),
      need(length(vars_selected())>1, "How many variables have you included? SourceXplorer requires at least two to make predictions.")
    )

    ###Run PCA
    pca.data <- prcomp( ~ ., data = TESources.transformedPCA(), scale. = TRUE)
    expl.var <-
      as.character(round(pca.data$sdev ^ 2 / sum(pca.data$sdev ^ 2) * 100))
    pca.data1 <- predict(pca.data)
    pca.data1 <- cbind(sitesInBounds(), pca.data1)
    PCAvalues <-
      data.frame(info = sitesInBounds()$shortName, pca.data$x)
    PCAloadings <-
      data.frame(Variables = rownames(pca.data$rotation), pca.data$rotation)

    ###Plot PCA
    pca <- ggplot(data = PCAvalues) +
      geom_point(aes(
        x = PC1,
        y = PC2,
        color = info,
        text = info
      ),
      shape = 3,
      size = 2.5) +

      ggpubr::stat_chull(linetype = "dotdash",fill = NA, data = PCAvalues,
                         aes( x = PC1,
                              y = PC2,
                              color = info),
                         alpha = 1, geom = "polygon", show.legend = F) +


      stat_ellipse(
        data = PCAvalues,
        aes(x = PC1, y = PC2, color = info),
        type = input$conf_type,
        level = as.numeric(input$confint)/100,
        show.legend = F
      ) +
      geom_segment(data = PCAloadings,
                   aes(
                     x = 0,
                     y = 0,
                     xend = (PC1 * 5),
                     yend = (PC2 * 5)
                   ),
                   color = "grey") +
      annotate(
        "text",
        x = (PCAloadings$PC1 * 5),
        y = (PCAloadings$PC2 * 5),
        label = PCAloadings$Variables
      ) +
      xlab(paste0("PC1 (", expl.var[1], "%)")) +
      ylab(paste0("PC2 (", expl.var[2], "%)")) +
      labs(color = "Sources",
           fill = "Samples",
           caption =
             str_wrap(paste0("Data source(s):\n",
                             toString(
                               unique(sitesInBounds()$data.source)
                             )))) +
      {
        if (!is.null(input$file1))
          geom_point(
            data = as.data.frame(cbind(
              dataIn(), predict(pca.data, newdata = TEArtRed.transformedPCA())
            )) ,
            aes(
              x = PC1,
              y = PC2,
              text = dataIn()[, input$artIdVar],
              fill = if (is.numeric(dataIn()[, input$artGrpVar])) {
                paste(colnames(dataIn()[input$artGrpVar]) ,
                      dataIn()[, input$artGrpVar],
                      sep = " ")
              }
              else if (!is.numeric(dataIn()[, input$artGrpVar])) {
                dataIn()[, input$artGrpVar]
              }
            ),
            shape = 21,
            size = 3.5
          )
      } +

      theme_classic()

    myplot <- ggplotly(pca,
             height = 800,
             width = 1200,
             tooltip = "text") %>%
      layout(
        autosize = FALSE,
        title = "PCA Plot",
        margin = list(b = 200, t = 120),
        annotations =
          list(
            x = 0,
            y = -0.2,
            text = str_wrap(paste0("Data source(s):\n",
                                   toString(
                                     unique(sitesInBounds()$data.source)
                                   ))),
            showarrow = F,
            xref = 'paper',
            yref = 'paper',
            xanchor = 'left',
            yanchor = 'auto',
            xshift = 0,
            yshift = 0,
            font = list(size = 12, color = "black")
          )
      )

    for (i in 1:length(myplot$x$data)){
      if (!is.null(myplot$x$data[[i]]$name)){
        myplot$x$data[[i]]$name =  gsub("\\(","",str_split(myplot$x$data[[i]]$name,",")[[1]][1])
      }
    }
    myplot
  })

  output$download3 <- downloadHandler(
    filename = function(){ paste("PCA plot", input$saveFormatPCA, sep = ".") },
    content = function(file) {
      ###Run PCA again
      pca.data <- prcomp(~ ., data = TESources.transformedPCA(), scale = TRUE)
      expl.var <- as.character(round(pca.data$sdev^2/sum(pca.data$sdev^2)*100))
      pca.data1 <- predict(pca.data)
      pca.data1 <- cbind(sitesInBounds(), pca.data1)
      PCAvalues <- data.frame(info = sitesInBounds()$shortName, pca.data$x)
      PCAloadings <- data.frame(Variables = rownames(pca.data$rotation), pca.data$rotation)

      pca <- ggplot(data= PCAvalues)+
        geom_point(
          aes(x=PC1,
              y=PC2,
              color = info,
              text = info),
          shape = 3,
          size = 2.5)+

        stat_ellipse(data= PCAvalues,aes(x=PC1,y=PC2, color = info),
                                         type = input$conf_type, level = as.numeric(input$confint)/100, show.legend = F) +

        ggpubr::stat_chull(linetype = "dotdash",fill = NA,data = PCAvalues,
                           aes( x = PC1,
                                y = PC2,
                                color = info),
                           alpha = 1, geom = "polygon", show.legend = F) +


        geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*5),
                                             yend = (PC2*5)),
                     color = "grey") +
        annotate("text", x = (PCAloadings$PC1*5), y = (PCAloadings$PC2*5),
                 label = PCAloadings$Variables)+
        xlab(paste0("PC1 (",expl.var[1],"%)"))+
        ylab(paste0("PC2 (",expl.var[2], "%)"))+
        labs(color = "Sources",
             fill = "Samples",
             caption =
               str_wrap(
                 paste0("Data source(s):\n",
                        toString(
                          unique(
                            sitesInBounds()$data.source
                          ))))
             ) +
        {if(!is.null(input$file1))geom_point(
          data= as.data.frame(
            cbind(
              dataIn(),predict(pca.data, newdata=TEArtRed.transformedPCA()))) ,
          aes(x=PC1,
              y=PC2,
              text = dataIn()[,input$artIdVar],
              fill = if(is.numeric( dataIn()[,input$artGrpVar] )){
                paste(
                  colnames( dataIn()[input$artGrpVar] ) ,
                  dataIn()[,input$artGrpVar],
                  sep = " " )
              }
              else if(!is.numeric(dataIn()[,input$artGrpVar])){
                dataIn()[,input$artGrpVar]
              }),
          shape = 21,
          size = 3.5)
          } +

        theme_classic()

      pca_legend <- get_legend(pca)
      pca_grid <- ggdraw(
        plot_grid(
          pca + theme(legend.position = "none"),
          pca_legend,
          ncol = 2
        ))

      ggsave(file, plot = pca_grid , device = input$saveFormatPCA, width = 10, height = 6)
    })

  output$download4 <- downloadHandler(
    filename = function() {
      paste("PCA Plot Data SourceXplorer", ".xlsx", sep = "")
    },
    content = function(file) {


      if (is.null(input$file1)){

        pca.data <- prcomp(~ ., data = TESources.transformedPCA(), scale = TRUE)
        expl.var <- as.character(round(pca.data$sdev^2/sum(pca.data$sdev^2)*100))
        pca.data1 <- predict(pca.data)
        pca.data1 <- cbind(sitesInBounds(), pca.data1)
        PCAvalues <- data.frame(info = sitesInBounds()$shortName, pca.data$x)
        PCAloadings <- data.frame(Variables = rownames(pca.data$rotation), pca.data$rotation)

        pca <- ggplot(data= PCAvalues)+
          stat_ellipse(data= PCAvalues,aes(x=PC1,y=PC2, color = info),
                       type = input$conf_type, level = as.numeric(input$confint)/100, show.legend = F)+

          ggpubr::stat_chull(linetype = "dotdash",data = PCAvalues,
                             aes(x=PC1,y=PC2, color = info),
                             alpha = 0.1, geom = "polygon", show.legend = F)
        build <- ggplot_build(pca)$data
        ellipses <- build[[1]]
        hulls <- build[[2]]

        listPCA <- list("Scores" = PCAvalues, "Loadings" = PCAloadings, "Ellipses" = ellipses, "Hulls" = hulls)

        write.xlsx(listPCA, file, row.names = FALSE)
      }

      else if (!is.null(input$file1)){
        pca.data <- prcomp(~ ., data = TESources.transformedPCA(), scale = TRUE)

        pca.data1 <- predict(pca.data)
        pca.data1 <- cbind(sitesInBounds()$info, pca.data1)
        pca.data3 <- as.data.frame(pca.data1)

        pca.data1 <- predict(pca.data, newdata=TEArtRed.transformedPCA())
        pca.data1 <- cbind(dataIn()[,input$artIdVar], pca.data1)
        pca.data2 <- as.data.frame(pca.data1)
        pca.data2 <- rbind(pca.data2, pca.data3)
        PCAvalues <- data.frame(info = sitesInBounds()$shortName, pca.data$x)
        PCAloadings <- data.frame(Variables = rownames(pca.data$rotation), pca.data$rotation)

        pca <- ggplot(data= PCAvalues)+
          stat_ellipse(data= PCAvalues,aes(x=PC1,y=PC2, color = info),
                       type = input$conf_type, level = as.numeric(input$confint)/100, show.legend = F)+

          ggpubr::stat_chull(linetype = "dotdash",data = PCAvalues,
                             aes(x=PC1,y=PC2, color = info),
                             alpha = 0.1, geom = "polygon", show.legend = F)
        build <- ggplot_build(pca)$data
        ellipses <- build[[1]]
        hulls <- build[[2]]

        listPCA <- list("Scores" = pca.data2, "Loadings" = PCAloadings,  "Ellipses" = ellipses, "Hulls" = hulls)

        write.xlsx(listPCA, file, row.names = FALSE)

      }
    })

  ####LDA plot####
    output$LDA <- renderPlotly({


      validate(
        need(!is.na(sitesInBounds()[1]), "Standby, I'm thinking... Where's your source data?"),
        need(length(unique(sitesInBounds()$shortName))>2, "How many sources have you included? SourceXplorer requires at least three to make predictions to make predictions."),
        need(length(sitesInBounds()$shortName)>9, "How many observations have you included? SourceXplorer requires at least 10 for LDA."),
        need(length(vars_selected())>1, "How many variables have you included? SourceXplorer requires at least two to make predictions.")
      )

      ###LDA data
      rad <- 3 # This sets the length of your lines.
      lda.data.out.scaling <- as.data.frame(lda.source.model()$scaling)
      lda.data.out.scaling$Variables <- rownames(lda.data.out.scaling)
      ##Plot LDA
      lda <- ggplot(data= lda.data())+
        geom_point(
          aes(x=LD1,
              y=LD2,
              color = shortName,
              text = shortName),
          shape = 3,
          size = 2.5)+
        ggpubr::stat_chull(linetype = "dotdash",fill = NA, data = lda.data(),
                           aes( x = LD1,
                                y = LD2,
                                color = shortName), geom = "polygon", show.legend = F) +

        stat_ellipse(data= lda.data(),aes(x=LD1,y=LD2, color = shortName),
                                 type = input$conf_type, level = as.numeric(input$confint)/100, show.legend = F) +

        geom_segment(data = lda.data.out.scaling, aes(x = 0, y = 0, xend = (LD1*3),
                                             yend = (LD2*3)),
                     color = "grey") +
        annotate("text", x = (lda.data.out.scaling$LD1*3), y = (lda.data.out.scaling$LD2*3),
                 label = lda.data.out.scaling$Variables)+

        {if(!is.null(input$file1))geom_point(
          data= prediction.data.lda(),
             aes(x=LD1,
                 y=LD2,
                 text = dataIn()[,input$artIdVar],
                 fill = if(is.numeric( dataIn()[,input$artGrpVar] )){
                   paste(
                     colnames( dataIn()[input$artGrpVar] ) ,
                     dataIn()[,input$artGrpVar],
                     sep = " " )
                 }
                 else if(!is.numeric(dataIn()[,input$artGrpVar])){
                   dataIn()[,input$artGrpVar]
                 }),
             shape = 21,
          size = 3.5)
          } +

        xlab(paste0("LD1"))+
        ylab(paste0("LD2"))+
        labs(color = "Sources",
             fill = "Samples",
             caption =
               str_wrap(
                 paste0("Data source(s):\n",
                        toString(
                          unique(
                            sitesInBounds()$data.source
                          ))))
             ) +
        theme_classic()

      myplot <- ggplotly(
        lda,
        height = 800,
        width = 1200,
        tooltip = "text") %>%
        layout(
          autosize = FALSE,
          title =

            str_wrap(
              paste0("LDA Plot, Model Accuracy:\n",
                     toString(
                       round(((lda_accuracy())*100), digits = 2)), "%")),

          margin = list(b = 200, t = 120),
          annotations =
            list(x = 0,
                 y = -0.2,
                 text = str_wrap(
                   paste0("Data source(s):\n",
                          toString(
                            unique(
                              sitesInBounds()$data.source
                            )))),
                 showarrow = F,
                 xref='paper',
                 yref='paper',
                 xanchor='left',
                 yanchor='auto',
                 xshift=0,
                 yshift=0,
                 font=list(size=12, color="black"))
          )

      for (i in 1:length(myplot$x$data)){
        if (!is.null(myplot$x$data[[i]]$name)){
          myplot$x$data[[i]]$name =  gsub("\\(","",str_split(myplot$x$data[[i]]$name,",")[[1]][1])
        }
      }
      myplot

    })

    output$download6 <- downloadHandler(
      filename = function(){ paste("LDA plot", input$saveFormatLDA, sep = ".") },
      content = function(file) {
        rad <- 3 # This sets the length of your lines.
        lda.data.out.scaling <- as.data.frame(lda.source.model()$scaling)
        lda.data.out.scaling$Variables <- rownames(lda.data.out.scaling)
        ##Plot LDA
        lda <- ggplot(data= lda.data())+
          geom_point(
            aes(x=LD1,
                y=LD2,
                color = shortName,
                text = shortName),
            shape = 3,
            size = 2.5)+
          ggpubr::stat_chull(linetype = "dotdash",fill = NA, data = lda.data(),
                             aes( x = LD1,
                                  y = LD2,
                                  color = shortName), geom = "polygon", show.legend = F) +

          stat_ellipse(data= lda.data(),aes(x=LD1,y=LD2, color = shortName),
                       type = input$conf_type, level = as.numeric(input$confint)/100, show.legend = F) +

          geom_segment(data = lda.data.out.scaling, aes(x = 0, y = 0, xend = (LD1*3),
                                                        yend = (LD2*3)),
                       color = "grey") +
          annotate("text", x = (lda.data.out.scaling$LD1*3), y = (lda.data.out.scaling$LD2*3),
                   label = lda.data.out.scaling$Variables)+

          {if(!is.null(input$file1))geom_point(
            data= prediction.data.lda(),
            aes(x=LD1,
                y=LD2,
                text = dataIn()[,input$artIdVar],
                fill = if(is.numeric( dataIn()[,input$artGrpVar] )){
                  paste(
                    colnames( dataIn()[input$artGrpVar] ) ,
                    dataIn()[,input$artGrpVar],
                    sep = " " )
                }
                else if(!is.numeric(dataIn()[,input$artGrpVar])){
                  dataIn()[,input$artGrpVar]
                }),
            shape = 21,
            size = 3.5)
          } +

          xlab(paste0("LD1"))+
          ylab(paste0("LD2"))+
          labs(color = "Sources",
               fill = "Samples",
               caption =
                 str_wrap(
                   paste0("Data source(s):\n",
                          toString(
                            unique(
                              sitesInBounds()$data.source
                            ))))
          ) +
          theme_classic()

        lda_legend <- get_legend(lda)
        lda_grid <- ggdraw(
          plot_grid(
            lda + theme(legend.position = "none"),
            lda_legend,
            ncol = 2
          ))

        ggsave(file, plot = lda_grid, device = input$saveFormatLDA, width = 10, height = 6)
      }
    )

    output$download5 <- downloadHandler(
      filename = function() {
        paste("LDA Plot Data SourceXplorer", ".xlsx", sep = "")
      },
      content = function(file) {
          if (is.null(input$file1)){
            LDAdata <- lda.data()
            LDAdata$info <- LDAdata$shortName
            LDASourceModel <- subset(LDAdata, select = c(LD1, LD2, info))

            lda.data.out.scaling <- as.data.frame(lda.source.model()$scaling)
            lda.data.out.scaling$Variables <- rownames(lda.data.out.scaling)

            lda <- ggplot(data= lda.data())+
              stat_ellipse(data= lda.data(),aes(x=LD1,y=LD2, color = shortName),
                           type = input$conf_type, level = as.numeric(input$confint)/100, show.legend = F)+
            ggpubr::stat_chull(data = lda.data(),
                                 aes(x=LD1,y=LD2, color = shortName),
                                 alpha = 0.1, geom = "polygon", show.legend = F)
            build <- ggplot_build(lda)$data
            ellipses <- build[[1]]
            hulls <- build[[2]]

            listLDA <- list("Scores" = LDASourceModel, "Scalings" = lda.data.out.scaling, "Ellipses" = ellipses, "Hulls" = hulls)

            write.xlsx(listLDA, file, row.names = FALSE)

          }

          else if (!is.null(input$file1)){
            LDAdata <- as.data.frame(lda.data())
            LDAdata$info <- LDAdata$shortName


            prediction.table <- as.data.frame(prediction.data.lda())
            prediction.table$info <- prediction.table$shortName

            LDASourceModel <- subset(LDAdata, select = c(LD1, LD2, info))
            LDAuser <- subset(prediction.table, select = c(LD1, LD2,  info))
            lda.data.out <- rbind(LDAuser, LDASourceModel)

            lda.data.out.scaling <- as.data.frame(lda.source.model()$scaling)
            lda.data.out.scaling$Variables <- rownames(lda.data.out.scaling)

            lda <- ggplot(data= lda.data())+
              stat_ellipse(data= lda.data(),aes(x=LD1,y=LD2, color = shortName),
                           type = input$conf_type, level = as.numeric(input$confint)/100, show.legend = F)+

              ggpubr::stat_chull(linetype = "dotdash",data = lda.data(),
                                 aes(x=LD1,y=LD2, color = shortName),
                                 alpha = 0.1, geom = "polygon", show.legend = F)
            build <- ggplot_build(lda)$data
            ellipses <- build[[1]]
            hulls <- build[[2]]

            listLDA <- list("Scores" = lda.data.out, "Scalings" = lda.data.out.scaling, "Ellipses" = ellipses, "Hulls" = hulls)

            write.xlsx(listLDA, file, row.names = FALSE)

          }

        }
      )


  ###Prediction Table Back End

        prediction.table <- reactive({


              ##LDA Data
                  LDAdata <- as.data.frame(lda.data())
                  LDAdata$Type <- "Source"
                  LDAdata$info <- LDAdata$shortName
                  LDAdata$rownum <- "0"

                  prediction.table <- as.data.frame(prediction.data.lda())
                  prediction.table$info <- prediction.table$LDAClassification
                  prediction.table$Type <- "User"
                  prediction.table$rownum <- seq.int(nrow(prediction.table))

                  LDASourceModel <- subset(LDAdata, select = c(LD1, LD2, Type, info, shortName, rownum))
                  LDAuser <- subset(prediction.table, select = c(LD1, LD2, Type, info, shortName, rownum))
                  LDAall <- rbind(LDASourceModel, LDAuser)

              ###PCA Data
                  pca.data <- prcomp(~ ., data = TESources.transformedPCA(), scale = TRUE)

                      pca.data1 <- predict(pca.data)
                      pca.data3 <- as.data.frame(pca.data1)
                      pca.data3$info <- sitesInBounds()$shortName
                      pca.data3$shortName <- sitesInBounds()$shortName
                      pca.data3$Type <- "Source"
                      pca.data3$rownum <- "0"

                      pca.data1 <- predict(pca.data, newdata=TEArtRed.transformedPCA())
                      pca.data2 <- as.data.frame(pca.data1)
                      pca.data2$info <- prediction.table$LDAClassification
                      pca.data2$shortName <- prediction.table$shortName
                      pca.data2$Type <- "User"
                      pca.data2$rownum <- seq.int(nrow(pca.data2))

                      PCASourceModel <- subset(pca.data3, select = c(PC1, PC2, Type, info, shortName, rownum))
                      PCAuser <- subset(pca.data2, select = c(PC1, PC2, Type, info, shortName, rownum))
                      PCAall <- rbind(PCASourceModel, PCAuser)

                      ###Groupings

                      lda_groups <- as.character(unique(LDAuser$info))

                      LDASourceModel <- LDASourceModel %>%
                        filter(info %in% lda_groups)

                      PCASourceModel <- PCASourceModel %>%
                        filter(info %in% lda_groups)

                      ###Group by 'source' (prediction)

                      lda.source.list <- LDASourceModel %>%
                        group_by(info) %>%
                        group_split()

                      lda.unk.list <- LDAuser %>%
                        group_by(info) %>%
                        group_split()

                      pca.source.list <- PCASourceModel %>%
                        group_by(info) %>%
                        group_split()

                      pca.unk.list <- PCAuser %>%
                        group_by(info) %>%
                        group_split()

              plot_lda <- function(user, source_model){
                lda <- ggplot()+
                  geom_point(data= user,
                             aes(
                               x = LD1,
                               y = LD2)) +

                  stat_ellipse(data= source_model,aes(x=LD1,y=LD2),
                               type = input$conf_type, level = as.numeric(input$confint)/100, show.legend = F)+

                  ggpubr::stat_chull(linetype = "dotdash",data = source_model,
                                     aes( x = LD1,
                                          y = LD2),
                                     alpha = 0.1, geom = "polygon", show.legend = F)

                build.lda <- ggplot_build(lda)$data
                points.lda <- build.lda[[1]]
                ell.lda <- build.lda[[2]]
                hull.lda <- build.lda[[3]]

                dat.lda <- data.frame(
                  rownum = user$rownum,
                  shortName = user$shortName,
                  points.lda[1:2],
                  in.ell.lda = as.logical(point.in.polygon(points.lda$x, points.lda$y, ell.lda$x, ell.lda$y)),
                  in.hull.lda = as.logical(point.in.polygon(points.lda$x, points.lda$y, hull.lda$x, hull.lda$y)))

                return(dat.lda)
              }

              plot_pca <- function(user, source_model){
                pca <- ggplot()+
                  geom_point(data= user,
                             aes(x=PC1,
                                 y=PC2))+

                  stat_ellipse(data= source_model,aes(x=PC1,y=PC2),
                               type = input$conf_type, level = as.numeric(input$confint)/100, show.legend = F) +

                  ggpubr::stat_chull(linetype = "dotdash", data = source_model,
                                     aes( x = PC1,
                                          y = PC2),
                                     alpha = 1, geom = "polygon", show.legend = F)

                build.pca <- ggplot_build(pca)$data
                points.pca <- build.pca[[1]]
                ell.pca <- build.pca[[2]]
                hull.pca <- build.pca[[3]]

                dat.pca <- data.frame(
                  rownum = user$rownum,
                  shortName = user$shortName,
                  points.pca[1:2],
                  in.ell.pca = as.logical(point.in.polygon(points.pca$x, points.pca$y, ell.pca$x, ell.pca$y)),
                  in.hull.pca = as.logical(point.in.polygon(points.pca$x, points.pca$y, hull.pca$x, hull.pca$y))
                )


                return(dat.pca)
              }

              dat_lda <- purrr::map2_dfr(lda.unk.list, lda.source.list, plot_lda)
              dat_pca <- purrr::map2_dfr(pca.unk.list, pca.source.list, plot_pca)

              prediction.table$loading1LDA <- as.character(loading1LDA())
              prediction.table$loading2LDA <- as.character(loading2LDA())

              prediction.table$loading1PCA <- as.character(loading1PCA())
              prediction.table$loading2PCA <- as.character(loading2PCA())

              post.hoc <- prediction.table %>%
                left_join(dat_lda, by = "rownum") %>%
                left_join(dat_pca, by = "rownum")

              return(post.hoc)

            }




        )




    ####Summary Table (All)####NEWWWWWWWW

    summary.table <- reactive({

      validate(
        need(!is.na(sitesInBounds()[1]), "Standby, I'm thinking... Where's your source data?"),
        need(!is.na(input$file1), "Standby, I'm thinking... Have you uploaded unknowns?"),
        need(length(unique(sitesInBounds()$shortName))>2, "How many sources have you included? SourceXplorer requires at least three to make predictions to make predictions."),
        need(length(vars_selected())>1, "How many variables have you included? SourceXplorer requires at least two to make predictions.")
      )

      if (is.null(input$file1))
        return(NULL)

      else if (!is.null(input$file1)){
        prediction.table <-  as.data.frame(prediction.table())

        source_groups <- as.character(unique(prediction.table$LDAClassification))

        probs <-  prediction.table()%>%
          dplyr::select(source_groups)




        prediction.table$LDAprob <- do.call(pmax, probs)

        prediction.table$LDAprob <- (prediction.table$LDAprob)*100

        prediction.table <- prediction.table %>%
          mutate_if(is.numeric, round, 2)

        prediction.table <- prediction.table %>%
          dplyr::select(c("shortName",
                          "LDAClassification",
                          "LDAprob",
                          "in.ell.lda",
                          "in.hull.lda",
                          "in.ell.pca",
                          "in.hull.pca"))



        prediction.table$LDAacc <- (lda_accuracy())*100

        prediction.table <- prediction.table %>%
          mutate(ProvenancePostulate = ifelse(LDAprob >= as.numeric(input$probthresh), "Pass", "Fail"))

        prediction.table <- prediction.table %>%
          mutate(LDAacc_test = ifelse(LDAacc >= as.numeric(input$accthresh), "Pass", "Fail"))

        source_groups_all1 <- as.character(unique(sitesInBounds()$shortName))
        prediction.table$source_groups_all <- length(source_groups_all1)

        prediction.table$source_obs_n <- as.numeric(nrow(sitesInBounds()))

        prediction.table$in.ell.pca <- as.character(prediction.table$in.ell.pca)
        prediction.table$in.ell.lda <- as.character(prediction.table$in.ell.lda)

        prediction.table$in.hull.pca <- as.character(prediction.table$in.hull.pca)
        prediction.table$in.hull.lda <- as.character(prediction.table$in.hull.lda)

        prediction.table <- prediction.table %>%
          mutate(in.ell.pca.result = ifelse(str_detect(in.ell.pca, 'FALSE'),
                                              "Outside of PCA Ellipse", "Within PCA Ellipse"))
        prediction.table <- prediction.table %>%
          mutate(in.hull.pca.result = ifelse(str_detect(in.hull.pca, 'FALSE'),
                                            "Outside of PCA Convex Hull", "Within PCA Convex Hull"))

        prediction.table <- prediction.table %>%
          mutate(in.ell.lda.result = ifelse(str_detect(in.ell.lda, 'FALSE'),
                                            "Outside of LDA Ellipse", "Within LDA Ellipse"))
        prediction.table <- prediction.table %>%
          mutate(in.hull.lda.result = ifelse(str_detect(in.hull.lda, 'FALSE'),
                                            "Outside of LDA Convex Hull", "Within LDA Convex Hull"))





        ###Shared Basic and Standard
        prediction.table <- prediction.table %>%
                mutate(LDATestPredictionStandard = ifelse(str_detect(in.ell.lda.result, "Outside of LDA Ellipse") &
                                                      str_detect(in.hull.lda.result, "Outside of LDA Convex Hull"),
                                                    "Outside of LDA Population", "Within LDA Population"))

        prediction.table <- prediction.table %>%
          mutate(PCATestPredictionStandard = ifelse(str_detect(in.ell.pca.result, "Outside of PCA Ellipse") &
                                               str_detect(in.hull.pca.result, "Outside of PCA Convex Hull"),
                                             "Outside of PCA Population", "Within PCA Population"))

        ###Basic = one population type in either diagram

        prediction.table <- prediction.table %>%
          mutate(PredictionSummaryBasic = ifelse(str_detect(LDATestPredictionStandard, "Outside of LDA Population") &
                                                      str_detect(PCATestPredictionStandard, "Outside of PCA Population"),
                                                    "Unknown", as.character(prediction.table$LDAClassification)))

        prediction.table <- prediction.table %>%
          mutate(PredictionSummaryBasic = ifelse(str_detect(ProvenancePostulate, 'Fail') |
                                                      str_detect(LDAacc_test, 'Fail'),
                                                    "Ambiguous", as.character(PredictionSummaryBasic)))


        ###Standard = one population type in both diagrams
        prediction.table <- prediction.table %>%
          mutate(PredictionSummaryStandard = ifelse(str_detect(LDATestPredictionStandard, "Outside of LDA Population") |
                                                   str_detect(PCATestPredictionStandard, "Outside of PCA Population"),
                                                 "Unknown", as.character(prediction.table$LDAClassification)))

        prediction.table <- prediction.table %>%
          mutate(PredictionSummaryStandard = ifelse(str_detect(ProvenancePostulate, 'Fail') |
                                                      str_detect(LDAacc_test, 'Fail'),
                                                    "Ambiguous", as.character(PredictionSummaryStandard)))

        ###Robust = all population types in both diagrams

        prediction.table <- prediction.table %>%
          mutate(PredictionSummaryRobust = ifelse(str_detect(in.ell.lda.result, "Outside of LDA Ellipse") |
                                                  str_detect(in.hull.lda.result, "Outside of LDA Convex Hull") |
                                                  str_detect(in.ell.pca.result, "Outside of PCA Ellipse") |
                                                  str_detect(in.hull.pca.result, "Outside of PCA Convex Hull"),
                                                  "Unknown", as.character(prediction.table$LDAClassification)))

              prediction.table <- prediction.table %>%
                mutate(PredictionSummaryRobust = ifelse(str_detect(ProvenancePostulate, 'Fail')|
                                                          str_detect(LDAacc_test, 'Fail'),
                                                        "Ambiguous", as.character(PredictionSummaryRobust)))

        prediction.table$probthresh <- input$probthresh
        prediction.table$accthresh <- input$accthresh
        prediction.table$vars <- as.character(var_list())
        prediction.table$confint <- input$confint
        prediction.table$conf_type <- input$conf_type

        prediction.table$artGrpVar <- dataIn()[, input$artGrpVar]

        prediction.table <- prediction.table %>%
          mutate_if(is.numeric, round, 2)

        prediction.table <-  subset(prediction.table, select = c(shortName,
                                                                 artGrpVar,
                                                                 vars,
                                                                 source_groups_all,
                                                                 source_obs_n,
                                                                 LDAacc,
                                                                 accthresh,
                                                                 LDAacc_test,
                                                                 LDAClassification,
                                                                 LDAprob,
                                                                 probthresh,
                                                                 ProvenancePostulate,
                                                                 in.ell.pca.result,
                                                                 in.ell.lda.result,
                                                                 in.hull.pca.result,
                                                                 in.hull.lda.result,
                                                                 confint,
                                                                 conf_type,
                                                                 PredictionSummaryBasic,
                                                                 PredictionSummaryStandard,
                                                                 PredictionSummaryRobust


        ))

        names(prediction.table)[names(prediction.table) == 'shortName'] <- 'User Data ID'
        names(prediction.table)[names(prediction.table) == 'artGrpVar'] <- 'User Data Grouping Variable'
        names(prediction.table)[names(prediction.table) == 'in.ell.pca.result'] <- 'PCA Ellipse Test'
        names(prediction.table)[names(prediction.table) == 'in.ell.lda.result'] <- 'LDA Ellipse Test'
        names(prediction.table)[names(prediction.table) == 'in.hull.pca.result'] <- 'PCA Convex Hull Test'
        names(prediction.table)[names(prediction.table) == 'in.hull.lda.result'] <- 'LDA Convex Hull Test'
        names(prediction.table)[names(prediction.table) == 'LDAacc'] <- 'LDA Model Accuracy (%)'
        names(prediction.table)[names(prediction.table) == 'LDAClassification'] <- 'LDA Prediction'
        names(prediction.table)[names(prediction.table) == 'LDAprob'] <- 'LDA Probability (%)'
        names(prediction.table)[names(prediction.table) == 'probthresh'] <- 'Selected LDA Probability Threshold (%)'
        names(prediction.table)[names(prediction.table) == 'ProvenancePostulate'] <- 'Provenance Postulate (Pass/Fail)'
        names(prediction.table)[names(prediction.table) == 'accthresh'] <- 'Selected LDA Accuracy Threshold (%)'
        names(prediction.table)[names(prediction.table) == 'LDAacc_test'] <- 'Model Accuracy (Pass/Fail)'
        names(prediction.table)[names(prediction.table) == 'confint'] <- 'Confidence Interval (%)'
        names(prediction.table)[names(prediction.table) == 'conf_type'] <- 'Confidence Interval Type'
        names(prediction.table)[names(prediction.table) == 'PredictionSummaryBasic'] <- 'Provenance Summary (Basic)'
        names(prediction.table)[names(prediction.table) == 'PredictionSummaryStandard'] <- 'Provenance Summary (Standard)'
        names(prediction.table)[names(prediction.table) == 'PredictionSummaryRobust'] <- 'Provenance Summary (Robust)'
        names(prediction.table)[names(prediction.table) == 'vars'] <- 'Included Variables'
        names(prediction.table)[names(prediction.table) == 'source_obs_n'] <- 'Source Observations (#)'
        names(prediction.table)[names(prediction.table) == 'source_groups_all'] <- 'Included Sources (#)'

        prediction.table$`LDA Prediction` <- sub( " n.*","", sub("\\)","", sub("\\(", "", prediction.table$`LDA Prediction`)))
        prediction.table$`Provenance Summary (Basic)` <- sub( " n.*","", sub("\\)","", sub("\\(", "", prediction.table$`Provenance Summary (Basic)`)))
        prediction.table$`Provenance Summary (Standard)` <- sub( " n.*","", sub("\\)","", sub("\\(", "", prediction.table$`Provenance Summary (Standard)`)))
        prediction.table$`Provenance Summary (Robust)` <- sub( " n.*","", sub("\\)","", sub("\\(", "", prediction.table$`Provenance Summary (Robust)`)))

        prediction.table$`Grouping Variable` <- dataIn()[, input$artGrpVar]
        prediction.table$`ID Variable` <- dataIn()[, input$artIdVar]


        return(prediction.table)

      }
    })




  output$summary.table <- DT::renderDataTable(server = FALSE,{

    validate(
      need(!is.na(sitesInBounds()[1]), "Standby, I'm thinking... Where's your source data?"),
      need(!is.na(input$file1), "Standby, I'm thinking... Have you uploaded unknowns?"),
      need(length(unique(sitesInBounds()$shortName))>2, "How many sources have you included? SourceXplorer requires at least three to make predictions to make predictions."),
      need(length(vars_selected())>1, "How many variables have you included? SourceXplorer requires at least two to make predictions.")
    )

    if (is.null(input$file1))
      return(NULL)

    else if (!is.null(input$file1)){
      summary.table <- subset(summary.table(), select = -c(`Grouping Variable`, `ID Variable`))
      return( summary.table )

    }
  }, extensions = 'Buttons',
  options = list(dom = 'Bfrtip',
                 buttons = c('copy', 'csv', 'pdf', 'print'), pageLength = 25)
  )


  ###Summary Table (Tabulated)

  output$summary.table.tabulated <- DT::renderDataTable(server = FALSE,{

    validate(
      need(!is.na(sitesInBounds()[1]), "Standby, I'm thinking... Where's your source data?"),
      need(!is.na(input$file1), "Standby, I'm thinking... Have you uploaded unknowns?"),
      need(length(unique(sitesInBounds()$shortName))>2, "How many sources have you included? SourceXplorer requires at least three to make predictions."),
      need(length(vars_selected())>1, "How many variables have you included? SourceXplorer requires at least two to make predictions.")
    )

    if (is.null(input$file1))
      return(NULL)

    else if (!is.null(input$file1)){

      summary.table.tabulated <- summary.table()

      summary.table.tabulated$level <- summary.table()[, input$prov_level_summary]

      summary.table.tabulated <-  summary.table.tabulated %>% tabyl(level, `Grouping Variable`)

      names(summary.table.tabulated)[1]<-paste(input$prov_level_summary)

      return(summary.table.tabulated)

    }
  }, extensions = 'Buttons',
  options = list(dom = 'Bfrtip',
                 buttons = c('copy', 'csv', 'pdf', 'print'), pageLength = 25)
  )



  #####Bar Plot#####
  output$barplot <- renderPlot({

    validate(
      need(!is.na(sitesInBounds()[1]), "Standby, I'm thinking... Where's your source data?"),
      need(!is.na(input$file1), "Standby, I'm thinking... Have you uploaded unknowns?"),
      need(length(unique(sitesInBounds()$shortName))>2, "How many sources have you included? SourceXplorer requires at least three to make predictions."),
      need(length(vars_selected())>1, "How many variables have you included? SourceXplorer requires at least two to make predictions.")
    )

    bar <- ggplot(summary.table(), aes(summary.table()[, input$prov_level]))

    if (is.null(input$file1))
      return(NULL)

    else if (!is.null(input$file1)
             ){
      return(
        bar + geom_bar(aes(fill = `Grouping Variable`)) +
          ylab("Counts") +
          xlab(input$prov_level)
      )
    }
  })

  output$download7 <- downloadHandler(
    filename = function(){ paste("Bar Chart", input$saveFormatBar, sep = ".") },
    content = function(file) {
      bar <- ggplot(summary.table(), aes(summary.table()[, input$prov_level]))+ eom_bar(aes(fill = `Grouping Variable`)) +
        ylab("Counts") +
        xlab(input$prov_level)

      ggsave(file, plot = bar , device = input$saveFormatBar, width = 7, height = 5)
    }
  )


  output$download8 <- downloadHandler(
    filename = function() {
      paste("Bar Plot Data SourceXplorer", ".xlsx", sep = "")
    },
    content = function(file) {
      if (is.null(input$file1)){

       return(NULL)
      }

      else if (!is.null(input$file1)){
        bar <- ggplot(summary.table(), aes(summary.table()[, input$prov_level]))+ eom_bar(aes(fill = `Grouping Variable`)) +
          ylab("Counts") +
          xlab(input$prov_level)
        build <- ggplot_build(bar)$data
        counts <- build[[1]]
        counts <- subset(counts, select = c('fill', 'y', 'x', 'count', 'group', 'ymin', 'ymax'))
        listbar <- list("Counts" = counts)
        write.xlsx(listbar, file, row.names = FALSE)

      }

    })


} #server close

# Create Shiny app
shinyApp(ui = ui, server = server)

}
