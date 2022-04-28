

#' quickpRep
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
quickpRep <- function (...) {
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(DT)


####UI####
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("quickPrep: Quick Data Preparation for SourceXplorer"),
    sidebarPanel(

    fileInput(inputId = "file1", label = "Upload CSV: Vanta chemistry output",
              multiple = FALSE,
              accept = c(".csv"),
              width = NULL,
              buttonLabel = "Browse...",
              placeholder = "No file selected"),

    checkboxInput("exclude2711a", "Exclude NIST callibration samples", TRUE),

    selectInput("selectAggVar", "Trimmed Data ID Variable", choices=c()),

    fileInput(inputId = "file2", label = "Upload CSV: Artifact Catalogue",
              multiple = FALSE,
              accept = c(".csv"),
              width = NULL,
              buttonLabel = "Browse...",
              placeholder = "No file selected"),

    selectInput("selectSiteVar", "Catalogue Site ID", choices=c()),
    selectInput("selectArtifactVar", "Catalogue Artifact ID", choices=c()),
    actionButton("button1", "Create artifact ID"),
    actionButton("button2", "Merge Data")

    ),
    mainPanel(
      ###Tab Set Panels###
      tabsetPanel(
        tabPanel("Input data", DT::dataTableOutput("table1"), value = 1),
        tabPanel("Trimmed data", DT::dataTableOutput("table2"), value = 2),
        tabPanel("Averaged value data", DT::dataTableOutput("table3"), value = 3),
        tabPanel("Catalogue data", DT::dataTableOutput("table4"), value = 4),
        tabPanel("Merged data", DT::dataTableOutput("table5"), value = 5)
        )#close tabsetPanel
    )
)

####Server####
# Define server logic required to draw a histogram
server <- function(input, output, session) {


  ###Upload Data###
  ###Unknowns###
  dataIn1 <- reactive({
    chem_data <- fread(input$file1$datapath)
#    chem_data <- na.omit(chem_data)

    if(
       input$exclude2711a == TRUE
    ){
      chem_data <- subset(chem_data,
                         chem_data$`Sample ID` != "2711a" &
                           chem_data$Notes != "2711a" &
                           chem_data$`Sample ID` != "2710a" &
                           chem_data$Notes != "2710a")
    }

    return(chem_data)
  }) #close dataIn1

  output$table1 <- DT::renderDT(server = FALSE,{

    if (is.null(input$file1)){
      return(NULL)}

    else if (!is.null(input$file1)){
      return(
        DT::datatable(
        dataIn1()
        ))
    }
  }) #close output$table1


  trimmed <- reactive({

    if("Latitude" %in% colnames(dataIn1() ))
      {

    trimmed <- dataIn1()[,c(
      "Latitude",
      "Longitude",
      "Rb Concentration",
      "Sr Concentration",
      "Y Concentration",
      "Zr Concentration",
      "Nb Concentration",
      "Ti Concentration",
      "Ca Concentration",
      "Si Concentration",
      "Notes",
      "Sample ID",
      "Project No." )]

    trimmed <- dplyr::rename( trimmed,
                              latitude = Latitude,
                              longitude = Longitude,
                              info = Notes,
                              ID = `Sample ID`,
                              #"Project No." = `Project No`,
                              Rb = `Rb Concentration`,
                              Sr = `Sr Concentration`,
                              Y = `Y Concentration`,
                              Zr = `Zr Concentration`,
                              Nb = `Nb Concentration`,
                              Si = `Si Concentration`,
                              Ti = `Ti Concentration`,
                              Ca = `Ca Concentration`
    )
    trimmed$latitude[is.na(trimmed$latitude)] <- 0
    trimmed$longitude[is.na(trimmed$longitude)] <- 0
    trimmed[trimmed == "<LOD"] <- 0
    trimmed$Rb <- as.numeric(trimmed$Rb)
    trimmed$Sr <- as.numeric(trimmed$Sr)
    trimmed$Y <- as.numeric(trimmed$Y)
    trimmed$Zr <- as.numeric(trimmed$Zr)
    trimmed$Nb <- as.numeric(trimmed$Nb)
    trimmed$Si <- as.numeric(trimmed$Si)
    trimmed$Ca <- as.numeric(trimmed$Ca)
    trimmed$Ti <- as.numeric(trimmed$Ti)
    }

    else if ( !("Latitude" %in% dataIn1() )){

      trimmed <- dataIn1()[,c(
        "Rb Concentration",
        "Sr Concentration",
        "Y Concentration",
        "Zr Concentration",
        "Nb Concentration",
        "Ti Concentration",
        "Ca Concentration",
        "Si Concentration",
        "Notes",
        "Sample ID",
        "Project No." )]

      trimmed <- dplyr::rename( trimmed,
                                info = Notes,
                                ID = `Sample ID`,
                                #"Project No." = `Project No`,
                                Rb = `Rb Concentration`,
                                Sr = `Sr Concentration`,
                                Y = `Y Concentration`,
                                Zr = `Zr Concentration`,
                                Nb = `Nb Concentration`,
                                Si = `Si Concentration`,
                                Ti = `Ti Concentration`,
                                Ca = `Ca Concentration`
      )

      trimmed[trimmed == "<LOD"] <- 0
      trimmed$Rb <- as.numeric(trimmed$Rb)
      trimmed$Sr <- as.numeric(trimmed$Sr)
      trimmed$Zr <- as.numeric(trimmed$Zr)
      trimmed$Y <- as.numeric(trimmed$Y)
      trimmed$Nb <- as.numeric(trimmed$Nb)
      trimmed$Si <- as.numeric(trimmed$Si)
      trimmed$Ca <- as.numeric(trimmed$Ca)
      trimmed$Ti <- as.numeric(trimmed$Ti)

    } #close else if

    return(trimmed)
  }) #trimmed

  output$table2 <- DT::renderDT(server = FALSE,{

    if (is.null(input$file1)){
      return(NULL)}
    else if (!is.null(input$file1)){

      return(
        DT::datatable(  trimmed(),
                        filter = "top",
                        rownames= FALSE,
                        extensions = 'Buttons',
                        options = list(
                          dom = "Bfrtip",
                          buttons = 'csv',
                          pageLength = 10)
        ))
    }
  },
  filter = "top",
  options = list(
    buttons = c('csv'),
    pageLength = 10)
  ) #close output$table2

  agg <- reactive({
    agg <- aggregate(trimmed()[,c(1:10)], by = list(trimmed()$ID), FUN = "mean")
    agg <- dplyr::rename(agg, ID = Group.1)
    agg <- agg %>%
      mutate_if(is.numeric, round, 3)
    agg <- dplyr::left_join(agg, unique(trimmed()[,c("ID","info", "Project No.")]), by = "ID")

    return(agg)
  }) #close agg

  observe(
    if (is.null(input$file1)){
      return(NULL)}
    else if(!is.null( dataIn1())){
      updateSelectInput(session,
                        "selectAggVar",
                        choices = colnames(agg())[!grepl("Rb|Sr|Y|Zr|Nb|Si|Ti|Ca", colnames(agg())) ],
                        selected = c("ID")
      )
    }
  )

  output$table3 <- DT::renderDT(server = FALSE,{

    if (is.null(input$file1)){
      return(NULL)}
    else if (!is.null(input$file1)){
      return(
      DT::datatable(  agg(),
                             filter = "top",
                             rownames= FALSE,
                             extensions = 'Buttons',
                             options = list(
                               dom = "Bfrtip",
                               buttons = 'csv',
                               pageLength = 10)
                             ))
    }
  }
  ) #close output$table3

  dataIn2 <- reactive({
    if(is.null(input$file2)){return(NULL)}
    else if(!is.null(input$file2)){
    cat_data <- read.csv(input$file2$datapath,
                         check.names=FALSE,
                         na.strings=c(""," ","NA")
                         )

    return(cat_data)
    }
  }) #close dataIn2

  observe(
    if (is.null(input$file2)){
      return(NULL)}
    else if(!is.null( dataIn2())){
      updateSelectInput(session,
                        "selectSiteVar",
                        choices = colnames(dataIn2()),
                        selected = c()
      )
    }
  )
  observe(
    if (is.null(input$file2)){
      return(NULL)}
    else if(!is.null( dataIn2())){
      updateSelectInput(session,
                        "selectArtifactVar",
                        choices = colnames(dataIn2()),
                        selected = c()
      )
    }
  )

  updatedCat <- eventReactive(input$button1,{

    if (is.null(dataIn2())){return(NULL)}

    else if (!is.null(dataIn2() )){
      updatedCat <- as.data.frame(dataIn2())
      updatedCat$SiteVar <- dataIn2()[, input$selectSiteVar]
      updatedCat$ArtifactVar <- dataIn2()[, input$selectArtifactVar]
      updatedCat$ArtifactVar <- sprintf("%03d", updatedCat$ArtifactVar)

      updatedCat$ID <- paste0(updatedCat$SiteVar, ":", updatedCat$ArtifactVar)

      # updatedCat$ID <- paste(
      #   updatedCat[colnames( dataIn2()[,input$selectSiteVar])],
      #   str_pad(
      #     updatedCat[colnames(dataIn2()[,input$selectArtifactVar])],
      #     3,
      #     pad = 0
      #   ),
      #   sep = ":"
      # )

      return(updatedCat )
    }
  }) #close mergedData

  output$table4 <- DT::renderDT(server = FALSE,{

    if (is.null(dataIn2())){
      return(NULL)}
    else if (!is.null(dataIn2()) ){
        return(
          DT::datatable(  updatedCat(),
                          filter = "top",
                          rownames= FALSE,
                          extensions = 'Buttons',
                          options = list(
                            dom = "Bfrtip",
                            buttons = 'csv',
                            pageLength = 10)
          ))
      }
  }) #close output$table4


    mergedData <- eventReactive(input$button2,{
      if (is.null( updatedCat() )){return(NULL)}

      else if (!is.null(updatedCat())){
        mergedDF <- agg() %>%left_join(updatedCat(), by = c("ID"))
        mergedDF <- mergedDF %>% replace(is.na(.), "N/A")
        return(mergedDF)
      }
    }) #close mergedData


  output$table5 <- DT::renderDT(server = FALSE,{

    if (is.null(mergedData())){
      return(NULL)}
    else if (!is.null( mergedData())){
      return(
        DT::datatable(  mergedData(),
                        filter = "top",
                        rownames= FALSE,
                        extensions = 'Buttons',
                        options = list(
                          dom = "Bfrtip",
                          buttons = 'csv',
                          pageLength = 10)
        ))
    }
  }
  ) #close output$table5

}

# Run the application
shinyApp(ui = ui, server = server)

}
