

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
    titlePanel("quickpRep: Quick Data Preparation for SourceXplorer"),
    sidebarPanel(

    fileInput(inputId = "file1", label = "Upload CSV: Raw Data",
              multiple = FALSE,
              accept = c(".csv"),
              width = NULL,
              buttonLabel = "Browse...",
              placeholder = "No file selected"),
    h3("Select Variables"),
    selectInput("selectAggVar", "Trimmed Data ID Variable", choices=c()),
    checkboxGroupInput(inputId = "varSelect", "Available Variables:", ""),

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
   chem_data <- na.omit(chem_data)

    return(chem_data)
  }) #close dataIn1

  vars <- reactive({

    if (is.null(input$file1)) {
      return(NULL)
    }

    else if (!is.null(input$file1)) {
      source_vars1 <- dataIn1()
      source_vars <- dplyr::select_if(source_vars1, is.numeric)
      names(source_vars)
    }

  })

  vars_selected <- reactive({
    vars_selected <- input$varSelect
    return(vars_selected)
  })

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

  observeEvent(
    input$file1,
    {

      updateCheckboxGroupInput(session,
                               "varSelect",
                               choices = vars(),
                               selected=vars_selected())
    })

  catvars <- reactive({

    if (is.null(input$file1)) {
      return(NULL)
    }

    else if (!is.null(input$file1)) {
      source_vars1 <- dataIn1()
      source_vars <- dplyr::select_if(source_vars1, negate(is.numeric))
      names(source_vars)
    }

  })

  observe(if (!is.null(input$file1)) {
    updateSelectInput(session,
                      "selectAggVar",
                      choices = catvars(),
                      selected = c())
  })


  trimmed <- reactive({

    dataIn_selectedAll <- dataIn1() %>% select(input$varSelect)

    dataIn_selectedID <- dataIn1() %>% select(input$selectAggVar)
    colnames(dataIn_selectedID) <- "ID"


    dataIn_selected <- dplyr::bind_cols(dataIn_selectedID, dataIn_selectedAll)

      dataIn <- as.data.frame(dataIn_selected)
      return(dataIn)
    })




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
    agg <- aggregate(trimmed(), by = list(trimmed()$ID), FUN = "mean")
    agg2 <- aggregate(trimmed(), by = list(trimmed()$ID), FUN = "sd")
    agg$ID <- agg$Group.1
    agg2$ID <- NULL
    agg$Group.1 <- NULL
    agg2$Group.1 <- NULL
    agg2 <- agg2 %>% dplyr::rename_with( ~ paste0(.x, " 1SD"))
    agg <- agg %>%
      mutate_if(is.numeric, round, 3)
    agg2 <- agg2 %>%
      mutate_if(is.numeric, round, 3)

    agg <- dplyr::bind_cols(agg, agg2)

    return(agg)
  }) #close agg





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
