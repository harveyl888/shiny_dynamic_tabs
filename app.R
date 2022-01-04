library(shiny)
library(shinyjs)
library(DT)
library(jsonlite)

## list of datasets
dataset_list <- c('iris', 'mtcars', 'USArrests', 'cars', 'airquality', 'CO2', 'faithful')

mod_data_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns('sel_dataset'), 'dataset', choices = dataset_list),
    DTOutput(ns("tab_dataset"))
  )
}

mod_data <- function(id, datasetname = "") {
  moduleServer(
    id,
    function(input, output, session) {

      ## dataset
      dataset <- reactive({
        req(input$sel_dataset)
        get(input$sel_dataset)
      })

      ## show dataset
      output$tab_dataset <- renderDT({
        req(dataset())
        datatable(dataset(), options = list(pageLength = 5))
      })


      ## return data
      rtn_data <- reactive({
        list(id = datasetname, name = input$sel_dataset, rows = nrow(dataset()), cols = ncol(dataset()))
      })

      return(reactive(rtn_data()))

    }
  )
}



server <- function(input, output, session) {

  rv <- reactiveValues(
    dataset_count = 0,                      # count of dataset tabs
    dataset_names = list(),                 # list of dataset tab names
    trigger_add_data_button = FALSE,        # trigger to add button to dataset tabPanel
    return_data = list()                    # list of dataset parameters
  )


  ## function to add a new dataset
  add_dataset <- function() {
    rv$dataset_count <- rv$dataset_count + 1
    dataset_name <- paste0("dataset_", rv$dataset_count)
    rv$dataset_names[[length(rv$dataset_names) + 1]] <- dataset_name
    rv$return_data[[dataset_name]] <<- mod_data(id = dataset_name, datasetname = dataset_name)
    appendTab(inputId = "tab_data", tabPanel(title = tab_title(dataset_name), value = dataset_name, mod_data_UI(dataset_name)))
  }


  ## tab title with close button
  tab_title <- function(name, type = "data") {
    tags$span(
      name,
      tags$span(icon("times"),
                style = "margin-left: 5px;",
                onclick = paste0("Shiny.setInputValue(\"", paste0("remove_", type, "_tab"), "\", \"", name, "\", {priority: \"event\"})"))
    )
  }


  ## tabs
  output$ui_tabs <- renderUI({

    isolate({
      rv$dataset_count <- rv$dataset_count + 1
      dataset_name <- paste0("dataset_", rv$dataset_count)
      rv$dataset_names[[length(rv$dataset_names) + 1]] <- dataset_name
      rv$return_data[[dataset_name]] <<- mod_data(id = dataset_name, datasetname = dataset_name)
      rv$trigger_add_data_button <- TRUE
    })
    tabsetPanel(id = "tab_data",
                tabPanel(title = tab_title(dataset_name), value = dataset_name, mod_data_UI(dataset_name)))
  })


  ## add a button to the tabPanel
  observeEvent(rv$trigger_add_data_button, {
    if (rv$trigger_add_data_button) {
      rv$trigger_add_data_button <- FALSE
      shinyjs::delay(100, session$sendCustomMessage(type = "addbutton", list(id = "tab_data", trigger = "add_data")))
      tryCatch(o_data$destroy(),
               error = function(e) NULL)
      o_data <<- observeEvent(input$add_data, {
        add_dataset()
      }, ignoreInit = TRUE)
    }
  }, once = FALSE)


  ## remove a dataset
  observeEvent(input$remove_data_tab, {
    removeTab(inputId = "tab_data", target = input$remove_data_tab)
    isolate({rv$dataset_names <- rv$dataset_names[!rv$dataset_names == input$remove_data_tab]})
  })



  ## reactive holding returned data
  returned_data <- reactive({
    use_data <- rv$return_data[unlist(rv$dataset_names)]
    l_data <- lapply(seq_along(use_data), function(i) {
      data <- use_data[[i]]
      if (length(data()) > 0) {
        data()
      } else {
        list()
      }
    })
    l_data
  })


  ## display returned data
  output$txt_returned_data <- renderPrint({
    req(returned_data())
    print(prettify(toJSON(returned_data(), auto_unbox = TRUE)))
  })

}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(tags$script(src = "script.js", type="text/javascript")),
  br(),
  fluidRow(
    column(8, uiOutput("ui_tabs")),
    column(4, verbatimTextOutput("txt_returned_data"))
  )
)

shinyApp(ui, server)
