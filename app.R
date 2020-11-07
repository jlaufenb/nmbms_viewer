#' @title Kodiak NWR Nearshore Survey viewer
#'
#' @description  An R shiny app for visualizing distance sampling transects.
#' @author Jared Laufenberg \email{jared_laufenberg@@fws.gov}
#'


# Install the required packages if they aren't already installed
packages <- c("shiny", "shinyjs", "tidyverse", "leaflet", "leaflet.minicharts", "shinyWidgets", "DT", "sp",
              "rgdal","adehabitatLT", "adehabitatHR", "lubridate", "geojsonio",
              "maptools", "dplyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
}

# Load the required packages
library(shiny)
library(shinyjs)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(shinyWidgets)
library(DT)
library(sp)
library(rgdal)
library(adehabitatLT)
library(adehabitatHR)
library(lubridate)
library(geojsonio)
library(maptools)
library(dplyr)

# Source custom functions
source("code/global.R")

# Define UI for application that draws a histogram
ui <- navbarPage("Transect-Viewer v.0.0.9000", id="nav",

                 # Tab 1: Load data
                 tabPanel("Load data",
                          # Sidebar layout
                          fluidPage(
                              fileInput(inputId = "file",
                                        label = "Select transect data:",
                                        multiple = FALSE,
                                        accept = ".RData")), # Get the file upload control option

                          DT::DTOutput("sum.tbl")
                 ),

                 # Tab 2: Interactive map
                 tabPanel("Interactive map",
                          div(class = "outer",

                              tags$head(includeCSS("css/style.css")),  # Add custom css style

                              leafletOutput("map", width="100%", height="100%"),  # Add the leaflet map

                              absolutePanel(id = "controls",
                                            class = "panel panel-default",
                                            fixed = TRUE,
                                            draggable = TRUE,
                                            top = 60,
                                            left = 20,
                                            right = "auto",
                                            bottom = "auto",
                                            width = "20%",
                                            height = "auto",

                                            htmlOutput("dataInfo"),  # Add a header of summaries of # of fixes, first/last fix, etc.

                                            shinyWidgets::selectizeGroupUI(  # Add the data filters
                                                id = "my-filters",
                                                inline = FALSE,
                                                params = list(
                                                    id = list(inputId = "id", title = "Survey:", placeholder = 'select'),
                                                    transect = list(inputId = "transect", title = "Transect:", placeholder = 'select'),
                                                    date = list(inputId = "date", title = "Date:", placeholder = 'select'))
                                            )
                              )
                          )
                 )
)


#-------------------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {

    options(shiny.maxRequestSize=30*1024^2)

    #----
    ## Tab 1: Load data

    # Define the uploaded .Rdata file
    dat <- reactive({
        req(input$file)
        if (is.null(input$file))
            return(NULL)
        inFile <- input$file
        file <- inFile$datapath
        # Load the file into new environment and get it from there
        e = new.env()
        name <- load(file, envir = e)
        dat <- e[[name]]
        dat <- dat %>%
            filter(!is.na(lat))  # Filter out rows that are missing latitude values
        dat
    })

    # Summarize data for the summary table
    dat.tbl <- reactive({
        req(input$file)
        dat() %>%
            group_by("Survey ID" = id) %>%
            summarise(
                "Transect" = first(transect),
                "Survey date" = first(date),
                "Source file" = first(sourcefile))
    })

    # Create a DT table of the summarized data
    output$sum.tbl <- DT::renderDT(dat.tbl(), options = list(pageLength = 20))


    #----
    ## Tab 2: Map

    # Subset the data based on user input from selectizeGroupUI:
    dat.sub <- callModule(
        id = "my-filters",
        module = selectizeGroupServer,
        data = dat,
        vars = c("id","transect","date")
    )

    # Create a map of the subsetted data:
    output$map <- renderLeaflet({
        nmbms_map(dat.sub())
    })

    # Output info for fixes selected in the map
    output$dataInfo <- renderUI({
        HTML(
            paste(sep = "<br/>",
                  paste("<br>"),
                  paste("<b>Total Surveys:</b> ", length(unique(dat.sub()$id))),
                  paste("<b>Total Transects:</b> ", length(unique(dat.sub()$transect))),
                  paste("<b>Min. Date:</b> ", as.Date(min(dat.sub()$datetime))),
                  paste("<b>Max. Date:</b> ", as.Date(max(dat.sub()$datetime))),
                  paste("<br>")
            ))
    })

}

# Run the application
shinyApp(ui = ui, server = server)
