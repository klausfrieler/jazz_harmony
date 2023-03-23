#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(jazzodata)
library(parkR)
library(thematic)
library(waiter)
thematic_shiny()

irb_composer <- unique(parkR::irb$composer)
irb_title <- unique(parkR::irb$title)
irb_key <- unique(parkR::irb$key)
irb_key <- unique(parkR::irb$id)
irb_key <- unique(parkR::irb$id)

get_intro_text <- function(){
  div(h3("Welcome to the Jazz Harmony  App"), 
         p("This app allows you visualize and analyses chord sequences from the iRealBook and the Weimar Jazz Database"),
      p("Have fun!"),
      style = "width:50%;text-align:justify")
}

impressum <- function(){
    p(
        "Jazz Harmony  App  v0.1", 
        shiny::tags$br(), 
        shiny::tags$br(), 
        "Author: Klaus Frieler", 
        shiny::tags$br(), 
        shiny::a(href = "https://www.aesthetics.mpg.de/en.html", 
                 "Max Planck Institute for Empirical Aesthetics, Frankfurt/M, Germany", 
                 target = "_blank"),
        shiny::tags$br(),
        shiny::tags$br(), 
        "Powered by",
        shiny::tags$br(),
        shiny::a(href = "http://www.music-psychology.de/",
                 "Deutsche Gesellschaft für Musikspsychologie", target = "_blank"),
        shiny::tags$br(), 
        shiny::tags$br(),
        shiny::a(href = "https://github.com/klausfrieler/jazz_harmony", "On Github", target = "_blank"), 
        style = "font-size: 10pt; display: block"
    )
    
}

input_width <- 300


ui_new <-   
    shiny::shinyUI(
        navbarPage(
            title = "Jazz Harmonies", 
            theme = shinytheme("spacelab"),
            id = "tabs",
            tabPanel(
                "Home",
                sidebarLayout(
                    sidebarPanel(
                        impressum(),
                        selectizeInput("analysis_id", "IRB Id", choices = unique(parkR::irb$id), multiple = F, selected = parkR::irb %>% filter(id == unique(id)[1])),
                        checkboxInput("analysis_ii_v_filter", label = "Use II-V filter", value = 1),
                        actionButton("analysis_trigger", "Analyze chords"),
                        textAreaInput("analysis_string", "Enter chord symbols"),
                        downloadButton("download_all_data_csv", "Download data"),
                        checkboxInput("dec", label = "Use German Format", value = 0),
                        width = 2
                    ),
                    # Main panel for displaying outputs ----
                    mainPanel(
                        waiter::useWaitress(),
                        htmlOutput("introduction"),
                        #h4("Analysis"),
                        htmlOutput("progress_bar"),
                        tableOutput("harmonic_analysis")
                    )
                )
            ),
            tabPanel(
                "Data",
                sidebarLayout(
                    sidebarPanel(
                        impressum(),
                        selectizeInput("data_composer", "Composer", choices = irb_composer, multiple = T),
                        selectizeInput("data_title", "Title", choices = irb_title, multiple = T),
                        selectizeInput("data_key", "Key", choices = irb_key, multiple = T),
                        checkboxInput("data_only_metadata", label = "Only Metadata", value = 1),
                        checkboxInput("data_extended", label = "Extended Annotation", value = 0),
                        width = 2
                    ),
                    # Main panel for displaying outputs ----
                    mainPanel(
                        DT::DTOutput("raw_data")
                    ))))
)

is_valid <- function(string){
  !(is.null(string) || !nzchar(string) || is.na(string))
}                    

apply_data_filters <- function(data, input){
  #browser()
  if(is_valid(input$data_composer)){
    data <- data  %>% filter(composer %in%  input$data_composer)
  }
  if(is_valid(input$data_title)){
    data <- data  %>% filter(title %in%  input$data_title)
  }
  if(is_valid(input$data_key)){
    data <- data  %>% filter(key %in%  input$data_key)
  }
  data
}

# Define server logic required to draw a plot
server <- function(input, output, session) {
  message("*** STARTING APP***")
  
  output$introduction <- renderUI({
    get_intro_text()
  })
  
  output$progress_bar <- renderUI({
    div(
      h4("Analysis"), style = "width:200px"
    )
  })
  
  w <- Waitress$new("#progress_bar", theme = "overlay", hide_on_render = T)
  trigger_button <- reactiveValues(triggered = TRUE)
  key_analysis <- reactive({
    browser()
    if(!trigger_button$triggered){
      return(NULL)
    }
    chord_stream <- isolate(input$analysis_string)
    
    if(!nzchar(input$analysis_id) & !nzchar(chord_stream)){
      return(NULL)
    }
    if(nzchar(chord_stream)){
      ka <- parkR::key_analysis(chord_stream = chord_stream, 
                                with_ii_v_filter = input$analysis_ii_v_filter)
      return(ka)
    }
    compid <- parkR::irb %>% filter(id == input$analysis_id) %>% pull(compid) %>% unique()
    if(compid == 1 && input$analysis_ii_v_filter){
      ka <- readRDS("first_key_analysis.rds")
      return(ka)
    }
    w$start(span("Analyzing...", style = "margin-left:-400px"))
    sheet <- parkR::create_leadsheet_from_irb(compid = compid)
    ka <- parkR::key_analysis(sheet, with_ii_v_filter = input$analysis_ii_v_filter)
    w$close()
    w$hide
    ka
  })
  ka <- eventReactive(input$analysis_trigger, {
    trigger_button$triggered <- TRUE
  })
  output$harmonic_analysis <- renderTable({
    browser()
    k <- ka()
    ka <- key_analysis()
    if(is.null(k)){
      ka
    }
    else {
      trigger_button$triggered <- FALSE
      ka
    }
  })  
  
  output$raw_data <- renderDataTable({
    data <- parkR::irb 
    if(input$data_extended){
      data <- parkR::irb_extended
    }
    data <- apply_data_filters(data, input)

    if(input$data_only_metadata){
      data <- data %>% distinct(title, composer, key, date, time, compid)  
    }
    data
  }, options = list(lengthMenu = list(c(25, 50,  -1), c("25", "50",  "All"))))
   
  output$download_all_data_csv <- downloadHandler(
    filename = "jazz_harmony.csv",
    content = function(file) {
      dec <- ifelse(input$dec, ",", ".") 
      write.table(irb, file, row.names = FALSE, dec = dec, sep = ";", quote = T)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui_new, server = server)

