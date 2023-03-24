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
                        selectizeInput("analysis_db", "Chord Database", 
                                       choices = c("iReadlBook" = "irb","Weimar Jazz Database" = "wjd"), 
                                       multiple = F),
                        selectizeInput("analysis_id", "Tune", 
                                       choices = c(1) %>% set_names(parkR::irb$id[1]), 
                                       multiple = F, 
                                       selected =  ),
                        textAreaInput("analysis_string", "Enter chords (space separated)"),
                        checkboxInput("analysis_ii_v_filter", label = "Use II-V filter", value = 1),
                        checkboxInput("analysis_use_chord_complexity", label = "Use Chord Complexity", value = 0),
                        actionButton("analysis_trigger", "Analyze!"),
                        impressum(),
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
                        textOutput("error_msg"),
                        tableOutput("harmonic_analysis")
                    )
                )
            ),
            tabPanel(
                "Data",
                sidebarLayout(
                    sidebarPanel(
                        impressum(),
                        selectizeInput("data_db", "Chord Database", 
                                       choices = c("iReadlBook" = "irb","Weimar Jazz Database" = "wjd"), 
                                       multiple = F),
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
  
  observe({
    if(input$analysis_db == "irb"){
      ids <- parkR::irb %>% distinct(id, compid)
    }  
    else{
      ids <- parkR::wjd_chord_db %>% distinct(id, compid)
    }
    ids <- ids$compid %>% set_names(ids$id %>% str_remove("_FINAL.sv"))
    updateSelectizeInput(session, "analysis_id", choices = ids)
  })  
  
  observe({
    input$data_db
    data <- raw_data()
    updateSelectizeInput(session, "data_composer", choices = unique(data$composer))
    updateSelectizeInput(session, "data_title", choices = unique(data$title))
    updateSelectizeInput(session, "data_key", choices = unique(data$key))
  })  
  
  message <- reactiveValues(text = "Analysis")
  
  output$progress_bar <- renderUI({
    div(
      h4(message$text)
    )
  })
  
  observeEvent(input$analysis_id, {
    updateTextAreaInput(session, "analysis_string", value = "")
  })
  
  w <- Waitress$new("#progress_bar", theme = "overlay", hide_on_render = T)
  button <- reactiveValues(triggered = TRUE, database = "irb")
  
  key_analysis <- reactive({
    #browser()
    input$analysis_trigger
    db <- input$analysis_db
    chord_stream <- isolate(input$analysis_string) 
    analysis_id <- isolate(input$analysis_id)
    analysis_use_chord_complexity <- isolate(input$analysis_use_chord_complexity)
    analysis_ii_v_filter <- isolate(input$analysis_ii_v_filter)
    #browser()
    if(!nzchar(analysis_id) & !nzchar(chord_stream)){
      return(NULL)
    }
    if(nzchar(chord_stream)){
      chord_stream <- str_split(chord_stream %>% trimws(), " ")[[1]] 
      pcs <- tryCatch({
          parkR::parse_chord(chord_stream)
        },
        error = function(e){
          message$text <- "Invalid chord symbols"
          return(NULL)
        })
      if(is.null(pcs)){
        return(NULL)
      }
      ka <- parkR::key_analysis(chord_stream = chord_stream, 
                          with_ii_v_filter = input$analysis_ii_v_filter,
                          use_chord_complexity = analysis_use_chord_complexity)  %>% 
        select(chord, local_key, local_scale_degree)
      
      return(ka)
    }
    compid <- as.integer(analysis_id)
    if(compid == 1 && analysis_ii_v_filter){
      if(db == "irb") {
        ka <- readRDS("first_key_analysis.rds")
      }
      else{
        ka <- readRDS("first_key_analysis_wjd.rds")
      }
      return(ka)
    }
    w$start(span("Analyzing...", style = "margin-left:-400px"))
    if(db == "irb"){
      sheet <- parkR::create_leadsheet_from_irb(compid = compid)
    } 
    else{
      sheet <- parkR::create_leadsheet_from_wjd_db(compid = compid)
    }
    ka <- parkR::key_analysis(sheet, 
                              with_ii_v_filter = analysis_ii_v_filter, 
                              use_chord_complexity = analysis_use_chord_complexity) %>% 
      select(Section = section, 
             Measure = bar, 
             Beat = beat, 
             Chord = chord, 
             `Local Key`= local_key, 
             `Scale Degree`= local_scale_degree, 
             `Annotated Key` = main_key)
    w$close()
    w$hide
    ka
  })
  
  # trigger_button <- eventReactive(input$analysis_trigger, {
  #   messagef("Triggered")
  #   input$analysis_id
  #   analysis$current <- key_analysis()
  # })
  output$harmonic_analysis <- renderTable({
    tab <- key_analysis()
    tab
  })  
  
  raw_data <- reactive({
    if(input$data_db == "irb"){
      data <- parkR::irb
      if(input$data_extended){
        data <- parkR::irb_extended
      }
      data <- data %>% rename(signature = time)
    }
    else{
      meta <- jazzodata::wjd_meta %>% mutate(compid = as.integer(factor(sprintf("%s-%s", title, composer)))) 
      data <- parkR::wjd_chord_db %>% 
        select(-compid) %>%  
        left_join(meta %>% select(id, composer, compid, key, title, signature, date = recordingyear), by = "id") 
    }
    data
  })
  
  output$raw_data <- renderDataTable({
    data <- raw_data()
    data <- apply_data_filters(data, input)

    if(input$data_only_metadata){
      data <- data %>% distinct(title, composer, key, date, signature, compid)  
    }
    data
  }, options = list(lengthMenu = list(c(25, 50,  -1), c("25", "50",  "All"))))
   
  output$download_all_data_csv <- downloadHandler(
    filename = "jazz_harmony.csv",
    content = function(file) {
      dec <- ifelse(input$dec, ",", ".") 
      data <- raw_data()
      write.table(data, file, row.names = FALSE, dec = dec, sep = ";", quote = F)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui_new, server = server)

