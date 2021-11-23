# Installing required Packages

if (!require(dplyr)) install.packages('dplyr') ; library(dplyr)
if (!require(ggplot2)) install.packages('ggplot2') ; library(ggplot2)
if (!require(magick)) install.packages('magick') ; library(magick)
if (!require(purrr)) install.packages('purrr') ; library(purrr)
if (!require(lubridate)) install.packages('lubridate') ; library(lubridate)

# Creating User Interface
ui <- navbarPage("Geospatial Heatmap",
                 tabPanel("Historic data",
  
                    fluidPage(
                    
                    #titlePanel("Geospatial Heatmap"),
                    sidebarLayout(
                      sidebarPanel(#EVENT
                        selectInput(
                          "hist_event", "Select Event:", 
                          choices=c(
                            read.csv("events_vec.csv", header = TRUE, check.names = TRUE)
                          )
                        ),
                        br(),
                        
                        sliderInput(#YEARS
                          "hist_timeframe", "Select Timeframe", 
                          min = 1950, 
                          max = 2016, value = c(1950, 2016),
                          sep = NULL),
                        br(),
                        
                        selectInput(#KPI
                          "hist_kpi", "Select Indicator:", 
                          choices=c(
                            "Count" = "Count",
                            "Injuries direct" = "INJURIES_DIRECT",
                            "Injuries indirect" = "INJURIES_INDIRECT",
                            "Deaths direct" = "DEATHS_DIRECT",
                            "Deaths indirect" = "DEATHS_INDIRECT"
                          )
                        ),
                        br(),
                        
                        selectInput(#SPEED
                          "hist_speed", "Select Speed", 
                          choices = list(
                            "Slow (1 fps)" = 1, 
                            "Medium (2 fps)" = 2, 
                            "Fast (4 fps)" = 4,
                            "Very fast (10 fps)" = 10), 
                          selected = 2),
                        br(),
                        
                        actionButton("hist_Go", tags$b("Click to run the chart")),
                        br(),
                        br()
                      ),
                      
                      mainPanel(
                        imageOutput("hist_plot1")
                      )
                    )
                  )
                ),
                  
                tabPanel("Prediction data",
                         fluidPage(
                           
                           #titlePanel("Geospatial Heatmap"),
                           sidebarLayout(
                             sidebarPanel(#EVENT
                               selectInput(
                                 "pred_event", "Select Event:", 
                                 choices=c(
                                   read.csv("events_vec.csv", header = TRUE, check.names = TRUE)
                                 )
                               ),
                               br(),
                               
                               dateRangeInput(#TIMEFRAME
                                 "pred_timeframe", "Select Timeframe",
                                  start = "2017-01-01",
                                  end = "2019-12-31",
                                  min = "2017-01-01",
                                  max = "2019-12-31"
                                 ),
                               br(),
                               
                               selectInput(#TIME GRANULARITY
                                 "pred_window", "Select Time Window", 
                                 choices = list(
                                   "Weekly" = 1, 
                                   "Monthly" = 2, 
                                   "Annualy" = 3), 
                                 selected = 2),
                               br(),
                               
                               selectInput(#KPI
                                 "pred_kpi", "Select Indicator:", 
                                 choices=c(
                                   "Count" = "Count",
                                   "Injuries direct" = "INJURIES_DIRECT",
                                   "Injuries indirect" = "INJURIES_INDIRECT",
                                   "Deaths direct" = "DEATHS_DIRECT",
                                   "Deaths indirect" = "DEATHS_INDIRECT"
                                 )
                               ),
                               br(),
                               
                               selectInput(#SPEED
                                 "pred_speed", "Select Speed", 
                                 choices = list(
                                   "Slow (1 fps)" = 1, 
                                   "Medium (2 fps)" = 2, 
                                   "Fast (4 fps)" = 4,
                                   "Very fast (10 fps)" = 10), 
                                 selected = 2),
                               br(),
                               
                               actionButton("pred_Go", tags$b("Click to run the chart")),
                               br(),
                               br()
                             ),
                             
                             mainPanel(
                               imageOutput("pred_plot1")
                             )
                           )
                         )
                )
)

# The Backend
server <- function(input,output){
  
  presense_flags_frame <- read.csv("presense_flags_frame.csv", stringsAsFactors = F)
  presense_flags_frame_prediction_yr <- read.csv("presense_flags_frame_prediction_yr.csv", stringsAsFactors = F)
  presense_flags_frame_prediction_mon <- read.csv("presense_flags_frame_prediction_mon.csv", stringsAsFactors = F)
  presense_flags_frame_prediction_wk <- read.csv("presense_flags_frame_prediction_wk.csv", stringsAsFactors = F)
  
  reactive_input_hist_min_year <- eventReactive(input$hist_Go, {min(input$hist_timeframe)})
  reactive_input_hist_max_year <- eventReactive(input$hist_Go, {max(input$hist_timeframe)})
  reactive_input_hist_event <- eventReactive(input$hist_Go, {input$hist_event})
  reactive_input_hist_kpi <- eventReactive(input$hist_Go, {input$hist_kpi})
  reactive_input_hist_speed <- eventReactive(input$hist_Go, {input$hist_speed})
  
  reactive_input_pred_min_date <- eventReactive(input$pred_Go, {min(input$pred_timeframe)})
  reactive_input_pred_max_date <- eventReactive(input$pred_Go, {max(input$pred_timeframe)})
  reactive_input_pred_window <- eventReactive(input$pred_Go, {input$pred_window})
  reactive_input_pred_event <- eventReactive(input$pred_Go, {input$pred_event})
  reactive_input_pred_kpi <- eventReactive(input$pred_Go, {input$pred_kpi})
  reactive_input_pred_speed <- eventReactive(input$pred_Go, {input$pred_speed})
  
  #render main plot
  output$hist_plot1 <- renderImage({
    
    #remove old gif
    unlink("hist_outfile.gif")
    
    list1 <- list.files(path = "pngs", pattern = "*.png", full.names = T)
    list2 <- list1[grepl(paste0("pngs/", reactive_input_hist_event(), "_"), list1)]
    list3 <- list2[grepl(paste0("pngs/", reactive_input_hist_event(), "_", reactive_input_hist_kpi(), "_"), list2)]
    list4 <- list3[grepl(paste0(reactive_input_hist_min_year() : reactive_input_hist_max_year(), collapse = "|"), list3)]
    
    #replace missing years with dummies
    if(length(list4) < length(reactive_input_hist_min_year():reactive_input_hist_max_year())){
      
      present_years <- as.numeric(substr(list4, nchar(list4) - (8-1), nchar(list4) - (4)))
      missing_years <- setdiff(reactive_input_hist_min_year():reactive_input_hist_max_year(), present_years)
      
      positions_actual <- which(reactive_input_hist_min_year():reactive_input_hist_max_year() %in% present_years)
      positions_missing <- which(reactive_input_hist_min_year():reactive_input_hist_max_year() %in% missing_years)
      
      list4_upd <- c(rep("dummy",length(reactive_input_hist_min_year():reactive_input_hist_max_year())))
      list4_upd[positions_actual] <- list4
      list4_upd[positions_missing] <- paste0("pngs/default_frame_", missing_years,".png")
      
      list4 <- list4_upd
    }
    
    list4 %>%
      map(image_read) %>% # reads each path file
      image_join() %>% # joins image
      image_animate(fps=as.numeric(reactive_input_hist_speed())) %>% # animates, can opt for number of loops
      image_write("hist_outfile.gif")
    
    # Return a list containing the filename
    list(src = "hist_outfile.gif",
         contentType = 'image/gif'
    )
    
  })
  
  output$pred_plot1 <- renderImage({
    
    #remove old gif
    unlink("pred_outfile.gif")
    
    #get inputs on data to be used for gif
    start_date <- as.Date(reactive_input_pred_min_date(), origin = "1970-01-01")
    end_date <- as.Date(reactive_input_pred_max_date(), origin = "1970-01-01")
    
    month_vec <- 
      presense_flags_frame_prediction_mon %>%
      filter(min_date <= !!end_date,
             min_date >= !!start_date) %>%
      filter(
        EVENT_TYPE == reactive_input_pred_event(),
        kpi == reactive_input_pred_kpi()
      ) %>%
      select(mon_flag)
    week_vec <- 
      presense_flags_frame_prediction_wk %>%
      filter(min_date <= !!end_date,
             min_date >= !!start_date) %>%
      filter(
        EVENT_TYPE == reactive_input_pred_event(),
        kpi == reactive_input_pred_kpi()
      ) %>%
      select(wk_flag)
    
    if(reactive_input_pred_window() == 1){
      
      list1 <- list.files(path = "pngs_pred_wk", pattern = "*.png", full.names = T)
      list2 <- list1[grepl(paste0("pngs_pred_wk/", reactive_input_pred_event(), "_"), list1)]
      list3 <- list2[grepl(paste0("pngs_pred_wk/", reactive_input_pred_event(), "_", reactive_input_pred_kpi(), "_"), list2)]
      list4 <- list3[grepl(paste0(week_vec$wk_flag, collapse = "|"), list3)]
      
      list4 %>%
        map(image_read) %>% # reads each path file
        image_join() %>% # joins image
        image_animate(fps=as.numeric(reactive_input_pred_speed())) %>% # animates, can opt for number of loops
        image_write("pred_outfile.gif")
      
      # Return a list containing the filename
      list(src = "pred_outfile.gif",
           contentType = 'image/gif'
      )
      
    } else if (reactive_input_pred_window() == 2) {
      
      list1 <- list.files(path = "pngs_pred_mon", pattern = "*.png", full.names = T)
      list2 <- list1[grepl(paste0("pngs_pred_mon/", reactive_input_pred_event(), "_"), list1)]
      list3 <- list2[grepl(paste0("pngs_pred_mon/", reactive_input_pred_event(), "_", reactive_input_pred_kpi(), "_"), list2)]
      list4 <- list3[grepl(paste0(month_vec$mon_flag, collapse = "|"), list3)]
      
      list4 %>%
        map(image_read) %>% # reads each path file
        image_join() %>% # joins image
        image_animate(fps=as.numeric(reactive_input_pred_speed())) %>% # animates, can opt for number of loops
        image_write("pred_outfile.gif")
      
      # Return a list containing the filename
      list(src = "pred_outfile.gif",
           contentType = 'image/gif'
      )
      
    } else {
      
      list1 <- list.files(path = "pngs_pred_yr", pattern = "*.png", full.names = T)
      list2 <- list1[grepl(paste0("pngs_pred_yr/", reactive_input_pred_event(), "_"), list1)]
      list3 <- list2[grepl(paste0("pngs_pred_yr/", reactive_input_pred_event(), "_", reactive_input_pred_kpi(), "_"), list2)]
      list4 <- list3[grepl(paste0(year(start_date) : year(end_date), collapse = "|"), list3)]
      
      list4 %>%
        map(image_read) %>% # reads each path file
        image_join() %>% # joins image
        image_animate(fps=as.numeric(reactive_input_pred_speed())) %>% # animates, can opt for number of loops
        image_write("pred_outfile.gif")
      
      # Return a list containing the filename
      list(src = "pred_outfile.gif",
           contentType = 'image/gif'
      )
      
    }
    
  })
  
}

# The Shinny
shinyApp(ui, server)
