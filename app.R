library(shiny)
library(tidyverse)
library(zoo)
library(readxl)
library(lubridate)
library(gganimate)
library(ggmap)
library(ggrepel)
library(shinycssloaders)
library(gifski)

## adding runner data

winner <- data.frame(runners = rep("wnr", each = 15),
                     km = as.numeric(rep(1:15, 1)),
                     time = as.character(c("00:02:45", "00:05:28",	"00:08:19",	"00:11:08",	"00:14:07",	"00:16:53",	"00:19:37",	"00:22:30",
                                           "00:25:17", "00:27:49",	"00:30:38",	"00:33:19",	"00:35:54",	"00:38:28",	"00:41:05")))

avarage <- data.frame(runners = rep("avg", each = 15),
                      km = as.numeric(rep(1:15, 1)),
                      time = as.character(c("00:05:07", "00:10:16",	"00:15:36",	"00:20:52",	"00:26:15",	"00:31:42",	"00:37:01",	"00:42:32",
                                            "00:47:58", "00:53:09",	"00:58:37",	"01:04:07",	"01:09:13",	"01:14:20",	"01:19:11")))

slowest <- data.frame(runners = rep("slw", each = 15),
                      km = as.numeric(rep(1:15, 1)),
                      time = as.character(c("00:07:01", "00:14:44",	"00:23:41",	"00:31:52",	"00:41:11",	"00:50:04",	"00:58:35",	"01:08:51",
                                            "01:19:40", "01:28:18",	"01:38:12",	"01:47:15",	"01:56:27",	"02:05:59",	"02:15:20")))


theme_set(theme_bw())

ui <- fluidPage(
  titlePanel("7-hills run app", windowTitle = "7-hills run app"),
  sidebarPanel(
    tags$head(
      tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: left; } 
                 #inline .form-group { display: table-row;}")
      ),
    tags$style(HTML(".shiny-input-container:not(.shiny-input-container-inline) {width: 100%;}")),
    tags$h4("copy and paste your km times below;"),
    tags$h5("including all the text"),
    tags$h5("or type the times, each km time on a new line:"),
    textAreaInput(label = "", inputId = "kmtimes", width = "250px", height = "320px",
                  placeholder =  "00:02:45
                  00:05:28
                  00:08:19
                  00:11:08
                  00:14:07
                  00:16:53
                  00:19:37
                  00:22:30
                  00:25:17
                  00:27:49
                  00:30:38
                  00:33:19
                  00:35:54
                  00:38:28
                  00:41:05"),
    selectInput("opponent", "select an opponent:",
                c("'18 winner" = "wnr", "'18 avarage runner" = "avg", "'18 slowest runner" = "slw", "all of them!" = "all")),
    tags$h5("more frames is longer animation but longer render time"),
    selectInput("frames", "select number of frames for the animation:",
                c("100", "150", "200", "250", "300", "400"), selected = "200"),
    actionButton("run", "animate my run!")
    ),
  imageOutput("plot1"),
  tableOutput("text1"),
  plotOutput("bar"))

server <- function(input, output) {
  
  ## input times processing
  user_data <- eventReactive(input$run, {
    text <- input$kmtimes
    text <- strsplit(text, "\n") %>% 
      as.data.frame()
    
    names(text) <- c("time")
    
    text <- text %>% 
      mutate(time = as.character(time),
             time = str_extract(time, "[0-9]+:[0-9]+"),
             time = str_pad(time, 8, side = "left", pad = "0"),
             time = ifelse(str_detect(time, "000") == T, str_replace(time, "000", "00:" ), time),
             runners = "you",
             km = row_number())
    
  })
  
  ## construct filter for animation
  filter_runners <- eventReactive(input$run,{
    if(input$opponent == "wnr"){
      filter_runners <- c("wnr", "you")
    }
    if(input$opponent == "avg"){
      filter_runners <- c("avg", "you")
    }
    if(input$opponent == "slw"){
      filter_runners <- c("slw", "you")
    }
    if(input$opponent == "all"){
      filter_runners <- c("wnr", "avg", "slw", "you")
    }
    
    return(filter_runners)
    
    
  })
  
  
  ## amount of frames
  nr_frames <- eventReactive(input$run,{
    as.numeric(input$frames)
  })
  
  output$plot1 <-  renderImage({
    withProgress(message = "Making the animation. This takes a while... Time to get some coffee or pet your dog =) !", {
    

      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')
      
      ## add user input to the gif
      times <- bind_rows(winner, avarage, slowest, user_data()) %>% 
        # filter(runners %in% filter_runners()) %>% 
        mutate(time = period_to_seconds(hms(time))) %>% 
        filter(runners %in% filter_runners())
      
      ##routecoordinaten
      route <- read.csv2("data/zevenheuvelenpointsm.csv", stringsAsFactors = FALSE)
      ##cumulative distances per point
      route <- route %>% 
        mutate(x = 15000/(nrow(route)-1),
               ##cumsum(x)-x because the first point is the start at 0 meter 
               meter = cumsum(x)-x) %>% 
        select(X, Y, meter)
      ##closest points each km to join times later
      route$km <- NA  
      
      for (i in seq(0, 15000, by = 1000)) {
        route$km[which(abs(route$meter-i) == min(abs(route$meter-i)))] <- i / 1000
      }
      
      # make a route each runner to fill in the times in between later
      # 2x join because of 'cannot allocate vector of size..' error
      route_times <- times %>% 
        full_join(route %>% select(meter, km), by = "km") %>% 
        complete(runners, meter) %>% 
        left_join(route %>% select(-km), by = "meter") %>% 
        mutate(km = if_else(meter == 0 , 1, km),
               time = if_else(meter == 0 , period_to_seconds(hms("00:00:00")), time)) %>% 
        filter(!is.na(runners)) %>% 
        ## fill in amount of seconds run so far for each point on the route.
        group_by(runners) %>% 
        mutate(sec = na.approx(time)) %>% 
        fill(km, .direction = "up") %>% 
        ungroup() %>% 
        mutate(X = as.numeric(as.character(X)),
               Y = as.numeric(as.character(Y)))
      
      ## calculate the closes position of the runner every 5 seconds. So runners progress in 
      ## even timeframes for easier animations
      
      route_times <-  route_times %>% 
        mutate(sec15 = ceiling(sec / 15) * 15)
      
      
      ## calculate avarage speed each km for labeling
      avg_speed <- route_times %>% 
        group_by(runners, km) %>% 
        summarise(sec_tot_prev = max(sec)) %>% 
        mutate(km = km + 1) %>% 
        right_join(route_times %>% group_by(runners, km) %>% 
                     summarise(sec_tot_km = max(sec)), 
                   by = c("runners", "km")) %>% 
        mutate(avg_speed = if_else(km == 1, round(3600 / sec_tot_km, 1),
                                   round(3600 / (sec_tot_km - sec_tot_prev), 1))) %>%   
        select(runners, km, avg_speed)
      
      ## finishtime for labeling
      finish_time <- route_times %>% 
        group_by(runners) %>% 
        filter(meter == max(meter)) %>% 
        ungroup() %>% 
        mutate(finishtime = seconds_to_period(time)) %>% 
        select(runners, finishtime)
      
      
      ## select the point where the runners are each 5 seconds
      route_times_15sec <- route_times %>%
        group_by(sec15, runners) %>% 
        summarise_all(.funs = first) %>% 
        ## add the labels
        left_join(avg_speed, by = c("runners", "km")) %>% 
        left_join(finish_time, by = "runners") %>% 
        ungroup() %>% 
        mutate(label_t = paste0(runners, ": ", finishtime, " -- ", avg_speed, " km/h"),
               label_time = seconds_to_period(sec15),
               label_time = paste0("race time: ", label_time)) %>% 
        select(-time)
      
      load(file = "data/my_map.RData")
      
      ## race time for labelling
      
      slwstrnr <- finish_time %>% 
        arrange(desc(finishtime)) %>% 
        head(1) %>% 
        select(runners) %>% 
        unlist()
      
      race_time <- route_times_15sec %>% 
        filter(runners == slwstrnr)
      
      ## plotting
      
      p <- ggmap(m) +
        scale_x_continuous(limits = c(min(route_times$X), max(route_times$X)), expand = c(0.0015, 0.0015)) +
        scale_y_continuous(limits = c(min(route_times$Y), max(route_times$Y)), expand = c(0.0015, 0.0015)) +
        geom_point(data = route_times_15sec, aes(x = X, y = Y,
                                                 color = as.factor(runners)), size = 5)+
        geom_label_repel(data = route_times_15sec,
                         aes(x = X, y = Y,
                             fill = as.factor(runners),
                             label = label_t),
                         size = 4, fontface = "bold",
                         box.padding = 0.35, point.padding = 1,
                         nudge_y = 0.0025, nudge_x = 0.02) +
        geom_label(data = race_time,
                   aes(x =  5.867, y = 51.79665,label = label_time),
                   size = 4.5, color = "black", fill = " white", fontface = "bold", hjust = "left") +
        theme_void() +
        theme(legend.position = "none") +
        # gg animate options
        transition_reveal(sec15) +
        ease_aes("linear")
      
      anim_save("outfile.gif", animate(p, nframes = nr_frames(), width = 400, height = 400))
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif')
           
        
      })
      }, deleteFile = TRUE)
  
}

shinyApp(ui, server)