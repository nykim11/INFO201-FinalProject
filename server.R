source('setup.R')
source('q1server_setup.R')

server <- function(input, output) {
  
  ####################################################################
  ###  SERVER 1 ######  SERVER 1 ######  SERVER 1 ######  SERVER 1 ###
  ####################################################################
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot_dblclick1, {
    brush <- input$plot_brush1
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  data_filtered_q1 <- reactive({
    
    data.filtered.q1 <- hurricane %>% 
      filter(as.numeric(substr(as.character(Date), 1, 4)) >= input$year.choice1[1] &
               as.numeric(substr(as.character(Date), 1, 4)) <= input$year.choice1[2]) %>% 
      filter(as.numeric(substr(as.character(Date), 5, 6)) >= input$month.choice1[1] &
               as.numeric(substr(as.character(Date), 5, 6)) <= input$month.choice1[2])
    
    if (input$location1 == "Pacific") {
      data.filtered.q1 <- data.filtered.q1 %>% filter(area == "pacific")
    } else if (input$location1 == "Atlantic") {
      data.filtered.q1 <- data.filtered.q1 %>% filter(area == "atlantic")
    }
    
    if (!is.null(data.filtered.q1) & nrow(data.filtered.q1) != 0) {
      
      if (input$categories1 == "Tropical Depression") {
        data.filtered.q1 <- data.filtered.q1 %>% filter(Total.Max.Wind <= 33)
      } else if (input$categories1 == "Tropical Storm") {
        data.filtered.q1 <- data.filtered.q1 %>% filter(Total.Max.Wind >= 34 & Total.Max.Wind <= 63)
      } else if (input$categories1 == "Category One") {
        data.filtered.q1 <- data.filtered.q1 %>% filter(Total.Max.Wind >= 64 & Total.Max.Wind <= 82)
      } else if (input$categories1 == "Category Two") {
        data.filtered.q1 <- data.filtered.q1 %>% filter(Total.Max.Wind >= 83 & Total.Max.Wind <= 95)
      } else if (input$categories1 == "Category Three") {
        data.filtered.q1 <- data.filtered.q1 %>% filter(Total.Max.Wind >= 96 & Total.Max.Wind <= 112)
      } else if (input$categories1 == "Category Four") {
        data.filtered.q1 <- data.filtered.q1 %>% filter(Total.Max.Wind >= 113 & Total.Max.Wind <= 136)
      } else if (input$categories1 == "Category Five") {
        data.filtered.q1 <- data.filtered.q1 %>% filter(Total.Max.Wind >= 137)
      }
    }
    
    
    if (!is.null(ranges$x) & !is.null(ranges$y)) {
      test <- test %>% filter(Longitude >= ranges$x[1] & Longitude <= ranges$x[2] &
                                Latitude >= ranges$y[1] & Latitude <= ranges$y[2])
      if (!is.null(data.filtered.q1) & nrow(data.filtered.q1) != 0) {
        data.filtered.q1 <- data.filtered.q1 %>% filter(Longitude >= ranges$x[1] & Longitude <= ranges$x[2] &
                                                          Latitude >= ranges$y[1] & Latitude <= ranges$y[2])
      }
    }
    
    
    return(data.filtered.q1)
  })
  
  output$plot1 <- renderPlot({
    
    filtered <- data_filtered_q1()
    
    if (nrow(filtered) != 0) {
      p <- ggplot() +
        geom_polygon(data = test, aes(x = Longitude, y = Latitude, group = group), fill = "gray70") +
        geom_point(data = filtered, aes(x = Longitude, y = Latitude, color = ID, size = Maximum.Wind), show.legend=F) +
        theme(panel.background = element_rect(fill = "white", colour = "white")) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        scale_size_continuous(range = c(1,3)) +
        coord_quickmap(xlim = ranges$x, ylim = ranges$y) 
      
    } else {
      p <- ggplot() +
        geom_polygon(data = test, aes(x = Longitude, y = Latitude, group = group), fill = "gray70") +
        theme(panel.background = element_rect(fill = "white", colour = "white")) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        coord_quickmap(xlim = ranges$x, ylim = ranges$y) 
    }
    
    return(p)
    
  })
  
  output$hover_info1 <- renderUI({
    hover <- input$plot_hover1
    if (nrow(data_filtered_q1()) != 0) {
      point <- nearPoints(data_filtered_q1(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    } else {
      return(NULL)
    }
    
    if (nrow(point) == 0) {
      return(NULL)
    }
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    if (point$Total.Max.Wind <= 33) {
      category <- "Tropical Depression"
    } else if (point$Total.Max.Wind >= 34 & point$Total.Max.Wind <= 63) {
      category <- "Tropical Storm"
    } else if (point$Total.Max.Wind >= 64 & point$Total.Max.Wind <= 82) {
      category <- "Category One"
    } else if (point$Total.Max.Wind >= 83 & point$Total.Max.Wind <= 95) {
      category <- "Category Two"
    } else if (point$Total.Max.Wind >= 96 & point$Total.Max.Wind <= 112) {
      category <- "Category Three"
    } else if (point$Total.Max.Wind >= 113 & point$Total.Max.Wind <= 136) {
      category <- "Category Four"
    } else if (point$Total.Max.Wind >= 137) {
      category <- "Category Five"
    }
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b>", "Name", ": </b>", point$Name))),
      p(HTML(paste0("<b>", "Category", ": </b>", category))),
      p(HTML(paste0("<b>", "Wind Speed", ": </b>", point$Maximum.Wind))),
      p(HTML(paste0("<b>", "Date", ": </b>", point$Date))),
      p(HTML(paste0("<b>", "Time", ": </b>", point$Time)))
    )
  })
  
  ####################################################################
  ###  SERVER 2 ######  SERVER 2 ######  SERVER 2 ######  SERVER 2 ###
  ####################################################################
  
  rdata_q2 <- reactiveValues(x = NULL, y = NULL, 
                             storm.id = '', storm.name = '', 
                             start.date = '', end.date = '', 
                             max.speed = NULL, max.speed.date = '')
  
  sLocMaxWind.df <- reactive({
    if(input$region_q2 == 'Atlantic') {
      atlantic.slocmaxwind
    } else if(input$region_q2 == 'Pacific') {
      pacific.slocmaxwind
    } else {
      rbind(atlantic.slocmaxwind, pacific.slocmaxwind)
    }
  })
  
  storm.track.df <- reactive({
    if(nchar(rdata_q2$storm.id) > 0) {
      temp <- all.maxwind
      
      temp <- temp %>% 
        filter(ID == rdata_q2$storm.id) %>% 
        select(ID, Name, Date, Time, Latitude, Longitude, Maximum.Wind)
      
      rdata_q2$storm.name <- unique(temp$Name)
      start <- temp$Date[1]
      end <- temp$Date[nrow(temp)]
      rdata_q2$start.date <- paste0(substr(start, 1, 4), '/', substr(start, 5, 6), '/', substr(start, 7, 8))
      rdata_q2$end.date <- paste0(substr(end, 1, 4), '/', substr(end, 5, 6), '/', substr(end, 7, 8))
      rdata_q2$max.speed <- max(temp$Maximum.Wind)
      msdate <- filter(temp, Maximum.Wind == rdata_q2$max.speed)$Date[1]
      rdata_q2$max.speed.date <- paste0(substr(msdate, 1, 4), '/', substr(msdate, 5, 6), '/', substr(msdate, 7, 8))
      
      temp
    }
  })
  
  output$yearSlider_q2 <- renderUI({
    years <- sLocMaxWind.df()$Date
    years <- as.integer(substr(years, 1, 4))
    
    low <- min(years)
    high <- max(years)
    
    sliderInput('yearSlider_q2', 'Year(s)', 
                low, high, value = c(low, high))
  })
  
  output$maxspeedSlider_q2 <- renderUI({
    max.wind <- sLocMaxWind.df()$Maximum.Wind
    
    low <- min(max.wind)
    high <- max(max.wind)
    
    sliderInput('maxspeedSlider_q2', 'Maximum Wind Speed', 
                low, high, value = c(low, high))
  })
  
  output$latSlider_q2 <- renderUI({
    lat <- sLocMaxWind.df()$Latitude
    
    low <- min(lat)
    high <- max(lat)
    
    sliderInput('latSlider_q2', 'Latitude', 
                low, high, value = c(low, high), 
                step = 0.1, round = -1)
  })
  
  output$longSlider_q2 <- renderUI({
    long <- sLocMaxWind.df()$Longitude
    
    low <- min(long)
    high <- max(long)
    
    sliderInput('longSlider_q2', 'Longitude', 
                low, high, value = c(low, high), 
                step = 0.1, round = -1)
  })
  
  maxWind.map <- reactive({
    plt <- ggplot() + 
      theme(panel.background = element_rect(fill = 'white', color = 'white'))
    
    if(!is.null(input$yearSlider_q2)) {
      values <- sLocMaxWind.df() %>% 
        filter(as.integer(substr(Date, 1, 4)) >= input$yearSlider_q2[1], 
               as.integer(substr(Date, 1, 4)) <= input$yearSlider_q2[2], 
               Maximum.Wind >= input$maxspeedSlider_q2[1], 
               Maximum.Wind <= input$maxspeedSlider_q2[2], 
               Latitude >= input$latSlider_q2[1], 
               Latitude <= input$latSlider_q2[2], 
               Longitude >= input$longSlider_q2[1], 
               Longitude <= input$longSlider_q2[2])
      
      values$interval <- cut(values$Maximum.Wind, 
                             breaks = seq(0, max(values$Maximum.Wind), 
                                          max(values$Maximum.Wind) / 6))
      
      region.map <- world %>% 
        filter(long >= min(values$Longitude) - 50, 
               long <= max(values$Longitude) + 50, 
               lat >= min(values$Latitude) - 50, 
               lat <= max(values$Latitude) + 50)
      
      plt <- ggplot() + 
        geom_polygon(data = region.map, aes(x = long, y = lat, group = group), fill = "gray70") + 
        geom_point(aes(values$Longitude, values$Latitude, 
                       color = values$interval), size = 3) + 
        labs(x = 'Longitude', y = 'Latitude', color = 'Max Wind Speed') + 
        scale_colour_brewer(palette = 1) + 
        theme(panel.background = element_rect(fill = 'white', color = 'white'), 
              legend.key = element_blank()) + 
        guides(color = guide_legend(override.aes = list(size = 10))) + 
        coord_quickmap(xlim = rdata_q2$x, ylim = rdata_q2$y)
    }
    
    plt
  })
  
  stormTrack.map <- reactive({
    plt <- ggplot() + 
      theme(panel.background = element_rect(fill = 'white', color = 'white'))
    
    if(nchar(rdata_q2$storm.id) > 0) {
      values <- storm.track.df()
      
      region.map <- world %>% 
        filter(long >= min(values$Longitude) - 50, 
               long <= max(values$Longitude) + 50, 
               lat >= min(values$Latitude) - 50, 
               lat <= max(values$Latitude) + 50)
      
      plt <- ggplot() + 
        geom_polygon(data = region.map, aes(x = long, y = lat, group = group), fill = "gray70") + 
        geom_point(aes(values$Longitude, values$Latitude, 
                       color = values$Date), size = 3) + 
        labs(x = 'Longitude', y = 'Latitude') + 
        theme(panel.background = element_rect(fill = 'white', color = 'white'), 
              legend.position = 'None') + 
        coord_quickmap()
      
      plt
    } 
    
    plt
  })
  
  output$main_map_q2 <- renderPlot({
    maxWind.map()
  })
  
  output$mapdesc_q2 <- renderText({
    paste0('The map above shows the starting locations of recorded storms in the ', input$region_q2, 
           ',  each color-coded by the maximum sustained wind speeds of each respective storm. Darker blues ', 
           'indicate higher maximum sustained wind speeds. Included above are storms that occured between ', 
           input$yearSlider_q2[1], ' and ', input$yearSlider_q2[2], ', at latitudes ', input$latSlider_q2[1], 
           ' degrees to ', input$latSlider_q2[2], ' degrees and longtidudes ', input$longSlider_q2[1], 
           ' degrees to ', input$longSlider_q2[2], ' degrees. Maximum wind speeds are filtered between ', 
           input$maxspeedSlider_q2[1], ' and ', input$maxspeedSlider_q2[2], ' knots.')
  })
  
  output$trackdesc_q2 <- renderText({
    if((nchar(rdata_q2$storm.id) > 0)) {
      paste0('Below is the track for storm of ID ', rdata_q2$storm.id, ', also known as ', rdata_q2$storm.name, 
             '. It started on ', rdata_q2$start.date, ' and ended on ', rdata_q2$end.date, ', reaching its ', 
             'maximum sustained wind speed of ', rdata_q2$max.speed, ' knots on ', rdata_q2$max.speed.date, 
             '. Its path from the darker blues and advances towards the lighter blues.')
    } 
  })
  
  output$track_q2 <- renderPlot({
    stormTrack.map()
  })
  
  observeEvent(input$plot_dblclick2, {
    brush <- input$plot_brush2
    if (!is.null(brush)) {
      rdata_q2$x <- c(brush$xmin, brush$xmax)
      rdata_q2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rdata_q2$x <- NULL
      rdata_q2$y <- NULL
    }
  })
  
  observeEvent(input$plot_click2, {
    selected <- nearPoints(sLocMaxWind.df(), input$plot_click2, 
                           xvar = 'Longitude', yvar = 'Latitude')
    
    if(nrow(selected) > 0) {
      rdata_q2$storm.id <- head(selected, 1)$ID
    } else {
      rdata_q2$storm.id <- ''
    }
  })
  
  ####################################################################
  ###  SERVER 3 ######  SERVER 3 ######  SERVER 3 ######  SERVER 3 ###
  ####################################################################
  
  final.data <- both.data
  
  # Choosing the storm based on the distinct ID number
  output$storm_id_q3 <- renderUI({
    if (input$radio_q3 == 'atlantic') {
      final.data <- atlantic.data
    } else if (input$radio_q3 == 'pacific') {
      final.data <- pacific.data
    } else {
      final.data <- both.data
    }
    all.storm.id <- unique(final.data$ID)
    selectInput('storm_id_q3', 'Storm ID', all.storm.id)
  })
  
  max.winds.q3.plot <- reactive({
    if (!is.null(input$storm_id_q3)) {
      # Max Winds vs. Time
      plt.df <- final.data %>%
        filter(ID == input$storm_id_q3)
      
      all.names <- (final.data %>%
                      group_by(ID) %>%
                      summarize(Name = max(Name)))$Name
      
      storm.name <- (final.data %>%
                       filter(ID == input$storm_id_q3))$Name[1]
      
      ggplot(plt.df,
             aes(
               x = Proper.Date,
               y = Maximum.Wind,
               color = Time,
               size = 3
             )) + geom_point() +
        ggtitle(paste('Maximum Winds for', storm.name)) +
        labs(x = 'Date', y = 'Maximum Winds (mph)') +
        theme(plot.title = element_text(size = 22))
    } else {
      ggplot() +
        theme(panel.background = element_rect(fill = 'white', color = 'white'))
    }
  })
  
  # Graph 1
  output$maxwinds_q3 <- renderPlot({
    max.winds.q3.plot()
  })
  
  # Graph 2
  output$intensity_q3 <- renderPlot({
    # Intensity of storm
    plt.df <- final.data %>%
      filter(ID == input$storm_id_q3)
    
    storm.name <- (final.data %>%
                     filter(ID == input$storm_id_q3))$Name[1]
    
    ggplot(plt.df, aes(x = Proper.Date, y = Status, size = 3)) +
      geom_jitter() +
      ggtitle(paste("Intensity of Storms", storm.name)) +
      labs(x = "Date", y = "Intensity") +
      theme(plot.title = element_text(size = 22))
  })
  
  output$desc.table_q3 <- renderTable({
    table <- storm.status.table[order(storm.intensity.size),]
    table
  })
  
  ####################################################################
  ###  SERVER 4 ######  SERVER 4 ######  SERVER 4 ######  SERVER 4 ###
  ####################################################################
  
  # Reactive data frame that will be used for the bar graph. Filters out
  # depending on region, and direction
  q5.df <- reactive ({
    temp <- rbind(atlantic.full.q5, pacific.full.q5)
    if (input$region == "Atlantic") {
      temp <- atlantic.full.q5
    } else if (input$region == "Pacific") {
      temp <- pacific.full.q5
    }
    
    dir.filter <- input$direction
    temp <- temp %>%
      filter(Direction %in% dir.filter)
    
    temp
  })
  
  # Reactive data frame to be displayed on the table below the bar graph
  # for information regarding the mean and median maximum wind speed alongside
  # the number of storms in each direction
  stat.direction <- reactive ({
    temp <- q5.df() %>% 
      group_by(Direction) %>% 
      summarize(Mean = mean(Maximum.Wind), Median = median(Maximum.Wind), Count = n()) %>% 
      arrange(desc(Mean))
  })
  
  # Outputs the bar graph
  output$bar_graph <- renderPlot({
    ggplot(data=q5.df(), aes(x=Maximum.Wind, group=Direction)) +
      geom_bar(aes(fill=Direction), position = "dodge", width = 5) + 
      scale_fill_brewer(palette = "RdYlGn")
  })
  
  # Outputs the table
  output$table <- renderTable({
    stat.direction()
  })
  
  # Analysis text for the bar graph and table
  output$infop1.q5 <- renderText({
    "Based on the starting point and ending point location of every storm, the angle between those points are 
    calculated and classified as one of the eight directions commonly found in compasses (which are equally 
    distributed from an angle of a full circle). In the cases where starting point and ending point are exactly 
    the same, the direction is classified to be South (since there were just a handful of storms that \"did not
    move\")."
  })
  
  output$infop2.q5 <- renderText({
    "The bar graph above shows a distribution of storms and their directions over their maximum wind speeds. Winds 
    with the direction of West and Northwest seems to be the most common direction for storms out of all the other 
    directions and peaking around similar maximum wind speeds of 60-70 knots for the storms in Pacific, Atlantic 
    and both regions (All)."
  })
  
  output$infop3.q5 <- renderText({
    "Storms with directions of North and Northeast seem to differ depending on the region selected. With the high 
    number of storms being a part of the data for these two directions, the table above, which displays the 
    mean and median of the maximum wind speed alongside the number of storms, indicates that higher wind speeds do 
    affect the direction of storms in the Atlantic - maximum wind speeds of above 70 knots would likely point towards 
    North/Northeast, and a bit below that would point West/Northwest. This is a different story in the Pacific region, 
    as storms going in the direction of North/Northeast are significantly lower, and have a significantly lower mean
    maximum wind speed."
  })
  
  output$infop4.q5 <- renderText({
    "Looking at all the storms, barely any storms are pointing towards the Southern direction. Even the Pacific storms 
    do not have any storms recorded as point towards the Southeast, while the Atlantic region has a handful relative 
    to other directions."
  })
  
}

shinyServer(server)