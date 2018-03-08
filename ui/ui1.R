ui1 <- fluidPage( 
  
  # make a sidebar layout
  sidebarLayout(
    
    # make a sidebar panel
    sidebarPanel(
      
      p(HTML(paste0("You can filter hurricanes by year, month, 
                    category, location, and direction."))),
      
      sliderInput("year.choice1", 
                  label = "Year", 
                  min = 2004, max = 2015, value = c(2015, 2015)),
      
      sliderInput("month.choice1", 
                  label = "Month", 
                  min = 1, max = 12, value = c(1, 12)),
      
      p(HTML(paste0("Hurricanes are classified 
                    into categories by their wind speed.
                    It is stronger as goes down the list."))),
      
      selectInput("categories1", 
                  label = "Category", 
                  choices = c("ALL", "Tropical Depression", "Tropical Storm", 
                              "Category One", "Category Two",
                              "Category Three", "Category Four",
                              "Category Five")),
      
      p(HTML(paste0("You can choose Northeast Pacific and North Atlantic hurricanes."))),
      
      selectInput("location1", label = "Location", 
                  choices = c("ALL", "Pacific", "Atlantic"))
      
      ),
    
    # make a main panel
    mainPanel(
      
      titlePanel("Hurricanes on the Map"),
      
      p(HTML(paste0("<h4>", "This map is showing hurricanes in the north america,
                    which is classified into pacific hurricanes and atlantic 
                    hurricanes. ", "<b>", "Each color", "</b>", " means the ",
                    "<b>", "same hurricane", "</b>", " with different time. The ",
                    "<b>", "size of dots", "</b>", " is propotional to ",
                    "<b>", "wind speed", "</b>", " of corresponding hurricanes. 
                    You can ", "<b>", "zoom in by dragging desired part to zoom 
                    in and doubleclicking", "</b>", " and just ", "<b>", "doubleclick 
                    anywhere to reset zooming", "</b>", ". Also, you can ", "<b>",
                    "get more information about that hurrican by hovering",
                    "</b>", "  your mouse pointer above the hurricane.", "</h4>"))),
      
      div(
        style = "position:relative",
        plotOutput("plot1", 
                   hover = hoverOpts("plot_hover1", delay = 1, delayType = "debounce"),
                   click = "plot_click1", dblclick = "plot_dblclick1", 
                   brush = brushOpts(id = "plot_brush1", resetOnNew = TRUE)
        ), 
        
        uiOutput("hover_info1")
      ),
      
      p(HTML(paste0("<h3>", "Analysis", "</h3>"))),
      
      p(HTML(paste0("<h4>", "One of our question is ", "<b>", 
                    "'Where is hurricane-affected area in the north america?'",
                    "</b>", ". We plot the hurricanes locations in the map so that
                    we can see which areas are affected. We can filter hurricanes:
                    ", "<b>", "(i) by years and months to check how affected areas are changed as
                    time passes", "</b>", ". ", "<b>", "(ii) by categories to check which ares have more
                    stronger or weaker hurricanes", "</b>", ".", "</h4>"))) 
      
      )
    )
  ) 