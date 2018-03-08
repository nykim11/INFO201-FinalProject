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
      
      p(HTML(paste0("<h4> This map shows hurricanes in North America, 
                    classified by region (Pacific and Atlantic). Each hurricane 
                    is differentiated by color. The size of each dot is 
                    propotional to wind speed. You can zoom in by diagonally 
                    dragging over the desired part and double-clicking it. 
                    Doubleclick again anywhere to reset the zoom. Also, you can 
                    hover over individual dots for specific details. </h4>"))),
      
      div(
        style = "position:relative",
        plotOutput("plot1", 
                   hover = hoverOpts("plot_hover1", delay = 1, delayType = "debounce"),
                   click = "plot_click1", dblclick = "plot_dblclick1", 
                   brush = brushOpts(id = "plot_brush1", resetOnNew = TRUE)
        ), 
        
        uiOutput("hover_info1")
      ),
      
      p(HTML(paste0("<h4> The map above allows the visualizing of which areas 
                    storms most often travel over. Storms can be filtered by year 
                    and month to check traveled regions during certain periods of 
                    time. In addition, they can also be filtered by category of 
                    intensity. </h4>")))
      
      )
    )
  ) 
