ui4 <- fluidPage(
  
  titlePanel("How does maximum wind speed affect the track of storms?"), 
  plotOutput("bar_graph"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("region", "Region",
                  c("All", "Atlantic", "Pacific")),
      checkboxGroupInput("direction", "Direction",
                         c("East", "Northeast", "North", "Northwest", 
                           "West", "Southwest", "South", "Southeast"),
                         selected = c("East", "Northeast", "North", "Northwest", 
                                      "West", "Southwest", "South", "Southeast"))
    ),
    
    mainPanel(
      tableOutput("table")
    )
  ),
  textOutput("infop1.q5"),
  br(),
  textOutput("infop2.q5"),
  br(),
  textOutput("infop3.q5"),
  br(),
  textOutput("infop4.q5"),
  br()
  
)
