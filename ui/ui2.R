ui2 <- fluidPage(
  
  titlePanel("Starting Location vs. Maximum Sustained Wind Speed"), 
  
  div(
    style = 'position:relative', 
    plotOutput('main_map_q2', 
               hover = hoverOpts('plot_hover2', delay = 10, delayType = 'debounce'), 
               click = 'plot_click2', dblclick = 'plot_dblclick2', 
               brush = brushOpts(id = 'plot_brush2', resetOnNew = TRUE)
               )
    ), 
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput('region_q2', 'Region', 
                  c('Atlantic & Pacific', 'Atlantic', 'Pacific')), 
      uiOutput('yearSlider_q2'), 
      uiOutput('maxspeedSlider_q2'), 
      uiOutput('latSlider_q2'), 
      uiOutput('longSlider_q2')
    ), 
    
    mainPanel(
      textOutput('mapdesc_q2'), 
      br(), 
      textOutput('trackdesc_q2'), 
      plotOutput('track_q2')
      )
    
    )
  )
