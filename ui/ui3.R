ui3 <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      # widget control. radio buttons to choose between pacific and atlantic
      radioButtons(
        'radio_q3',
        label = 'Storm Ocean',
        choices = list(
          'Atlantic Storms' = 'atlantic',
          'Pacific Storms' = 'pacific',
          'Both Storms' = 'both'
        )
      ),
      
      uiOutput('storm_id_q3'),
      hr()
    ),
    
    mainPanel(
      tabsetPanel(
        type = 'tabs',
        tabPanel(
          'Maximum Storm Winds',
          titlePanel('Maximum Winds'),
          p(
            'This graph shows the correlation between the duration of the storm and the time it reaches maximum winds.'
          ),
          plotOutput('maxwinds_q3'),
          p('In this graph, we can see the time period of the storm over the few days it takes place. Most storms last
            between 4-6 days. The x axis shows the Date of the storm and the y axis shows the maximum winds for the storm.
            The color scheme shows that the darker the plot, the earlier the associated wind takes place.'),
          p('The trend that most of these graphs follow is that as the storm occurs and time passes, the winds get stronger. 
            It is usually in Day 3 - 4 where the winds are at their highest and the storm is the strongest in terms of winds.'),
          p('The reason that the plots are placed on top of each other is because the data shows the time interval in 6 hours
            in 24 hour time (as shown on the side of the graph.'),
          p('In the Atlantic dataset, the maximum winds reached by a storm is 160 mph by Wilma and the lowest winds 
            reached at 10 mph. In the Pacific dataset, the maximum winds are reached by storm Patricia at 185 mph. 
            The lowest are reached by Elevent at 15 mph.')
          
        ),
        tabPanel(
          'Storm Intensity',
          titlePanel('Intensity'),
          p(
            'This graph shows the correlation between the time of the year that the storm takes place and its intensity.'
          ),
          plotOutput('intensity_q3'),
          tableOutput('desc.table_q3'),
          p('In this graph, we can see the status or the intensity of the storm. The number of plots are heavier in categories
            that the storm stays the longest. The general trends for this is that storms will start off in a lower category, 
            such as Low and then make its way to a higher intensity, maybe Hurricane and stay there for a day or two and then
            start making its way down the levels again.'),
          p('The x axis in this graph shows the Date and the y axis shows the Status of the storm. The table below the graph
            shows the intensity level and the Description of the storm status. This helps the user understand the key for the 
            graph and understand what the initials on the y axis mean.'),
          p('The most common categories that storms fell into were Tropical Storms and Hurricanes. The graph shows how 
            the instensity of the storm progresses throughout the duration.')
          )
        )
      )
    )
  )
