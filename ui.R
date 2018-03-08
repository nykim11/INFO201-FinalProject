source('setup.R')

source('ui/ui_home.R')
source('ui/ui1.R')
source('ui/ui2.R')
source('ui/ui3.R')
source('ui/ui4.R')

ui <- navbarPage("Hurricanes in North America", 
                 tabPanel.home, 
                 tabPanel('Tracks on the Map', ui1), 
                 tabPanel('Origin vs. Maximum Speed', ui2), 
                 tabPanel('Speeds and Intensities', ui3), 
                 tabPanel('Direction of Travel', ui4)
                 )

shinyUI(ui)