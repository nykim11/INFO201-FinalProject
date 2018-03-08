ui_home <- fluidPage(
  titlePanel('Final Project: Team 5'),
  p('Created by: Anushree Gopal, Clifford Silfanus, Namyoung Kim, Vincent Widjaya'),
  
  p('For our final team project, we decided to analyse Pacific and Atlantic oceans storm data spanning from 2014-2015.
    The data set that we will be using is a tropical cyclone history database in a format known as HURDAT 
    (HURricane DATabase). It was collected by the National Hurricane Center and Central Pacific Hurricane 
    Center sourced by Kaggle [1]. It consists of data from post-storm analyses, that may not have been available in 
    real time, of each tropical cyclone’s location, wind, and pressure in the North Pacific Ocean. This data set 
    is ongoing review of past analysis and updates appropriately to reflect any changes.'),
  
  p('The date and time of the analysis is of course included as well, in standard synoptic times of 0000, 0600, 1200, 
    and 1800 UTC. Some rows will be records of data taken at a time that is not one of those, and a record identifier 
    is given to show why it is relevant. The status of the tropical cyclone is provided to indicate its category of
    intensity.'),
  
  p(HTML("<a href = 'http://www.kaggle.com/noaa/hurricane-database'> [1] Source .</a>"))
)

tabPanel.home <- tabPanel('Home', ui_home)
