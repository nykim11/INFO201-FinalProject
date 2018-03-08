ui_home <- fluidPage(
  titlePanel('Hurricanes in the Atlantic and Pacific 1851-2015'),
  p(HTML(paste0('<h4> INFO 201 (Winter 2018) Group AC5: <br/> 
                Anushree Gopal, Clifford Silfanus, Namyoung Kim, Vincent Widjaya </h4>'))), 
  
  br(), 
  
  p('For our final team project, we decided to analyse Pacific and Atlantic oceans storm data spanning from 1851-2015.
    The data set that we will be using is a tropical cyclone history database in a format known as HURDAT 
    (HURricane DATabase). It was collected by the National Hurricane Center and Central Pacific Hurricane 
    Center sourced by Kaggle [1]. It consists of data from post-storm analyses, that may not have been available in 
    real time, of each tropical cyclone’s location, wind, and pressure in the North Pacific Ocean. This data set 
    is ongoing review of past analysis and updates appropriately to reflect any changes.'),
  
  p('The date and time of the analysis is of course included as well, in standard synoptic times of 0000, 0600, 1200, 
    and 1800 UTC. Some rows will be records of data taken at a time that is not one of those, and a record identifier 
    is given to show why it is relevant. The status of the tropical cyclone is provided to indicate its category of
    intensity.'),
  
  p(HTML("<a href = 'http://www.kaggle.com/noaa/hurricane-database'> [1] Source: 
         http://www.kaggle.com/noaa/hurricane-database.</a>")), 
  
  br(), 
  
  p(HTML(paste0("<h4> What questions are answered by this data? </h4>"))), 
  p(HTML(paste0('<ul>
                  <li>Which areas do storms often travel over, and how do their intensities change 
                      throughout their paths?</li>
                  <li>How does the starting location (longitude and latitude) of a storm affect its 
                      maximum sustained wind speed?</li>
                  <li>How does the wind speed of a storm change as time passes, and after what length 
                      of time does a storm usually reach its maximum wind speed?</li>
                  <li>Is there a correlation between the time of the year that the storm occurs 
                      and its status (intensity)?</li>
                  <li>Is there a correlation between the maximum wind speed of a storm and the its track, 
                      as in the direction of its path?</li>
                </ul>'))), 
  
  br(), 
  
  p(HTML(paste0("<h4> What features of the available dataset are used? </h4>"))), 
  p(HTML(paste0('<ul>
                  <li>ID* & Name</li>
                  <li>Date & Time</li>
                  <li>Status (Intensity Category)</li>
                  <li>Latitude & Longitude</li>
                  <li>Maximum Sustained Wind Speed</li>
                </ul>'))), 
  p(HTML(paste0("* ID is composed of indicators of region, year, and storm number"))), 
  
  br(), 
  
  p(HTML(paste0("<h4> Who would benefit from this data? </h4>"))), 
  p(HTML(paste0('<ul>
                  <li><b>Meteorologists</b> study the atmosphere and forecast weather. They 
                      would benefit from this data visualization for an idea of storm patterns 
                      in North America.</li>
                  <li><b>Students</b> often encounter projects where they are free to decide 
                      the topic. Some might choose to research and write about storms!</li>
                </ul>'))), 
  
  br()
)

tabPanel.home <- tabPanel('Home', ui_home)
