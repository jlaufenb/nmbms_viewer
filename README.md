# USFWS Disclaimer
The United States Fish and Wildlife Service (FWS) GitHub project code is provided on 
an "as is" basis and the user assumes responsibility for its use. FWS has relinquished 
control of the information and no longer has responsibility to protect the integrity, 
confidentiality, or availability of the information. Any reference to specific 
commercial products, processes, or services by service mark, trademark, manufacturer, 
or otherwise, does not constitute or imply their endorsement, recommendation or 
favoring by FWS. The FWS seal and logo shall not be used in any manner to imply 
endorsement of any commercial product or activity by FWS or the United States 
Government.

# Nearshore Marine Bird and Mammal Survey Viewer
An R shiny app for visualizing and summarizing distance sampling data. Users upload a RData file. These data are imported and a summary table is produced. 
Users can view a leaflet map of the dataset and subset the data by survey, transect, and date. 

## Instructions
`if (!require("shiny")) install.packages("shiny")`  
`shiny::runGitHub( "nmbms_viewer", "jlaufenb", launch.browser=T)`
