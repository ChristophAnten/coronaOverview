# coronaOverview

A small shiny-App showing daily new corona cases and deaths of the selected countries and/or German counties/cities.
The displayed numbers can be adjusted by respective population and averaging over several days. 
Data is downloaded directly from the ECDC and RKI.

To run the code on your desktop:
```
 # This will also install a number of basic packages from CRAN which are necessary for this app to run.
 library(shiny)    
 runGitHub("coronaOverview", "ChristophAnten") 
```
