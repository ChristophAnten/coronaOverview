# coronaOverview

A simple App showing total daily new corona cases and deaths of the selected countries. 
The displayed numbers can be adjusted by averaging over several days. 
Data is downloaded directly from the ecdc.

To run the code on your desktop:
```
 library(shiny)    
 runGitHub("coronaOverview", "ChristophAnten") 
```

The following packages are mandatory and can be installed via:
```
install.packages(c("utils", "plyr", "dplyr", "tidyr", 
                   "ggpubr", "ggplot2", "grid", "gridExtra",
                   "plotly", "shiny", "stringr"))

```
