library(shiny)    
runGitHub("coronaOverview", "ChristophAnten") 

mandatory_packages <- c("utils", "plyr", "dplyr", "tidyr", 
                        "ggpubr", "ggplot2", "grid", "gridExtra",
                        "plotly", "shiny", "stringr")
for (mp in mandatory_packages){
  if(!require(mp)){
    install.packages(mp)
  }
}
