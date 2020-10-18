library(utils)
library(plyr)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2)
library(grid)
library(gridExtra)
library(plotly)
library(shiny)

library(ABACUS)

ENV <- environment()

mean_dist <- function(x,dist){
    if (dist<1)
        return(x)
    n <- length(x)
    y <- x
    i=1
    for(i in 1:dist){
        y[1:(n-i)] <- y[1:(n-i)]+x[(1+i):n]
        y[(1+i):n] <- y[(1+i):n]+x[1:(n-i)]
    }
    div <- c(seq(1+dist,2*dist),rep(1+2*dist,n-(2*dist)),seq(2*dist,1+dist))
    return(y/div)
}
plotCovid <- function(countries,average = 3,asList=FALSE){
    workDat <- data$raw %>% dplyr::filter(countriesAndTerritories %in% countries) %>%
        group_by(countriesAndTerritories) %>%
        mutate(cases = ifelse(cases<0,0,cases),
               deaths =ifelse(deaths<0,0,deaths)) %>%
        mutate(cases_averaged = mean_dist(cases,average),
               deaths_averaged = mean_dist(deaths,average),
               cases_per_100k_pop = cases_averaged/popData2019*100000,
               deaths_per_100k_pop = deaths_averaged/popData2019*100000) %>% 
        mutate(dateRep = as.Date(dateRep,format = "%d/%m/%Y"))
    
    p1 <- workDat %>% 
        ggplot(aes(x=dateRep,y=cases_averaged,col=countriesAndTerritories)) +
        geom_line() +
        geom_point() +
        ylab(sprintf("abs. cases",average)) +
        theme_bw() +
        ggtitle("Absolute daily cases")
    p2 <- workDat %>% 
        ggplot(aes(x=dateRep,y=deaths_averaged,col=countriesAndTerritories)) +
        geom_line() +
        geom_point() +
        ylab(sprintf("abs. deaths",average)) +
        theme_bw()+
        ggtitle("Absolute daily deaths")
    p3 <- workDat %>% 
        ggplot(aes(x=dateRep,y=cases_per_100k_pop,col=countriesAndTerritories)) +
        geom_line() +
        geom_point() +
        ylab(sprintf("cases/100k pop. (mean over %i days)",average)) +
        theme_bw()+
        ggtitle("daily cases per 100.000 population")
    p4 <- workDat %>% 
        ggplot(aes(x=dateRep,y=deaths_per_100k_pop,col=countriesAndTerritories)) +
        geom_line() +
        geom_point() +
        ylab(sprintf("deaths/100k pop. (mean over %i days)",average)) +
        theme_bw()+
        ggtitle("daily deaths per 100.000 population")
    if (asList) {
        return(list(p1,p2,p3,p4))
    }
    pAll <- ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="top")
    return(pAll)
}
loadData <- function(from="local"){
    if (from=="ecdc"){
        data$from <- "ECDC"
        data$time <- Sys.time()
        data$raw <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                             na.strings = "")
    }
    if (from=="local")
        load(file="data.RData",envir = ENV)
    countryChoices <<- data$raw$countriesAndTerritories %>% unique()
}
saveData <- function(){
    save(data,file="data.RData")
}
data <- list()
loadData()

selectedCountries <- c(
    "Netherlands",
    "United_States_of_America",
    # "Spain",
    # "France",
    # "United_Kingdom",
    # "Italy",
    # "Sweden",
    # "Czechia",
    "Germany"
    
)#[c(1,4,7)]

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Corona overview"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textOutput("dataInfoOrigin"),
            textOutput("dataInfoTime"),
            actionButton("download","Load newest data",icon=icon("download")),
            selectInput("countries","Select Countries",multiple = TRUE,choices = countryChoices,
                        selected = selectedCountries),
            checkboxInput("relative","adjusted to total poulation",value = TRUE),
            numericInput("smooth","average over days",value=3,min=0,step=1)#,
            # uiOutput("legend")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distPlotCases"),
            plotlyOutput("distPlotDeaths")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    res <- reactiveValues()
    
    output$dataInfoOrigin <- renderText({paste("Data from:",data$from)})
    
    output$dataInfoTime <- renderText({paste("Last update at:",data$time)})
    
    observeEvent(input$download,{
        print("--> load data start")
        loadData("ecdc")
        print("--> load data end")
    })
    
    toListen <- reactive({
        list(input$countries,input$smooth)
    })
    observeEvent(toListen(),{
        res$plots <- plotCovid(input$countries,input$smooth,T)
    })
    
    output$distPlotCases <- renderPlotly({
        # hide_legend(res$plots[[1]])
        if (!input$relative)
            res$plots[[1]]
        else
            res$plots[[3]]
    })
    output$distPlotDeaths <- renderPlotly({
        # hide_legend(res$plots[[2]])
        if (!input$relative)
            res$plots[[2]]
        else
            res$plots[[4]]
    })

    # output$legend <- renderUI({
    #     col <- scales::hue_pal()(length(input$countries))
    #     lapply(1:length(col), function(i) {
    #         div(
    #             div(
    #                 style = paste("background-color:",col[i],";margin:2px;width:10px;height:5px")
    #             ),
    #             div(
    #                 (input$countries[i]),
    #                 style = "padding:5px;width:80%"
    #             ),
    #             style ="display:flex"
    #         )
    #     })
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
