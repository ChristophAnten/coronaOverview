###############################################################################
mandatory_packages <- c("utils", "plyr", "dplyr", "tidyr", 
                        "ggpubr", "ggplot2", "grid", "gridExtra",
                        "plotly", "shiny", "stringr",
                        "data.table","bit64")
for (mp in mandatory_packages){
    if(!require(mp,character.only = TRUE)){
        install.packages(mp,dep=TRUE)
    }
}
###############################################################################
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
library(stringr)
library(bit64)
library(data.table)

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
gen_workDat <- function(){
    ecdc_world <- data$raw$ecdc %>%
        mutate(data = "ecdc") %>% 
        dplyr::select(dateRep,cases,deaths,countriesAndTerritories,popData2019,data) %>% 
        mutate(dateRep = as.Date(dateRep,format = "%d/%m/%Y"))
    
    rki_bundesland <- data$raw$rki %>%
        rename(dateRep = Refdatum) %>%
        group_by(Bundesland,dateRep,.drop=FALSE) %>%
        summarize(cases = sum(AnzahlFall),
                  deaths = sum(AnzahlTodesfall),
                  popData2019 = mean(EWZ_BL)) %>%
        ungroup()  %>%
        mutate(data = "rki",
               countriesAndTerritories = paste(Bundesland)) %>%
        dplyr::select(dateRep,cases,deaths,countriesAndTerritories,popData2019,data) %>% 
        mutate(dateRep = as.Date(dateRep,format = "%Y/%m/%d"))
    
    
    rki_landkreis <- data$raw$rki %>% 
        rename(dateRep = Refdatum) %>%
        mutate(Landkreis = paste(Bundesland,"-",Landkreis)) %>% 
        group_by(Landkreis,dateRep) %>%
        summarize(cases = sum(AnzahlFall),
                  deaths = sum(AnzahlTodesfall),
                  popData2019 = mean(EWZ)) %>%
        ungroup()  %>%
        mutate(data = "rki",
               countriesAndTerritories = Landkreis) %>%
        dplyr::select(dateRep,cases,deaths,countriesAndTerritories,popData2019,data) %>% 
        mutate(dateRep = as.Date(dateRep,format = "%Y/%m/%d"))
    
    return(rbind(ecdc_world, 
                 rki_bundesland, 
                 rki_landkreis) 
    )
}
plotCovid <- function(selected_CandT,average = 3,asList=FALSE,timeLimits){
    workDat <- workDat %>%
        dplyr::filter(countriesAndTerritories %in% selected_CandT,
                      dateRep < timeLimits[2],
                      dateRep > timeLimits[1]) %>% 
        group_by(countriesAndTerritories) %>%
        mutate(cases = ifelse(cases<0,0,cases),
               deaths =ifelse(deaths<0,0,deaths)) %>%
        mutate(cases_averaged = mean_dist(cases,average),
               deaths_averaged = mean_dist(deaths,average),
               cases_per_100k_pop = cases_averaged/popData2019*100000,
               deaths_per_100k_pop = deaths_averaged/popData2019*100000)
    
    p1 <- workDat %>%
        ggplot(aes(x=dateRep,y=cases_averaged,col=countriesAndTerritories)) +
        geom_line() +
        geom_point() +
        ylab(sprintf("abs. cases",average)) +
        theme_bw() +
        ggtitle("absolute daily cases")
    p2 <- workDat %>% 
        ggplot(aes(x=dateRep,y=deaths_averaged,col=countriesAndTerritories)) +
        geom_line() +
        geom_point() +
        ylab(sprintf("abs. deaths",average)) +
        theme_bw()+
        ggtitle("absolute daily deaths")
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

#res = list(workDat = gen_workDat())
loadData <- function(from="local"){
    if (from=="ecdc"){
        print(">>>>> loading ecdc data >>>>>")
        print(system.time({
            data$from$ecdc <<- "ECDC"
            data$time$ecdc <<- Sys.time()
            data$raw$ecdc <<- fread("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                                       na.strings = "")  
        }))
        print("<<<<< loading ecdc data <<<<<")
    }
    if (from == "rki"){
        print(">>>>> loading rki data >>>>>")
        print(system.time({
            rki_pop <- fread("RKI_Corona_Landkreise.csv",encoding = "UTF-8")
            data$from$rki <<- "RKI"
            data$time$rki <<- Sys.time()
            data$raw$rki <<- fread("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv", encoding = "UTF-8") %>% 
                merge(rki_pop %>% 
                          dplyr::select(county,EWZ,EWZ_BL) %>%
                          rename(Landkreis = county),by=c("Landkreis"))
        }))
        print("<<<<< loading rki data <<<<<")
    }
    if (from=="local"){
        print(">>>>> loading local data >>>>>")
        print(system.time({
            load(file="data.RData",envir = ENV)
        }))
        print("<<<<< loading local data <<<<<")
    }
}

saveData <- function(){
    save(data,file="data.RData")
}
genChoices <- function(){
    ecdc = workDat %>% dplyr::filter(data=="ecdc") %>% 
        dplyr::select(countriesAndTerritories) %>% 
        unique()
    rki.tmp = workDat %>% dplyr::filter(data=="rki") %>% 
        dplyr::select(countriesAndTerritories) %>% 
        unique() %>% 
    ddply("countriesAndTerritories",function(x){
        s <- unlist(stringr::str_locate_all(x," - "))[2]
        out <- ifelse(is.na(s),
                      substring(x,first = 1),
                      paste("--",substring(x,first = s)))
        return(out)
    })
    rki <- factor(rki.tmp$countriesAndTerritories)
    names(rki) <- rki.tmp$V1
    return(
        list(ecdc=ecdc,
             rki=rki)
    )
}

ENV <- environment()
data = list(from=list(),
            time=list(),
            raw = list())
# loadData("ecdc")
# loadData("rki")
# saveData()
loadData("local")

workDat = gen_workDat()
choices_all <- genChoices()
ecdc_countryChoices <- choices_all$ecdc
rki_countyChoices <- choices_all$rki

ecdc_selectedCountries <- c(
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
rki_selectedCounties <- c()
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Corona overview"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textOutput("ecdc_dataInfoOrigin"),
            textOutput("ecdc_dataInfoTime"),
            actionButton("ecdc_download","Load newest ecdc-data",icon=icon("download")),
            selectInput("countries","Select Countries",multiple = TRUE,choices = ecdc_countryChoices,
                        selected = ecdc_selectedCountries),
            # uiOutput("legend")
            hr(),
            textOutput("rki_dataInfoOrigin"),
            textOutput("rki_dataInfoTime"),
            actionButton("rki_download","Load newest rki-data",icon=icon("download")),
            selectInput("rki_counties","Select County",multiple = TRUE,choices = rki_countyChoices,
                        selected = rki_selectedCounties),
            hr(),
            checkboxInput("relative","adjusted to total poulation",value = TRUE),
            numericInput("smooth","average over days",value=3,min=0,step=1),
            sliderInput("timeSlide","xAxis",min=min(workDat$dateRep),max=max(workDat$dateRep),
                        value=c(min(workDat$dateRep),max(workDat$dateRep)),dragRange = TRUE)
            
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
    
    res <- reactiveValues(
        from = data$from,
        time = data$time)
    
    # ========================================================================== ECDC ====
    # -------------------------------------------------------------------------- info 
    output$ecdc_dataInfoOrigin <- renderText({paste("Data from:",res$from$ecdc)})
    output$ecdc_dataInfoTime <- renderText({paste("Last update at:",res$time$ecdc)})
    # -------------------------------------------------------------------------- download 
    observeEvent(input$ecdc_download,{
        loadData("ecdc")
        saveData()
        res$from = data$from
        res$time = data$time
        print(res$workDat)
    })
    # ========================================================================== RKI ====
    # -------------------------------------------------------------------------- info 
    output$rki_dataInfoOrigin <- renderText({paste("Data from:",res$from$rki)})
    output$rki_dataInfoTime <- renderText({paste("Last update at:",res$time$rki)})
    # -------------------------------------------------------------------------- download 
    observeEvent(input$rki_download,{
        loadData("rki")
        saveData()
        res$from = res$from
        res$time = res$time
    })
    
    # ========================================================================== PLOT ====
    # -------------------------------------------------------------------------- selection
    toListen <- reactive({
        list(input$countries,
             input$rki_counties,
             input$smooth,
             input$timeSlide)
    })
    observeEvent(toListen(),{
        req(input$smooth)
        res$plots <- plotCovid(c(input$countries,input$rki_counties),input$smooth,T,input$timeSlide)
    })
    
    # -------------------------------------------------------------------------- plot
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
