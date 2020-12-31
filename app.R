###############################################################################
mandatory_packages <- c("utils", "plyr", "dplyr", "tidyr", 
                        "ggpubr", "ggplot2", "grid", "gridExtra",
                        "shiny", "stringr",
                        "data.table","bit64","scales")
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
library(shiny)
library(stringr)
library(bit64)
library(data.table)
library(scales)


theme_set(
    theme_bw(base_size = 15)
)

mean_dist <- function(x,dist,type="latest",sum=TRUE){
    if (dist<1)
        return(x)
    if (dist>31){
        print("Warning: average_Distance can not be set greater than 31 and thus is set to 31!")
        dist <- 31
    }
    n <- length(x)
    y <- x
    if (type == "latest"){
        for(i in 1:dist){
            y[1:(n-i)] <- y[1:(n-i)]+x[(1+i):n]
        }
        if (sum)
            return(y)
        div <- c(rep(dist+1,n-dist),seq(dist,1))
        return(y/div)
    }
    if (type == "PlusMinus"){
        for(i in 1:dist){
            y[1:(n-i)] <- y[1:(n-i)]+x[(1+i):n]
            y[(1+i):n] <- y[(1+i):n]+x[1:(n-i)]
        }
        if (sum)
            return(y)
        div <- c(seq(1+dist,2*dist),rep(1+2*dist,n-(2*dist)),seq(2*dist,1+dist))
        return(y/div)
    }
    if (type == "oldest"){
        for(i in 1:dist){
            y[(1+i):n] <- y[(1+i):n]+x[1:(n-i)]
        }
        if (sum)
            return(y)
        div <- c(seq(1,dist),rep(dist+1,n-dist))
        return(y/div)
    }
}
gen_workDat <- function(){
    ourWorldInData_world <- data$raw$ourWorldInData %>%
        mutate(data = "ourWorldInData") %>%
        rename(cases = new_cases,
               deaths = new_deaths,
               dateRep = date,
               countriesAndTerritories = location,
               popData2019 = population) %>%
        mutate(countriesAndTerritories = paste(countriesAndTerritories,"(OWID)")) %>%
        dplyr::select(dateRep,cases,deaths,countriesAndTerritories,popData2019,data) %>%
        mutate(dateRep = as.Date(dateRep,format = "%d/%m/%Y"))
    
    ecdc_world_weekly <- data$raw$ecdc %>%
        rename(cases = cases_weekly,
               deaths = deaths_weekly) %>%
        dplyr::select(dateRep,cases,deaths,countriesAndTerritories,popData2019) %>%
        mutate(dateRep = as.Date(dateRep,format = "%d/%m/%Y"))

    a <- ecdc_world_weekly
    b=data.frame(dateRep=NULL)
    for (i in a$countriesAndTerritories %>% unique()){
        b <- rbind(b,data.frame(dateRep=seq(max(a$dateRep[a$countriesAndTerritories==i]), 
                     min(a$dateRep[a$countriesAndTerritories==i])-6,
                     by=-1),
                   countriesAndTerritories = i))
    }
    ecdc_world = b %>% 
        merge(a,by=c("dateRep","countriesAndTerritories"),all.x=T,sort = FALSE) %>%
        arrange(desc(dateRep),countriesAndTerritories) %>%
        group_by(countriesAndTerritories) %>%
        fill(popData2019,cases,deaths,.direction = c("down")) %>%
        mutate(cases = cases/7,
               deaths = deaths/7) %>%
        mutate(data = "ecdc") %>%
        mutate(countriesAndTerritories = paste(countriesAndTerritories,"(ECDC)"))
    
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
        mutate(dateRep = as.Date(dateRep,format = "%Y/%m/%d")) %>%
        mutate(countriesAndTerritories = paste(countriesAndTerritories,"(RKI)"))
    
    
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
        mutate(dateRep = as.Date(dateRep,format = "%Y/%m/%d")) %>%
        mutate(countriesAndTerritories = paste(countriesAndTerritories,"(RKI)"))
    
    return(rbind(ecdc_world, 
                 rki_bundesland, 
                 rki_landkreis,
                 ourWorldInData_world) 
    )
}
cumsumInv <- function(x,ref){
    return(cumsum(x[order(ref,decreasing=TRUE)])[order(ref)])
}
iff <- function(cond,x,y) {
    if(cond) return(x) else return(y)
}
gen_plotData <- function(selected_CandT,average = 3,average_type = "latest",
                         sum=TRUE,adjToPop=TRUE,
                         timeLimits,cummulative=FALSE){
    return(
        workDat %>%
            dplyr::filter(countriesAndTerritories %in% selected_CandT) %>%
            dplyr::arrange(desc(dateRep)) %>%
            group_by(countriesAndTerritories) %>%
            iff(cummulative,
                mutate(.,cases = cumsumInv(cases,dateRep),
                       deaths = cumsumInv(deaths,dateRep)),
                .) %>%
            iff(adjToPop,
                mutate(.,cases = cases/popData2019*100000,
                       deaths = deaths/popData2019*100000),
                .) %>%
            mutate(cases = mean_dist(cases,average,average_type,sum),
                   deaths = mean_dist(deaths,average,average_type,sum)) %>%
            dplyr::filter(dateRep < timeLimits[2],
                          dateRep > timeLimits[1])
    )
}
plotCovid_old <- function(plotData, average=average, asList=FALSE,logscale=FALSE){
    p1 <- plotData %>%
        ggplot(aes(x=dateRep,y=cases_averaged,col=countriesAndTerritories)) +
        geom_line() +
        geom_point() + 
        ylab(sprintf("abs. cases")) +
        ggtitle("absolute daily cases")
    p2 <- plotData %>% 
        ggplot(aes(x=dateRep,y=deaths_averaged,col=countriesAndTerritories)) +
        geom_line() +
        geom_point() + 
        ylab(sprintf("abs. deaths")) +
        ggtitle("absolute daily deaths")
    p3 <- plotData %>% 
        ggplot(aes(x=dateRep,y=cases_per_100k_pop,col=countriesAndTerritories)) +
        geom_line() +
        geom_point() +
        ylab(sprintf("cases/100k pop. (mean over %i days)",average)) +
        ggtitle("daily cases per 100.000 population")
    p4 <- plotData %>% 
        ggplot(aes(x=dateRep,y=deaths_per_100k_pop,col=countriesAndTerritories)) +
        geom_line() +
        geom_point() +
        ylab(sprintf("deaths/100k pop. (mean over %i days)",average)) +
        ggtitle("daily deaths per 100.000 population")
    
    if(logscale){
        p1 = p1 +  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                 labels = trans_format("log10", math_format(10^.x)))
        p2 = p2 +  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                 labels = trans_format("log10", math_format(10^.x)))
        p3 = p3 + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x)))
        p4 = p4 + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x)))
    }
    
    if (asList) {
        return(list(p1,p2,p3,p4))
    }
    pAll <- ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="top")
    return(pAll)
}
plotCovid <- function(plotData, average=average, asList=FALSE,logscale=FALSE,adjToPop=TRUE){
    
    p1 <- plotData %>%
        ggplot(aes(x=dateRep,y=cases,col=countriesAndTerritories)) +
        geom_line() +
        geom_point() + 
        ylab(sprintf("abs. cases")) +
        ggtitle("absolute daily cases")
    p2 <- plotData %>% 
        ggplot(aes(x=dateRep,y=deaths,col=countriesAndTerritories)) +
        geom_line() +
        geom_point() + 
        ylab(sprintf("abs. deaths")) +
        ggtitle("absolute daily deaths")
    
    if(logscale){
        p1 = p1 +  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                 labels = trans_format("log10", math_format(10^.x)))
        p2 = p2 +  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                 labels = trans_format("log10", math_format(10^.x)))
    }
    
    if (asList) {
        return(list(p1,p2))
    }
    pAll <- ggarrange(p1, p2, ncol=2, nrow=2, common.legend = TRUE, legend="top")
    return(pAll)
}

#res = list(workDat = gen_workDat())
loadData <- function(from="local"){
    if (from=="ourWorldInData"){
        print(">>>>> loading ourWorldInData data >>>>>")
        print(system.time({
            data$from$ourWorldInData <<- "ourworldindata.org"
            data$time$ourWorldInData <<- Sys.time()
            data$raw$ourWorldInData <<- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                                    na.strings = "")  
        }))
        print("<<<<< loading ourWorldInData data <<<<<")
    }
    
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
            data$raw$rki <<- fread("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data", encoding = "UTF-8") %>% 
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
    
    ourWorldInData = workDat %>% dplyr::filter(data=="ourWorldInData") %>% 
        dplyr::select(countriesAndTerritories) %>% 
        unique()
    return(
        list(ecdc=ecdc,
             rki=rki,
             ourWorldInData=ourWorldInData)
    )
}

ENV <- environment()
data = list(from=list(),
            time=list(),
            raw = list())
# loadData("ecdc")
# loadData("rki")
# loadData("ourWorldInData")
# saveData()

# identify weather it is my computer or any other one
d <- utf8ToInt(as.list(Sys.info())$nodename)
j=0
for (i in d){
    j = (j + (i * 9808358)) %% 24862048
}
#j=0
# load data either from local or directly from the sources
if (j==3796478){
    loadData("local") 
} else {
    loadData("ecdc")
    loadData("rki")
    loadData("ourWorldInData")
}

workDat = gen_workDat()
choices_all <- genChoices()
smooth_type_choices <- c("latest","PlusMinus","oldest")
ecdc_countryChoices <- choices_all$ecdc
rki_countyChoices <- choices_all$rki
ourWorldInData_countryChoices <- choices_all$ourWorldInData

ecdc_selectedCountries <- c(
    "Netherlands (ECDC)",
    "United_States_of_America (ECDC)",
    "Germany (ECDC)"
)
rki_selectedCounties <- c()
ourWorldInData_selectedCountries <- c()

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # infoBox for plot data
    tags$head(
        tags$script(
            HTML("
           // Get mouse coordinates
           var mouseX, mouseY;
           $(document).mousemove(function(e) {
           mouseX = e.pageX;
           mouseY = e.pageY;
           }).mouseover();

           // Function to position draggable, place on current mouse coordinates
           Shiny.addCustomMessageHandler ('placeDraggable',function (message) {
           var element = $('#click_info').parent();
           element.css({'top': mouseY + 'px', 'left' : mouseX + 'px'})
           });
           ")
        ),
        tags$style("#point_info table {background-color: rgba(255,255,255,.9); }",
                   media="screen", 
                   type="text/css")
    ),
    # Application title
    titlePanel("Corona overview"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textOutput("ecdc_dataInfoOrigin"),
            textOutput("ecdc_dataInfoTime"),
            selectInput("ecdc_countries","Select Countries",multiple = TRUE,choices = ecdc_countryChoices,
                        selected = ecdc_selectedCountries),
            # uiOutput("legend")
            hr(),
            textOutput("rki_dataInfoOrigin"),
            textOutput("rki_dataInfoTime"),
            selectInput("rki_counties","Select County",multiple = TRUE,choices = rki_countyChoices,
                        selected = rki_selectedCounties),
            hr(),
            textOutput("ourWorldInData_dataInfoOrigin"),
            textOutput("ourWorldInData_dataInfoTime"),
            selectInput("ourWorldInData_countries","Select County",multiple = TRUE,choices = ourWorldInData_countryChoices,
                        selected = ourWorldInData_selectedCountries),
            hr(),
            # checkboxGroupInput(inputId = "abc",label = NULL,choices = list("adj. to population","cummulative","logscale"),
            #                    inline = TRUE,selected = 1),
            checkboxInput("adjToPop","adj. to population",value = TRUE),
            checkboxInput("cummulative","cummulative",value = FALSE),
            checkboxInput("logscale","logscale",value = FALSE),
            numericInput("smooth","sum/average over past (1-31) days:",value=7,min=1,step=1,max = 31),
            radioButtons("meanOrSum",label=NULL,choices = c("sum","average"),selected = "sum",inline = TRUE),
            sliderInput("timeSlide","xAxis",min=min(workDat$dateRep),max=max(workDat$dateRep),
                        value=c(min(workDat$dateRep),max(workDat$dateRep)),dragRange = TRUE),
            hr(),
            "To filter the Datatable for specific datapoints drag an area in either of the plots or filter directly in the data table."
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlotCases",
                       click = "plotCases_click",
                       hover = "plotCases_hover",
                       brush = brushOpts(
                           id = "plotCases_brush"
                       )),
            plotOutput("distPlotDeaths",
                       click = "plotDeaths_click",
                       hover = "plotDeaths_hover",
                       brush = brushOpts(
                           id = "plotDeaths_brush"
                       )),
            
            absolutePanel(fixed=TRUE, draggable = TRUE, uiOutput("click_info")),
            dataTableOutput("selectedPlotOut")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    res <- reactiveValues(
        from = data$from,
        time = data$time)
    
    # ========================================================================== ECDC ====
    # -------------------------------------------------------------------------- info 
    output$ecdc_dataInfoOrigin <- renderText({paste("Data origin:",res$from$ecdc)})
    output$ecdc_dataInfoTime <- renderText({paste("Last update:",res$time$ecdc)})
    
    # ========================================================================== RKI ====
    # -------------------------------------------------------------------------- info 
    output$rki_dataInfoOrigin <- renderText({paste("Data origin:",res$from$rki)})
    output$rki_dataInfoTime <- renderText({paste("Last update:",res$time$rki)})
    
    # ========================================================================== ourWorldInData ====
    # -------------------------------------------------------------------------- info 
    output$ourWorldInData_dataInfoOrigin <- renderText({paste("Data origin:",res$from$ourWorldInData)})
    output$ourWorldInData_dataInfoTime <- renderText({paste("Last update:",res$time$ourWorldInData)})

    # ========================================================================== PLOT ====
    # -------------------------------------------------------------------------- selection
    toListen <- reactive({
        list(input$ecdc_countries,
             input$rki_counties,
             input$ourWorldInData_countries,
             input$smooth,
             input$timeSlide,
             input$cummulative,
             input$logscale,
             input$meanOrSum,
             input$adjToPop)
    })
    observeEvent(toListen(),{
        req(input$smooth)
        if (!is.null(c(input$ecdc_countries,
                       input$rki_counties,
                       input$ourWorldInData_countries))){
            res$plotDf <- gen_plotData(selected_CandT = c(input$ecdc_countries,input$rki_counties,input$ourWorldInData_countries),
                         average = input$smooth-1,
                         average_type = "latest",
                         adjToPop = input$adjToPop,
                         sum = (input$meanOrSum=="sum"),
                         timeLimits = input$timeSlide,
                         cummulative = input$cummulative)
            res$plots <- plotCovid(res$plotDf,
                                   asList = TRUE,
                                   logscale = input$logscale,
                                   adjToPop=input$adjToPop)
        }
    })
    
    # -------------------------------------------------------------------------- plot
    output$distPlotCases <- renderPlot({
        # hide_legend(res$plots[[1]])
            res$plots[[1]]
    })
    output$distPlotDeaths <- renderPlot({
            res$plots[[2]]
    })
    # output$plotCases_clickInfo <- renderPrint({
    #     # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    #     # were a base graphics plot, we'd need those.
    #     nearPoints(res$plotDf, input$plotCases_click, addDist = TRUE)
    # })
    # 
    # output$plotCases_hoverInfo <- renderPrint({
    #     nearPoints(res$plotDf, input$plotCases_hover, addDist = TRUE)
    # })
    # 
    # output$plotCases_brushInfo <- renderPrint({
    #     brushedPoints(res$plotDf, input$plotCases_brush)
    # })
    
    # show_this  = reactiveVal(NULL)
    # print_this = reactiveVal(NULL)
    # observeEvent(input$plotCases_click, {
    #     p <- nearPoints(res$plotDf, input$plotCases_click, maxpoints=1)
    #     if( nrow(p) == 0 ) {
    #         show_this(NULL)
    #     }
    #     else {
    #         session$sendCustomMessage(type = 'placeDraggable', message = list())
    #         show_this(tagList(
    #             { actionButton("input_button","OK") },
    #             { br() },
    #             { tableOutput("point_info") }
    #         )
    #         )
    #         tmp <- res$plotDf %>% dplyr::filter(dateRep == p$dateRep)
    #         q <- t(tmp[,c("cases","deaths")])
    #         colnames(q) <- tmp$countriesAndTerritories
    #         print_this({q})
    #     }
    # })
    # observeEvent(input$plotDeaths_click, {
    #     p <- nearPoints(res$plotDf, input$plotDeaths_click, maxpoints=1)
    #     if( nrow(p) == 0 ) {
    #         show_this(NULL)
    #     }
    #     else {
    #         session$sendCustomMessage(type = 'placeDraggable', message = list())
    #         show_this(tagList(
    #             { actionButton("input_button","OK") },
    #             { br() },
    #             { tableOutput("point_info") }
    #         )
    #         )
    #         tmp <- res$plotDf %>% dplyr::filter(dateRep == p$dateRep)
    #         q <- t(tmp[,c("cases","deaths")])
    #         colnames(q) <- tmp$countriesAndTerritories
    #         print_this({q})
    #     }
    # })
    # output$click_info <- renderUI   (show_this() )
    # output$point_info <- renderTable(print_this(),rownames = TRUE,bordered = TRUE)
    # 
    # observeEvent(input$plotCases_hover, {
    #     p <- nearPoints(res$plotDf, input$plotCases_hover, maxpoints=1)
    #     if( nrow(p) == 0 ) {
    #         show_this(NULL)
    #     }
    #     else {
    #         session$sendCustomMessage(type = 'placeDraggable', message = list())
    #         show_this(tagList(
    #             { actionButton("input_button","OK") },
    #             { br() },
    #             { tableOutput("point_info") }
    #         )
    #         )
    #         tmp <- res$plotDf %>% dplyr::filter(dateRep == p$dateRep)
    #         q <- t(tmp[,c("dateRep","cases","deaths")])
    #         colnames(q) <- tmp$countriesAndTerritories
    #         print_this({q})
    #     }
    # })
    # observeEvent(input$input_button,{
    #     if (input$input_button) { show_this(NULL) }
    # })
    output$selectedPlotOut <- renderDataTable({
        # res$plotDf
        if(!is.null(input$plotCases_brush)){
            if(!is.null(input$plotDeaths_brush)){
                out <- res$plotDf %>% 
                    dplyr::filter((cases<input$plotCases_brush$ymax &
                                       cases>input$plotCases_brush$ymin &
                                       dateRep<input$plotCases_brush$xmax&
                                       dateRep>input$plotCases_brush$xmin)|
                                      (deaths<input$plotDeaths_brush$ymax &
                                           deaths>input$plotDeaths_brush$ymin &
                                           dateRep<input$plotDeaths_brush$xmax&
                                           dateRep>input$plotDeaths_brush$xmin))
            } else {
                out <- res$plotDf %>% 
                    dplyr::filter(cases<input$plotCases_brush$ymax &
                                      cases>input$plotCases_brush$ymin &
                                      dateRep<input$plotCases_brush$xmax&
                                      dateRep>input$plotCases_brush$xmin)
            }
        } else {
            if(!is.null(input$plotDeaths_brush)){
                out <- res$plotDf %>% 
                    dplyr::filter(deaths<input$plotDeaths_brush$ymax &
                                      deaths>input$plotDeaths_brush$ymin &
                                      dateRep<input$plotDeaths_brush$xmax&
                                      dateRep>input$plotDeaths_brush$xmin)
            } else {
                # print(input$plotDeaths_brush)
                # print(input$plotCases_brush)
                out <- res$plotDf
            }
        }
        out
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

# To Do:
# logscale gives warnings
# adjust ylab in cummulative
# https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html
# optional loading data from a different file
# not mean over week but sum!
# add table to hovering position with all data
# change plots case deaths
# change load to download data or put a link