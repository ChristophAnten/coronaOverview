library(shiny)    
runGitHub("coronaOverview", "ChristophAnten") 

dataRKI <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
dataRKI <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"
b <- dataRKI
b <- read.csv(dataRKI, encoding = "UTF-8")

"https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
names(b)
b$AnzahlFall %>% unique()
p <- b %>% dplyr::filter(Bundesland == "Hamburg") %>% 
  dplyr::select("Refdatum","AnzahlFall", "AnzahlTodesfall","AnzahlGenesen") %>%
  dplyr::group_by(Refdatum) %>% 
  summarise(cases = mean(AnzahlFall)) %>%
  # head() %>% View()
  ggplot(aes(x=Refdatum,y=cases)) +
  geom_line() +
  geom_point()
ggplotly(p)
names()


data$raw$ecdc %>% names()
c("dateRep","cases","deaths","countriesAndTerritories","popData2019")
data$raw$rki %>% names()
c("Bundesland","Landkreis","Altersgruppe","Geschlecht","AnzahlFall","AnzahlTodesfall","Refdatum","AnzahlGenesen")
ecdc_world <- data$raw$ecdc %>% head() %>%
  mutate(data = "ecdc") %>% 
  dplyr::select(dateRep,cases,deaths,countriesAndTerritories,popData2019,data) %>% 
  mutate(dateRep = as.Date(dateRep,format = "%d/%m/%Y"))

rki_bundesland <- data$raw$rki %>% head() %>%
  rename(dateRep = Refdatum) %>%
  group_by(Bundesland,dateRep,.drop=FALSE) %>%
  summarize(cases = sum(AnzahlFall),
            deaths = sum(AnzahlTodesfall)) %>%
  ungroup()  %>%
  mutate(data = "rki",
         popData2019 = popGer,
         countriesAndTerritories = paste(Bundesland)) %>%
  dplyr::select(dateRep,cases,deaths,countriesAndTerritories,popData2019,data) %>% 
  mutate(dateRep = as.Date(dateRep,format = "%Y/%m/%d"))

rki_landkreis <- data$raw$rki %>% head() %>% 
  rename(dateRep = Refdatum) %>%
  mutate(Landkreis = paste(Bundesland,"-",Landkreis)) %>% 
  group_by(Landkreis,dateRep) %>%
  summarize(cases = sum(AnzahlFall),
            deaths = sum(AnzahlTodesfall)) %>%
  ungroup()  %>%
  mutate(data = "rki",
         popData2019 = popGer,
         countriesAndTerritories = Landkreis) %>%
  dplyr::select(dateRep,cases,deaths,countriesAndTerritories,popData2019,data) %>% 
  mutate(dateRep = as.Date(dateRep,format = "%Y/%m/%d"))

workDat <- rbind(ecdc_world, 
                 rki_bundesland, 
                 rki_landkreis) %>% group_by(countriesAndTerritories) %>%
  mutate(cases = ifelse(cases<0,0,cases),
         deaths =ifelse(deaths<0,0,deaths)) %>%
  mutate(cases_averaged = mean_dist(cases,average),
         deaths_averaged = mean_dist(deaths,average),
         cases_per_100k_pop = cases_averaged/popData2019*100000,
         deaths_per_100k_pop = deaths_averaged/popData2019*100000) %>% 
  mutate(dateRep = as.Date(dateRep,format = "%d/%m/%Y"))

list.files()
rki_pop <- read.csv("RKI_Corona_Landkreise.csv",encoding = "UTF-8")
data$raw$rki %>% 
  merge(rki_pop %>% 
          dplyr::select(county,EWZ,EWZ_BL) %>%
          rename(Landkreis = county),by=c("Landkreis"))
df1$EWZ
df1 %>% group_by(BL) %>%
  summarise(ew = mean(EWZ_BL))
