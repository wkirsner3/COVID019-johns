install.packages("tidyverse", repos = "http://cran.us.r-project.org")
install.packages("lubridate", repos = "http://cran.us.r-project.org")
install.packages("rvest", repos = "http://cran.us.r-project.org")
install.packages("Recon", repos = "http://cran.us.r-project.org")
install.packages("incidence", repos = "http://cran.us.r-project.org")
install.packages("tinytex", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(rvest)
library(lubridate)
library(incidence)
library(tinytex)
library(dplyr)

tinytex::install_tinytex()

url_in1 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

url_in2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

url_in3 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

url_in4 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

global_cases <- read.csv(url_in2)

global_deaths <- read_csv(url_in4)

US_cases <- read_csv(url_in1)

US_deaths <- read_csv(url_in3)


global_cases <- global_cases %>% 
  select(-c(Lat, Long))


global_cases <- global_cases %>%
  pivot_longer(cols = -c('Province.State',
                         'Country.Region'),
               names_to = "date" ,
               values_to = "cases" )


global_deaths <- global_deaths %>%
  select(-c(Lat, Long))

global_deaths <- global_deaths %>%
  pivot_longer(cols = -c('Province/State',
                         'Country/Region'),
               names_to = "date",
               values_to = "deaths")

global <- global_cases %>%
  full_join(global_deaths) %>%
  rename(Country_Region = 'Country.Region',
         Province_State = 'Province.State') %>%
  mutate(date =mdy(date))

global <- global %>% filter(cases > 0)

UID <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"

summary(global)


US_cases <- US_cases %>%
  pivot_longer(cols = -(UID:Combined_Key),
               names_to = "date",
               values_to = "cases") %>%
  select(Admin2:cases)%>%
  mutate(date =mdy(date)) %>%
  select(-c(Lat, Long_))

US_deaths <- US_deaths %>%
  pivot_longer(cols = -(UID:Population),
               names_to = "date",
               values_to = "deaths") %>%
  select(Admin2:deaths)%>%
  mutate(date =mdy(date)) %>%
  select(-c(Lat, Long_))

US <- US_cases %>%
  full_join(US_deaths)


global <- global %>%
  unite("Combined_Key",
        c(Province_State, Country_Region),
        sep = ", ",
        na.rm = TRUE,
        remove = FALSE)

UIDs <- read.csv(UID) %>%
  select(-c(Lat, Long_, Combined_Key, code3, iso2, iso3, Admin2))

global <- global %>% 
  left_join (UIDs, b = c("Province_State", "Country_Region"))%>%
  select(Province_State, Country_Region, date, cases, deaths, Population, Combined_Key)

US_by_State <- US %>%
  group_by(Province_State, Country_Region, date) %>%
  summarize(cases = sum(cases), deaths = sum(deaths),
            Population = sum(Population)) %>%
  mutate(deaths_per_mill = deaths *1000000 / Population) %>%
  select(Province_State, Country_Region, date, cases, deaths, deaths_per_mill, Population) %>%
  ungroup()

US_totals <- US_by_State %>%
  group_by(Country_Region, date) %>%
  summarize(cases = sum(cases), deaths =sum(deaths),
            Population = sum(Population)) %>%
  mutate(deaths_per_mill = deaths *1000000 / Population) %>%
  select(Country_Region, date, 
         cases, deaths, deaths_per_mill, Population) %>%
  ungroup()

US_state_totals <- US_by_State %>%
  group_by(Province_State) %>%
  summarize(deaths = max(deaths), cases = max(cases),
            population = max(Population),
            cases_per_tenthou = 10000* cases / population,
            deaths_per_tenthou = 10000* deaths / population)%>%
  filter(cases > 0, population > 0)

US_by_State <- US_by_State %>%
  mutate(new_cases = cases -lag(cases),
         new_deaths = deaths - lag(deaths))
US_totals <- US_totals %>%
  mutate(new_cases = cases - lag(cases),
         new_deaths = deaths - lag(deaths))

State <- "New Jersey"
US_by_State %>%
  filter(Province_State == State) %>%
  ggplot(aes(x=date, y = new_cases))+
  geom_line(aes(color="new_cases"))+
  geom_point(aes(color = "new_cases"))+
  geom_line(aes(y = new_deaths, color = "new_deaths"))+
  geom_point(aes(y = new_deaths, color = "new_deaths"))+
  scale_y_log10()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90))+
  labs(title = str_c("COVID-19 in ", State), y = NULL)

mods <- lm(deaths_per_tenthou ~ cases_per_tenthou, data = US_state_totals)
d_grid <-seq(1,30)
df <- tibble(cases_per_tenthou = d_grid)
Pred_Death <- US_state_totals %>% mutate(pred = predict(mods))

Pred_Death %>% ggplot()+
  geom_point(aes(x = cases_per_tenthou, y = deaths_per_tenthou), color = "green")+
  geom_point(aes(x = cases_per_tenthou, y = pred), color = "orange")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90))+
  labs(title = "Prediction of Deaths from Covid-19 as a function of Cases", y = NULL)