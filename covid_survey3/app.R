library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(sf)
library(shinythemes)
library(readr)
library(tmap)
library(countrycode)
library(RColorBrewer)
library(ggrepel)
library(ggthemes)
library(devtools)
#devtools::install_github('rensa/ggflags')
library(ggflags)
library(shinyWidgets)

### functions

normalize <- function(x) {
    return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x,na.rm = TRUE)))
    }

  
  

get_most_recent_value <- function(df) {
  df <- df %>%
    pivot_longer(cols = colnames(df[6:65]), names_to = "year") %>%
    mutate_at('year', as.numeric) %>% group_by(`Country Code`) %>%
    arrange(-year, .by_group=TRUE) %>%
    filter(!is.na(value)) %>%
    select(`Country Name`, `Country Code`, year, value)
  df <- distinct(df,'Country Code', .keep_all=TRUE)
  df <- df[-c(5:5)]
  return(df)
}

###

df_covid=read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
df_covid <- df_covid %>%mutate(mortality_rate = Cumulative_deaths/Cumulative_cases)


df_health = read_csv('data/health_expenditures.csv', skip = 3)
df_child_mortality = read_csv('data/child_mortality_rate.csv', skip = 3)
df_life_expectancy = read_csv('data/life_expectancy.csv', skip = 3)
df_physicans = read_csv('data/physicians.csv', skip=3)



df_child_mortality <- get_most_recent_value(df_child_mortality) %>%
  rename(child_mortality_per_1k = value)
df_physicans <- get_most_recent_value(df_physicans) %>%
  rename(physicans_per_1k = value)
df_life_expectancy <- get_most_recent_value(df_life_expectancy) %>%
  rename(life_expectancy = value)
df_health <- get_most_recent_value(df_health) %>%
  rename(health_expenditures_usd = value)

df_joined <- df_child_mortality %>%
  full_join(df_physicans,by=c('Country Name','Country Code')) %>%
  full_join(df_life_expectancy,by=c('Country Name','Country Code')) %>%
  full_join(df_health,by=c('Country Name', 'Country Code'))


df_joined <- df_joined %>%
  mutate(iso2 = countrycode(`Country Name`, origin="country.name", destination = "iso2c")) %>%
  filter(!is.na(iso2)) %>%
  select(iso2 ,child_mortality_per_1k, physicans_per_1k, life_expectancy, health_expenditures_usd)
df_covid <- df_covid %>%
  mutate(iso2 = countrycode(Country, origin="country.name", destination = "iso2c")) %>%
  filter(!is.na(iso2)) %>%
  select(Country, iso2, mortality_rate,Cumulative_cases, Date_reported)
df <- df_covid %>%
  left_join(df_joined, by= "iso2")

data("World", package = "tmap")
world <- World
world <- world %>%
  mutate(iso2 = countrycode(iso_a3, origin = "iso3c", destination = "iso2c"))

world <- world %>%
  left_join(df, by= "iso2")


# UI

ui <- bootstrapPage(
    navbarPage("Covid Survey",
    id = "navbar",
               theme = shinytheme("flatly"),
               tabPanel(
                   title = "Home",
                   h3("Introduction")
               ),
               tabPanel(
                   title = "World Map",
                   sidebarLayout(
                       sidebarPanel(
                           dateInput("date_world_map",
                           label= "Please choose a date",
                           min= min(df$Date_reported, na.rm=TRUE),
                           max= max(df$Date_reported, na.rm=FALSE),
                           value=max(df$Date_reported, na.rm=TRUE))
                       ),
                       mainPanel(
                           plotOutput(outputId = 'myplot'))
                       )
                   ),
               tabPanel(
                   title = "Correlation",
                   sidebarLayout(
                       sidebarPanel(
                         # how can i set minimum amount of selection?
                         # checkboxGroupInput(
                         #   inputId = "correlation_multiselect", 
                         #   label= "Choose your metrics", 
                         #   choices = list("child_mortality_norm","health_expenditures_norm","physicans_norm","life_expectancy_norm"), 
                         #   #multiple = TRUE,
                         #   selected = list("child_mortality_norm", "physicans_norm")
                         #   ),
                         pickerInput(
                           inputId = "score_picker",
                           label = "Select all metrics which should be included in the Health Score",
                           choices = list("Child Mortality"="child_mortality_norm", "Health Expenditures"="health_expenditures_norm","Physicans"="physicans_norm","Life Expectancy"="life_expectancy_norm"),
                           options = list(
                             "actions-box" = TRUE,
                             size = 10,
                             "selected-text-format" = "count > 2"
                           ),
                           multiple = T,
                           selected = list("child_mortality_norm", "physicans_norm", "life_expectancy_norm")
                         )
                       ),
                       mainPanel(
                         plotOutput(outputId = 'scatterplot_second'),
                         verbatimTextOutput(outputId = "value")
                         )
                       
                   
              ) )
   ) )



# SERVER

server <- function(input, output) {

  # World Map
  
  output$myplot <- renderPlot({
    df_world  <- world %>% filter(Date_reported == input$date_world_map & Cumulative_cases >=1000)
    ggplot(data= df_world) +
        geom_sf(aes(fill=mortality_rate)) +
        scale_fill_gradient2(
            midpoint = 0.13,
            low = "green",
            mid="red",
            high = "purple",
            labels=scales::percent) +
    theme_void() +
    ggtitle(
        "Corona mortality rate",
        subtitle = "(Data from 2020-12-31)") +
    theme(
        plot.title =  element_text(hjust=0.5),
        legend.position = "top",
        legend.title = element_blank(),
        plot.subtitle= element_text(hjust=0.5,vjust = 5,  size = 8))
  })

  #Plot Correcation

  countrysample = c("sm","cu","ye","cf","mx","de","it","ng","in")
  output$scatterplot_second <- renderPlot({
    df_normalize <- df %>% filter(Date_reported == as.Date("2020-12-31") &Cumulative_cases >= 10000)
    df_normalize$child_mortality_norm <- 1-normalize(df_normalize$child_mortality_per_1k)
    df_normalize$physicans_norm <- normalize(df_normalize$physicans_per_1k)
    df_normalize$life_expectancy_norm <- normalize(df_normalize$life_expectancy)
    df_normalize$health_expenditures_norm <- normalize(df_normalize$health_expenditures_usd)
    
    df_normalize$iso2_Lower <- tolower(df_normalize$iso2)
    df_normalize <- df_normalize %>% mutate(score_health = select(.,input$score_picker) %>% rowSums())
    avg_mortality <- mean(df_normalize$mortality_rate, na.rm=TRUE)
    avg_healthscore <- mean(df_normalize$score_health, na.rm=TRUE)
    df_normalize$color <- "High mortality/
  High score"
    df_normalize$color[df_normalize$mortality_rate <= avg_mortality & df_normalize$score_health>=avg_healthscore] <- "Low mortality/
  High score"
    df_normalize$color[df_normalize$mortality_rate <= avg_mortality & df_normalize$score_health<=avg_healthscore] <- "Low mortality/
  Low score"
    df_normalize$color[df_normalize$mortality_rate > avg_mortality & df_normalize$score_health<=avg_healthscore] <- "High mortality/
  Low score"
    ggplot(data = df_normalize, mapping = aes(x=mortality_rate, y = score_health)) +
    geom_point(aes(color=color)) +
    geom_flag(data = filter(df_normalize,iso2_Lower %in% countrysample),aes(country=iso2_Lower))+
    theme_linedraw() +
    theme(
        axis.line = element_line(color="#2C3E50", linetype="solid"),
        axis.text = element_text(color="#2C3E50"),
        panel.border = element_rect(color = "#2C3E50"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        panel.grid.major = element_line(colour='grey'),
        panel.grid.minor = element_line(colour='grey')
        )+
    labs(y="Health score", x="Mortality Corona") +
    geom_hline(yintercept = avg_healthscore, color = "#2C3E50", size =1.25) +
    geom_vline(xintercept = avg_mortality, color = "#2C3E50", size =1.25)+
    guides(colour = guide_legend(label.position = "bottom"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
