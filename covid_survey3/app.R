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

df_covid=read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
df_covid <- df_covid %>%mutate(mortality_rate = Cumulative_deaths/Cumulative_cases)
df_covid_current <- df_covid %>%filter(Date_reported == as.Date("2020-12-31") & Cumulative_cases >=1000)

df_health = read_csv('data/health_expenditures.csv', skip = 3)
df_child_mortality = read_csv('data/child_mortality_rate.csv', skip = 3)
df_life_expectancy = read_csv('data/life_expectancy.csv', skip = 3)
df_physicans = read_csv('data/physicians.csv', skip=3)

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

library(countrycode)
df_joined <- df_joined %>%
  mutate(iso2 = countrycode(`Country Name`, origin="country.name", destination = "iso2c")) %>%
  filter(!is.na(iso2)) %>%
  select(iso2 ,child_mortality_per_1k, physicans_per_1k, life_expectancy, health_expenditures_usd)
df_covid_current <- df_covid_current %>%
  mutate(iso2 = countrycode(Country, origin="country.name", destination = "iso2c")) %>%
  filter(!is.na(iso2)) %>%
  select(Country, iso2, mortality_rate,Cumulative_cases)
df <- df_covid_current %>%
  left_join(df_joined, by= "iso2")

data("World", package = "tmap")
world <- World
world <- world %>%
  mutate(iso2 = countrycode(iso_a3, origin = "iso3c", destination = "iso2c"))

world <- world %>%
  left_join(df, by= "iso2")

# data preparation scatterplot health_exp

df_healthExpenditures <-df %>% filter(Cumulative_cases >= 10000)
avg_mortality <- mean(df_healthExpenditures$mortality_rate, na.rm=TRUE)
avg_healthExpenditures <- mean(df_healthExpenditures$health_expenditures_usd, na.rm=TRUE)
df_healthExpenditures$color <- "High mortality/
High score"
df_healthExpenditures$color[df_healthExpenditures$mortality_rate <= avg_mortality & df_healthExpenditures$health_expenditures_usd>=avg_healthExpenditures] <- "Low mortality/
High score"
df_healthExpenditures$color[df_healthExpenditures$mortality_rate <= avg_mortality & df_healthExpenditures$health_expenditures_usd<=avg_healthExpenditures] <- "Low mortality/
Low score"
df_healthExpenditures$color[df_healthExpenditures$mortality_rate > avg_mortality & df_healthExpenditures$health_expenditures_usd<=avg_healthExpenditures] <- "High mortality/
Low score"
df_healthExpenditures$iso2_Lower <- tolower(df_healthExpenditures$iso2)

# data preparation scatterplot health_score

df_normalize <- df
normalize <- function(x) {
    return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x,na.rm = TRUE)))
    }
df_normalize$child_mortality_norm <- 1-normalize(df_normalize$child_mortality_per_1k)
df_normalize$physicans_norm <- normalize(df_normalize$physicans_per_1k)
df_normalize$life_expectancy_norm <- normalize(df_normalize$life_expectancy)
df_normalize <- df_normalize %>% mutate(score_health = child_mortality_norm+life_expectancy_norm+physicans_norm)
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

df_normalize$iso2_Lower <- tolower(df_normalize$iso2)


ui <- bootstrapPage(
    navbarPage("Covid Survey",
               theme = shinytheme("flatly"),
               tabPanel(
                   title = "Home",
                   h3("Introduction")
               ),
               tabPanel(
                   title = "World Map",
                   sidebarLayout(
                       sidebarPanel(
                           "Interactive Widgets"
                       ),
                       mainPanel(
                           plotOutput(outputId = 'myplot')
                       )
                   )
               ),
               tabPanel(
                   title = "Correlation",
                   sidebarLayout(
                       sidebarPanel(
                           "Interactive Widgets"
                       ),
                       mainPanel(
                           plotOutput(outputId = 'scatterplot_first'),
                           plotOutput(outputId = 'scatterplot_second')
                       )
                   )
               )
    )
)

server <- function(input, output) {
# Plot 1
 output$myplot <- renderPlot({
    ggplot(data= world) +
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



#Plot 2

countrysample = c("mx","us", "de","ie","ch","ec","au","ye","sg")
output$scatterplot_first <- renderPlot({
    ggplot(data = df_healthExpenditures, mapping = aes(x=mortality_rate, y = health_expenditures_usd)) +
    geom_point(aes(color=color)) +
    geom_flag(data = filter(df_healthExpenditures,iso2_Lower %in% countrysample),aes(country=iso2_Lower))+
    theme_linedraw() +
    theme(
        axis.line = element_line(color="black", linetype="solid"),
        axis.text = element_text(color="black"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=8))+
    labs(y="Health Expenditures", x="Mortality Corona") +
    geom_hline(yintercept = avg_healthExpenditures) +
    geom_vline(xintercept = avg_mortality)+
    guides(colour = guide_legend(label.position = "bottom"))
})

#plot3

countrysample = c("sm","cu","ye","cf","mx","de","it","ng","in")
output$scatterplot_second <- renderPlot({
    ggplot(data = df_normalize, mapping = aes(x=mortality_rate, y = score_health)) +
    geom_point(aes(color=color)) +
    geom_flag(data = filter(df_normalize,iso2_Lower %in% countrysample),aes(country=iso2_Lower))+
    theme_linedraw() +
    theme(
        axis.line = element_line(color="black", linetype="solid"),
        axis.text = element_text(color="black"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=8))+
    labs(y="Health score", x="Mortality Corona") +
    geom_hline(yintercept = avg_healthscore) +
    geom_vline(xintercept = avg_mortality)+
    guides(colour = guide_legend(label.position = "bottom"))
})
}

# Run the application
shinyApp(ui = ui, server = server)
