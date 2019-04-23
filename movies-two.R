# Already inside server

#PACKAGES
library(shiny)
library(jsonlite)
library(rjson)
library(tidyverse)
library(ggvis)
library(chorddiag)
library(plotly)
library(scales)
library(igraph)
library(ggthemes)
library(shiny)
library(plotly)
options(scipen = 999)
options(scipen = 999)

#**********************************RENDER UI START HERE *************************************

output$pageStub = renderUI(
  
  #**********************************FLUID PAGE START HERE *************************************
  fluidPage(
    # for SIdebar Aesthetics/HTML
    tags$head(tags$style(
      HTML('
         #sidebar {
            border: #dfe6e9 ;
            background-color:white;
        }
        body, label, input, button, select { 
          font-family: "Arial";
        }')
    )),
    
    titlePanel('Hollywood: Box Office Economics!'),
    #SidebarLayout START
    sidebarLayout(
      #SidebarPanel START
      sidebarPanel(width =3 , id="sidebar",
                   selectInput("budget", label = h4("Budget"),
                               choices = list("budget"),
                               selected = ("budget")),
                   
                   selectInput("studios", label = h4("Studios"), 
                               choices = list("parent_studio"),
                               selected = list("parent_studio")),
                   
                   selectInput("year", label = h4("Release Year"), 
                               choices = list("release_year"),
                               selected = list("release_year")),
                   
                   sliderInput('plotHeight', 'Height of plot (in pixels)', 
                               min = 100, max = 500, value = 300)
      ), #SidebarPanel START
      # MAIN PANEL STARTS
      mainPanel(
        #tableOutput(""),
        h4("Budget & Spending Power of Studios ($)"),
        plotlyOutput("smoothplot")
    
      ) # MAIN PANEL ENDs
    )
    #SidebarLayout ENDS
  )
  #********************************** FLUID_PAGE_END_HERE *************************************
)



#******************************* DATA_SOURCE+MUNGING_START *********************************


# subset movie_a manipulation 
movies_a <- movies_all %>%  select(budget, movie_id,original_language,title,
                                   production_companies,production_countries ,release_date,
                                   release_year,revenue, vote_average,runtime,vote_count) %>%
  filter(original_language == 'en')

# BIG FIVE/major studio Column Data
#Creating a new column major studio to represent the big film studio data ( 5 companies)

movies_a =  movies_a %>% mutate(major_studio = case_when(
  str_detect(production_companies, "Disney") ~ "Disney",
  str_detect(production_companies,"Fox") ~ "Disney",
  str_detect(production_companies,"Warner") ~ "Warner",
  str_detect(production_companies,"Sony") ~ "SonyColumbia",
  str_detect(production_companies,"Universal") ~ "Universal",
  str_detect(production_companies,"Paramount") ~ "Paramount",
  TRUE~ "Minor Studios"
)) 

# --!!!!!!!!!!!!!!!!!!!!!! DO not overwrite "  movie_a " !!!!!!!!!!!!!!!!!!!!!!!!!

#Reading Boxoffice data scrapd from www.NUMBERS
bo_data <-  read_csv("C:/Users/Avinash/Documents/R Studio/Project/bo_data.csv")

# Renaming BOx office data to match MOVIE_a
bo_data <-  bo_data %>% rename(title = movie, release_year = year_released )
names(bo_data)
str(bo_data)
#JOINS ----> movies_a + box_ofice data -> TMDB movies_a + box office 



# for output data 1988 to 2007 /  budget /spending power of major vs minor
# note the file" movie_a " is common across tabs
movies_a_1988 = movies_a %>% select(.,release_year,budget,title,major_studio) %>%
  filter(.,release_year >=1988 & release_year <=2007) 

# for output data 2008 onwards /  budget /spending power of major vs minor
#Creating a new column parent studio
studiodata = read.csv("C:/Users/Avinash/Documents/R Studio/Project/Bigfive_1.csv")
#for 2008 -> comprehenssive string select to show M&A activity where possible
movies_a_2008 =  movies_a %>% mutate(parent_studio = case_when(
  str_detect(production_companies, "Disney") ~ "Disney",
  str_detect(production_companies, "Marvel") ~ "Disney",
  str_detect(production_companies, "Lucas") ~ "Disney",
  str_detect(production_companies, "Pixar") ~ "Disney",
  str_detect(production_companies,"Fox") ~ "Disney",
  str_detect(production_companies,"Warner") ~ "Warner",
  str_detect(production_companies,"New Line") ~ "Warner",
  str_detect(production_companies,"DC") ~ "Warner",
  str_detect(production_companies,"Sony") ~ "SonyColumbia",
  str_detect(production_companies,"Columbia") ~ "SonyColumbia",
  str_detect(production_companies,"Funimation") ~ "SonyColumbia",
  str_detect(production_companies,"Madhouse") ~ "SonyColumbia",
  str_detect(production_companies,"Universal") ~ "Universal",
  str_detect(production_companies,"DreamWorks") ~ "Universal",
  str_detect(production_companies,"Paramount") ~ "Paramount",
  str_detect(production_companies,"Nickelodeon") ~ "Paramount",
  TRUE~ "Minor Studios"
))%>% filter(.,release_year >=2008) %>% 
  select(.,release_year,budget,title,parent_studio)


#******************************* DATA_SOURCE+MUNGING_ENDS*************************


#******************************* OUTPUT_PLOT_FILES_START *********************************


#--------------- PLOT OUTPUT for a) 1988 to 2007 b) 2008 onwards

#names(movies_a_2008)

plot2008 = ggplot(movies_a_2008, mapping = aes(x = release_year,y = budget,
                                               color = parent_studio)) + 
  geom_smooth(method = "loess", se = F) +
  scale_y_continuous(labels = comma)

output$smoothplot = renderPlotly({
  
  
  a = list(tickangle = 45)
  
  ggplotly(plot2008) %>% 
    layout(height = input$plotHeight, xaxis = a)
})
  
  
