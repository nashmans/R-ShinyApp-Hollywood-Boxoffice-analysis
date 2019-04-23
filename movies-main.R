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
                 selectInput("BoxOffice", label = h4("Select a box office:"),
                             choices = list("american_box_office", 
                                            "international_box_office",
                                            "total_box_office"),
                             selected = "total_box_office"),
                 
                 selectInput("studios", label = h4("Studios"), 
                             choices = list("studio"),
                             selected = list("studio")),
                 
                 selectInput("year", label = h4("Release Year"), 
                             choices = list("release_year"),
                             selected = list("release_year")),
                 
                 sliderInput('plotHeight', 'Height of plot (in pixels)', 
                             min = 100, max = 500, value = 300),
                 #sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 10)
                 br(), # Extra Break
                 br(), # Extra Break  
                 br(), # Extra Break
                 radioButtons('select_market',"Select Market",inline = TRUE,
                              choices = c("America","Internat_l"),
                              selected = 'America')
    ), #SidebarPanel START
    # MAIN PANEL STARTS
    mainPanel(
      #tableOutput(""),
      h4("Major Studio vs. Independent Revenue ($)"),
      plotlyOutput("plot"),
      h4("Market Share : Global"),
      chorddiagOutput("chordPlot")
    ) # MAIN PANEL ENDs
  )
  #SidebarLayout ENDS
)
#********************************** FLUID_PAGE_END_HERE *************************************
)
#**********************************RENDER UI START HERE *************************************



#******************************* DATA_SOURCE+MUNGING_START *********************************

#read files
movies <-  read_csv("C:/Users/Avinash/Documents/R Studio/Project/Shiny_movie_multipages/tmdb_5000_movies.csv")
credits <-  read_csv("C:/Users/Avinash/Documents/R Studio/Project/Shiny_movie_multipages/tmdb_5000_credits.csv")
# rename and join by movie id,title[NOTE:LEft join]
movies <-  movies %>% rename(movie_id = id )  # rename id to movie_id
movies_all <-  movies %>% left_join(credits, by=c('movie_id','title' )) 
names(movies_all)
# COLUMN RENAMES/NEW IMP COLUMNS!!!
#movies_all <-  movies_all %>% rename(title = title.x)  # renaming to << TITLE >>
#movies_all <-  movies_all %>% select(-title.y)  # droppping duplicate title_id
# creating new column -> YEAR
movies_all <-  movies_all %>% mutate(release_date = as.Date(release_date,"%Y-%m-%d"))
movies_all <-  movies_all %>% mutate(release_year = as.numeric(format(release_date,"%Y")))
# drop original title -- > incorrect and duplicate 
movies_all <-  movies_all %>% select(-original_title)

#                  ************ MASTER DATA (above)***************

# subset movie_a manipulation 
movies_a <- movies_all %>%  select(budget, movie_id,original_language,title,
                                   production_companies,production_countries ,release_date,
                                   release_year,revenue, vote_average,runtime,vote_count) %>%
  filter(original_language == 'en')
names(movies_a)
str(movies_a)

# BIG FIVE/major studio Column Data
#Creating a new column production to represent the big film studio data ( 5 companies)
movies_a <-  movies_a %>% mutate(major_studio = case_when(
  str_detect(production_companies, "Disney") ~ "Disney",
  str_detect(production_companies,"Fox") ~ "Disney",
  str_detect(production_companies,"Warner") ~ "Warner",
  str_detect(production_companies,"Sony") ~ "SonyColumbia",
  str_detect(production_companies,"Universal") ~ "Universal",
  str_detect(production_companies,"Paramount") ~ "Paramount",
  TRUE~ "Minor Studios"
)) 
#Reading Boxoffice data scrapd from www.NUMBERS
bo_data <-  read_csv("C:/Users/Avinash/Documents/R Studio/Project/Shiny_movie_multipages/bo_data.csv")

# Renaming BOx office data to match MOVIE_a
bo_data <-  bo_data %>% rename(title = movie, release_year = year_released )
names(bo_data)
str(bo_data)
#JOINS ----> movies_a + box_ofice data -> TMDB movies_a + box office 

#Join for HISTOGRAM ONLY ----> Major&Minor, Box Office revenues(total_box_office)
major_studio_bo <-  movies_a %>% select(title, major_studio,release_year) %>% 
  # filter(major_studio != "Minor Studios") %>% 
  inner_join(bo_data,by = c( "title","release_year" )) %>% 
  mutate(as.numeric(release_year))
#Join for CHORD_DIAG ONLY ----> Major,Box Office revenues
major_studio_chord <-  movies_a %>% select(title, major_studio,release_year) %>% 
  filter(major_studio != "Minor Studios") %>% 
  inner_join(bo_data,by = c( "title","release_year" )) %>% 
  mutate(as.numeric(release_year))

#LINE CHART data ## rename file names 
histo_bo <-  major_studio_bo %>% select(major_studio,american_box_office,
                                        international_box_office,release_year,total_box_office) %>% 
  mutate(studio = if_else(major_studio == "Minor Studios", "Minor Studios", "Major Studios")) %>% 
  group_by(studio, release_year) %>% 
  summarise(american_box_office = sum(american_box_office, na.rm = TRUE),
            international_box_office = sum(international_box_office, na.rm = TRUE),
            total_box_office = sum(total_box_office, na.rm = TRUE))
#names(histo_bo)
#CHORD DIAG data ## rename file names
major_bo <-  major_studio_chord %>%  select(major_studio,american_box_office,
                                            international_box_office) %>% 
  group_by(major_studio) %>% summarise(AMER_BoxOffice=sum(american_box_office, na.rm = TRUE), 
                                       INT_BoxOffice=sum(international_box_office, na.rm = TRUE))

#CREATING outputs for two plots based on radio button selection
# conversion DF to matrix and assign row names
# problem with selection of individual box office category
set.seed(1)   # for reproducibility
major_bo_AMER = data.matrix(major_bo[,2:3])
row.names(major_bo_AMER) = c("Disney","Paramount","SonyColombia","Universal","Warner")
#major_bo_AMER = major_bo_AMER[,-1] # na column created on conversion
major_bo_AMER = t(major_bo_AMER)
#matrix 2nd time to retain only one row (#pain!!)
cdg = as.matrix(major_bo_AMER[1,])
cdg = t(cdg)
row.names(cdg) = c("America$ BoxOffice")
#class(major_bo_AMER)
#class(cdg)
set.seed(2)   # for reproducibility
major_bo_INT = data.matrix(major_bo)
row.names(major_bo_INT) = c("Disney","Paramount","SonyColombia","Universal","Warner")
major_bo_INT = major_bo_INT[,-1] # na column created on conversion
major_bo_INT = t(major_bo_INT)

# Define server logic required to draw a histogram # do I need this?
minx <- min(histo_bo$american_box_office)
maxx <- max(histo_bo$american_box_office)


#******************************* DATA_SOURCE+MUNGING_ENDS*************************


#******************************* OUTPUT_PLOT_FILES_START *********************************

# LINE GRAPH PLOT OUTPUT
output$plot <- renderPlotly({
  # size of the bins depend on the input 'bins'
  #size <- (maxx - minx) / input$bins
  
  p <- ggplot(histo_bo, aes_string(x  = input$year, y = input$BoxOffice,  
                                   color = input$studios)) + 
    geom_line(alpha=0.5) +
    scale_y_continuous(labels = comma)+
    labs(color = "Studio Type")+
    # theme_few() + scale_fill_few() # plain white/black border
    theme_tufte() + scale_fill_tableau() +
    ylab("Box Office Revenue $") + xlab("Years of Release")
  a <- list(tickangle = 45)
  
  ggplotly(p) %>% 
    layout(height = input$plotHeight, xaxis = a)
})

# CHORD DIAG PLOT OUTPUT
output$chordPlot <- renderChorddiag({
  
  if(input$select_market =="America"){
    chorddiag(cdg, type = "bipartite",showTicks = F, 
              groupnameFontsize = 13, groupnamePadding = 10, 
              groupPadding = 20,margin = 25,
              groupColors = c("#636e72","#778beb","#81ecec","#f8a5c2","#ffeaa7","#fab1a0"),
              tooltipGroupConnector = "    &#x25B6;    ",
              chordedgeColor = "#B3B6B7")
  }else{
    chorddiag(major_bo_INT, type = "bipartite", showTicks = F, 
              groupnameFontsize = 13, groupnamePadding = 10, 
              groupPadding = 20,margin = 25,
              groupColors = c("#636e72","#dfe6e9","#81ecec","#f8a5c2","#c3f6f6a","#ffeaa7","#fab1a0"),
              tooltipGroupConnector = "    &#x25B6;    ",
              chordedgeColor = "#B3B6B7"
    )
  }
})

#******************************* OUTPUT_PLOT_FILES_BEGIN *********************************


















