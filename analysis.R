library(tidyverse)
library(leaflet)

# The functions might be useful for A4
setwd("/Users/kaiaarmas/Documents/info201/assignments/a4-karmas-2120818-main/source")
source("a4-helpers.R")

#Questions:
  # Is there a disproportionate number of black people in prison?
  # How has the disproportionate number of black people changed over time?

  ## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
incarceration_date<-read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
#----------------------------------------------------------------------------#
Proprtion_of_black_jail<-incarceration_date %>%
 summarize(prop=sum(black_jail_pop,na.rm=TRUE)/sum(total_jail_pop,na.rm=TRUE)) %>%
pull(prop)
#----------------------------------------------------------------------------#
black_proportions<-incarceration_date %>%
  group_by(year) %>%
summarize(prop=sum(black_jail_pop,na.rm=TRUE)/sum(total_jail_pop,na.rm=TRUE))  

max_prop<-black_proportions %>%
 filter(prop==max(prop)) %>%
  pull(prop)

max_year<- black_proportions%>%
filter(year==max(year)) %>%
  pull(year)

min_prop<- black_proportions %>%
 filter(year>=1985) %>%
   filter(prop==min(prop)) %>%
  pull(prop)

min_year<-black_proportions %>%
  filter(year>=1985) %>%
  filter(year==min(year)) %>%
  pull(year)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  total<-incarceration_date %>%
    group_by(year) %>%
    summarize(jail_population=sum(total_jail_pop,na.rm=TRUE))
                
                
               
  # TODO: Implement this function 
return(total)   
}


# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  total<-get_year_jail_pop()
  plot<-ggplot(total,aes(x=year, y=jail_population)) +
    geom_bar(stat = "identity") +
    ggtitle("Increase of Jail Population in U.S.(1970-2018) ")+
    xlab("year") +
    ylab("Total Jail Population")+
    labs(caption = "Jail Population in the U.S. from 1970-2018")
  # TODO: Implement this function 
  return(plot)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
get_jail_pop_by_states<-function(states) {
  df<-incarceration_date %>%
    filter(state %in% states) %>%
    group_by(state,year) %>%
    summarize(jail_population=sum(total_jail_pop,na.rm=TRUE))
    
  
  
  
  
  return(df)   
}


plot_jail_pop_by_states<- function(states)  {
  total<-get_jail_pop_by_states(states)
  plot<-ggplot(total,aes(x=year, y=jail_population,colour=state)) +
    geom_line()+
    ggtitle("Increase of Jail Population in U.S.(1970-2018) ")+
    xlab("year") +
    ylab("Total Jail Population")+
    labs(caption = "Jail Increase in the U.S. from 1970-2018")
  # TODO: Implement this function 
  return(plot)   
} 




#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# 
#<variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas

  get_year_demo <- function() {
    total<-incarceration_date %>%
       group_by(year) %>%
      summarize(white_pop=mean(white_jail_pop_rate,na.rm=TRUE),black_pop=mean(black_jail_pop_rate,na.rm=TRUE))%>%
      drop_na()
    
   
    
    # TODO: Implement this function 
    return(total)   
  }


# This function ... <todo:  update comment>
plot_demo <- function()  {
  total<-get_year_demo()
 column1<-c(rep("Black",29),rep("White",29))
 column2<-c(total$black_pop,total$white_pop)
   temp<-cbind(column1,column2)
  result<-data.frame(year=total$year,race=factor(column1,levels = c("Black","White")),population=column2)
    

  plot<-ggplot(result,aes(x=year,y=population, fill=race)) +
    geom_bar(stat = "identity",position = position_dodge(), alpha = 0.75)+
    ggtitle("Increase of Jail Population in U.S.(1970-2018) ")+
    xlab("year") +
    ylab("Total Jail Population")+
    labs(caption = "Increase of Black Jail Population in the U.S from 1970-2018 ")
  # TODO: Implement this function 
  return(plot)   
} 


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
## Load data frame ---- 
demo_states<-function(){
    total<-incarceration_date %>%
      filter(year==max(year)) %>%
      group_by(state) %>%
      summarize(state_name=state.name[grep(state,state.abb)],black_pop=mean(black_jail_pop_rate,na.rm=TRUE))%>%
      replace_na(list(black_pop=0))
    total<-total %>%
     ungroup() %>%  
      add_row(state="DC",state_name="District of Columbia",black_pop=0) %>%
      arrange(state_name)
      
    
    
    
    # TODO: Implement this function 
    return(total)     
  
}

plot_demo_states<-function(){
  total<-demo_states()
  states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
  class(states)
  m <- leaflet(states) %>%
    setView(-96, 37.8, 4) %>%
    addTiles()
  m %>% addPolygons()
  bins <- c(0, 200, 500, 1000, 1500, 2000, 2500, 3000, Inf)
  pal <- colorBin("YlOrRd", domain = total$black_pop, bins = bins)
  
  m %>% addPolygons(
    fillColor = ~pal(total$black_pop),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7) %>%
  addLegend("bottomright", pal = pal, values = ~total$black_pop,
            title = "Black Prison Rate",
            labFormat = labelFormat(prefix = ""),
            opacity = 1
  )
}
plot_demo_states()