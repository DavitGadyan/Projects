library(shiny)
library(shinydashboard)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(qdap)
library(ggplot2)
library(dplyr)
library(data.table)
library(plotly)
library(DT)

library(tidyverse)
library(wordcloud)
library(tm)

jobs_2005_2018<-read.csv('df_final5.csv',stringsAsFactors = F)
jobs_2005_2018[,1]<-NULL
jobs_2005_2018$opening_date<-as.Date(jobs_2005_2018$opening_date,'%Y-%m-%d')
jobs_2005_2018$application_deadline_date<-as.Date(jobs_2005_2018$application_deadline_date,'%Y-%m-%d')

## Title plotting fucntion
it_wordcloud_f<-function(year=2018,min_freq=3,max_words =30){
  if (year[1]==year[-1]){
    it_title<-jobs_2005_2018%>%
      mutate(Year=format(jobs_2005_2018$opening_date,'%Y'))%>%
      filter(Year %in% c(year)&industry %in% c('Information Technology'))%>%
      group_by(title)%>%
      summarise(count=n())%>%
      mutate(per=count/sum(count)) %>% 
      arrange(desc(per))
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
  text(x=0.5, y=0.5, labels = paste("Most popular IT jobs in ",year,'year'),cex=1.7)
  w<-wordcloud(words = it_title$title,freq = it_title$count,min.freq = min_freq,max.words = max_words,random.order = F,scale = c(3,0.9),
               colors=brewer.pal(8,'Dark2'),main='Title')
  }else if(year[1]!=year[-1]){
    it_title<-jobs_2005_2018%>%
      mutate(Year=format(jobs_2005_2018$opening_date,'%Y'))%>%
      filter(Year %in% c(year)&industry %in% c('Information Technology'))%>%
      group_by(title)%>%
      summarise(count=n())%>%
      mutate(per=count/sum(count)) %>% 
      arrange(desc(per))
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, labels = paste("Most popular IT jobs from ",year[1],' to ',year[-1],'years'),cex=1.7)
    w<-wordcloud(words = it_title$title,freq = it_title$count,min.freq = min_freq,max.words = max_words,random.order = F,scale = c(3,0.9),
                 colors=brewer.pal(8,'Dark2'),main='Title')
  }

}

skills_v<-c('HTML','CSS','\\s+R{1}\\s+','PYTHON','C\\+{1,2}','C\\#' ,'GIT|GITHUB','\\.NET','VISUALBASIC\\.NET','JQUERY','AGILE','ASP\\.NET','JAVASCRIPT','JAVA','SQL','MYSQL','SQL SERVER','PHP','JSP','ASP','UNIX','ORACLE','XML','XSLT','\\s+OOP\\s+','\\s+OOD\\s+','DHMTL','FLASH','APACHE','ASP.NET','LINUX','APACHE','MS ACESS','WINDOWS')

skills_f<-function(year=2018){
  if (year[1]==year[-1]){
  it_df<-jobs_2005_2018%>%
    mutate(Year=format(jobs_2005_2018$opening_date,'%Y'))%>%
    filter(Year %in% c(year)&industry %in% c('Information Technology'))
  t<-toupper(it_df$required_qualifications)
  skills_l<-str_extract_all(t,pattern =paste(skills_v,collapse = '|') )
  skills_l<-na.omit(unlist(skills_l))
  skills_l<-data.frame(skills_l)
  skills_df<-skills_l%>%
    group_by(skills_l)%>%
    summarise(count=n())%>%
    mutate(per=count/sum(count)) %>% 
    arrange(desc(per))
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, labels = paste("Most demanded IT skills in ",year,'year'),cex=1.7)
  w<-wordcloud(words = skills_df$skills_l,freq = skills_df$count,min.freq = 50,scale=c(5,1),random.order = F,
               colors=brewer.pal(8,'Dark2'),main='Title')
  }else if(year[1]!=year[-1]){
                 it_df<-jobs_2005_2018%>%
                   mutate(Year=format(jobs_2005_2018$opening_date,'%Y'))%>%
                   filter(Year %in% c(year)&industry %in% c('Information Technology'))
                 t<-toupper(it_df$required_qualifications)
                 skills_l<-str_extract_all(t,pattern =paste(skills_v,collapse = '|') )
                 skills_l<-na.omit(unlist(skills_l))
                 skills_l<-data.frame(skills_l)
                 skills_df<-skills_l%>%
                   group_by(skills_l)%>%
                   summarise(count=n())%>%
                   mutate(per=count/sum(count)) %>% 
                   arrange(desc(per))
                 layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
                 par(mar=rep(0, 4))
                 plot.new()
                 text(x=0.5, y=0.5, labels = paste("Most demanded IT skills from ",year[1],' to ',year[-1],'years.'),cex=1.7)
                 w<-wordcloud(words = skills_df$skills_l,freq = skills_df$count,min.freq = 50,scale=c(5,1),random.order = F,
                              colors=brewer.pal(8,'Dark2'),main='Title')}
  
}

## dashboard header
header <- dashboardHeader(title = 'Armenian Job Market Analysis')

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
    menuItem("Explore Raw Data", tabName = "data", icon = icon("calendar", lib = 'glyphicon')),
    menuItem("Plots", tabName = "plots", icon = icon("bar-chart"), badgeLabel = "app", badgeColor = "purple"),
    menuItem("Most popular IT jobs", tabName = "pop_jobs", icon = icon("bullhorn", lib = 'glyphicon'), badgeLabel = "app", badgeColor = "purple"),
    menuItem("Most demanded IT skills", tabName = "dem_skills", icon = icon("bullhorn", lib = 'glyphicon'), badgeLabel = "app", badgeColor = "purple")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "instructions",
            h2("Armenian Job Market Analysis"),
            br(),
            h4("In this shiny dashboard we analyze Armenian Job Market for the period starting from 2005 to 2018 years."),
            h4("As a matter of fact online job openings have become very popular in recent years and more and more representing the true demand for jobs in Armenia, particularly for IT industry."),
            h4("This dashboard comprized of three analysis parts. First is 'Explore Raw Data', where user may observe the whole dataset."),
            h4("In the'Plots' section section we have created several interactive plots  for you to play with and explore our data on the historical basis by industry."),
            h4("Next we present two sections,'Most popular IT jobs' and 'Most demanded IT skills', devoted to the most popular IT jobs and most demanded IT skils on a yearly basis in the from of wordclouds."),
            br(),
            h4("The dataset for analysis is manually scrapped from a popular Armenian online job posting website."),
            h4("You may acess the website via the following link:>>>>>>>"),
            a(">>>>>>>CareerCenter", href = "https://groups.yahoo.com/neo/groups/careercenter-am/info", style = "font size: 14pt"),
            br(),
            br(),
            h4("This dashboard primary serves as a mean of communicating the results from Armenian Job Market Analyis report but you may also find some interactivity tools to explore insights from data on your own. So feel free to use them!!!!")
            
    ),
    
    tabItem(tabName = "plots",
            
            fluidRow(
              box(
                plotOutput('plot1'), 
                width = 8, 
                height = 500),
              br(),
              br(),
              br(),
              h4("As we may easily observe the online job openings had doubled from 2005 to 2017. The number of postings
peacked in 2017. Nevertheless, in 2018 the number of job openings was lower compared to 2017.
We will look closer to this situation because we cannot conclude conlusively looking only on the above
barplot Chart1. The primary reason for the previous statement is that 2018 year does not include
full year data as year did not ended at the time of this analysis.")
              
            ),
            
            br(),
            br(),
            h4("As we can observe the latest date is 2018-08-31. So we will visualize in our next graph 2017 and 2018 from years start and
until this date."),
            fluidRow(
              box(
                plotOutput('plot2'), 
                width = 10, 
                height = 400)
            ),
            br(),
            br(),
            h4("As we stated previously 2018 year does not represent full year. It only includes period from January to August.
Despite this we may observe some decline in the number of job postings in 2018 compared to 2017, particularly in
April and August. Let's further discover this two years."),
            fluidRow(
              box(
                plotOutput('plot3'), 
                width = 10, 
                height = 400)
            ),
            br(),
            br(),
            h4("The above graph puts side by side 2017 and 2018 years and makes comparison between them easier.
Chart3 supported our preliminary observation during the June, July and August number of online job postings
               reduced despite the fact thet in 2018's first two months clearly dominated over the same period in 2017.
               "),
            fluidRow(
              box(
                plotOutput('plot4'), 
                width = 12, 
                height = 400)
            ),
            br(),
            h4("As we may easily observe the biggest proportion in our dataset takes Information Technology industry or
job category other way around. This is very interesting pattern in our online job postings dataset and
we will conduct deeper analysis especially into this job sector further in our analysis."),
            fluidRow(
              box(
                plotOutput('plot5'), 
                width = 10, 
                height = 400)
            ),
            br(),
            h4("The Chart5 perfectly presents situationin terms of whether 2018 was lower in the number of job
postings. As we stated before 2018 year data was not a full as it included months from January-August.
it is obvious taht in 2018 in first 8 months there was any decline and in the number of online
job postings and results at comparabale periods are pretty similar even with 2018 in January and February
clealry leading in the number of jobs. Nevertheless this results stalled in March and especially
in April. The timing follows the Armenian Velvet Revolution which had taken place during April and
clearly people and businesses we generally consumed by political activities rather than mere business market.
"),
            fluidRow(
              box(
                plotOutput('plot6'), 
                width = 12, 
                height = 500)
            ),
            br(),
            h4("As we clealry observe the IT industry represents the biggest proportion of all online job postings.
The second in the number of job postings is Management category. Let's observe whether the same holds true in 2017 year.
"),
            fluidRow(
              box(
                plotOutput('plot7'), 
                width = 12, 
                height = 500)
            ),
            br(),
            h4("As we see in 2017 the proprtion of IT was even higher around 44.1% compared to 2018 41.4% i.e fall by approximatelly 3%.
Now let's see what job title comprises IT industry in order to define the most demanded IT job in Armenia.
"),
            br(),
            br(),
            br(),
            h4("Now is you turn!!! You may choose year and analyse the proportions of job postings for each industry in for any period from 2015-2018 year.
"),
            fluidRow(
              box(
                width = 12,
                selectInput(inputId = 'year',
                            label = 'Choose a Year for Analysis',
                            choices = 2005:2018,
                            selectize = TRUE,
                            selected = 2018)
              )
            ),
          
  
            fluidRow(
              box(
                plotlyOutput('plot8'), 
                width = 12, 
                height = 500)
            )
            
    ),
    
    tabItem(tabName = "pop_jobs",
            h4('You are free to choose the year or even period of years to detect Most Popular IT Jobs for that time span.'),
            fluidRow(
              box(width = 12,sliderInput("Year_slider", 
                              label = "Choose year range",
                              min = 2005, 
                              max = 2018, 
                              value = c(2018, 2018), 
                              sep = "")
              )
              
            ),
           
            fluidRow(
              box(width = 12,height = 850,
                plotOutput("word_plot_job_title")
              )
            ),
            br(),
            h4('The initial wordcloud is selected with the best parameters in our view but we gave chance to play around with your own parameteres!!!'),
            tags$b('Nevertheless we  strongly suggest to use the default parameters because you may run at
               risk not fully representing worcloud value as it may not to appear due to size.'),
            fluidRow(
              box(width = 6,sliderInput("min_freq_slider", label = h3("Choose minimum frequency for the wordcloud"), min = 3, 
                                         max = 20, value = 3)
              ),
              box(width = 6,sliderInput("max_words_slider", label = h3("Choose maximum # of words for the wordcloud"), min = 7, 
                                        max = 100, value = 30)
              )
              
            )
            
            
    ),
    tabItem(tabName = "dem_skills",
            h4('You are free to choose the year or even period of years to detect Most Demanded IT Skills for that time span.'),
            fluidRow(
              box(width = 12,sliderInput("Year_slider2", 
                                         label = "Choose year range",
                                         min = 2005, 
                                         max = 2018, 
                                         value = c(2018, 2018), 
                                         sep = "")
              )
              
            ),
            
            fluidRow(
              box(width = 12,height = 520,
                  plotOutput("word_plot_job_skills")
              )
            )
      
            
    ),
    
    tabItem(tabName = "data",
            
            fluidRow(
              box(
                dataTableOutput("table"),
                width = 12
              )
            )
            
    )
  )
)


ui <- dashboardPage(header, sidebar, body, skin = 'blue')



server <- function(input,output){
  
  options(scipen = 666)
  
  df_jobs1<-reactive({
    df_jobs1<-jobs_2005_2018%>%
      mutate(Year=format(jobs_2005_2018$opening_date,'%Y'))%>%
      filter(Year %in% c(input$year))%>%
      group_by(Year,industry)%>%
      summarise(count=n())%>%
      mutate(per=count/sum(count)) %>% 
      arrange(desc(per))
  })
  

  jobs <- reactive({
    jobs_2005_2018 <- data.frame(jobs_2005_2018)
    
    jobs_2005_2018 <- con_types(jobs_2005_2018)
  })
  

  output$plot1 <- renderPlot({
    
    jobs_2005_2018%>%
      group_by(format(jobs_2005_2018$opening_date,'%Y'))%>%
      summarise(count=n())%>%
      filter(`format(jobs_2005_2018$opening_date, "%Y")`>2004)%>%
      ggplot(aes(x=`format(jobs_2005_2018$opening_date, "%Y")`,y=count))+geom_bar(stat = 'identity')+
      labs(title='Chart1.Historical Job Posting in Armenia 2005-2018*',x='Year',y='Number of job postings')+
      theme(axis.text.x = element_text(angle=45))
  })
  
  output$plot2 <- renderPlot({
    
    jobs_2005_2018%>%
      mutate(Year=format(jobs_2005_2018$opening_date,'%Y'),Month=format(jobs_2005_2018$opening_date,'%B'))%>%
      filter(Year %in% c(2017,2018))%>%
      group_by(Year,Month)%>%
      summarise(count=n())%>%
      arrange(match(Month, month.name))%>%
      ggplot(aes(x=factor(Month, levels = month.name),y=count,fill=Year))+geom_bar(stat = 'identity')+
      labs(title='Chart2. Job Posting in Armenia 2017-2018*',x='Month',y='Number of job postings')+
      theme(axis.text.x = element_text(angle=45))+facet_grid(Year~.)
  })
  
  output$plot3 <- renderPlot({
    
    jobs_2005_2018%>%
      mutate(Year=format(jobs_2005_2018$opening_date,'%Y'),Month=format(jobs_2005_2018$opening_date,'%B'))%>%
      filter(Year %in% c(2017,2018))%>%
      group_by(Year,Month)%>%
      summarise(count=n())%>%
      arrange(match(Month, month.name))%>%
      ggplot(aes(x=factor(Month, levels = month.name),y=count,fill=Year))+geom_bar(stat = 'identity',position ='dodge')+
      labs(title='Chart3. Job Posting in Armenia 2017-2018*',x='Month',y='Number of job postings')+
      theme(axis.text.x = element_text(angle=45))
  })
  
  output$plot4 <- renderPlot({
    
    jobs_2005_2018%>%
      mutate(Year=format(jobs_2005_2018$opening_date,'%Y'),Month=format(jobs_2005_2018$opening_date,'%B'))%>%
      filter(Year %in% c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))%>%
      group_by(Year,Month,industry)%>%
      summarise(count=n())%>%
      arrange(match(Month, month.name))%>%
      ggplot(aes(x=Year,y=count,fill=industry))+geom_bar(stat = 'identity')+
      labs(title='Chart4. Job Posting in Armenia by Industry 2015-2018*',x='Year',y='Number of job postings')+
      theme(axis.text.x = element_text(angle=45))+facet_wrap(industry~.)
    
  })
  
  output$plot5 <- renderPlot({
    
    jobs_2005_2018%>%
      mutate(Year=format(jobs_2005_2018$opening_date,'%Y'),Month=format(jobs_2005_2018$opening_date,'%B'))%>%
      filter(Year %in% c(2014,2015,2016,2017,2018))%>%
      group_by(Year,Month,industry)%>%
      summarise(count=n())%>%
      arrange(match(Month, month.name))%>%
      filter(Month %in% c("January","February","March","April","May","June","July","August" ))%>%
      ggplot(aes(x=Year,y=count,fill=factor(Month, levels = month.name)))+geom_bar(stat = 'identity')+
      labs(title='Chart5. Job Posting in Armenia by Industry 2014-2018 from January-August*',x='Year',y='Number of job postings')+
      theme(axis.text.x = element_text(angle=45))+guides(fill=guide_legend(title="Months"))
  })
  
  output$plot6 <- renderPlot({
    
    jobs_2005_2018%>%
      mutate(Year=format(jobs_2005_2018$opening_date,'%Y'))%>%
      filter(Year %in% c(2018))%>%
      group_by(Year,industry)%>%
      summarise(count=n())%>%
      mutate(per=count/sum(count)) %>% 
      arrange(desc(per))%>%
      ggplot(aes(x='',y=per,fill=industry))+geom_bar(width = 1, size = 1, color = "white", stat='identity')+
      coord_polar("y", start=0)+geom_text(aes(label = paste(round(100*per, 2), "%", sep="")),position = position_stack(vjust = 0.5))+
      labs(x = NULL, y = NULL, fill = NULL, 
           title = "Chart6. Jobs by categories 2018") +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, color = "#666666"))
  })
  
  output$plot7 <- renderPlot({
    
    jobs_2005_2018%>%
      mutate(Year=format(jobs_2005_2018$opening_date,'%Y'))%>%
      filter(Year %in% c(2017))%>%
      group_by(Year,industry)%>%
      summarise(count=n())%>%
      mutate(per=count/sum(count)) %>% 
      arrange(desc(per))%>%
      ggplot(aes(x='',y=per,fill=industry))+geom_bar(width = 1, size = 1, color = "white", stat='identity')+
      coord_polar("y", start=0)+geom_text(aes(label = paste(round(100*per, 2), "%", sep="")),position = position_stack(vjust = 0.5))+
      labs(x = NULL, y = NULL, fill = NULL, 
           title = "Chart7. Jobs by categories 2017") +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, color = "#666666"))  })
  
  output$plot8 <- renderPlotly({
    
    plot_ly(
      df_jobs1(), labels = ~industry, values = ~count, type = 'pie') %>%
      layout(title = paste('Chart8. Jobs by categories in ',input$year),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) })
  
  ## text mining part
  
  output$word_plot_job_title <- renderPlot({
    
    it_wordcloud_f(year=input$Year_slider,min_freq = input$min_freq_slider,max_words = input$max_words_slider)
    
  }, height = 820, width = 1000)
  
  
  output$word_plot_job_skills <- renderPlot({
    
    skills_f(year=input$Year_slider2)
    
  }, height = 500, width = 1000)
  
  
  
  
  
  output$table<-DT::renderDataTable({
    DT::datatable(select(jobs_2005_2018,company,title,required_qualifications,opening_date,industry),options = list(lengthMenu = c(5, 30, 50), pageLength = 20))
  })
  
}


shinyApp(ui = ui, server = server)