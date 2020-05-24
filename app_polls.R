##################################################
## Project: Polls analysis
## Script purpose: Shiny app for analysis and visualization of election polls in Sweden
## Date: 2018-10-01 
## Author: Marcus SjÃ¶lin
##################################################


# Read in necessary libraries
library("plyr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("reshape2")
library("ggrepel")
library("ggthemes")
library("readr")
library("stringr")
library(hrbrthemes)
library(lubridate)
library(directlabels) # Plotting labels

library(shiny)
library(shinydashboard)

########################## READ IN AND PREPARE DATA ##########################

# Read in data
polls<-read.delim("https://raw.githubusercontent.com/hjnilsson/SwedishPolls/master/Data/Polls.csv", header=TRUE, sep=",")
elections<-read.delim("https://raw.githubusercontent.com/msjoelin/polls_analysis/master/Election.csv", header=TRUE, sep=",")

# Clean poll-data, put into long format for easier reporting
polldata<-polls %>% 
  gather(Party, Percentage, c("M", "L", "C", "KD", "S", "V", "MP", "SD", "FI", "Uncertain")) %>%
  select(PublDate, house, Party, Percentage) %>%
  filter(!(Party %in% c("FI", "Uncertain"))) %>% # Remove small parties
  mutate(type=ifelse(house %in% c("YouGov", "Inizio", "Sentio", "United Minds"), "Internet", "Classic"),  # Group polls into Internet and Classic Polls
                                block=ifelse(Party %in% c("M", "L", "C", "KD"), "Alliansen",  # Group parties into traditional coalitions
                                             ifelse(Party=="SD", "SD", 
                                                    ifelse(Party %in% c("MP", "S", "V"), "RÃ¶dgrÃ¶na", "Other"))),
                                PublYearMonth=as.Date(paste(year(PublDate), str_pad(month(PublDate), 2, pad="0"), "01", sep="-")),
                                ElectionYear=ifelse(PublYearMonth<="2002-09-15", NA, # Add electionyear
                                                    ifelse(PublYearMonth<="2006-09-17", 2006,
                                                           ifelse(PublYearMonth<="2010-09-19", 2010,
                                                                  ifelse(PublYearMonth<="2014-09-14", 2014, 2018)))))

# Set formats
polldata$PublDate<-as.Date(polldata$PublDate)
polldata$type<-as.factor(polldata$type)


# Put electiondata into long format
electiondata<-elections %>% 
  gather(Party, Result, c("M", "L", "C", "KD", "S", "V", "MP", "SD")) %>%
  mutate(ElectionYear=year(ElectionDate), 
         ElectionDate=as.Date(ElectionDate), 
         PublYearMonth=ElectionDate, 
         Percentage=Result)

# Join in electiondata to polls and create new variables
polldata<- left_join(polldata, electiondata[,c("ElectionDate", "Party", "Result", "ElectionYear")]) %>%
  mutate(MonthBeforeElection=as.numeric(round((ElectionDate-PublYearMonth)/30,0)),
         DiffElection=Result-Percentage,
         DiffElection_sign=ifelse(DiffElection>=0, "pos", "neg"))


# Create dataset with summary per house
overview_houses<- polldata %>%
                  group_by(house, type, PublYearMonth) %>% 
                  summarize(NrPolls=n()) %>%
                  arrange(desc(type)) %>%
                  filter(PublYearMonth>="2015-01-01")

# Set theme and define color vector for the parties
old<-theme_set(theme_light())
party_col<-c("MP"="green","V"="darkred", "S"="red" , "FI"="pink", "C"="forest green", "KD"="purple", "L"="skyblue", 
             "M"="darkblue", "SD"="gold", "Uncertain"="grey", "RÃ¶dgrÃ¶na"="red","Alliansen"="orange", "Other"="grey")

txt_parties<-as.character(unique(polldata$Party))
txt_houses<-as.character(unique(polldata$house))

#################  SET DASHBOARD UI  ########################

ui <- dashboardPage(
  
  dashboardHeader(title = "Svensk Politik"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Opinionen per månad", tabName = "polls_hist", icon = icon("line-chart")),
      menuItem("Opinion vs. valresultat", tabName="polls_dev_election", icon=icon("poll")),
      menuItem("Institut vs. valresultat", tabName="institut_dev_election", icon=icon("chart-bar")),
      menuItem("Koalitioner", tabName="coal_hist", icon=icon("handshake")),
      menuItem("Information", tabName="info_raw", icon=icon("info"))
    )
    
  ),
  
  dashboardBody(
  tabItems(
    
    # Tab 1: Polls_Hist
    tabItem(tabName = "polls_hist",
      h2("Opinionen per månad"),
      fluidRow(
          box(title="Partier att inkludera", width=3, status="primary", 
                  checkboxGroupInput(inputId = "parties", inline=TRUE, label="Partier att inkludera i grafen", choices=txt_parties, 
                             selected=txt_parties),
                 checkboxInput('all_party', 'Alla partier / Inga partier', value=TRUE)),
          
          box(title="Opinionsinstitut att inkludera", width=5, status="primary",  
                 checkboxGroupInput(inputId = "houses", inline=TRUE, label="Opinionsinstitut att inkludera i grafen", choices=txt_houses, 
                                    selected=txt_houses), 
                 checkboxInput('all_houses', 'Alla institut / Inga institut', value=TRUE)),
          box(title="Date input:", width=2, status="primary", 
                dateInput('startmonthpolls',
                          label = 'Start date:',
                          value = Sys.Date()-2000
              )),
          box(title="Visa i graf:",  width=2, status="primary",
              column(12,
                     selectInput("includepoints", "Varje institut",
                                 c("Ja", "Nej"), selected="Ja"),
                     selectInput("includeelections", "Valresultat",
                                 c("Ja", "Nej"), selected="Ja")
                     )
              )
      ),
      fluidRow(
        column(5, 
               plotOutput(outputId="bar_current_month")),
        column(7, 
               plotOutput(outputId = "party_monthly_mean"))
      )
      ),
    
    # Tab 2: polls_Dev_election
    tabItem(tabName = "polls_dev_election", 
            h2("Opinionläge per månad vs. valresultat"),
            h4("Positiva värden -> Partiets hade högre valresultat än opinionen visade den månaden"),
            hr(),
            fluidRow(plotOutput(outputId = "polls_deviation"))
    ),
    
    # Tab 3: institut_dev_election
    tabItem(tabName = "institut_dev_election", 
            h2("Institut vs. valresultat"),
            h4("Differens opinionssiffror vs verkligt valresultat, per institut och månad före valet"),
            hr(),
            fluidRow(plotOutput(outputId = "polls_deviation_houses"))
    ),
    
    # Tab 4: coal_hist
    tabItem(tabName="coal_hist",
            h2("Koalitioner"),
            h4("Använd sliders för att simulera koalitioner vid olika utfall"),
            hr(),
            fluidRow(
              column(2,
                     sliderInput("slider_S", h5("Socialdemokraterna (S)"), 
                                 min=2, max=35, value=30),
                     sliderInput("slider_MP", h5("Miljöpartiet (MP)"), 
                                 min=2, max=35, value=30),
                     sliderInput("slider_V", h5("Vansterpartiet (V)"), 
                                 min=2, max=35, value=30),
                     sliderInput("slider_SD", h5("Sverigedemokraterna"), 
                                 min=0, max=35, value=30)),
              column(2,
                     sliderInput("slider_M", h5("Moderaterna"), 
                                 min=2, max=35, value=30),
                     sliderInput("slider_C", h5("Centerpartiet"), 
                                 min=2, max=35, value=30),
                     sliderInput("slider_L", h5("Liberalerna (L)"), 
                                 min=2, max=35, value=30),
                     sliderInput("slider_KD", h5("Kristdemokraterna (KD)"), 
                                 min=2, max=35, value=30)),
              column(8,
                     plotOutput(outputId = "block_combination"))
            ),
            hr(),
            h3("Koalitioner över tid"), 
            fluidRow(
              column(2,
                     checkboxGroupInput(inputId = "parties_coalition", label="Välj partier att summera", 
                                        inline=TRUE, choices=txt_parties)),
              column(10, 
                     plotOutput(outputId = "coalitions_monthly"))
            )
    ),
  
  # Tab 5: info_raw
  tabItem(tabName = "info_raw", 
          h2("Översikt inkluderade opinionsinstitut per månad med data"),
          h4("Orange = klassiska (slumpvis), blå=Internet (självrekryterade)"),
          fluidRow(plotOutput(outputId = "overview_polls")),
          fluidRow(
            column(6,
                   h4("Visa rådata från månad: ")),
            column(6,
                   selectInput("startmonthtbl", "Start month: ", unique(polldata$PublYearMonth),
                      selected="2018-02-01"))
            ),
          fluidRow(tableOutput(outputId="tbl_raw"))
          )
  )
  )
)

###### DEFINE SERVER FUNCTION #########################

server <- function(input, output,session) {
  
  observe({
  
    # Button for check / uncheck all parties
    updateCheckboxGroupInput(
    session, 'parties', choices = txt_parties, inline=TRUE,
    selected = if (input$all_party) txt_parties)
  
    # Button for check / uncheck all houses
    updateCheckboxGroupInput(
      session, 'houses', choices = txt_houses, inline=TRUE,
      selected = if (input$all_houses) txt_houses)
    
    # Get latest available month in data
    monthly_avg_recent<- polldata_monthly_avg() %>%
    filter(PublYearMonth==max(polldata$PublYearMonth, na.rm=TRUE))
    
    # Set startvalues for sliderinput (on tab 5) to the average value from the latest reporting month 
    updateSliderInput(
      session, "slider_S", value=monthly_avg_recent[which(monthly_avg_recent$Party=="S"),]$Percentage,
      min=2,max=35,step=0.1)
    updateSliderInput(
      session, "slider_MP", value=monthly_avg_recent[which(monthly_avg_recent$Party=="MP"),]$Percentage,
      min=2,max=35,step=0.1)
    updateSliderInput(
      session, "slider_V", value=monthly_avg_recent[which(monthly_avg_recent$Party=="V"),]$Percentage,
      min=2,max=35,step=0.1)
    updateSliderInput(
      session, "slider_M", value=monthly_avg_recent[which(monthly_avg_recent$Party=="M"),]$Percentage,
      min=2,max=35,step=0.1)
    updateSliderInput(
      session, "slider_C", value=monthly_avg_recent[which(monthly_avg_recent$Party=="C"),]$Percentage,
      min=2,max=35,step=0.1)
    updateSliderInput(
      session, "slider_L", value=monthly_avg_recent[which(monthly_avg_recent$Party=="L"),]$Percentage,
      min=2,max=35,step=0.1)
    updateSliderInput(
      session, "slider_KD", value=monthly_avg_recent[which(monthly_avg_recent$Party=="KD"),]$Percentage,
      min=2,max=35,step=0.1)
    updateSliderInput(
      session, "slider_SD", value=monthly_avg_recent[which(monthly_avg_recent$Party=="SD"),]$Percentage,
      min=2,max=35,step=0.1)
    
    })

  # Create reactive dataframes - entire data is filtered when selecting polltypes
  polldata_monthly_avg <- reactive({
    polldata %>% 
      filter(!is.na(Percentage)) %>%
      group_by(PublYearMonth, Party, block, MonthBeforeElection, ElectionYear) %>%
      summarize(Percentage=mean(Percentage, na.rm=TRUE),
                Result=mean(Result, na.rm=TRUE),
                NrPolls=n()) %>%
      mutate(DiffElection=Result-Percentage,
             DiffElection_sign=ifelse(DiffElection>=0, "pos", "neg"))
  })
  
  
  # Reactive textframe
  text_coal<- reactive({
    paste("Koalition av ",paste(input$parties_coalition, sep="+"))
  })
  
  # DataFrame connected to sliders on tab5
  coalitions_sliders<-reactive({
    data.frame(Party=c("S", "MP", "V", "M", "C", "L", "KD", "SD"), 
               Percentage=c(input$slider_S, input$slider_MP, input$slider_V, 
                input$slider_M, input$slider_C, input$slider_L, input$slider_KD, input$slider_SD)) %>%
    
      mutate(S_MP_V=ifelse(Party %in% c("S", "MP", "V"), Percentage, 0),
           M_C_L_KD=ifelse(Party %in% c("M", "C", "L", "KD"), Percentage, 0),
           S_MP_L_C=ifelse(Party %in% c("S", "MP", "L", "C"), Percentage, 0),
           M_KD_SD=ifelse(Party %in% c("M", "KD", "SD"), Percentage, 0),
           S_M=ifelse(Party %in% c("S", "M"), Percentage, 0),
           M_C_L_KD_MP=ifelse(Party %in% c("M", "C", "L", "KD", "MP"), Percentage, 0),
           S_L_C=ifelse(Party %in% c("S", "L", "C"), Percentage, 0)) %>%
      select(-one_of("Percentage")) %>%
      gather(Coalition, Percentage, -Party) %>%
      filter(Percentage>=4)
  })

  # Tab 1 Plot 1 Barchart: Current percentage
  output$bar_current_month<- renderPlot({
    
    polldata_monthly_avg() %>%
      filter(PublYearMonth>=max(polldata$PublYearMonth, na.rm=TRUE)-30 & !is.na(Percentage)) %>%
      ungroup() %>%
      ggplot(aes(x=factor(PublYearMonth), y=Percentage, fill=as.factor(Party), alpha=factor(PublYearMonth), width=0.9))+
      geom_bar(position="dodge2", stat="sum") + 
      scale_fill_manual(values=party_col) + 
      scale_alpha_manual(values = c(0.3, 1)) + 
      theme(legend.position = "none",
            text = element_text(size=20),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank()) + 
      ggtitle("Genomsnitt aktuell (inkl. föregående månad)") +
      facet_grid(~Party) 
    
  })
  
  # Tab 1 Plot 2: Parties over time
  output$party_monthly_mean <- renderPlot({
    
    p<- {    
              polldata %>%
             filter(!is.na(Percentage) & Party %in% input$parties & PublYearMonth>=input$startmonthpolls 
                    & house %in% input$houses) %>%
             ggplot(aes(x=PublYearMonth, y=Percentage, group=Party, color=Party, fill=Party))+
                stat_summary(fun.y=mean, geom="line", size=1)+
        geom_dl(aes(label=Party), method=list(dl.combine("first.points"), cex=0.9, size=3))
    } 

    if (input$includepoints=="Ja")
    {
      p<- p+geom_point(alpha=0.3)
    }
    
    if (input$includeelections=="Ja")
    {
      p<- 
        p+
        geom_point(data=electiondata[which(electiondata$PublYearMonth>=input$startmonthpolls),], shape=15, size=3)
    }
    
    # Add features to plot
    p+geom_hline(yintercept=4, color="black", linetype=2)+
      theme(
        text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
        )+ 
      scale_color_manual(values=party_col)+
      ggtitle("Opinionsundersökningar. Linje: Medelvärde. Punkter: Enskilda undersökningar")
  })


  # Tab 2 Plot 1: Deviations from elect result
  output$polls_deviation <- renderPlot({
    
    diff_by_year <-  
      polldata %>% 
      filter(!is.na(Percentage) & MonthBeforeElection>1 & MonthBeforeElection<6) %>%
      group_by(MonthBeforeElection, ElectionYear, Party) %>%
      summarize(DiffElection = mean(DiffElection))
    
    ggplot(diff_by_year, aes(x=MonthBeforeElection, y=factor(ElectionYear), fill= DiffElection, height=0.8)) + 
      geom_tile() + 
      geom_text(aes(label=round(DiffElection,0))) + 
      scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint=0) + 
      scale_color_manual(values="black") + 
      facet_grid(~Party) + 
      theme(
        text = element_text(size=16),
        legend.title = element_blank(),
        legend.position='none',
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.title.y=element_blank()
      ) + 
      labs(x = "Månader innan val") + 
      scale_x_reverse() + 
      ggtitle(paste("Avvikelse mot valresultat i procentenheter -", input$year_poll_elect, sep=" "))
  
  })
  
  
  #Tab3 Plot 1: Deviation per house (total over all parties) 
  output$polls_deviation_houses <- renderPlot({
    
    diff_by_institute_sum <-  
      polldata %>% 
      filter(!is.na(Percentage) & MonthBeforeElection>0 & MonthBeforeElection<6) %>%
      group_by(MonthBeforeElection, house, ElectionYear, Party) %>%
      summarize(DiffElection = mean(DiffElection))
      
    diff_by_institute_sum <- 
      diff_by_institute_sum %>%
      group_by(MonthBeforeElection, house, ElectionYear) %>%
      summarize(DiffElection = sum(abs(DiffElection)))
    
    ggplot(diff_by_institute_sum, aes(x=MonthBeforeElection, y=house, fill= DiffElection, height=0.8)) + 
      geom_tile() + 
      geom_text(aes(label=round(DiffElection,0))) + 
      scale_fill_gradient2(low = "green", high = "red", midpoint=10) + 
      scale_color_manual(values="black") + 
      facet_grid(~ElectionYear) + 
      theme(
        text = element_text(size=16),
        legend.title = element_blank(),
        legend.position='none',
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.title.y=element_blank()
      ) + 
      labs(x = "Månader innan val") + 
      scale_x_reverse() + 
      ggtitle("Totalt över alla partier per ar")
    
  })
  
  # Tab 5 Plot 1: Bar chart with block combinations
  output$block_combination<- renderPlot({
    
    act_values<-data.frame(Percentage=c(input$slider_S, input$slider_MP, input$slider_V, 
                                        input$slider_M, input$slider_C, input$slider_L, input$slider_KD, input$slider_SD))
    
    majority<-sum(act_values[which(act_values$Percentage>=4),])/2
    
    coalitions_sliders() %>%
      ggplot(aes(x=Coalition, y=Percentage, fill=Party))+
      geom_col()+
      theme(text=element_text(size=14),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank()
      )+
      geom_text(aes(label=round(Percentage,1), group=Party), fontface="bold", position=position_stack(vjust=.5))+
      scale_fill_manual(values=party_col)+
      geom_hline(yintercept=majority, color="black", linetype=2)+
      geom_label(aes(x=0.7, y=majority, label=paste(round(majority,1), "% for majority", sep="")), fill="green", size=6)+
      coord_flip()+
      labs(x = "Koalition",
           y= "Procent")
    
  })
  
  #Tab5 Plot 2 Area chart: Coalitions over time
  output$coalitions_monthly <-renderPlot({
    
    filter(polldata, Party %in% input$parties_coalition & !is.na(Percentage)) %>%
    ggplot(aes(x=PublYearMonth, y=Percentage, fill=Party))+
      stat_summary(fun.y=mean, geom="area", position="stack")+
      scale_fill_manual(values=party_col)+
      theme(text = element_text(size=20),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank()
            )+
      geom_hline(yintercept = 50)
  })

  # Tab 6: Overview all poll institutes
  output$overview_polls<-renderPlot({
    
    overview_houses %>%
    ggplot(aes(x=1, y=1, fill=type))+
    geom_tile(color="white")+
    facet_grid(house~PublYearMonth)+
    theme(text = element_text(size=16),
          legend.position = "bottom",
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          strip.text.y = element_text(angle = 360, face="bold"),
          strip.text.x=element_text(angle=90, face="bold"))+
     ylab("")+
      xlab("")+
      scale_fill_manual(values=c("Internet"="blue", "Classic"="orange"))
  }
  )
 
  # Tab 6: Table with all data
  output$tbl_raw <-renderTable({
    polls %>% 
      filter(as.Date(PublDate)>=input$startmonthtbl)
  })
    
}

shinyApp(ui=ui, server=server)










