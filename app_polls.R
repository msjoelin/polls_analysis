##################################################
## Project: Polls analysis
## Script purpose: Shiny app for analysis and visualization of election polls in Sweden
## Date: 2018-10-01 
## Author: Marcus Sjölin
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
                                                    ifelse(Party %in% c("MP", "S", "V"), "Rödgröna", "Other"))),
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
  gather(Party, Result, c("M", "L", "C", "KD", "S", "V", "MP", "SD"))

electiondata$ElectionYear<-year(electiondata$ElectionDate)
electiondata$ElectionDate<-as.Date(electiondata$ElectionDate)

# Join in electiondata to polls and create new variables
polldata<- left_join(polldata, electiondata) %>%
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
             "M"="darkblue", "SD"="gold", "Uncertain"="grey", "Rödgröna"="red","Alliansen"="orange", "Other"="grey")
block_col<-c("Rödgröna"="red","Alliansen"="orange", "SD"="gold", "Other"="grey")

txt_parties<-as.character(unique(polldata$Party))
txt_houses<-as.character(unique(polldata$house))

#################  SET DASHBOARD UI  ########################

ui <- dashboardPage(
  
  dashboardHeader(title = "Svensk Politik"),
  dashboardSidebar(
    
    sidebarMenu(
      checkboxGroupInput(inputId="polltypes", label="Inkludera följande typer", choices=c("Internet", "Classic"),
                         selected=c("Internet", "Classic")),
      selectInput("party_or_block", "Party / Block", c("Parti", "Block"),
                  selected="Parti"),
      menuItem("Opinionen per månad", tabName = "polls_hist", icon = icon("line-chart")),
      menuItem("Opinion vs. valresultat", tabName="polls_dev_election", icon=icon("exchange")),
      menuItem("Institut vs. valresultat", tabName="institut_dev_election"),
      menuItem("Koalitioner över tid", tabName="coal_hist", icon=icon("area-chart")),
      menuItem("Simulering av koalitionsmöjligheter", tabName = "koalitionsalternativ", icon = icon("pie-chart")),
      menuItem("Information", tabName="info_raw", icon=icon("info"))
    )
    
  ),
  
  dashboardBody(
  tabItems(
    
    # Tab 1: Polls_Hist
    tabItem(tabName = "polls_hist",
      h2("Opinionen per månad"),
      fluidRow(
          box(title="Partier att inkludera", width=2, status="primary", 
                  checkboxGroupInput(inputId = "parties", inline=TRUE, label="Partier att inkludera i grafen", choices=txt_parties, 
                             selected=txt_parties),
                 checkboxInput('all_party', 'Alla partier / Inga partier', value=TRUE)),
          box(title="Opinionsinstitut att inkludera", width=2, status="primary",  
                 checkboxGroupInput(inputId = "houses", inline=TRUE, label="Opinionsinstitut att inkludera i grafen", choices=txt_houses, 
                                    selected=txt_houses), 
                 checkboxInput('all_houses', 'Alla institut / Inga institut', value=TRUE)),
          column(6,
                 selectInput("startmonthpolls", "Tidsperiod från:", unique(polldata$PublYearMonth),
                                                                          selected="2005-01-01"))
      ),
      fluidRow(
        column(9, 
               plotOutput(outputId = "party_monthly_mean")),
        column(3,selectInput("monthtables", "Tabell för år/månad ", unique(polldata$PublYearMonth),
                             selected=max(polldata$PublYearMonth, na.rm=TRUE))),
        column(3, tableOutput(outputId="tbl_current_month"))
      )
      ),
    
    # Tab 2: polls_Dev_election
    tabItem(tabName = "polls_dev_election", 
            h2("Opinion vs. valresultat"),
            fluidRow(plotOutput(outputId = "polls_deviation"))
    ),
    
    # Tab 3: institut_dev_election
    tabItem(tabName = "institut_dev_election", 
            h2("Institut vs. valresultat"),
            h3("Analys av skillnaden mellan opinionsundersökningar vid en vald tidpunkt innan valet, och det faktiska valresultatet"),
            h3("Positiva värden betyder att partiets valresultat var högre än opinionsundersökningarna visade vid valt antal månader före valet"),
            fluidRow(
              column(6, 
                     selectInput("month_bf_elect", "Månader innan val", c(1:6), 
                                 selected=2)),
              column(6,
                     selectInput("year_poll_elect", "År", unique(polldata$ElectionYear),
                                 selected=2014))
              ),
            fluidRow(
              column(6,
                     plotOutput(outputId = "polls_deviation_houses")),
              column(6, 
                     plotOutput(outputId = "polls_deviation_houses_year")))
    ),
    
    # Tab 4: coal_hist
    tabItem(tabName="coal_hist",
            h2("Koalitioner över tid"),
            fluidRow(checkboxGroupInput(inputId = "parties_coalition", inline=TRUE, label="Välj partier att summera", choices=txt_parties)),
            fluidRow(plotOutput(outputId = "coalitions_monthly"))
    ),
  
    # Tab 5: koalitionsalternativ
    tabItem(tabName="koalitionsalternativ",
            h2("Simulering av koalitionsalternativ"),
            h3("Fördelningen använder värdena i sliders. Dessa kan ändras för att simulera alternativa scenarios."),
            h3("Partier under 4 % visas ej i grafen och inkluderas ej vid beräkning av majoritets-gränsen"),
            fluidRow(
              selectInput("startmonth", "Månad att använda som startvärden i sliders", unique(polldata$PublYearMonth))
              ),
            fluidRow(
              column(6,
                    sliderInput("slider_S", h5("Socialdemokraterna (S)"), 
                          min=2, max=35, value=30),
                    sliderInput("slider_MP", h5("Miljöpartiet (MP)"), 
                          min=2, max=35, value=30),
                    sliderInput("slider_V", h5("Vänsterpartiet (V)"), 
                          min=2, max=35, value=30),
                    sliderInput("slider_SD", h5("Sverigedemokraterna"), 
                                min=0, max=35, value=30)),
              column(6,
                    sliderInput("slider_M", h5("Moderaterna"), 
                          min=2, max=35, value=30),
                    sliderInput("slider_C", h5("Centerpartiet"), 
                          min=2, max=35, value=30),
                    sliderInput("slider_L", h5("Liberalerna (L)"), 
                          min=2, max=35, value=30),
                    sliderInput("slider_KD", h5("Kristdemokraterna (KD)"), 
                          min=2, max=35, value=30))
      ),
      
      fluidRow(plotOutput(outputId = "block_combination"))
  
  ),
  
  # Tab 6: info_raw
  tabItem(tabName = "info_raw", 
          h2("Översikt över inkluderade opinionsinstitut, och vilka månader de utkommit med opinionsdata "),
          h3("Orange = klassiska undersökningar (slumpvis), blå=Internet-undersökningar (självrekryterade)"),
          fluidRow(plotOutput(outputId = "overview_polls")),
          fluidRow(
            column(6,
                   h3("Visa rådata från månad: ")),
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
    filter(PublYearMonth==input$startmonth)
    
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
      filter(type %in% input$polltypes & !is.na(Percentage)) %>%
      group_by(PublYearMonth, Party, block, MonthBeforeElection, ElectionYear) %>%
      summarize(Percentage=mean(Percentage, na.rm=TRUE),
                Result=mean(Result, na.rm=TRUE),
                NrPolls=n()) %>%
      mutate(DiffElection=Result-Percentage,
             DiffElection_sign=ifelse(DiffElection>=0, "pos", "neg"))
  })
  
  
  polldata_monthly_avg_block <- reactive({
    polldata_monthly_avg() %>% 
      group_by(PublYearMonth, block, MonthBeforeElection, ElectionYear) %>%
      summarize(Percentage=sum(Percentage, na.rm=TRUE),
                Result=sum(Result, na.rm=TRUE)) %>%
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

  
  # Tab 1 Plot 1: Parties over time
  output$party_monthly_mean <- renderPlot({
    
    p<- if (input$party_or_block=="Parti") # Depending on choice: Show Party or coalition of parties
    {    
              polldata %>%
             filter(!is.na(Percentage) & Party %in% input$parties & PublYearMonth>=input$startmonthpolls &
                    type %in% input$polltypes & house %in% input$houses) %>%
             ggplot(aes(x=PublYearMonth, y=Percentage, group=Party, color=Party, fill=Party))+
                geom_point(alpha=0.3)+
                stat_summary(fun.y=mean, geom="line", size=1)+
        geom_dl(aes(label=Party), method=list(dl.combine("first.points"), cex=0.9, size=3))
    } else
    {
      polldata %>% 
        filter(!is.na(Percentage) & PublYearMonth>=input$startmonthpolls & block %in% c("Alliansen", "Rödgröna", "SD") & 
                 type %in% input$polltypes & house %in% input$houses) %>%
        group_by(PublYearMonth, Party, block) %>%
        summarize(Percentage=mean(Percentage)) %>%
        ggplot(aes(x=PublYearMonth, y=Percentage, group=block, color=block))+
        stat_summary(fun.y=sum, geom="line", size=1)
    }

    # Add features to plot
    p+geom_hline(yintercept=4, color="black", linetype=2)+
      theme(text = element_text(size=14),
      axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_color_manual(values=party_col)+
      ggtitle("Opinionsundersökningar. Linje: Medelvärde. Punkter: Enskilda undersökningar")+
      labs(x = "År / Månad",
           y= "Procent")
  })
  
  # Tab 1 Table 1: Percentage from last month
  output$tbl_current_month<- renderTable({
    
    if (input$party_or_block=="Parti") {
    
    polldata_monthly_avg() %>%
      filter(PublYearMonth==input$monthtables & !is.na(Percentage)) %>%
      ungroup() %>%
      select(Party, block, Percentage)
    } else {
      polldata_monthly_avg() %>% 
        filter(PublYearMonth==input$monthtables & !is.na(Percentage)) %>%
        group_by(block) %>% 
        summarize(Percentage=sum(Percentage))
    }
  })

  # Tab 2 Plot 1: Deviations from elect result
  output$polls_deviation <- renderPlot({
    
    polldata_monthly_avg() %>% 
      filter(MonthBeforeElection<=6) %>%
      ggplot(aes(x=MonthBeforeElection, y=DiffElection, 
                 color=as.factor(ElectionYear), group=as.factor(ElectionYear), label=round(DiffElection,1)))+
              facet_wrap(~Party, ncol=4, nrow=2) +
              geom_line(size=2)+
              geom_text(fill="white", color="black", size=6)+
              scale_fill_manual(values=c("pos"="green", "neg"="red"))+
              theme(text = element_text(size=20),
                    legend.title=element_blank(),
                    strip.text.y = element_text(angle = 360, face="bold"),
                    strip.text.x = element_text(face="bold"),
                    strip.placement = "outside")+
              geom_hline(yintercept =0)+
              scale_x_reverse() + 
              ggtitle("Avvikelse från valresultatet i procentenheter")+
              labs(x = "Månader innan val")
  }, height=800)
  
  #Tab2 Plot 2: Deviation per house (total over all parties) 
  output$polls_deviation_houses <- renderPlot({
    
    polldata %>% 
      filter(type %in% input$polltypes & !is.na(Percentage)) %>%
      group_by(Party, MonthBeforeElection, ElectionYear, house, type) %>% 
      summarize(DiffElection=mean(DiffElection, na.rm=TRUE)) %>%
      filter(MonthBeforeElection==input$month_bf_elect) %>%
      
      ggplot(aes(x=house, y=abs(DiffElection), color=as.factor(ElectionYear)))+
      stat_summary(fun.y=sum, geom="point", size=4)+
      scale_y_continuous(limits=c(0, 30))+
      theme(text = element_text(size=16),
            axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_color_manual(values=c("red", "Purple", "Green"),
                         name="Valår")+
      ggtitle("Total absolut avvikelse per opinionsinstitut och år")+
      facet_wrap(~type, scales="free")+
      labs(x = "Opinionsinstitut")
            
  })
  
  #Tab3 Plot 1: Deviation from election monthly before election (total average over all houses) 
  output$polls_deviation_houses_year <- renderPlot({
    
    polldata %>% 
      filter(type %in% input$polltypes & !is.na(Percentage) & ElectionYear==input$year_poll_elect & MonthBeforeElection==input$month_bf_elect) %>%
      
      ggplot(aes(x=house, y=abs(DiffElection), group=Party, fill=Party))+
      stat_summary(fun.y=mean, geom="col", position="stack")+
      scale_y_continuous(limits=c(0, NA))+
      theme(text = element_text(size=16))+
      ggtitle(paste("Total absolut avvikelse i procentenheter -", input$year_poll_elect, sep=" "))+
      scale_fill_manual(values=party_col)+
      coord_flip()+
      labs(x = "Opinionsinstitut")
    
  })
  
  #Tab4: Coalitions over time
  output$coalitions_monthly <-renderPlot({
    
    filter(polldata, Party %in% input$parties_coalition & type %in% input$polltypes & !is.na(Percentage)) %>%
    ggplot(aes(x=PublYearMonth, y=Percentage, fill=Party))+
      stat_summary(fun.y=mean, geom="area", position="stack")+
      scale_fill_manual(values=party_col)+
      theme(text = element_text(size=16))+
      geom_hline(yintercept = 50)+
      labs(x = "År / Månad",
           y= "Procent")
  })
  
  # Tab 5: Bar chart with block combinations
  output$block_combination<- renderPlot({
    
   act_values<-data.frame(Percentage=c(input$slider_S, input$slider_MP, input$slider_V, 
                                     input$slider_M, input$slider_C, input$slider_L, input$slider_KD, input$slider_SD))

    majority<-sum(act_values[which(act_values$Percentage>=4),])/2
  
    coalitions_sliders() %>%
    ggplot(aes(x=Coalition, y=Percentage, fill=Party))+
    geom_col()+
    theme(text=element_text(size=12))+
    geom_text(aes(label=round(Percentage,1), group=Party), fontface="bold", position=position_stack(vjust=.5))+
    scale_fill_manual(values=party_col)+
    geom_hline(yintercept=majority, color="black", linetype=2)+
    geom_label(aes(x=0.7, y=majority, label=paste(round(majority,1), "% for majority", sep="")), fill="green", size=6)+
    coord_flip()+
      labs(x = "Koalition",
           y= "Procent")
    
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










