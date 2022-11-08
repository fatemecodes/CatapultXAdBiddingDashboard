# CatapultXAdBiddingDashboard
It is the project with show the probability of wining with diffrent  parameters  like times series.
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# ====================
# Loading libraries
# ====================
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)


# Define UI for dashboard
ui <- fluidPage(

    # Application title
    titlePanel(h2(style = "color: #E13156",strong("CatapultX Ad Bidding Dashboard"))),
    tabsetPanel(
        tabPanel(strong("Home"), fluid = TRUE,
                 mainPanel(
                         h3(style = "text-decoration: underline; color: #E13156",strong("Business Context")),
                         p(style = "font-size:17px; text-align: justify;",strong(em("CatapultX"))," is a Supply Side Platform (SSP) that works with Publishers who have ad spaces on their websites. The publishers ask CatapultX to place ", strong("Bid Requests"), " for these ad spaces on their behalf, whenever a user visits their website. Bid Requests typically contain ", strong("User Information")," like user demographics, browsing history, etc. and ", strong("Bid Floor amount")," which is the minimum amount the Publisher expects to display an advertisement to the user on that ad space."),
                         p(style = "font-size:17px; text-align: justify;","CatapultX places a series of requests for this ad space across multiple Remote Feeds (which can be thought of as a market place for ads). Out of these, only some requests will be recognized by the Remote Feeds/advertisers and they will respond to it with a bidding price (on a per impression basis). The responses can be of 3 categories: Wins, Loss or Errors. The Advertiser/Remote Feed with the highest bid wins the impression and the ad is displayed to the user using the Real Time Bidding (RTB) system."),
                         h3(style = "text-decoration: underline; color: #E13156",strong("Business Problem")),
                         p(style = "font-size:17px; text-align: justify;","CatapultX needs help in exploring the bidding data they already have to answer the following questions."),
                         p(style = "font-size:17px; text-align: justify;","1.	Generally speaking, is there is connection between the average bid price and the probability of winning the bid?"),
                         p(style = "font-size:17px; text-align: justify;","2.	Does this probability differ based on different time frames (time of day, month, etc)?"),
                         p(style = "font-size:17px; text-align: justify;","3.	What other dynamics in the data impact the probability of winning?"),
                         h3(style = "text-decoration: underline; color: #E13156",strong("Data")),
                         p(style = "font-size:17px; text-align: justify;", "The Bidding Data used for this analysis has been collected over  a span of 6 months from ", strong(em("01-Oct-2021"))," to ",strong(em("01-Apr-2022"))," for 3 Publishers: ", strong("Daily Motion", style = "color: royalblue"),", ", strong("Rolling Stone", style = "color: royalblue")," and ",strong("Taste of Home", style = "color: royalblue"), ". The information has been collected for each Publisher-Remote Feed combination at an hourly interval during the said time frame. The level of data is Date-Hour-Remote Feed."),
                         p(style = "font-size:17px; text-align: justify; text-decoration: underline;",strong("Some terminologies and variable relations in the dataset:")),
                         p(style = "font-size:17px; text-align: justify;","- ",strong("Ad Impression:")," Single instance of a single ad appearance"),
                         p(style = "font-size:17px; text-align: justify;","- ",strong("eCPM:")," Effective Cost Per Mille (or thousand impressions)"),
                         p(style = "font-size:17px; text-align: justify;","- ",strong("Remote Feed Coverage %:")," (Remote Feed Responses / Remote Feed Requests)*100"),
                         p(style = "font-size:17px; text-align: justify;","- ",strong("Remote Feed Errors %:")," (Remote Feed Errors / Remote Feed Requests)*100"),
                         p(style = "font-size:17px; text-align: justify;","- ",strong("Publisher Gross Impressions:")," Publisher Net Impressions + Publisher Filtered Impressions"),
                         p(style = "font-size:17px; text-align: justify;","- ",strong("Win Price per Impression (Engineered column):")," Publisher Wins Price / Publisher Impression Wins"),
                         p(style = "font-size:17px; text-align: justify;","- ",strong("Publisher Estimated Revenue:")," Win Price per Impression * Publisher Net Impression"),
                         p(style = "font-size:17px; text-align: justify;","- ",strong("Publisher Estimated eCPM:")," (Publisher Estimated Revenue / Publisher Gross Impressions) * 1000"),
                         p(style = "font-size:17px; text-align: justify;","- ",strong("Remote Feed Impression Win Rate:")," Publisher Gross Impressions / Remote Feed Responses"),
                         p(style = "font-size:17px; text-align: justify;","- ",strong("Remote Feed Gross eCPM:")," (Remote Feed Gross Revenue / Publisher Net Impressions) * 1000"),
                         width = 12
                           )
                 ),
        tabPanel(strong("Tab I"), fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         checkboxGroupInput("publisher"
                                            , "Select Publisher: "
                                            , choices = c("Daily Motion", "Rolling Stone", "Taste of Home")
                                            , selected = c("Daily Motion", "Rolling Stone", "Taste of Home"))
                         , dateRangeInput("date"
                                          ,"Select Date Range:"
                                          , start = as.Date("2021-10-01")
                                          , end = as.Date("2022-04-01")
                                              )
                                  , width = 2),
                     mainPanel(
                         fluidRow(
                             splitLayout(cellWidths = c("47%","50%")
                            ,plotOutput("scatterPlot1")
                            ,plotOutput("scatterPlot2"))
                            ),
                         fluidRow(
                             splitLayout(cellWidths = c("33%", "33%", "33%")
                            , plotOutput("timePlot1")
                            , plotOutput("timePlot2")
                            , plotOutput("timePlot3"))
                            )
                         , width = 10
                            )
                     )
                 ),
        tabPanel(strong("Tab II"), fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("publisher_dropdown"
                                            , "Select Publisher: "
                                            , choices = c("Daily Motion", "Rolling Stone", "Taste of Home")
                                            , selected = c("Daily Motion"))
                         , width = 2),
                     mainPanel(
                         fluidRow(
                             splitLayout(cellWidths = c("100%")
                             ,plotOutput("remote_feed1")
                             )
                         ),
                         fluidRow(
                             splitLayout(cellWidths = c("100%")
                             , plotOutput("remote_feed2")
                             )
                         ),
                         fluidRow(
                             splitLayout(cellWidths = c("100%")
                             , plotOutput("remote_feed3")
                             )
                         )
                         , width = 10
                     )
                 )
        )
    )
)

# Define server logic for the dashboard
server <- function(input, output) {
    
    # Load the saved R dataset obtained from wrangling operations
    load("df_master.RData")
    
    # -----------------------------------------------------------------------------
    # Set color code mapping to keep colors for publishers constant over all charts
    # -----------------------------------------------------------------------------
    color_num <- c("Daily Motion", "Rolling Stone", "Taste of Home")
    color_names <- c("#E13156","#00BA38", "#619CFF")
    color_codes <- set_names(color_names, color_num)
    
    col_val <- c("Publisher_Impression_Wins","Publisher_Gross_Impressions","Publisher_Net_Impressions")
    col_key <- c("#F08B13","#15CFDF","#DEDA14")
    col_codes <- set_names(col_key, col_val)
    
    # ----------------------------------------
    # Create ggplot charts to display in UI
    # ----------------------------------------
    
    output$scatterPlot1 <- renderPlot({
        df_1 <- df_master %>% filter(Publisher_Name %in% input$publisher)
        ggplot(df_1, aes(x = Win_Price_Per_Impression, y = Prob_Win_Req_PCT,color = Publisher_Name))+
            geom_point()+
            scale_color_manual(values = color_codes)+
            scale_y_continuous(limits = c(0,105),breaks = seq(0,100, by = 25), expand = c(0,0))+
            scale_x_continuous(limits = c(0,0.038),breaks = seq(0,0.05, by = 0.01), expand = c(0,0))+
            theme(text = element_text(size=13, face = "bold"),legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
            labs(title = "Probability of Winning (%) vs Avg Win Price per Impression across Publishers"
                 , x = "Avg Win Price ($) per Impression"
                 , y = "Probability of Winning (%)")
    })
    
    output$scatterPlot2 <- renderPlot({
        df_2 <- df_master %>% filter(Publisher_Name %in% input$publisher)
        ggplot(df_2, aes(x = Remote_Feed_Average_Bid_eCPM, y = Prob_Win_Req_PCT,color = Publisher_Name))+
            geom_point()+
            scale_color_manual(values = color_codes)+
            scale_y_continuous(limits = c(0,105),breaks = seq(0,100, by = 25), expand = c(0,0))+
            scale_x_continuous(limits = c(0,80),breaks = seq(0,80, by = 20), expand = c(0,0))+
            theme(text = element_text(size=13, face = "bold"),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
            labs(title = "Probability of Winning (%) vs Avg Bid Price per Impression across Publishers"
                 , x = "Avg Bid Price ($) per Impression"
                 , y = "Probability of Winning (%)")
    })
    output$timePlot1 <- renderPlot({
        df_3 <- df_master %>% filter(Publisher_Name %in% input$publisher & between(Date ,input$date[1], input$date[2])) %>% group_by(Publisher_Name, Date) %>% summarise(Avg_Prob_Win_Req_PCT = mean(Prob_Win_Req_PCT),.groups = "keep")
        ggplot(df_3, aes(x = Date, y = Avg_Prob_Win_Req_PCT, color = Publisher_Name))+
            geom_line(stat = "identity", size = 1.2) +
            geom_point(stat = "identity", size = 1.2) +
            scale_color_manual(values = color_codes)+
            scale_y_continuous(limits = c(0,4),breaks = seq(0,5, by = 1), expand = c(0,0))+
            theme(text = element_text(size=13, face = "bold"),legend.position = "none", axis.text.x = element_text(angle=90), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
            scale_x_date(date_labels = "%b %d", date_breaks = "7 day", date_minor_breaks = "1 day", expand = c(0,0))+
            labs(title = "Probability of Winning (%) across Time Frame and Publishers"
                 , x = "Date"
                 , y = "Probability of Winning (%)")
    })
    
    output$timePlot2 <- renderPlot({
        df_4 <- df_master %>% filter(Publisher_Name %in% input$publisher & between(Date ,input$date[1], input$date[2])) %>% group_by(Publisher_Name, Day_Of_Week) %>% summarise(Avg_Prob_Win_Req_PCT = mean(Prob_Win_Req_PCT),.groups = "keep")
        ggplot(df_4, aes(x = factor(Day_Of_Week, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday")), y = Avg_Prob_Win_Req_PCT, fill = Publisher_Name))+
            geom_bar(position = "dodge",stat = "identity") +
            scale_fill_manual(values = color_codes)+
            scale_y_continuous(limits = c(0,NA),breaks = seq(0,20, by = 0.5), expand = c(0,0))+
            theme(text = element_text(size=13, face = "bold"),legend.position = "bottom", legend.text = element_text(size = 16, face= "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
            labs(title = "Probability of Winning (%) across Day of Week and Publishers"
                 , x = "Day of Week"
                 , y = "Probability of Winning (%)"
                 , fill = "")
    })
    
    output$timePlot3 <- renderPlot({
        df_5 <- df_master %>% filter(Publisher_Name %in% input$publisher & between(Date ,input$date[1], input$date[2]))
        hour_prob_win_pub <- df_5 %>% group_by(Publisher_Name, Hour) %>% summarise(Avg_Prob_Win_Req_PCT = mean(Prob_Win_Req_PCT), .groups = "keep")
        
        ggplot(hour_prob_win_pub, aes(x = Hour, y = Avg_Prob_Win_Req_PCT, color = Publisher_Name, group = Publisher_Name))+
            geom_line(stat="identity", size = 1.5)+
            scale_color_manual(values = color_codes)+
            scale_y_continuous(limits = c(0,NA),breaks = seq(0,20, by = 0.25), expand = c(0,0))+
            theme(text = element_text(size=13, face = "bold"),legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
            labs(title = "Probability of Winning (%) across Hour of Day and Publishers"
                 , x = "Hour of Day"
                 , y = "Probability of Winning (%)")
    })
    
    output$remote_feed1 <- renderPlot({
        df_6 <- df_master %>% filter(Publisher_Name == input$publisher_dropdown) %>% group_by(Publisher_Name, Remote_Feed_Name) %>% summarise(Avg_RF_Bid_eCPM = mean(Remote_Feed_Average_Bid_eCPM, na.rm = TRUE), .groups = "keep")
        ggplot(df_6, aes(x = Remote_Feed_Name, y = Avg_RF_Bid_eCPM, fill = factor(Publisher_Name, c("Daily Motion", "Rolling Stone","Taste of Home"))))+
            geom_bar(stat = "identity") +
            scale_fill_manual(values = color_codes)+
            scale_y_continuous(limits = c(0,10.0),breaks = seq(0,10.0, by = 2.0), expand = c(0,0))+
            theme(text = element_text(size=13, face = "bold"),axis.title.x= element_blank(), axis.text.x= element_text(angle=90), axis.ticks.x= element_blank(), legend.position = "bottom",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
            labs(title = "Average Bidding Price across Remote Feeds"
                 , x = "Remote Feeds"
                 , y = "Average Bidding Price ($)"
                 , fill = "")
    })
    
    output$remote_feed2 <- renderPlot({
        df_7 <- df_master %>% filter(Publisher_Name == input$publisher_dropdown) %>% group_by(Publisher_Name, Remote_Feed_Name) %>% summarise(Avg_Win_price_Per_Impression = mean(Win_Price_Per_Impression, na.rm = TRUE), .groups = "keep")
        ggplot(df_7, aes(x = Remote_Feed_Name, y = Avg_Win_price_Per_Impression, fill = factor(Publisher_Name, c("Daily Motion", "Rolling Stone","Taste of Home"))))+
            geom_bar(stat = "identity") +
            scale_fill_manual(values = color_codes)+
            scale_y_continuous(limits = c(0,0.004),breaks = seq(0,0.004, by = 0.001), expand = c(0,0))+
            theme(text = element_text(size=13, face = "bold"),axis.title.x= element_blank(), axis.text.x= element_text(angle=90), axis.ticks.x= element_blank(), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
            labs(title = "Average Winning Price across Remote Feeds"
                 , x = "Remote Feeds"
                 , y = "Average Winning Price ($)")
    })
    
    output$remote_feed3 <- renderPlot({
        df_8 <- df_master %>% filter(Publisher_Name == input$publisher_dropdown) %>% gather(key = var, value = value, Publisher_Impression_Wins,Publisher_Gross_Impressions, Publisher_Net_Impressions ) %>% select(Publisher_Name,Remote_Feed_Name, var, value)  %>% group_by(Publisher_Name, Remote_Feed_Name, var) %>% summarise(Avg_value = mean(value), .groups = "keep")
        ggplot(df_8,aes(x = Remote_Feed_Name, y = Avg_value, fill = factor(var, c("Publisher_Impression_Wins","Publisher_Gross_Impressions","Publisher_Net_Impressions"))))+    
            geom_bar(position = "dodge",stat = "identity") +
            scale_fill_manual(values = col_codes)+
            theme(text = element_text(size=13, face = "bold"),legend.position = "bottom",axis.text.x = element_text(angle=90), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
            labs(title = "Impression Wins, Gross and Net Impressions across Remote Feeds and Publishers"
                 , x = "Remote Feeds"
                 , y = "Average count"
                 , fill = "")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
