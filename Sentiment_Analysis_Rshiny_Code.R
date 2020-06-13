########################################################################################
# Visulization of sentiment data in R Shiny 
#
# Visualization of Twitter download data, Sentiment Data, Words of clouds, Tokenisatoion 
# Sentiment Radar, Polarity , Most popular Tweets, Emotions table and Pie chart
# Sentiment Map, Most positive and negative Tweet.
#
# Author: Kartik Patel
# Date : 08/11/2019
#
########################################################################################

####################################
#
# Install and load the packages
#
####################################

library(shiny) # Web Application Framework for R
library(shinydashboard) # this library provides a theme on top of 'Shiny', making it easy to create attractive dashboards.
library(highcharter) # very mature and flexible javascript charting library 
library(dplyr) # data manipulation library
library(twitteR) # provides access to the Twitter API
library(rtweet) # designed to collect and organize Twitter data via Twitter's REST and stream API
library(NLP) # Natural Language Processing Infrastructure
library(tm) # text mining
library(stringr) # simple, consistent wrappers for common string operations
library(SnowballC) #  word stemming algorithm for collapsing words to a common root to aid comparison of vocabulary
library(RColorBrewer) # color schemes for maps (and other graphics) 
#library(wordcloud)
library(wordcloud2) # provides an HTML5 interface to wordcloud for data visualization
library(topicmodels) # rovides an interface to the C code for Latent Dirichlet Allocation (LDA) models and Correlated Topics Models (CTM) 
library(tidytext) # Text mining for word processing and sentiment analysis using 'dplyr', 'ggplot2', and other tidy tools.
library(slam) # Data structures and algorithms for sparse arrays and matrices, based on index arrays and simple triplet representations, respectively.
library(tidyr) # Tools to help to create tidy data, where each column is a variable, each row is an observation, and each cell contains a single value.
#library(igraph)
#library(ggraph)
library(widyr) # Encapsulates the pattern of untidying data into a wide matrix, performing some processing, then turning it back into a tidy form
library(quantmod) # Specify, build, trade, and analyse quantitative financial trading strategies.
library(rlist) # Provides a set of functions for data manipulation with list objects, including mapping, filtering, grouping, sorting, updating, searching, and other useful functions.
library(tweenr) # In order to create smooth animation between states of data, tweening is necessary
#library(graphlayouts)
library(plotrix) # Lots of plots, various labeling, axis and color scaling functions.
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggmap) # Spatial Visualization with ggplot2
library(maps) # Draw Geographical Maps
library(mapdata) # Supplement to maps package, providing the larger and/or higher-resolution databases.
library(leaflet) # Create Interactive Web Maps with the JavaScript 'Leaflet' Library
library(curl) # A Modern and Flexible Web Client for R
library(tidyverse) # This package is designed to make it easy to install and load multiple 'tidyverse' packages in a single step.
library(sf)  # Support for simple features, a standardized way to encode spatial vector data.
library(sentimentr) # Calculate text polarity sentiment at the sentence level and optionally aggregate by rows or grouping variable(s).
library(magrittr) # Provides a mechanism for chaining commands with a new forward-pipe operator, %>%
library(xts)  # Require to load for leaflet , eXtensible Time Series
library(zoo) # Require to load for leaflet, S3 Infrastructure for Regular and Irregular Time Series
library(TTR)  # Require to load for leaflet, unctions and data to construct technical trading rules with R.

# Set working directory
setwd("D:/Study/Semester 3/Big Data/Tutorial/Big Data Project")

####################
# Dashboard Starts
####################

# Header of dashboard
header <- dashboardHeader(title= "Sentiment Analysis")

# Side bar of dashboard
sidebar <- dashboardSidebar(

  # Side bar menu item add and seperate by comma
  sidebarMenu(id = 'menu',
              menuItem(strong("Twitter Data"),tabName = 'tdata', icon = icon("table")),
              menuItem(strong("Token Frequency"),tabName = 'token', icon = icon("th-list", lib = "glyphicon")),
              menuItem(strong("Sentiment"),tabName = 'sentiment', icon = icon("thumbs-up", lib = "glyphicon")),
              menuItem(strong("Popular Tweets"),tabName = 'popular', icon = icon("twitter")),
              menuItem(strong("Emotion Pecentage"),tabName = 'emotionalpercentages', icon = icon("percentage")),
              menuItem(strong("Sentiment Map"),tabName = 'map', icon = icon("map")),
              menuItem(strong("Sentiment Data"),tabName = 'sdata', icon = icon("table"))
  ),
  hr(), # Horizontal line adding 
  htmlOutput("topic_selector") # HTML ouput renderring
)

####################
# Dashboard End
###################

##################
# Body started
##################

body <- dashboardBody(
  
######################
# HTML Tag Statred
######################

  tags$head(tags$style(HTML(
    ' 
    /* main sidebar */
    
    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
    font-weight: bold;font-size: 16px;
    }
    '))),
  tags$head(tags$style(HTML(
    '
    /* Add text on right of shinydashboarad header */
    
    .myClass { 
    font-size: 20px;
    line-height: 50px;
    text-align: left;
    font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
    padding: 0 15px;
    overflow: hidden;
    color: white;
    }
    '))),
  
  tags$script(HTML('
                    /* function for add text on right of shinydashboarad header */
                    
                   $(document).ready(function() {
                   $("header").find("nav").append(\'<span class="myClass" style="white-space:pre">      </span>\');
                   })
                   ')),
  tags$head(tags$style(HTML('
                              /* Customize the size of a modal window */
                              
                            .modal-sm {width: 40px;}'))),
  
  # Css code for all charts 
  tags$head(tags$style(HTML("
                            /* bootstrap grid padding for charts box */
  
                            .col-sm-2,.col-sm-12,.col-sm-4,.col-sm-12,.col-sm-6,.col-sm-7,.col-sm-5 {
                            position: relative;
                            min-height: 1px;
                            padding-right: 5px;
                            padding-left: 5px;"))),
  
  tags$head(tags$style(HTML(" 
                            /* Removing Padding or margin when screen size is smaller */
                            
                            .container-fluid {padding-left: 5px; padding-right: 5px;}"))),
  tags$head(tags$style(HTML("
                            /* Reduce spacing between bootstrap horizontal form controls */
                            
                            .form-group {margin-bottom: -15px;}"))),
  
  tags$head(tags$style(HTML("
                            /* Charts sub item box margin */
                              
                            .box {margin-bottom: 10px;}"))),
  
  
  tags$head(tags$style(HTML("#col_word_cloud,#col_freq {padding-left:0px;padding-right:0px;} "))),
  tags$head(tags$style(HTML("#emotionpertable,#emotionperchart {padding-left:0px;padding-right:0px;} "))),
  tags$head(tags$style(HTML("
                            /* Center align shiny box header in CSS and HTML */
                            
                            .box-header {text-align: center;} "))),

######################
# HTML Tags End
######################

################################
# Dashboard tab items Starts
###############################

  tabItems(
    
    # Create a tab for Twitter data display
    tabItem("tdata",
            fluidPage(
              titlePanel("Data"),
              
              # Create a new Row in the Dashboard for Twitter Data
              fluidRow(
                DT::dataTableOutput("table") # Display/render twitter data table
              )
              
            )
    ),

    # Create a new  tab for sentiment data display
    tabItem("sdata",
            fluidPage(
              titlePanel("Sentiment Data"),
              
              # Create a new Row in the dashboard for Sentiment Data
              fluidRow(
                DT::dataTableOutput("stable") # Display/render sentiment data table
              )
              
            )
    ),

    # Create a new  tab for emotion table and chart
    tabItem("emotionalpercentages",
            fluidPage(
              
              # Create a new Row in the dashboard for Emotion table and Chart
              fluidRow(
                
                #emotion table column create with bootstrap 7 wide grid system box
                column(7, id = "emotionpertable",
                       box(width=12, height=450, solidHeader = F, title = strong("Emotion Pecentage Table"),
                           DT::dataTableOutput("emotionpertable")) # Display/render emotion data table
                ),
                
                #emotion chart column create with bootstrap 5 wide grid system box
                column(5, id = "emotionperchart",
                       box(width=12, height=450, solidHeader = F, title = strong("Emotion PIE Chart"),
                           plotOutput("distPie") )   # render Plot of emotion PIE chart 
                       
                 )
              )
          )
    ),
    
    # Create a new  tab for Sentiment Map
    tabItem("map",
            fluidPage(
              
              # Create a new Row in the dashboard for Sentiment Map
              fluidRow(
                
                #sentiment map column create with bootstrap 12 wide grid system box
                column(width = 12,
                       box(width = NULL, solidHeader = TRUE,
                           
                           # render the sentiment map widget
                           leafletOutput("tweet_sentiment", height = 500)
                       )#,
                       #box(width = NULL,
                       #     dataTableOutput("top_tweets")
                       # )
                )
              )
            )
    ),

    # Create a new  tab for Word of cloud and Frequencies of word
    tabItem("token",
            fluidPage(

              # Create a new Row in the dashboard for word of cloud and frquencies of word
              fluidRow(
                
                #cloud of word column create with bootstrap 7 wide grid system box
                column(7, id = "col_word_cloud",
                       box(width=10, height=450, solidHeader = F, title = strong("The Word Cloud"),
                           radioButtons("word_cloud_gram",NULL, c("Uni-gram","Bi-gram"), selected = "Uni-gram", inline = T),
                           
                           # render Word cloud plot in dashboard
                           wordcloud2Output("word_cloud_plot",height = 500)) 
                ),
                
                #frequencies of word column create with bootstrap 5 wide grid system box
                column(5, id = "col_freq",
                       box(width=12, height=450, solidHeader = F, title = strong("Here are the frequent words.."),
                          
                           # render word freq high chart in dashboard
                            highchartOutput("word_freq_plot", height=500)
                       )
                       
                )
              )
            )
            
    ),

    # Create a new  tab for sentiment radar, polarity chart, and postive and negative tweet
    tabItem("sentiment",
            fluidPage(

              # Create a new  row for sentiment radar, polarity chart, and postive and negative tweet             
              fluidRow(
                
                #Emotion radar column create with bootstrap 6 wide grid system box
                column(width = 6, id = "col_emotion",
                       box(width=NULL, height=550, solidHeader = F, title = strong("Emotions Radar"),
                          
                           # render emotion ploar/spider radar high chart in dashboard
                            highchartOutput("emotion_polar_plot",height=500) 
                       )
                ),
                
                #Emotion radar column create with bootstrap 6 wide grid system box
                column(width = 6, 
                       box(width=NULL, height=270, solidHeader = F, title = strong("Sentiment Polarity"),
                           
                           # render polarity bar high chart in dashboard
                           highchartOutput("sentiment_plot",height = 210) 
                       ),
                       box(width=NULL, height=270, solidHeader = F, title = strong("Most Positive/Negative Tweets"),
                           htmlOutput("pos_tweet"), # HTML output of positive tweet render in dashboard
                           htmlOutput("neg_tweet") # HTML output of negative tweet render in dashboard
                       )
                )
              )
            )
            
    ),

    # Create a new  tab for Most popluar tweets
    tabItem("popular",
            fluidPage(
              
              # Create a new  row for most popular tweets
              fluidRow(
                box(width=12, height=370, title = strong("Let us check some popular tweets !!"),
                    radioButtons("fav_rt_button",NULL, c("Most Favorited","Most Retweeted"), selected = "Most Favorited", inline = T),hr(),
                    htmlOutput("fav_rt_tweets")), # HTML output most popular tweets render in dashboard
                
              )
            )
            
    )
    
  )

##############################
# Dashboard tab items Ends
##############################
  
)

#################
# Body Ends
#################

# Shiny dashboard page renderring
ui <- dashboardPage(title = 'Sentiment Analysis', header , sidebar, body) 

############################
# Server Code Starts
############################

server <- function(input, output, session) {

# topic selector, this variable created for election contradiction with different candidate
# Due to data issue by twitter right now not use for all candidate. I'm using this var for single candidate donald trump 
  
  topic_selector="Donald Trump"


###########################################################  
#
# Function: Twitter Data Cleaning
# This function is used for cleaning twitter data
# Input: Downloaded Twitter Data
# Output: Clean tweets dataset
#
###########################################################
  tweets_cleaner <- function(tweet.df){
    
    tweets_txt <- unique(tweet.df$text) # Unique tweets
    clean_tweet = gsub("&amp", "", tweets_txt) # Remove Amp
    clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet) # Remove Retweet
    clean_tweet = gsub("@\\w+", "", clean_tweet) # Remove @
    clean_tweet = gsub("#", " ", clean_tweet) # Before removing punctuations, add a space before every hashtag
    clean_tweet = gsub("[[:punct:]]", "", clean_tweet) # Remove Punct
    clean_tweet = gsub("[[:digit:]]", "", clean_tweet) # Remove Digit/Numbers
    clean_tweet = gsub("http\\w+", "", clean_tweet) # Remove Links
    clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet) # Remove tabs
    clean_tweet = gsub("^\\s+|\\s+$", " ", clean_tweet) # Remove extra white spaces
    clean_tweet = gsub("^ ", "", clean_tweet)  # remove blank spaces at the beginning
    clean_tweet = gsub(" $", "", clean_tweet) # remove blank spaces at the end
    clean_tweet = gsub("[^[:alnum:][:blank:]?&/\\-]", "", clean_tweet) # Remove Unicode Char
    
    
    clean_tweet <- str_replace_all(clean_tweet," "," ") #get rid of unnecessary spaces
    clean_tweet <- str_replace_all(clean_tweet, "https://t.co/[a-z,A-Z,0-9]*","") # Get rid of URLs https
    clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*","") # Get rid of URLs http
    clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","") # Take out retweet header, there is only one
    clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","") # Get rid of hashtags
    clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","") # Get rid of references to other screennames
    
    clean_tweet # Return clean tweets data
  }

######################################################
#
# Function: Transform clean tweets dataset
# This function is used for transform clean tweets 
# Input: Clean tweets dataset, Custom stop words
# Output: transform dataset
#
##################################################### 

  tweets_cleaner_tm <- function(clean_tweet, custom_stopwords = c("bla bla")){
    
    docs <- Corpus(VectorSource(clean_tweet)) # Create corpus
    #inspect(docs)
  
    # tm_map: Interface to apply transformation functions (also denoted as mappings) to corpora.
    docs <- tm_map(docs, content_transformer(tolower)) # Convert the text to lower case
    docs <- tm_map(docs, removeNumbers) # Remove numbers
    docs <- tm_map(docs, removeWords, stopwords("english")) # Remove english common stopwords
    docs <- tm_map(docs, removeWords, custom_stopwords)  # Remove your own stop word
    docs <- tm_map(docs, removePunctuation) # Remove punctuations
    docs <- tm_map(docs, stripWhitespace) # Eliminate extra white spaces
   
    docs # Return transform data
  }
  
  output$topic_selector <- renderUI({
    
  })
  
#########################################################
#
# Read Data from working directory
# read data of downloaded twitter data and emotion data.
# Append/bind the topic
#
########################################################
  
  tweet_df_ds <- readRDS(file = "tweet_df_ds.rds") # read twitter downloaded data

  # bind the topic with twitter data
  tweet_df_final <- rbind(
    cbind(tweet_df_ds, topic = "Donald Trump")
  )
  
  emotion_df_ds <- readRDS(file = "emotion_df_ds.rds") # read emotion data

  # bind the topic with emotion data
  emotion_df_final <- rbind(
    cbind(emotion_df_ds, topic = "Donald Trump")
  )
  
####################################################################
#
# This reactive expression that generates a cleaned tweets
#
# 1. It is "reactive" and therefore should be automatically
#    re-executed when it's called
# 2. Its output type is a clean tweet dataset
#
###################################################################
  
  cleaned_tweets <- reactive({
    
    progress <- shiny::Progress$new() # create a new shiny progress object
    progress$set(message = "Cleaning Tweets", value = 0) # set message and value 0 in progress object, It's display in start of this reactive function.
    on.exit(progress$close())  # close the progress when this reactive exits (even if there's an error)
    
    x <- tweet_df_final %>% filter(topic == topic_selector) # filter twitter data by topic name, currently only have single topic donald trump due to twitter data issue.
    progress$set(detail = "Cleaning of Data is more....", value = 0.5) # Increment the progress bar, and update the detail text.
    tweets_cleaner(x) # called tweets_cleaner function and passing the filter twitter data
    
  })
  
###################################################################################
#
# This reactive expression that transform cleaned tweets and generated corpus data
#
# 1. It is "reactive" and therefore should be automatically
#    re-executed when it's called
# 2. Its output type is a transform clean tweet dataset
#
###################################################################################
  
  docs <- reactive({

    # custom stopwords by topic donald trump.
    # create array of custom stop words "donaldtrump","trump","donald","whitehouse"
    # no topic define in data set then custom stop words is null
    
    custom_stopwords <- if(topic_selector == "Donald Trump"){
      c("donaldtrump","trump","donald","whitehouse")
    }
    
    else{
      NULL
    }
    
    progress <- shiny::Progress$new()  # create a new shiny progress object
    progress$set(message = "Preparing Docs", value = 0)  # set message and value 0 in progress object, It's display in start of this reactive function.
    on.exit(progress$close())  # close the progress when this reactive exits (even if there's an error)
    
    progress$set(detail = "Docking...", value = 0.5)  # Increment the progress bar, and update the detail text.
    
    # called function tweets_cleaner_tm, passing cleaned tweets dataset and custom stopwords
    
    tweets_cleaner_tm(cleaned_tweets(), custom_stopwords = custom_stopwords)  # return transform data sets
    
  })

###################################################################################
#
# This reactive expression that create term document matrix
#
# 1. It is "reactive" and therefore should be automatically
#    re-executed when it's called
# 2. Its output type is TDM dataset
#
###################################################################################
  
  tdm <- reactive({
    
    progress <- shiny::Progress$new()  # create a new shiny progress object
    progress$set(message = "Creating TDM", value = 0)  # set message and value 0 in progress object, It's display in start of this reactive function.
    on.exit(progress$close()) # close the progress when this reactive exits (even if there's an error)
    
    progress$set(detail = "Almost there...", value = 0.5) # Increment the progress bar, and update the detail text.
   
    # Constructs or coerces to a term-document matrix or a document-term matrix.
    # pass the argument as a  the trsnform data function
    
    TermDocumentMatrix(docs())  # return TDM dataset
    
  })

###############################################################################################
#
# This reactive expression that sort and create data frame of word and frquency of TDM dataset
#
# 1. It is "reactive" and therefore should be automatically
#    re-executed when it's called
# 2. Its output type is sorted data frame which contain words and it's frequency 
#
###############################################################################################
  
  d <- reactive({
    v <- sort(row_sums(tdm()),decreasing=TRUE) # TDM dataset words sum by row and sort by decreasing order
    data.frame(word = names(v),freq=v) # create data frame of sorted dataset by word and frequency
  })
  
######################################################################################################
#
# This reactive expression that calculate emotion score
#
# 1. It is "reactive" and therefore should be automatically re-executed when it's called
# 2. Its output type is data frame of emotion score data set 
# 3. Use NRC to calculate emotion score
#  
# The NRC emotion lexicon is a list of english word and their associations
# with eight basic emotions (anger,fear,anticipation,trust,surprise,sadness,joy,and disgust)
# and two sentiments (positive and negative). The annotations were manually done by crowssourcing.
#  
#####################################################################################################
  
  emotion_score <- reactive({
    
    progress <- shiny::Progress$new() # create a new shiny progress object
    progress$set(message = "Getting Emotions", value = 0.2) # set message and value 0 in progress object, It's display in start of this reactive function.
    on.exit(progress$close()) # close the progress when this reactive exits (even if there's an error)

    x <- data.frame(tweet_nbr = 1:length(cleaned_tweets()), clean_tweet = cleaned_tweets()) # create a dataframe of tweet number and it's clean tweet
    x$clean_tweet <- as.character(x$clean_tweet) # The function returns  a character vector of clean tweet

###########################################################################################################
#
# Calcualte the emotion score
# 1.unnest_tokens function Split a column into tokens using the tokenizers package, 
# splitting the table into one-token-per-row. Input: clean tweet, Output: word tokenize, Token by words.
# This function supports non-standard evaluation through the tidyeval framework.
# 2.Inner join by words with get the emotions using the NRC dictionary.
# 3.In last data frame group by sentiment and summrise score value.
# 
###########################################################################################################
    
    df <- x  %>% unnest_tokens(output = word, input = clean_tweet, token = "words")
    df <- df %>% inner_join(get_sentiments("nrc")) # get the emotions using the NRC dictionary and innner join with tokenize data frame

    # Increment the progress bar, and update the detail text. This progress bar setup for notify the successful execution of function get_sentiments    
    progress$set(value = 0.8, detail = paste("COllating..")) 
   
    df <- df %>% group_by(sentiment) %>% summarise(score = n()) # group by sentiment and summarise the value of data frame
    
    names(df) <- c("emotion", "score") # assign the header name in data frame
    df[c(1:8,9:10),] # Return data frame which contain column 1 to 8 emotions and 9 to 10 is positive and negative
    #df
  })
  
######################################################################################################
#
# This reactive expression that calculate sentiment score
#
# 1. It is "reactive" and therefore should be automatically re-executed when it's called
# 2. Its output type is list of polarity dataset, postive tweet and negative tweet
# 3. Use AFINN to calculate sentiment score
#  
# AFINN is a list of english words rated for valence with an integer between -5 (negative) and
# +5 (positive). The words have been manually lablled by Finn arup nielsen in 2009 - 2011
#  
#####################################################################################################
  
  
  sentiment_score <- reactive({
    
    progress <- shiny::Progress$new() # create a new shiny progress object
    progress$set(message = "Working on Sentiment", value = 0) # set message and value 0 in progress object, It's display in start of this reactive function.
    on.exit(progress$close()) # close the progress when this reactive exits (even if there's an error)

#########################################################################################################
#
# Calcualte the sentiment score
# 1.unnest_tokens function Split a column into tokens using the tokenizers package, 
# splitting the table into one-token-per-row. Input: clean tweet, Output: word tokenize, Token by words.
# This function supports non-standard evaluation through the tidyeval framework.
# 2.Inner join by words with get the emotions using the AFINN dictionary.
# 3.In last data frame group by sentiment and summrise score value.
# 
########################################################################################################
    
    x <- data.frame(tweet_nbr = 1:length(cleaned_tweets()), clean_tweet = cleaned_tweets()) # create a dataframe of tweet number and it's clean tweet
    x$clean_tweet <- as.character(x$clean_tweet) # The function returns  a character vector of clean tweet

    # Increment the progress bar, and update the detail text. This progress bar setup for notify the successful execution of function get_sentiments    
    progress$set(detail = "Getting score...", value = 0.6)
    
    df <- x  %>% unnest_tokens(output = word, input = clean_tweet, token = "words")
    df <- df %>% inner_join(get_sentiments("afinn")) # get the emotions using the AFINN dictionary and innner join with tokenize data frame
    df <- df %>% group_by(tweet_nbr) %>% summarize(score = sum(value)) # group by tweet_nbr(tweet number) and summarise the value of data frame
    
    progress$set(detail = "Getting score...", value = 0.8) # Increment the progress bar, and update the detail text.
   
    # category data frame by score value
    df$category_senti <- ifelse(df$score < 0, "Negative", ifelse(df$score > 0, "Positive", "Neutral"))
    df1 <- df %>% left_join(x) # category sentiment data frame left join with tweet data
    
    # create a list.In this list included polarity(sentiment) data set, postive tweet value and negative tweet value
    x <- list() # create a list
    x[[1]] <- as.data.frame(df1) # First element of list is sentiment data set.
    x[[2]] <- as.character(df1[df1$score == max(df1$score),"clean_tweet"][1,1]) # Second element is positive tweet
    x[[3]] <- as.character(df1[df1$score == min(df1$score),"clean_tweet"][1,1]) # Third element is negative tweet
    
    x # return list of sentiment values
  })

######################################################################################################
#
# This reactive expression that generate polarity chart data frame
#
# 1. It is "reactive" and therefore should be automatically re-executed when it's called
# 2. Its output type is data frame of polartiy or sentiment data
# 
#####################################################################################################
  
  senti_df <- reactive({
    
    # first element of sentiment data set group by it's three category and it's score value
    # and calcualte it's pecentage with color code. In last prepared the data frame of above all
    # condition and return this created data frame. %>%> is pipe oprator.
    
    sentiment_score()[[1]] %>% group_by(category_senti) %>% summarise(score = n()) %>% 
      mutate(score_pct = score/sum(score)*100, coloract = c("#d35400", "#2980b9", "#2ecc71"))
  })
  
######################################################################################################
#
# This reactive expression that generate list of popular tweets
#
# 1. It is "reactive" and therefore should be automatically re-executed when it's called
# 2. Its output type is list. In this list included ost favotite tweets and most retwweted tweets.
# 
#####################################################################################################
  
  fav_rt_tweets <- reactive({
    
    tweets_df <- tweet_df_final %>% filter(topic == topic_selector) # twitter download data filter by donad trump
    x <- list() # create a list
    x[[1]] <- head(tweets_df %>% select(text, favorite_count) %>% arrange(desc(favorite_count))) # top 5 twitter tweets extract by it's favorite count order by descending 
    x[[2]] <- head(tweets_df %>% select(text, retweet_count) %>% arrange(desc(retweet_count))) # top 5 twitter tweets extract by it's retweet count order by descending 
    x # return list
  })
  
########################
# UI output Render
########################
########################################################################################  
# 
# This expression that generates a twitter downloded data datatable is wrapped in a call
# to renderDataTable to indicate that:
#
# 1. It is "reactive" and therefore should be automatically
# 2. Its output type is a  twitter downloded data data table
#
#######################################################################################
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- tweet_df_final # twitter download data with topic
  }))

###########################################################################################  
# 
# This expression that generates a word frequncy basic bar high chart is wrapped in a call
# to renderHighchart to indicate that:
#
# 1. It is "reactive" and therefore should be automatically
# 2. Its output type is a word frequncy basic bar high chart
#
###########################################################################################
  
  output$word_freq_plot <- renderHighchart(
    
    # highchart hc htmlwidget object render
    hc <- highchart() %>% # create highchart object
      
      hc_chart(type = "bar") %>% # pipe high chart type is bar
      
      hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
                 formatter = JS(paste0("function() {
                                       // this javascript use for modified/customize the tooltip of high chart
                                       // final result for this java script is word name with value convert in nK format
                                       var result='';
                                       result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.x.name+'</span>:<b> '
                                       +Math.round(this.point.y.toFixed(0)/100)/10 + 'K' + '</b>';
                                       return result;
      }"))) %>% # cutomize the tooltip of bar chart, the result is name of word with nK value(like news: 12K).
      hc_xAxis(categories = d()[1:100,]$word,
               
               labels = list(style = list(fontSize= '11px')), max=20, scrollbar = list(enabled = T)
      )    %>% # change/add axies labels or styles with words name as a categories
      hc_add_series(name="Word", data = d()[1:100,]$freq, type ="column",
                    
                    color = "#4472c4", showInLegend= F) # adding and removing series with it's frquency from highcharts object
    
  )
  
##################################################################################################################################################
# 
# This expression that generates a word cloud chart is wrapped in a call
# to renderWordcloud2 to indicate that:
#
# 1. It is "reactive" and therefore should be automatically
# 2. Its output type is a word cloud chart
# 3. If user select Uni gram then return list of uni gram words and it's frequency
# 4. else if user click on bi gram then return list of two words and it's frquency
# Bi gram is use for more meaningful text mining in NLP
# worldclod package not included, word cloud crops certain words or simply doesn't show them and show too many words that have little frequency
# The wordcloud2 package is a bit more fun to use, allowing us to do some more advanced visualisations.
#   
##################################################################################################################################################

  output$word_cloud_plot <- renderWordcloud2({
    
    if(input$word_cloud_gram == "Uni-gram"){
      
      set.seed(1234) # for reproducibility 
      
      d1 <- (d() %>% filter(freq>1) %>% arrange(desc(freq)))[1:100,] # filter TDM frequency by >1 and order by frequency in desecding and take 100 first records
      wordcloud2(data = d1, size=0.8, minSize = 0.0, fontWeight = 'bold', 
                 ellipticity = 0.65) # plot/render wordcloud2 of uni gram
      
    } else if(input$word_cloud_gram == "Bi-gram"){
      
      progress <- shiny::Progress$new() # create a new shiny progress object
      progress$set(message = "Bi-gram", value = 0) # set message and value 0 in progress object, It's display in start of this reactive function.
      on.exit(progress$close()) # close the progress when this reactive exits (even if there's an error)
      
      progress$set(value = 0.3, detail = "Creating Bitokens...")  # Increment the progress bar, and update the detail text.
      bitoken <- data.frame(text = sapply(docs(), as.character), stringsAsFactors = FALSE) %>%  # convert data frame columns from factor to charactes 
      unnest_tokens(bigram, text, token = "ngrams", n = 2) # tokenizer for bia gram n=2
      two_word <- bitoken %>% count(bigram, sort = TRUE) # measure the frequency of words by using dplyr's count() to find the most common words in the twitter data
      # Increment the progress bar, and update the detail text.
      progress$set(value = 0.9, detail = paste("Parsing a dataframe..Almost done"))
      sort_two <- two_word[order(two_word$n,decreasing=TRUE),] # sorting of word pair data set by it's frquencies by desecending order
      names(sort_two) <- c("word", "freq") # assign column name word and frquency
      d1 <- (sort_two %>% filter(freq>2) %>% arrange(desc(freq)))[1:50,] # filter TDM frequency by >2 and order by frequency in desecding and take 50 first records
      wordcloud2(data = d1, size=0.8, minSize = 0.0, fontWeight = 'bold', 
                 ellipticity = 0.65) # plot/render wordcloud2 of uni gram
      
      
    }
    
  })
  
###############################################################################  
# 
# This expression that generates a emotoin data datatable is wrapped in a call
# to renderDataTable to indicate that:
#
# 1. It is "reactive" and therefore should be automatically
# 2. Its output type is a emotion data data table
#
###############################################################################
  
  output$stable <- DT::renderDataTable(DT::datatable({
    data <- emotion_df_final # emotion data table render
  }))
  
####################################################################################  
# 
# This expression that generates an emotion percentage datatable is wrapped in a call
# to renderDataTable to indicate that:
#
# 1. It is "reactive" and therefore should be automatically
# 2. Its output type is an emotion percentage data table
#
####################################################################################
 
  output$emotionpertable<-DT::renderDataTable(DT::datatable({
    
    # another way of progress bar update in notify text
    withProgress(message = 'Calculating Emotional Sentiment',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    value <- emotion_df_ds # assign emotion data in value variable
    
    prop.table(value[,1:8]) # proportions calculations with emotion data
    
    # calcalute sentiment score on round up the value with proportions table
    sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    
    sentimentscores <- as.data.frame(sentimentscores) # convert sentiment score data into data frame
    colnames(sentimentscores) <- c("Percentages") # crete column name as a percentages
    
    Emotions <- c("anger","anticipation","disgust","fear","joy","sadness",
                  "surprise","trust") # create list/ array of emotion rows data name
    
    Percentages<- sentimentscores$Percentages
    emotionality<- cbind(Emotions,Percentages) # bind the emotions array with pecentages and create emotion table data set
    emotionality # return the emotion pecentages table
    
  }))

##########################################################################################
# 
# This expression that generates an emotion Percentage pie plot chart is wrapped in a call
# to renderPlot to indicate that:
#
# 1. It is "reactive" and therefore should be automatically
# 2. Its output type is an emotion Percentage pie plot chart
#
##########################################################################################

  output$distPie <- renderPlot({
    
    value <- emotion_df_ds # assign emotion data in value variable
    
    prop.table(value[,1:8]) # proportions calculations with emotion data
    
    # calcalute sentiment score on round up the value with proportions table
    sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    
    sentimentscores <- as.data.frame(sentimentscores) # convert sentiment score data into data frame
    colnames(sentimentscores) <- c("Percentages") # crete column name as a percentages
    
    slices <- sentimentscores$Percentages # crete slice of percentages
    
    labels <- c("anger","anticipation","disgust","fear","joy","sadness",
                "surprise","trust") # create list/ array of emotion rows data label
    
    lbls <- paste(labels, slices) # add percents to labels
    lbls <- paste(lbls,"%",sep="") # add % to labels
    pie3D(slices,labels=lbls,explode=0.1,radius = 1.5
          ,main="") # render 3D pie chart
  })
  
##########################################################################################
# 
# This expression that generates a sentiment map leftet is wrapped in a call
# to renderLeaflet to indicate that:
#
# 1. It is "reactive" and therefore should be automatically
# 2. Its output type is a sentiment map leftet
#
##########################################################################################
  
  
  output$tweet_sentiment <- renderLeaflet({
    
    progress <- shiny::Progress$new()  # create a new shiny progress object
    progress$set(message = "Preparing Sentiment Map", value = 0) # set message and value 0 in progress object, It's display in start of this reactive function.
    on.exit(progress$close()) # close the progress when this reactive exits (even if there's an error)
    
    sentimentmap_df_ds <- readRDS(file = "sentimentmap_df_ds.rds") # read sentiment map data
    
    locations <- sentimentmap_df_ds # assign data into loations data set
    
    
    from_min <- min(locations$avg_sent) # minimum avg sentiment
    to_min <- 0
    
    from_max <- max(locations$avg_sent) # maximum avg sentiment
    to_max <- 1
    
    # calculate centre value from min and max value
    z <- (locations$avg_sent - from_min) * (to_max - to_min) / (from_max - from_min) + to_min
    tbl <- bind_cols(locations %>% select(city_name, pos, neg, avg_sent)) # extract useful column and bind together
    
    # assign color numeric againt centre value of location
    pal <- colorNumeric(
      palette = palette(c("#F54C54", "blue")),
      domain = z
    )
    
    # map htmlwidget render
    map <- leaflet(locations) %>% # create leaflet map object
      addTiles() %>% 
      # add centre circle with it's value and color
      addCircles(radius = z*(10^5.2), stroke = FALSE,  fillOpacity = 1,
                 popup = paste("City:", locations$city_name, "<br>", 
                               "Sentiment:", round(locations$avg_sent,4)),  color = ~pal(z))  %>% 
      
      leaflet::addLegend(position = "bottomleft", pal = pal, values = ~z, opacity = 1, 
                         title = "Average Sentiment") # add legent of setiment score in bottom left side
    
    map # return map object
    
  })
  
##########################################################################################
# 
# This expression that generates an emotoin polar(radar) high chart is wrapped in a call
# to renderHighchart to indicate that:
#
# 1. It is "reactive" and therefore should be automatically
# 2. Its output type is an emotoin polar(radar) high chart
#
##########################################################################################
  
  
  output$emotion_polar_plot <- renderHighchart(
    
    # highchart hc htmlwidget object render
    hc <- highchart() %>% # create highchart object
      
      hc_chart(polar = T) %>% # pipe high chart type is polar spider web
      # change/add axies labels or styles with emotion name as a categories
      hc_xAxis(categories = emotion_score()$emotion, 
               labels = list(style = list(fontSize= '14px')), 
               title =NULL, tickmarkPlacement = "on", lineWidth = 0) %>% 
      hc_plotOptions(series = list(marker = list(enabled = F))) %>% # config objects for series type
      hc_yAxis(gridLineInterpolation = "polygon", lineWidth = 0, min = 0) %>% # draw plygon line across x axis
      # adding and removing series with it's emotion score in polar spider web  highchart
      hc_add_series(name = "Emotions Score", emotion_score()$score, type ="area", color = "#4472c4", pointPlacement = "on")
  )
  
##########################################################################################
# 
# This expression that generates a sentiment(polarity) bar high chart is wrapped in a call
# to renderHighchart to indicate that:
#
# 1. It is "reactive" and therefore should be automatically
# 2. Its output type is a sentiment(polarity) bar high chart
#
##########################################################################################
  
  output$sentiment_plot <- renderHighchart(
    
    # highchart hc htmlwidget object render
    hc <- highchart() %>% # create highchart object
      
      hc_chart(type = "bar") %>% # pipe high chart type is bar
      
      hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
                 formatter = JS(paste0("function() {
                                       // this javascript use for modified/customize the tooltip of high chart
                                       // final result for this java script is polarity name with value convert in % format
                                       var result='';
                                       result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.series.name+'</span>:<b> '+Math.round(this.point.y.toFixed(0))/1 + '%' + '</b>';
                                       return result;
                   }")))%>% # cutomize the tooltip of bar chart, the result is name of polarity with % value(like news: 12%).
      hc_xAxis(categories = senti_df()$category_senti,
               
               labels = list(style = list(fontSize= '12px')) #max=20, scrollbar = list(enabled = T)
      )    %>% # change/add axies labels or styles with sentiment name(polarity) as a categories
      hc_colors(colors = senti_df()$coloract) %>% # add colors by sentiment name(polarity)
      hc_add_series(name="Sentiment", data = senti_df()$score_pct, colorByPoint = TRUE, 
                    type ="column",
                    
                    color = "#4472c4", showInLegend= F) %>% # adding and removing series with it's sentiment % from highcharts object
      hc_yAxis(labels=list(format = '{value}%'),min=0,
               max=100,showFirstLabel = TRUE,showLastLabel=TRUE) # draw bar line across x axis
    
  )
  
##########################################################################################
# 
# This expression that generates a positive tweet HTML output is wrapped in a call
# to renderUI to indicate that:
#
# 1. It is "reactive" and therefore should be automatically
# 2. Its output type is a positive tweet HTML output
#
##########################################################################################
  
  output$pos_tweet <- renderUI({
    
    # HTML object return, positive tweet from sentiment list second value
    HTML(paste("<b>","Most positive tweet: ","</b>"),"<br>","<i style = 'font-weight: bold'>","\"",sentiment_score()[[2]][1],"\"","</i>","<hr>")
  })
  
##########################################################################################
# 
# This expression that generates a negative tweet HTML output is wrapped in a call
# to renderText to indicate that:
#
# 1. It is "reactive" and therefore should be automatically
# 2. Its output type is a negative tweet HTML output
#
##########################################################################################
  
  output$neg_tweet <- renderText({
    
    # HTML object return, negative tweet from sentiment list third value
    HTML(paste("<b>","Most negative tweet: ","</b>"),"<br>","<i style = 'font-weight: bold'>","\"",sentiment_score()[[3]][1],"\"","</i>")
  })
  
##########################################################################################
# 
# This expression that generates a popular tweets HTML output is wrapped in a call
# to renderUI to indicate that:
#
# 1. It is "reactive" and therefore should be automatically
# 2. Its output type is a popular tweets HTML output
# 3. This is based on radio button selection HTML output is geneated.
# 4. If user select Most Favorited  then render most favorited tweets HTML output
#  else if user select Most Retweeted then render most retweeted tweets HTML output
#
##########################################################################################
  
  
  output$fav_rt_tweets <- renderUI({
    
    # if user click button Most Favorited
    if(input$fav_rt_button == "Most Favorited"){
      
      # Create an unordered list (i.e., a list of bullet points).
      # Display tweets and it's count from popular tweet list first element.
      tags$ul(
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][1,]$text,"-",paste(fav_rt_tweets()[[1]][1,]$favorite_count))), style = "margin-bottom: 5px; color: #1D65A6; font-weight: bold"), 
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][2,]$text,"-",paste(fav_rt_tweets()[[1]][2,]$favorite_count))), style = "margin-bottom: 5px; color: #F1931B; font-weight: bold"), 
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][3,]$text,"-",paste(fav_rt_tweets()[[1]][3,]$favorite_count))), style = "margin-bottom: 5px; color: #1D65A6; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][4,]$text,"-",paste(fav_rt_tweets()[[1]][4,]$favorite_count))), style = "margin-bottom: 0px; color: #F1931B; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][5,]$text,"-",paste(fav_rt_tweets()[[1]][5,]$favorite_count))), style = "margin-bottom: 0px; color: #1D65A6; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[1]][6,]$text,"-",paste(fav_rt_tweets()[[1]][6,]$favorite_count))), style = "margin-bottom: 0px; color: #F1931B; font-weight: bold")
      )
    }
    
    # if user click button Most Retweeted
    else if(input$fav_rt_button == "Most Retweeted"){
      
      # Create an unordered list (i.e., a list of bullet points).
      # Display tweets and it's count from popular tweet list second element.
      tags$ul(
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][1,]$text,"-",paste(fav_rt_tweets()[[2]][1,]$retweet_count))), style = "margin-bottom: 5px; color: #1D65A6; font-weight: bold"), 
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][2,]$text,"-",paste(fav_rt_tweets()[[2]][2,]$retweet_count))), style = "margin-bottom: 5px; color: #F1931B; font-weight: bold"), 
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][3,]$text,"-",paste(fav_rt_tweets()[[2]][3,]$retweet_count))), style = "margin-bottom: 5px; color: #1D65A6; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][4,]$text,"-",paste(fav_rt_tweets()[[2]][4,]$retweet_count))), style = "margin-bottom: 0px; color: #F1931B; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][5,]$text,"-",paste(fav_rt_tweets()[[2]][5,]$retweet_count))), style = "margin-bottom: 0px; color: #1D65A6; font-weight: bold"),
        tags$li(tags$i(paste(fav_rt_tweets()[[2]][6,]$text,"-",paste(fav_rt_tweets()[[2]][6,]$retweet_count))), style = "margin-bottom: 0px; color: #F1931B; font-weight: bold")
        
      )
    }
    
  })
  
  
}

############################
# Server Code Ends
############################

# Run the app ----
shinyApp(ui = ui, server = server)