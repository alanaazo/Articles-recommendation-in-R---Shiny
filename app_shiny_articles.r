
library(shiny)
library(lubridate)
library(dplyr)
library(stringr)
library(shinyjs)
artic = artic1000
sub = sort(unique(artic$submitter))

artic = artic %>% select(kmeans8,submitter, title, abstract, link, categories,update_date,cat_hep:cat_gr_qc)
artic$kmeans8 = ifelse(artic$kmeans8 == 1,'a',ifelse(artic$kmeans8 == 2,'b',ifelse(artic$kmeans8 == 3,'c',ifelse(artic$kmeans8 == 4,'d',ifelse(artic$kmeans8 == 5,'e',ifelse(artic$kmeans8 == 6,'f',ifelse(artic$kmeans8 == 7,'g','h')))))))

artic$update_date = ymd(artic$update_date) 
artic_orig = artic

ui <- navbarPage(
  
  # Application title
  titlePanel("Search for scientific articles"),
  
  tabPanel("Filtering",
  sidebarLayout(
    sidebarPanel(
      
      useShinyjs(),
      
      actionButton("resetAll", "Reset my choices"),
      div(
      id = "form",
      sliderInput(inputId = "number", label = "Choose desirable number of articles", min = 1, max = 10, value = 5, ticks = F),
      selectizeInput(inputId = "author", label = "Choose the author", 
                  choices = sub, multiple = T), 
      checkboxGroupInput(inputId = "themes", label = "Choose the categories of interest", choices = c("High Energy Physics" =  "cat_hep",
                                                                                                "Astrophysics" = "cat_astro",
                                                                                                "Physics" = "cat_physics",
                                                                                                "Quantitative Biology" = "cat_q_bio",
                                                                                                "Condensed Matter" = "cat_cond_mat",
                                                                                                "Math" = "cat_math",
                                                                                                "Computer science" = "cat_cs",
                                                                                                "Nonlinear Sciences" = "cat_nlin",
                                                                                                "Nuclear Physics" = "cat_nucl",
                                                                                                "Quantum Physics" = "cat_quant",
                                                                                          "General Relativity and Quantum Cosmology" = "cat_gr_qc"), selected = c("cat_math" )),
      dateRangeInput(inputId = "date", label = "Choose the dates of article update", min = min(ymd(artic1000$update_date)), max = max(ymd(artic1000$update_date)), start = min(ymd(artic1000$update_date)), end = max(ymd(artic1000$update_date)))), 
    
     
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("Found"),
      tableOutput("recArticle")
    )
  )),
  tabPanel("Recommendation",
           sidebarLayout(
             sidebarPanel(
               sliderInput(inputId = "number2", label = "Choose the number of articles which were found", min = 1, max = 10, value = 5, ticks = F),
               "Rate the articles",
               checkboxInput("r1", 
                             "1st article",
                             value = F),
               conditionalPanel(
                 condition = "input.r1",
                 sliderInput(inputId = "Rating1", label = "Rate the 1st article", min = 1, max = 5, value = 3, ticks = F),),
               
               conditionalPanel(
                 condition = "input.number2>1",
                 checkboxInput("r2", 
                               "2nd article",
                               value = F),
                 conditionalPanel(
                   condition = "input.r2",
                   sliderInput(inputId = "Rating2", label = "Rate the 2nd article", min = 1, max = 5, value = 3, ticks = F),)),
               conditionalPanel(
                 condition = "input.number2>2",
                 checkboxInput("r3", 
                               "3rd article",
                               value = F),
                 conditionalPanel(
                   condition = "input.r3",
                   sliderInput(inputId = "Rating3", label = "Rate the 3rd article", min = 1, max = 5, value = 3, ticks = F),)),
               conditionalPanel(
                 condition = "input.number2>3",
                 checkboxInput("r4", 
                               "4th article",
                               value = F),
                 conditionalPanel(
                   condition = "input.r4",
                   sliderInput(inputId = "Rating4", label = "Rate the 4th article", min = 1, max = 5, value = 3, ticks = F),)),
               conditionalPanel(
                 condition = "input.number2>4",
                 checkboxInput("r5", 
                               "5th article",
                               value = F),
                 conditionalPanel(
                   condition = "input.r5",
                   sliderInput(inputId = "Rating5", label = "Rate the 5th article", min = 1, max = 5, value = 3, ticks = F),)),
               conditionalPanel(
                 condition = "input.number2>5",
                 checkboxInput("r6", 
                               "6th article",
                               value = F),
                 conditionalPanel(
                   condition = "input.r6",
                   sliderInput(inputId = "Rating6", label = "Rate the 6th article", min = 1, max = 5, value = 3, ticks = F),)),
               conditionalPanel(
                 condition = "input.number2>6",
                 checkboxInput("r7", 
                               "7th article",
                               value = F),
                 conditionalPanel(
                   condition = "input.r7",
                   sliderInput(inputId = "Rating7", label = "Rate the 7th article", min = 1, max = 5, value = 3, ticks = F),)),
               conditionalPanel(
                 condition = "input.number2>7",
                 checkboxInput("r8", 
                               "8th article",
                               value = F),
                 conditionalPanel(
                   condition = "input.r8",
                   sliderInput(inputId = "Rating8", label = "Rate the 8th article", min = 1, max = 5, value = 3, ticks = F),)),
               conditionalPanel(
                 condition = "input.number2>8",
                 checkboxInput("r9", 
                               "9th article",
                               value = F),
                 conditionalPanel(
                   condition = "input.r9",
                   sliderInput(inputId = "Rating9", label = "Rate the 9th article", min = 1, max = 5, value = 3, ticks = F),)),
               conditionalPanel(
                 condition = "input.number2>9",
                 checkboxInput("r10", 
                               "10th article",
                               value = F),
                 conditionalPanel(
                   condition = "input.r10",
                   sliderInput(inputId = "Rating10", label = "Rate the 10th article", min = 1, max = 5, value = 3, ticks = F),))
             ),
             mainPanel(
               tableOutput("recom"),
               textOutput("Print")
             ))
))
             
  

# Define server logic 
server <- function(input, output) {
  
  observeEvent(input$resetAll, {
    reset("form")
  }) 
  
  

    getart = function(authors_set, art_numb, period_beg,period_end,topic_set){
      int <- interval(ymd(period_beg), ymd(period_end))
      artic = artic %>% filter(update_date %within% int)
      if (nrow(artic)==0){
       return()
        break
      }else{
        m = artic %>% select(topic_set,title)
        df=data.frame()
        for (i in 1:length(topic_set)){
          t = m%>% filter(m[i] == '1')
          df = rbind(df,t)
          df = unique(df)
        }
        artic = artic %>% inner_join(df)
        
        if (nrow(artic)==0){
        
          return()
          break
        }else
        
        if (length(topic_set) < 1){
          
          return()
          break
        }else{  
          
          artic_aut = artic %>% filter(submitter %in% authors_set)
          
        
          if (length(authors_set) < 1){
            res <<- artic[2:5]
            
            if (art_numb>nrow(res)){final_numb=nrow(res)} else {final_numb = art_numb}
            return(res[sample(nrow(res), final_numb), ])
          }else{
            
            if (nrow(artic_aut)==0){
              return()
              break
            }else
            
            res <<- artic_aut[2:5]
            
            if (art_numb>nrow(res)){final_numb=nrow(res)
            } else {final_numb = art_numb}
            return(res[sample(nrow(res), final_numb), ])
          }   
        }
      }
    }
    
 
     
    output$recArticle <- renderTable({
      getart(input$author, input$number, str_split(input$date, " ")[1], str_split(input$date, " ")[2],input$themes)  })
    
    
    
    getart2 = function(authors_set, art_numb, period_beg,period_end,topic_set){
      int <- interval(ymd(period_beg), ymd(period_end))
      artic = artic %>% filter(update_date %within% int)
      if (nrow(artic)==0){
        return("Choose another period, please")
        break
      }else{
        m = artic %>% select(topic_set,title)
        df=data.frame()
        for (i in 1:length(topic_set)){
          t = m%>% filter(m[i] == '1')
          df = rbind(df,t)
          df = unique(df)
        }
        artic = artic %>% inner_join(df)
        
        if (nrow(artic)==0){
          
          return( "Choose another topic or period (there is no articles on chosen topics for this period)")
          break
        }else
          
          if (length(topic_set) < 1){
            
            return("You should choose the topics of interest")
            break
          }else{  
            
            artic_aut = artic %>% filter(submitter %in% authors_set)
            
            
            if (length(authors_set) < 1){
              res <<- artic[2:5]
              
              if (art_numb>nrow(res)){ 
              n = as.character(nrow(res))
             
              if(nrow(res) == 1){
                return("We could find only one article (which is less than you wished), change the settings if you want more")} 
              else{ 
                founded = c("We could find only", n , "articles (which is less than you wished), change the settings if you want more")
                {return(founded)} }
              
            }}else{
              
              if (nrow(artic_aut)==0){
                return("Choose another author, topic or period (there is no articles of selected authors on chosen topics for this period)")
                break
              }else
                
                res <<- artic_aut[2:5]
              
              if (art_numb>nrow(res)){    
                n = as.character(nrow(res)) 
                if(nrow(res) == 1){
                  return("We could find only one article (which is less than you wished), change the settings if you want more")} 
                else{ 
              founded = c("We could find only", n , "articles (which is less than you wished), change the settings if you want more")
              {return(founded)} }} 
               
          }
      }
    }}
    
    
    output$Found <- renderText({
      getart2(input$author, input$number, str_split(input$date, " ")[1], str_split(input$date, " ")[2],input$themes)
            })
    
    eval = function(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,art_numb,topic_set,rate_num){
      res = getart(input$author, input$number, str_split(input$date, " ")[1], str_split(input$date, " ")[2],input$themes)
      if (length(res)==0){
        return()
        break
      }else 
        {
      rating_set = c(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10)
      rating_set = rating_set[1:nrow(res)]
      res$rating = rating_set
      res = res %>% left_join(artic) %>% select(submitter,title,abstract,link,kmeans8,rating,categories)
      try = res %>% group_by(kmeans8,categories) %>% summarise(fin_rating = sum(rating)) %>% arrange(-fin_rating) %>% top_n(1)
      cluster = try$kmeans8
      topic = try$categories
      artic = artic %>% filter(kmeans8 == cluster,categories == topic) %>% select(submitter,title,abstract,link)
      return(head(artic,art_numb))
    }}
    
    
    output$recom <- renderTable({
      eval(input$Rating1,input$Rating2,input$Rating3,input$Rating4,input$Rating5,input$Rating6,input$Rating7,input$Rating8,input$Rating9,input$Rating10,input$number,input$themes,input$number2)
    })
    
    check = function(rate_num){
      res = getart(input$author, input$number, str_split(input$date, " ")[1], str_split(input$date, " ")[2],input$themes)
      if (length(res)==0){
        return("Sorry, we didn't manage to find any articles, please choose other criteria for filtering")
      } else{if (nrow(res)<rate_num){ return("Don't try to rate more articles than it was found, please")}} 
    
    }
    output$Print <- renderText({
      check(input$number2)
    })
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
