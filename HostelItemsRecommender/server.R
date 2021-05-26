##############################################  CONNECTING TO DATABASE #########################################################

options(mongodb = list(
  "host" = "rshiny-project.nihpp.mongodb.net",
  "username" = "jitesh",
  "password" = "omsrisairam"
))
databaseName <- "newDatabase"

db <- mongo(collection = "Hostel store items", #getting the question collection to get name of survey and questions
            url = sprintf(
                      "mongodb+srv://%s:%s@%s/%s",
                      options()$mongodb$username,
                      options()$mongodb$password,
                      options()$mongodb$host,
                      databaseName
                    ),
                    options = ssl_options(weak_cert_validation = TRUE))
data <- db$find()
write.csv(data ,"./www/HSILive.csv",  quote = FALSE, row.names = TRUE)



############################################### MARKET BASKET ANALYSIS #########################################################
#Loading Data
if(length(data[,1]>50)){
storeData = read.transactions(file="www/HSILive.csv", rm.duplicates= FALSE, header = T , format="basket",sep=",",cols=1);
}else{
  storeData = read.transactions(file="www/HSI.csv", rm.duplicates= FALSE, header = T , format="basket",sep=",",cols=1);
}


#forming rules
brules <- apriori(storeData, parameter= list(support= 0.01, confidence= 1))


#Removing tm if any
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:sentiment, unload=TRUE)
  detach(package:tm, unload=TRUE)
}

#Removing empty LHS rules
brules <- brules[!size(lhs(brules))==0]

#Converting rules into data frame *****very important
df_basket <- as(brules,"data.frame")
df_basket$confidence <- df_basket$confidence * 100
df_basket$support <- df_basket$support * nrow(df_basket)

# Mining rules for recommendations:

# split lhs and rhs into two columns
library(reshape2)
df_basket <- transform(df_basket, rules = colsplit(rules, pattern = "=>", names = c("lhs","rhs")))

# Remove curly brackets around rules
df_basket$rules$lhs <- gsub("[[:punct:]]", "", df_basket$rules$lhs)
df_basket$rules$rhs <- gsub("[[:punct:]]", "", df_basket$rules$rhs)

# convert to character
df_basket$rules$lhs <- as.character(df_basket$rules$lhs)
df_basket$rules$rhs <- as.character(df_basket$rules$rhs)

###################################################### Server function  #################################################################

items <- read.delim ('www/items.txt')

# Define server function  
server <- function(input, output) {
  
###########################################  Dashboard part ###################################################################

  insertUI(selector = "#numberOfRules", 
           ui = div(
                    h2("Number of Rules"),
                    h4(length(brules))))
  
  output$scatter <- renderPlot(
     #plot(brules,  method = "graph", control = list(type= "items"), interactive= 1)
    plot(brules[1:input$scatterRules,],measure=c("support","confidence"), shading="lift" )
  )
  
  output$frequency <- renderPlot(
    itemFrequencyPlot(storeData, topN= input$itemCount, horiz= T)
  )  
  
  output$graph <- renderVisNetwork(
    plot(brules[input$graphRules1:input$graphRules2,], method="graph", control=list(type="items"), engine= "htmlwidget")
  )
  
  output$grouped <- renderPlot(
    plot(brules, method = "matrix", engine= "3d")
  )
  
  output$paracord <- renderPlot(
    plot(brules[input$paracord1:input$paracord2,], method="paracoord",control=list(reorder=T))
  )
  
###########################################################################################################################
  
  
  observeEvent(input$showlist, {
    
    for(i in 1:40){
    insertUI(
      selector = "#itemlister",
      where = 'beforeBegin',
      ui=
        div(
        column(2,
        wellPanel(
         div(
          material_card(
          style= "border: solid; border-width: 0.5px; width:100%; border-color: orange; padding: 5px 5px;",
          class= "card",
          title = p(items[i,1], style= "background-color: rgba(0,0,0,0.7); font-size: 15px; color: white; padding: 0 5px; height: 40px"),
          img(src= paste0(items[i,1],".jpg"),style= "width: 100%; height: 100px;"),
          color = '#d32f2f',
          shiny:: actionButton(paste0(items[i,1]) , "Buy", icon("shopping-cart"), style= "background-color: orange; color: white; font-weight: 900;  width: 100%"),
          divider = T
              )
          )
        )
      )
     )
    )
    removeUI(selector = "#showlist")
    }
  
    
  })
  
################################################# Show recommendation function #######################################  
 showReccomendation <- function(recommendation){
   
   if(length(recommendation[,1]) > 0)
   {  
   output$recommender1 <- renderUI({
      div(
        div(
      div(style= "height: 1px; background-color: black"),
      h3("Suggested for you", style= "padding: 5px; margin-left: 10px; background-color: orange; color: white"),  
      column(2, wellPanel( div( id= "suggest1",
              material_card(
               style= "border: solid; border-width: 0.5px; width:100%; border-color: orange; padding: 5px 5px;",
               class= "card",
               title = p(recommendation[1,1],style= "background-color: rgba(0,0,0,0.7); font-size: 15px; color: white; padding: 0 5px; height: 40px"),
               img(src= paste0(recommendation[1,1],".jpg"),style= "width: 100%; height: 100px;"),
               divider = T)))),
     )
    )
  })#render ui
 } else { output$recommender1 <- renderUI({ div(h3('OOPs! no suggestions for this product :(') ) })  }
   
   
   if(length(recommendation[,1]) > 1)
   {  
     output$recommender2 <- renderUI({
       
       column(2, wellPanel( div( id= "suggest2",
                                 material_card(
                                   style= "border: solid; border-width: 0.5px; width:100%; border-color: orange; padding: 5px 5px;",
                                   class= "card",
                                   title = p(recommendation[2,1],style= "background-color: rgba(0,0,0,0.7); font-size: 15px; color: white; padding: 0 5px; height: 40px"),
                                   img(src= paste0(recommendation[2,1],".jpg"),style= "width: 100%; height: 100px;"),
                                   divider = T))))
     })
   }#if
   else{  output$recommender2 <- renderUI({ div() }) }
  
  if(length(recommendation[,1]) > 2)
  {  
     output$recommender3 <- renderUI({
     
     column(2, wellPanel( div( id= "suggest3",
                               material_card(
                                 style= "border: solid; border-width: 0.5px; width:100%; border-color: orange; padding: 5px 5px;",
                                 class= "card",
                                 title = p(recommendation[3,1],style= "background-color: rgba(0,0,0,0.7); font-size: 15px; color: white; padding: 0 5px; height: 40px"),
                                 img(src= paste0(recommendation[3,1],".jpg"),style= "width: 100%; height: 100px;"),
                                 divider = T))))
      })
  }#if
   else{  output$recommender3 <- renderUI({ div() }) }
   
   if(length(recommendation[,1]) > 3)
   {  
     output$recommender4 <- renderUI({
       
       column(2, wellPanel( div( id= "suggest4",
                                 material_card(
                                   style= "border: solid; border-width: 0.5px; width:100%; border-color: orange; padding: 5px 5px;",
                                   class= "card",
                                   title = p(recommendation[4,1],style= "background-color: rgba(0,0,0,0.7); font-size: 15px; color: white; padding: 0 5px; height: 40px"),
                                   img(src= paste0(recommendation[4,1],".jpg"),style= "width: 100%; height: 100px;"),
                                   divider = T))))
     })
   }#if
   else{  output$recommender4 <- renderUI({ div() }) }
   
   if(length(recommendation[,1]) > 4)
   {  
     output$recommender5 <- renderUI({
       
       column(2, wellPanel( div( id= "suggest5",
                                 material_card(
                                   style= "border: solid; border-width: 0.5px; width:100%; border-color: orange; padding: 5px 5px;",
                                   class= "card",
                                   title = p(recommendation[5,1],style= "background-color: rgba(0,0,0,0.7); font-size: 15px; color: white; padding: 0 5px; height: 40px"),
                                   img(src= paste0(recommendation[5,1],".jpg"),style= "width: 100%; height: 100px;"),
                                   divider = T))))
     })
   }#if
   else{  output$recommender5 <- renderUI({ div() }) }
   
   if(length(recommendation[,1]) > 5)
   {  
     output$recommender6 <- renderUI({
       
       column(2, wellPanel( div( id= "suggest6",
                                 material_card(
                                   style= "border: solid; border-width: 0.5px; width:100%; border-color: orange; padding: 5px 5px;",
                                   class= "card",
                                   title = p(recommendation[6,1],style= "background-color: rgba(0,0,0,0.7); font-size: 15px; color: white; padding: 0 5px; height: 40px"),
                                   img(src= paste0(recommendation[6,1],".jpg"),style= "width: 100%; height: 100px;"),
                                   divider = T))))
     })
   }#if
   else{  output$recommender6 <- renderUI({ div() }) }

}#End of showrecommendation()
 
##################################### OBSERVING BUTTONS IN ITEMS LIST #########################################################
 
  #1 
  observeEvent(input$"veg roll",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "veg roll" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #2
  observeEvent(input$"paneer roll",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "paneer roll" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #3
  observeEvent(input$"veg puff",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "veg puff" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #4
  observeEvent(input$"paneer puff",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "paneer puff" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #5
  observeEvent(input$"samosa",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "samosa" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #6
  observeEvent(input$"onion samosa",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "onion samosa" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #7
  observeEvent(input$"burger",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "burger" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #8
  observeEvent(input$"pizza",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "pizza" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #9
  observeEvent(input$"coke",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "coke" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #10
  observeEvent(input$"sprite",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "sprite" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #11
  observeEvent(input$"maaza",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "maaza" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #12
  observeEvent(input$"gems",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "gems" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #13
  observeEvent(input$"goodday biscuit",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "goodday biscuit" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #14
  observeEvent(input$"hide and seek",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "hide and seek" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #15
  observeEvent(input$"bathing soap",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "bathing soap" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #16
  observeEvent(input$"washing powder",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "washing powder" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #17
  observeEvent(input$"washing soap",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "washing soap" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #18
  observeEvent(input$"apple",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "apple" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #19
  observeEvent(input$"watermelon",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "watermelon" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #20
  observeEvent(input$"mango",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "mango" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #21
  observeEvent(input$"papaya",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "papaya" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #22
  observeEvent(input$"banana",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "banana" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #23
  observeEvent(input$"fruit salad",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "fruit salad" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #24
  observeEvent(input$"ear mufflers",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "ear mufflers" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #25
  observeEvent(input$"sports tshirt",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "sports tshirt" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #26
  observeEvent(input$"socks",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "socks" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #27
  observeEvent(input$"fruit juice",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "fruit juice" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #28
  observeEvent(input$"gobi manchurian",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "gobi manchurian" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #29
  observeEvent(input$"white pant",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "white pant" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #30
  observeEvent(input$"white shirt",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "white shirt" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #31
  observeEvent(input$"cadbury dairymilk",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "cadbury dairymilk" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #32
  observeEvent(input$"amul tricone",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "amul tricone" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #33
  observeEvent(input$"notebook",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "notebook" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #34
  observeEvent(input$"pen",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "pen" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #35
  observeEvent(input$"other stationary items",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "other stationary items" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #36
  observeEvent(input$"chips",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "chips" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #37
  observeEvent(input$"lays",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "lays" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #38
  observeEvent(input$"belt",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "belt" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation)
  })
  #39
  observeEvent(input$"hand fan",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "hand fan" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation) 
  })
  #40
  observeEvent(input$"sitting mat",{
    recommendation <-   df_basket$rules %>% filter(stri_detect_fixed(lhs, "sitting mat" )) %>% select(rhs) %>% distinct()
    showReccomendation(recommendation) 
  })
}#server
