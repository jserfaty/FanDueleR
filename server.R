
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)
library(tidyr)
library(RCurl)
library(XML)

generate_lineup <- function(players,num_iter,min_salary,max_salary,salary_cap) {
  
  archive_results <- data.frame(Cost = 0
                                ,Points = 0)  
  
  best_results <- list(LineUp = NULL
                       ,Cost = 0
                       ,Points = 0)
  
  players <- players %>% filter(Salary >= min_salary
                                ,Salary <= max_salary)
  
  for (i in 1:num_iter){
    
    LineUp <- rbind(players[sample(which(players$Pos == "PG"),2),]
                    ,players[sample(which(players$Pos == "SG"),2),]
                    ,players[sample(which(players$Pos == "SF"),2),]
                    ,players[sample(which(players$Pos == "PF"),2),]
                    ,players[sample(which(players$Pos == "C"),1),])
    
    Cost <- LineUp %>% summarize(Cost = sum(Salary))
    
    Points <- LineUp %>% summarize(Points = sum(FPPG))
    
    archive_results[i,1] <- Cost
    archive_results[i,2] <- Points
    
    if (Points > best_results$Points && Cost <= salary_cap){
      
      best_results <- list(LineUp = LineUp
                           ,Cost = Cost
                           ,Points = Points)
    }
    
    if (i%%100==0) print(paste(i,length(best_results$Cost),best_results$Points,best_results$Cost,sep=" "))
  }
  
  result_set <- list(best_results = best_results
                     ,archive_results = archive_results)
  return(result_set)
}

# Download Injury Stats (Thanks, Rotoworld!)
  injuryURL <- "http://www.rotoworld.com/teams/injuries/nba/all/"
  injuryTable <- readHTMLTable(getURL(injuryURL),stringsAsFactors=FALSE,header=TRUE)
  injuryTable <- injuryTable[!unlist(lapply(injuryTable,is.null))]
  injuryTable <- Reduce(rbind, injuryTable)
  injuryTable <- injuryTable[,-c(2:6)]

shinyServer(function(input, output, session) {
  
  sim_params <- reactive({
    
    input$run_sim
    
    isolate({
      
      game_id <- input$game_id_input
      table_id <- input$table_id_input
      min_salary <- input$salary_range[1]
      max_salary <- input$salary_range[2]
      salary_cap <- input$salary_cap_input
      num_iter <- input$num_iter_input
      
      list(game_id = game_id
          ,table_id = table_id
          ,min_salary = min_salary
          ,max_salary = max_salary
          ,salary_cap = salary_cap
          ,num_iter = num_iter)
    })

  })
  
  fan_duel <- reactive({
        
    input$run_sim

     isolate({
       # Download Player Names, Positions, and Prices
       # Get Table
       fdURL <- paste0("https://www.fanduel.com/e/Game/",sim_params()$game_id,"?tableId=",sim_params()$table_id,"&fromLobby=true")
       fanDuelTableRaw <- readHTMLTable(getURL(fdURL,ssl.verifypeer=FALSE),stringsAsFactors=FALSE,header=TRUE)
       fanDuelTableRaw <- fanDuelTableRaw[!unlist(lapply(fanDuelTableRaw,is.null))]
       fanDuelTableRaw <- tbl_df(fanDuelTableRaw[[which.max(unlist(lapply(fanDuelTableRaw,nrow)))]])
       
       # Clean Table
       fanDuelTable <- fanDuelTableRaw %>% separate(Game,into=c("RoadTeam","HomeTeam"),sep="@")
       fanDuelTable <- fanDuelTable %>% mutate(Name = sub("O$|GTD$","",Name))
       fanDuelTable <- fanDuelTable %>% mutate(Salary = sub("[$]","",Salary))
       fanDuelTable <- fanDuelTable %>% mutate(Salary = sub("[,]","",Salary))
       fanDuelTable <- fanDuelTable %>% transform(FPPG = as.numeric(FPPG),Salary = as.numeric(Salary))
       # Fix Names
       fanDuelTable$Name <- fanDuelTable$Name %>% sub("C.J.","CJ",.)
       fanDuelTable$Name <- fanDuelTable$Name %>% sub("J.J.","JJ",.)
       fanDuelTable$Name <- fanDuelTable$Name %>% sub("K.J.","KJ",.)
       fanDuelTable$Name <- fanDuelTable$Name %>% sub("P.J.","PJ",.)
       fanDuelTable$Name <- fanDuelTable$Name %>% sub("Nene Hilario","Nene",.)
       fanDuelTable$Name <- fanDuelTable$Name %>% sub("Luc Richard Mbah a Moute","Luc Mbah a Moute",.)
       fanDuelTable$Name <- fanDuelTable$Name %>% sub("Brad Beal","Bradley Beal",.)
       fanDuelTable$Name <- fanDuelTable$Name %>% sub("Louis Williams","Lou Williams",.)
       fanDuelTable$Name <- fanDuelTable$Name %>% sub("Patrick Mills","Patty Mills",.)
       # Remove uneccessary "Add" column
       fanDuelTable <- fanDuelTable[,-8]
       
       # Filter out injured players
       active_players <- anti_join(fanDuelTable,injuryTable)
     
    })
    
   
  })
  
   gen_lineup <- reactive({

     input$run_sim
     
     isolate({     
        generate_lineup(players = fan_duel()
                        ,num_iter = sim_params()$num_iter
                        ,min_salary = sim_params()$min_salary
                        ,max_salary = sim_params()$max_salary
                        ,salary_cap = sim_params()$salary_cap)
     })
   })

output$plot <- renderPlot({
  plot(gen_lineup()$archive_results)
})
  
  
output$lineup <- renderDataTable({
  gen_lineup()$best_results$LineUp
},options = list(paging = FALSE
                 ,searching = FALSE))

})
