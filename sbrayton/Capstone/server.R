library("shiny")
library("DT")

#global db creds and connection set
getDataSql <- function( query ) {  #Set global database credentials
  mySqlCreds <- list(dbhostname = "52.203.27.250",
                     dbname   = "blownAway",
                     username = "BlownAway",
                     pass = "blownAway123!",
                     port = 3306
  )
  drv <- dbDriver("MySQL")
  #set global database connection variable
  conn<-dbConnect(RMySQL::MySQL(),  host=mySqlCreds$dbhostname, dbname=mySqlCreds$dbname, 
                  user=mySqlCreds$username, password=mySqlCreds$pass, port = mySqlCreds$port)
  result <- dbGetQuery(conn, query)
  dbDisconnect(conn)
  result
  }

#load initial packages. called at the beginning of the session
loadPkgs<-function(){
  # get shiny, DBI, dplyr and dbplyr from CRAN
  pacman::p_load(shiny,DBI,dplyr,dbplyr,cluster,DBI,rJava,tidyr,RJDBC,RMySQL)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
}
loadPredPkgs<-function(){  # get shiny, DBI, dplyr and dbplyr from CRAN
  pacman::p_load(stringr, sjPlot, sjmisc, questionr, stargazer, rio, ggeffects, FSelector,forcats, DT, broom, tidyverse,DMwR)
  library("rpart.plot")
  }


#get the data
  getData1<- function(){
    
    query<-"select * from blownAway.yr_2017"
    
    shoot<- getDataSql(query)

    shoot<-as.data.frame(shoot)

  }
  getData2<- function(){
    #connect to database and pull query into a table
    query<-"
                             select b.ballhandler_id
                             ,`False`
                             ,`True`
                             ,`NA`
                             ,`Total Access`
                             from `blownAway`.`yr_2017` as b
                             left join (
                             select   ballhandler_id,count(*) as `False` from `blownAway`.`yr_2017`
                             where shot_result='False' 	group by ballhandler_id
                             )f on f.ballhandler_id = b.ballhandler_id
                             left join (
                             select   ballhandler_id,count(*) as `True` from `blownAway`.`yr_2017`
                             where shot_result='True' 	group by ballhandler_id
                             )t on t.ballhandler_id = b.ballhandler_id
                             left join (
                             select   ballhandler_id,count(*) as `NA` from `blownAway`.`yr_2017`
                             where shot_result='NA' 	group by ballhandler_id
                             )na   on na.ballhandler_id = b.ballhandler_id
                             left join (
                             select   ballhandler_id,count(*)as `Total Access` from `blownAway`.`yr_2017` 	group by ballhandler_id
                             )a on a.ballhandler_id = b.ballhandler_id
                             
                             group by b.ballhandler_id,`False`,`True`,`NA` ,`Total Access`                        "
    
    db_2017<- getDataSql(query)
    

    #alter the table to a dataframe
    db_2017<- as.data.frame(db_2017)
    

    
    DataLng <- db_2017
    
    DataLng$True <- as.numeric(DataLng$True)
    DataLng$False <- as.numeric(DataLng$False)
    DataLng$'NA' <- as.numeric(DataLng$'NA')
    DataLng$`Total Access` <- as.numeric(DataLng$`Total Access`)
    
    DataLng[is.na(DataLng)] <- 0
    
    DataLng$percOfTot <- (DataLng$True + DataLng$False )/(DataLng$`Total Access`)
    DataLng$percOfTrue <- (DataLng$True  )/(DataLng$`Total Access`)
    DataLng$percOfFalse <- (DataLng$False  )/(DataLng$`Total Access`)
    DataLng$percOfnoAttempt <- (DataLng$'NA'  )/(DataLng$`Total Access`)  
    
    DataLng
    
    # df.means.fit <- kmeans(DataLng[,7:9], 3, nstart = 10000) # k = 3
    # 
    # dat <- as.data.frame(df.means.fit$centers)
    # 
    # 
    # dat
   

}

#function to get and compute the data
compute_data <- function() {
 if(! exists(db_2017)  ){getData2()}
  
  df.means.fit <- kmeans(DataLng[,7:9], 3, nstart = 10000) # k = 3
  
  dat <- as.data.frame(df.means.fit$centers)
  

  dat
}

#function to get and compute the data
cluster_plot_data <- function(x) {
  
  df.means.fit <- kmeans(DataLng[,7:9], 6, nstart = 10000) # k = 3

  library("factoextra")
  fviz_cluster(df.means.fit, data = DataLng[,7:9])
  
}

compute_ballerTrue <-function(){
  shoot %>% 
    filter (shot_result == "False") %>%
    group_by(defender_id, led_to_shot == "True") %>% 
    summarise(shot_result =n())
}

compute_shotClockChange<-function(){
  # The average time on the shot clock when each defender charges the offender
  shoot <-getData1()
  
  shoot %>% 
    group_by(defender_id, ballhandler_id) %>% 
    summarise(average_shot_clock = mean(start_shot_clock))
  
}

compute_avgDist<-function(){
  # The average distance from the busket when each defender charges the offender
  shoot %>% 
    group_by(defender_id, ballhandler_id) %>% 
    summarise(average_distance_busket = mean(basket_distance))
}

compute_prediction<- function(shoot){

 subset_2017=shoot[c(1,5,6,7,8)]
  
  #average time on clock
  avg_shot=subset_2017 %>% 
    group_by(chance_id) %>% 
    summarise(avg_clock = mean(start_shot_clock))
  
  # number of passes
  passes=subset_2017 %>%
    count(chance_id)
  #merge tables
  subset_2017=merge(passes,subset_2017,by.x=('chance_id'),by.y=('chance_id'))
  subset_2017=merge(avg_shot,subset_2017,by.x=('chance_id'),by.y=('chance_id'))
  # filter instances that are led_to_shot=True
  #70/30 split for training and testing
  
  sample_df=subset_2017[1:20000,]
  
  trPerc <- 0.7
  sp <- sample(1:nrow(sample_df),as.integer(trPerc*nrow(sample_df)))
  ## division in two samples
  tr <- sample_df[sp,]
  ts <- sample_df[-sp,]
  
  sample_df$shot_result=as.factor(sample_df$shot_result)
  subset_2017_led_to_shot=subset(subset_2017, led_to_shot == 'True')
  
  #build decision tree model
  set.seed(1234)
  acdt <- rpartXse(shot_result ~ avg_clock+n+start_shot_clock+basket_distance,tr)
  
  # visualize decision tree
  prp(acdt,type=4,extra=101)
  
  # calculate and print out accuracy
  
  # 
  # psdt <- predict(acdt,ts,type='class')
  # mcdt=table(psdt,ts$shot_result)
  # 
  # errdt <- 100*(1-sum(diag(mcdt))/sum(mcdt))
  # errdt
  # 
  
}

shinyServer(function(input, output, session) {

  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 
                 setProgress(message = "Initialization...",value = .25)
                 loadPkgs() 
                 
                 setProgress(message = "getting data...",value = .5)
                 DataLng<<-getData2()
                   
                 setProgress(message = "getting data...",value = .75)
                 shoot<<-getData1()   
                 
               })
  
              ##cluster data            
              output$plot1 <- renderPlot({
                
                withProgress(message = 'Calculation in progress',
                             detail = 'This may take a while...', value = 0, {
                               
                               palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                                         "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
                               par(mar = c(5.1, 4.1, 0, 1))
                               
                               setProgress(message = "Generating Custers...",value = .25)
                               Sys.sleep(1)
                               setProgress(message = "Generating Clusters...",value = .5)
                               
                               plot(cluster_plot_data(DataLng),
                                    col = cluster_plot_data()$cluster,
                                    pch = 20, cex = 3)
                               points(cluster_plot_data()$centers, pch = 4, cex = 4, lwd = 4)
                               
                               setProgress(message = "Ok !",value = 100)
                               Sys.sleep(1)
                               
                             })
                
                
              })

              
              ##random forrest plot
              output$plot2 <- renderPlot({
                
                withProgress(message = 'Calculation in progress',
                             detail = 'This may take a while...', value = 0, {

                               setProgress(message = "Loading Prediction Packages..",value = .25)
                               loadPredPkgs()
                               
                               setProgress(message = "Running Random Forrest...",value = .5)
                               palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                                         "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
                               par(mar = c(5.1, 4.1, 0, 1))
                               plot(compute_prediction(shoot))
                               
                               setProgress(message = "Ok !",value = 100)
                               Sys.sleep(1)
                               
                             })
                

              })
              
              
              # # Output the data
              # output$data_table <- renderTable({
              #
              #   getData2()
              #
              # })
              # output$data_table2 <- renderTable({
              #
              #   compute_ballerTrue()

              #})
              # output$data_table3 <- renderTable({ 
              # 
              #   compute_shotClockChange()
              #   
              # })  
              # output$data_table4 <- renderTable({ 
              #   
              #   compute_avgDist()
              #   
              # }) 
              #
  
              

  
})
