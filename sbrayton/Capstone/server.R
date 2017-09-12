# get shiny, DBI, dplyr and dbplyr from CRAN
pacman::p_load(shiny,DBI,dplyr,dbplyr,cluster,DBI,rJava,tidyr,RJDBC,RMySQL, ggplot2,  reader, forcats, DT, broom, tidyverse, shiny, dplyr,dbplyr,stringr, sjPlot, sjmisc, questionr, stargazer, rio, ggeffects, FSelector)
# get pool from GitHub, since it's not yet on CRAN
devtools::install_github("rstudio/pool")

mySqlCreds <- list(dbhostname = "52.203.27.250",
                   dbname   = "blownAway",
                   username = "BlownAway",
                   pass = "blownAway123!",
                   port = 3306
)
drv <- dbDriver("MySQL")

conn<-dbConnect(drv, host=mySqlCreds$dbhostname, dbname=mySqlCreds$dbname, 
                user=mySqlCreds$username, password=mySqlCreds$pass, port = mySqlCreds$port)


#connect to database and pull query into a table
db_2017 <- tbl(conn ,sql("
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

group by b.ballhandler_id,`False`,`True`,`NA` ,`Total Access`                        "))

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


data_sets <- c("data_table")

shinyServer(function(input, output) {
  
  
  # Output the data
  output$data_table <- renderTable({ DataLng
  })   

  
})
