library(RMySQL)


options(mysql = list(
  "host" = "52.203.27.250",
  "port" = 3306,
  "user" = "BlownAway",
  "password" = "blownAway123!"
))

databaseName <- "blownAway"
table <- "responses"

saveData <- function(data) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

loadData <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  query <- sprintf("select b.defender_id
                   ,`False`
                   ,`True`
                   ,`NA`
                   ,`Total Access`
                   from `blownAway`.`yr_2017` as b
                   left join (
                   select   defender_id,count(*) as `False` from `blownAway`.`yr_2017`
                   where shot_result='False' 	group by defender_id
                   )f on f.defender_id = b.defender_id
                   left join (
                   select   defender_id,count(*) as `True` from `blownAway`.`yr_2017`
                   where shot_result='True' 	group by defender_id
                   )t on t.defender_id = b.defender_id
                   left join (
                   select   defender_id,count(*) as `NA` from `blownAway`.`yr_2017`
                   where shot_result='NA' 	group by defender_id
                   )na   on na.defender_id = b.defender_id
                   left join (
                   select   defender_id,count(*)as `Total Access` from `blownAway`.`yr_2017` 	group by defender_id
                   )a on a.defender_id = b.defender_id
                   
                   group by b.defender_id,`False`,`True`,`NA` ,`Total Access`           ", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data

}



