
library(DBI)
library(RPostgres)
library(tidyverse)


# Connect to DB -----------------------------------------------------------


tryCatch({
  print("Connecting to Database…")
  con <-
    dbConnect(
      RPostgres::Postgres(),
      dbname = 'gp_practice_data',
      host = 'localhost',
      port = 5432,
      user = 'postgres',
      password =.rs.askForPassword("Password:")
    )
  print("Database Connected!")
},
error = function(con) {
  print("Unable to connect to Database.")
})

tables <- dbListTables(con)
print(tables)


# User Requirements --------------------------------------------------------


# Select GP and five most common drug

select_gp <- function() {
  s_practiceid <- readline(prompt = "Please enter a practiceid: ")
  
  if (grepl('^W[0-9]{5}$', s_practiceid)) {
    logical_id <- TRUE
  } else {
    cat(
      '\nThe entered practice ID (',
      s_practiceid,
      ') is not logical, please try again.\n',
      sep = ''
    )
  }
  if (!logical_id) {
    stop('\nStopping: the practice ID was not logical,
                     no entry will be found.')
  }
  
  cat('\nAnalysing data from the database, please wait ...\n\n')
  
  gp_cdrugs <- dbGetQuery(
    con,
    paste0(
      "SELECT practiceid, bnfname,quantity 
      FROM gp_data_up_to_2015
      WHERE practiceid= '", s_practiceid,"'
      ORDER BY quantity desc limit 5"
    )
  )
  
  cat('\nThe 5 most prescibed drugs for practice, ', s_practiceid,', were:\n',
      sep = '')
  print(gp_cdrugs$bnfname)
  
  gp_size <- dbGetQuery(
    con,
    "SELECT practiceid, COUNT(*) 
     FROM gp_data_up_to_2015
     GROUP BY practiceid"
  )
  gp_size$status = NA
  
  gp_size$status[gp_size$count< median(gp_size$count)] = "Small"
  
  gp_size$status[gp_size$count > median(gp_size$count)] = "Large"
  
  
  cat("\n", s_practiceid, "is a ", gp_size$status[gp_size$practiceid ==  s_practiceid], "GP Practice in Wales")
  
  
  # Disease Prevalence ------------------------------------------------------
  cat("Lets have a close look at the practice ", s_practiceid ,"\n______________")
  #Query to select BNF data where Obesity is in section description colunm 
  bnf <- dbGetQuery(con,
                    "select *
                    from bnf
                   where sectiondesc Like 'Obesity%' OR sectiondesc Like 'Hypertension%' ")
  
  #Query to select gp data for the selected gp Practice
  gp_data <- dbGetQuery(
                      con, 
                        paste0("Select *
                      from gp_data_up_to_2015
                      where practiceid = '", s_practiceid,"'"))
  
  # Split with regrex to  get the BNFCHEMICAL 
  
  gpdata <- gp_data %>% 
    extract(bnfcode, c("BNFCHEMICAL", "BCode"), "([[:alnum:]]{9})([[:alnum:]]+)")
  
  gpdata$CVD_Status = NA
  
  gpdata$CVD_Status[(gpdata$BNFCHEMICAL %in% bnf$bnfchemical)] = "CVD" 
  
  gpdata$CVD_Status[!(gpdata$BNFCHEMICAL %in% bnf$bnfchemical)] = "No_CVD"
  
  
  df <- data.frame(table(gpdata$CVD_Status))
  colnames(df) <- c('CVD_Status','Freq')
  df$Perc <- df$Freq / sum(df$Freq) * 100
  
  # Wales GP data
  gp_data_all<- dbGetQuery(con, 
                           "Select *
                         from gp_data_up_to_2015")
  
  gpdata_all <- gp_data_all %>% 
    extract(bnfcode, c("BNFCHEMICAL", "BCode"), "([[:alnum:]]{9})([[:alnum:]]+)")
  
  gpdata_all$CVD_Status = NA
  
  gpdata_all$CVD_Status[(gpdata_all$BNFCHEMICAL %in% bnf$bnfchemical)] = "CVD" 
  
  gpdata_all$CVD_Status[!(gpdata_all$BNFCHEMICAL %in% bnf$bnfchemical)] = "No_CVD"
  
  
  df_all <- data.frame(table(gpdata_all$CVD_Status))
  colnames(df_all) <- c('CVD_Status','Freq')
  df_all$Perc <- df_all$Freq / sum(df_all$Freq) * 100
  
  # Practice size 
  Practice_filter = gp_size$practiceid[gp_size$status == gp_size$status[gp_size$practiceid ==  s_practiceid]]
  
  
  gpdata_size = gpdata_all %>% 
    subset(practiceid %in% Practice_filter)
  
  
  df_size = data.frame(table(gpdata_size$CVD_Status))
  colnames(df_size) <- c('CVD_Status','Freq')
  df_size$Perc <- df_size$Freq / sum(df_size$Freq) * 100
  
  
  #Bind table together 
  df$Practice = s_practiceid
  df_all$Practice = "Wales"
  df_size$Practice = "Size Category"
  
  df2 = rbind(df,df_all, df_size)
  
  
  
  #summary(glm(df2$Freq ~ df2$Practice, family=poisson))
  
  
  #xtabs(~ Practice + CVD_Status, data = df2)
  
  
  df2 <- df2 %>%
    group_by(Practice) %>%
    mutate(label_y = Perc)
  
  # Visualize
  
  gg <- ggplot(df2)
  gg <- gg + geom_bar(aes(x = CVD_Status, y = Perc, fill = CVD_Status), 
                      position = "stack", stat = "identity") +
    theme() +
    facet_grid(~Practice) +
    labs(title="Rate of obesity and hypertension of Practice")+
    geom_text(mapping = aes(x=CVD_Status,
                            y=Perc,
                            label = round(Perc,1)))
  print(gg)
  
  
  
  
  
  
  
  # gp_cat <- dbGetQuery(
  #   con,
  #   paste0(
  #     "SELECT bnfname, quantity
  #      FROM gp_data_up_to_2015 WHERE bnfname= '", gp_cdrugs$bnfname,"'"
  #   )
  # )
  # 
  # Practice_cat <- t.test(gp_cdrugs$quantity, gp_cat$quantity,
  #                        paired = FALSE)
  
  #find the count of the practice in up_to_2015
  #find the median of the count of visit
  # find practice count is higher than te median, its large practics
  #else small practices
}

data <- select_gp()

