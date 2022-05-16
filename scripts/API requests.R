list.of.packages <- c("httr","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(httr)
library(jsonlite)

url <- "https://candidates.democracyclub.org.uk/api/next/results/"

output <- data.frame(url = character(),
                     turnout_num = numeric(),
                     turnout_pct = numeric(),
                     num_spoilt_ballots = numeric(),
                     source = character(),
                     ballot_id = character(),
                     con22 = numeric(),
                     lab22 = numeric(),
                     ld22 = numeric(),
                     green22 = numeric(),
                     ind22 = numeric(),
                     ukip22 = numeric(),
                     plaid22 = numeric(),
                     other22=numeric())

main_parties <- c("PP52","PP53","PP90","PP63","ynmp-party:2","PP85","PP77","PP101","joint-party:53-119")

numeric_0 <- function(x){if (length(x)<1) 0 else x}

for (i in 1:10000){
  
  res <- GET(url)
  data <- fromJSON(rawToChar(res$content))
  
  con_vote <- c()
  lab_vote <- c()
  ld_vote <- c()
  green_vote <- c()
  ind_vote <- c()
  ukip_vote <- c()
  plaid_vote <- c()
  other_vote <- c()
  
  for (j in 1:length(data[["results"]][["candidate_results"]])){
    
    temp <- data[["results"]][["candidate_results"]][[j]]
    
    by_party <- temp %>% group_by(party$ec_id) %>% summarise(sum=sum(num_ballots),n=n())
    
    con <- numeric_0(by_party$sum[which(by_party$`party$ec_id`=="PP52")]/by_party$n[which(by_party$`party$ec_id`=="PP52")])
    lab <- numeric_0(sum(by_party$sum[which((by_party$`party$ec_id`=="PP53")|(by_party$`party$ec_id`=="joint-party:53-119"))])/sum(by_party$n[which((by_party$`party$ec_id`=="PP53")|(by_party$`party$ec_id`=="joint-party:53-119"))]))
    ld <- numeric_0(by_party$sum[which(by_party$`party$ec_id`=="PP90")]/by_party$n[which(by_party$`party$ec_id`=="PP90")])
    green <- numeric_0(by_party$sum[which(by_party$`party$ec_id`=="PP63")]/by_party$n[which(by_party$`party$ec_id`=="PP63")])
    ind <- numeric_0(by_party$sum[which(by_party$`party$ec_id`=="ynmp-party:2")])
    ukip <- numeric_0(by_party$sum[which(by_party$`party$ec_id`=="PP85")]/by_party$n[which(by_party$`party$ec_id`=="PP85")])
    plaid <- numeric_0(by_party$sum[which(by_party$`party$ec_id`=="PP77")])
    snp <- numeric_0(by_party$sum[which(by_party$`party$ec_id`=="PP101")])
    other <- numeric_0(sum(by_party$sum[which(!by_party$`party$ec_id`%in%main_parties)])/sum(by_party$n[which(!by_party$`party$ec_id`%in%main_parties)]))
    
    con_vote <- append(con_vote,con)
    lab_vote <- append(lab_vote,replace_na(lab,0))
    ld_vote <- append(ld_vote,ld)
    green_vote <- append(green_vote,green)
    ind_vote <- append(ind_vote,ind)
    ukip_vote <- append(ukip_vote,ukip)
    plaid_vote <- append(plaid_vote,plaid)
    other_vote <- append(other_vote,replace_na(other,0))
    
  }
  
  to_add <- data.frame(url = data[["results"]][["url"]],
                       turnout_num = data[["results"]][["num_turnout_reported"]],
                       turnout_pct = data[["results"]][["turnout_percentage"]],
                       num_spoilt_ballots = data[["results"]][["num_spoilt_ballots"]],
                       source = data[["results"]][["source"]],
                       ballot_id = data[["results"]][["ballot"]][["ballot_paper_id"]],
                       con22 = con_vote,
                       lab22 = lab_vote,
                       ld22 = ld_vote,
                       green22 = green_vote,
                       ind22 = ind_vote,
                       ukip22 = ukip_vote,
                       plaid22 = plaid_vote,
                       other22 = other_vote)
  
  output <- rbind(output,to_add)
  
  url <- data[["next"]]
  
}

output$election_type <- sapply(strsplit(as.character(output$ballot_id), "\\."), `[`, 1)

output <- output[which(output$election_type=="local"),]

output$la <- sapply(strsplit(as.character(output$ballot_id), "\\."), `[`, 2)
output$ward <- sapply(strsplit(as.character(output$ballot_id), "\\."), `[`, 3)
output$date <- sapply(strsplit(as.character(output$ballot_id), "\\."), `[`, 4)

output2 <- output[which((output$date=="2018-05-03")|(output$date=="2022-05-05")),]

write.csv(output2, "Downloads/democracyclub2.csv")
