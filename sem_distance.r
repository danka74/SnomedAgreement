library(RMySQL)

lin_sem_distance <- function (data) {
  options(warn = -1)
  con <- dbConnect(MySQL(),
                   user="user", password="password",
                   dbname="snomed_20160731", host="127.0.0.1")
  
  codes <- sort(na.omit(unique(as.vector(data))))
  n_codes <- length(codes)
  
  weight <- matrix(nrow=n_codes, ncol=n_codes)
  
  for(i in 1:n_codes) {
    weight[i,i] <- 1
    if(i > 1) {
      for(j in 1:(i-1)) {
        descIrs <- dbSendQuery(con, paste("SELECT descendants FROM concepts WHERE Id = ", codes[i]))
        descI <- as.numeric(dbFetch(descIrs))
        dbClearResult(descIrs)
        descJrs <- dbSendQuery(con, paste("SELECT descendants FROM concepts WHERE Id = ", codes[j]))
        descJ <- as.numeric(dbFetch(descJrs))
        dbClearResult(descJrs)
        lcsRs <- dbSendQuery(con, paste("SELECT t1.SupertypeId, c.descendants FROM transitiveclosure t1 join transitiveclosure t2 on t1.SupertypeId = t2.SupertypeId JOIN concepts c ON t1.SupertypeId = c.Id WHERE t1.SubtypeId = ",
                                        codes[i], " AND t2.SubtypeId = ", codes[j], " ORDER BY (t1.PathLength + t2.PathLength) ASC LIMIT 1"))
        lcs <- dbFetch(lcsRs)
        descLcs <- as.numeric(lcs[2])
        dbClearResult(lcsRs)
        
        lin <- 2 * -log(descLcs / 321901 ) / (-log(descI / 321901) + -log(descJ / 321901))
    
        #print(lin)
        
        weight[i,j] <- lin 
        weight[j,i] <- lin
      }
    }
  }
  
  dbDisconnect(con)

  return(weight)
}
