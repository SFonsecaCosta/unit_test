if (!requireNamespace("reshape2", quietly=TRUE))
  install.packages("reshape2")
library(reshape2)

popmean <- function(data, intervention, years){
  
  ## if .tsv file is provided in the data argument instead of the object directly.
  if(typeof(data) == "character"){
    data <- read.table(data, header = TRUE, sep = "\t")
  } 
  ## re-format table dataset 'dat'
  dat2 <- melt(data, id.vars = c("zone",	"population", "year"),
               variable.name = "intervention", 
               value.name = "PR")
  
  collect_weighted <- c()
  for (approach in intervention) {
    
    for (year in years) {
      
      select_pr <- dat2$PR[dat2$intervention == approach & dat2$year == year]
      select_popSize <- dat2$population[dat2$intervention == approach & dat2$year == year]
      
      # custom weighted_mean is equivalent to: round(weighted.mean(select_pr, select_popSize), 5)
      weighted_mean <- round(sum(select_popSize*select_pr)/sum(select_popSize), 5)
      message("National prevalence = ",weighted_mean, " for year ", year, " and intervention ", approach, ".")
      collect_weighted <- rbind(collect_weighted, weighted_mean)
    }
  }
  return(collect_weighted)
}