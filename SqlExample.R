for (pkg in c("knitr", "RMySQL", "dplyr", "ggplot2", "tidyr", "data.table")) {
  if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
    install.packages(pkg, repos="http://cran.fhcrc.org", dependencies=TRUE)
    if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
      stop(paste0(c("Can't load package: ", pkg, "!"), collapse = ""))
    }
  }
}


opts_chunk$set(tidy=FALSE, cache=TRUE)
options(digits=4)

library(RMySQL)

con <- dbConnect(MySQL(), 
                 host="localhost", 
                 username="anonymous", 
                 password="Ank7greph-", 
                 dbname="brfss")

if (file.exists("con.R")) source("con.R")


sql <- "SELECT COUNT(*) AS rows FROM brfss;"
rs <- dbGetQuery(con, sql)
cat(rs$rows, "rows")

sql <- "SELECT * FROM brfss LIMIT 1;"
rs <- dbGetQuery(con, sql)
cat(ncol(rs), "columns")

sql <- "SHOW TABLE STATUS IN brfss;"
rs <- dbGetQuery(con, sql)
cat(sum(rs[1, c("Data_length", "Index_length")]) / (1024^3), "GB")


sql <- "SELECT IYEAR AS Year, COUNT(*) AS Respondents 
        FROM brfss 
        WHERE X_STATE = 53 
        GROUP BY IYEAR 
        ORDER BY IYEAR;"
dbGetQuery(con, sql)


sql <- "SELECT X_EDUCAG AS Education, COUNT(*) AS Respondents 
        FROM brfss 
        WHERE IYEAR = 2014 AND X_STATE = 53 
GROUP BY X_EDUCAG 
ORDER BY X_EDUCAG;"
dbGetQuery(con, sql)



sql <- "SELECT X_EDUCAG AS Education, 
        COUNT(USENOW3) AS Smokers 
        FROM brfss 
WHERE IYEAR = 2014 AND X_STATE = 53 AND X_EDUCAG <= 4 
AND (USENOW3 = 1 OR USENOW3 = 2) 
GROUP BY X_EDUCAG 
ORDER BY X_EDUCAG;"
dbGetQuery(con, sql)


sql <- "SELECT X_EDUCAG AS Education, 
        COUNT(*) AS Respondents, 
        COUNT(IF(USENOW3 = 1 OR USENOW3 = 2, 1, NULL)) AS Smokers 
        FROM brfss 
        WHERE IYEAR = 2014 AND X_STATE = 53 AND X_EDUCAG <= 4 
        GROUP BY X_EDUCAG 
        ORDER BY X_EDUCAG;"
rs <- dbGetQuery(con, sql)
rs

library(dplyr)
rs %>% group_by(Education) %>% 
  mutate(Smoking.Prevalence=Smokers/Respondents) -> smokers
smokers


edu.labels <- c("some school", "high school grad", 
                "some college", "college grad")
smokers$Education <- factor(smokers$Education, levels=1:4, labels=edu.labels)
smokers


library(ggplot2)
ggplot(data=smokers, aes(x=Education, y=Smoking.Prevalence, fill=Education)) +
  geom_bar(stat="identity")


sql <- "SELECT IYEAR AS Year, X_EDUCAG AS Education, 
        COUNT(*) AS Respondents, 
        COUNT(IF(USENOW3 = 1 OR USENOW3 = 2, 1, NULL)) AS Smokers
        FROM brfss 
        WHERE (IYEAR = 2011 OR IYEAR = 2012 OR IYEAR = 2013 OR IYEAR = 2014)
              AND X_STATE = 53 
              AND X_EDUCAG <= 4 
        GROUP BY IYEAR, X_EDUCAG 
        ORDER BY IYEAR, X_EDUCAG DESC;"


rs <- dbGetQuery(con, sql)
rs %>% group_by(Year, Education) %>% 
  mutate(Smoking.Prevalence=Smokers/Respondents) -> smokers


smokers$Education <- factor(smokers$Education, levels=1:4, labels=edu.labels)
smokers$Year <- factor(smokers$Year)
ggplot(data=smokers, aes(x=Education, y=Smoking.Prevalence, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")


sql <- "SELECT X_EDUCAG AS Education, 
        COUNT(*) AS Respondents, 
        COUNT(IF(DRNKANY5 = 1, 1, NULL)) AS Drinkers 
        FROM brfss 
        WHERE IYEAR = 2014
              AND X_STATE = 53 
              AND X_EDUCAG <= 4 
        GROUP BY X_EDUCAG 
        ORDER BY X_EDUCAG DESC;"

rs <- dbGetQuery(con, sql)


rs %>% group_by(Education) %>% 
  mutate(Drinking.Prevalence=Drinkers/Respondents) -> drinkers
drinkers$Education <- factor(drinkers$Education, levels=1:4, labels=edu.labels)
drinkers


ggplot(data=drinkers, aes(x=Education, y=Drinking.Prevalence, fill=Education)) +
  geom_bar(stat="identity")


sql <- "SELECT IYEAR AS Year, X_EDUCAG AS Education, 
        COUNT(*) AS Respondents, 
        COUNT(IF(DRNKANY5 = 1, 1, NULL)) AS Drinkers 
FROM brfss 
WHERE (IYEAR = 2011 OR IYEAR = 2012 OR IYEAR = 2013 OR IYEAR = 2014)
AND X_STATE = 53 
AND X_EDUCAG <= 4 
GROUP BY IYEAR, X_EDUCAG 
ORDER BY IYEAR, X_EDUCAG DESC;"

rs <- dbGetQuery(con, sql)
rs %>% group_by(Year, Education) %>% 
  mutate(Drinking.Prevalence=Drinkers/Respondents) -> drinkers


drinkers$Education <- factor(drinkers$Education, levels=1:4, labels=edu.labels)
drinkers$Year <- factor(drinkers$Year)
ggplot(data=drinkers, aes(x=Education, y=Drinking.Prevalence, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")



sql <- "SELECT IYEAR AS Year, X_EDUCAG AS Education, 
        COUNT(*) AS Respondents, 
        COUNT(IF(USENOW3 = 1 OR USENOW3 = 2, 1, NULL)) AS Smokers, 
        COUNT(IF(DRNKANY5 = 1, 1, NULL)) AS Drinkers 
        FROM brfss 
        WHERE (IYEAR = 2011 OR IYEAR = 2012 OR IYEAR = 2013 OR IYEAR = 2014)
              AND X_STATE = 53 
              AND X_EDUCAG <= 4 
        GROUP BY IYEAR, X_EDUCAG 
        ORDER BY IYEAR, X_EDUCAG;"

rs <- dbGetQuery(con, sql)
rs %>% group_by(Year, Education) %>% 
  mutate(Smoking=Smokers/Respondents, 
         Drinking=Drinkers/Respondents) -> consumers
consumers$Education <- factor(consumers$Education, levels=1:4, 
                              labels=edu.labels)
consumers$Year <- factor(consumers$Year)


library(tidyr)
consumers <- consumers %>% 
  select(Year, Education, Smoking, Drinking) %>% 
  gather(key=Factor, value=Prevalence, -Year, -Education)
head(consumers, 8)


ggplot(data=consumers, aes(x=Year, y=Prevalence, group=Factor, color=Factor)) + 
  geom_line() + facet_grid(Factor ~ Education, scales="free_y")


# Get a subset of the dataset for 2011-2014 and state of Washington
sql <- "SELECT * FROM brfss 
        WHERE (IYEAR = 2011 OR IYEAR = 2012 OR IYEAR = 2013 OR IYEAR = 2014) 
AND X_STATE = 53;"

# Use a data.table instead of a data.frame for improved performance
library(data.table)
brfsswa1114 <- as.data.table(dbGetQuery(con, sql))

# You can also save this data.table as a SQL table in the MySQL database
#dbWriteTable(con, name = "brfsswa1114", brfsswa1114, row.names=F)

# Report data table size and dimensions
cat("The data table consumes", object.size(brfsswa1114) / 1024^2, "MB", 
    "with", dim(brfsswa1114)[1], "observations and", 
    dim(brfsswa1114)[2], "variables", "\n")

# Save as a CSV and check on the size
filename <- "brfsswa1114.csv"
if (! file.exists(filename)) write.csv(brfsswa1114, filename, row.names=FALSE)
cat(paste(c("Size of CSV file is", 
            round(file.size(filename) / 1024^2, 1), "MB", "\n")))


# Rename columns to match our SQL results
setnames(brfsswa1114, "IYEAR", "Year")
setnames(brfsswa1114, "X_EDUCAG", "Education")

# Use order() to set sort order like in SQL
brfsswa1114 <- brfsswa1114[order(Year, Education)]

# Use DT[i, j, by=...] syntax to query and aggregate like in SQL
consumers <- brfsswa1114[Education <= 4, list(
  Smoking = sum(USENOW3 == 1 | USENOW3 == 2, na.rm = TRUE)/.N,
  Drinking = sum(DRNKANY5 == 1, na.rm = TRUE)/.N), 
  by = list(Year, Education)]

# Use the same factor() commands as before
consumers$Education <- factor(consumers$Education, levels=1:4, 
                              labels=edu.labels)
consumers$Year <- factor(consumers$Year)


# Use the same gather() command as before
consumers <- consumers %>% 
  gather(key=Factor, value=Prevalence, -Year, -Education)
consumers %>% head(16)


# User the same ggplot() command as before
ggplot(data=consumers, aes(x=Year, y=Prevalence, group=Factor, color=Factor)) + 
  geom_line() + facet_grid(Factor ~ Education, scales="free_y")



# Close connection
dbDisconnect(con)

