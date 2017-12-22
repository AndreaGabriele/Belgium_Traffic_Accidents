<<<<<<< HEAD

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(readxl)
library(httr)

setwd("C:/Users/gabriele.a/Documents/R/")
 
## Download data from StatBel and append data in a Dataframe
# url with MetaData on Column Names
url_list <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2016_tcm326-283523.xlsx"
url_list[2] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2015_tcm326-283522.xlsx"
url_list[3] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2014_tcm326-283521.xlsx"
url_list[4] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2013_tcm326-283520.xlsx"
url_list[5] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2012_tcm326-283519.xlsx"
url_list[6] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2011_tcm326-283518.xlsx"
url_list[7] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2010_tcm326-283517.xlsx"


for (url in url_list){
  tf <- tempfile(fileext = ".xlsx")
  
  GET(url, write_disk(tf))
  
  if (!exists("df")){
    df <- read_xlsx(path = tf,sheet = NULL, range = NULL, col_names = TRUE,
                    col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf )
  }
  
  if (exists("df")){
    
    df_temp <- read_xlsx(path = tf,sheet = NULL, range = NULL, col_names = TRUE,
                         col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf )
    df <- rbind(df, df_temp)
    rm (df_temp)
    file.remove(tf)  
    }
}


# use read metadata



file_list <- list.files(pattern = "*.txt")

print(file_list)

rm (acc_temp, acc)

for (file_ in file_list) {
  
  if (!exists("acc")){
    acc <- read_delim(file = file_ ,delim = "|",col_names = TRUE) 
  }
  
  if (exists("acc")){
  
  acc_temp <- read_delim(file = file_ ,delim = "|",col_names = TRUE)  
  acc <- rbind(acc, acc_temp)
  rm (acc_temp)
  
  }

}

# acc_2015 <- read_delim("TF_ACCIDENTS_VICTIMS_2015.txt",delim = "|",col_names = TRUE)

acc_reduce <- acc %>% select(-ends_with("NL")) 

# summary of numeber of victims by year and comune
View(head(acc_reduce))

acc_reduce_summary <- acc_reduce %>% 
  separate(DT_DAY,into = c("year","month","day")) %>% 
  mutate(year, class("numeric")) %>% 
  group_by(year, TX_AGE_CLS_DESCR_FR)  %>%  #, TX_MUNTY_DESCR_FR
  arrange(desc(TX_AGE_CLS_DESCR_FR)) 

barplot(table(acc_reduce_summary$TX_AGE_CLS_DESCR_FR), horiz = F)
barplot(table(acc_reduce_summary$TX_LIGHT_COND_DESCR_FR), horiz = F)
barplot(table(acc_reduce_summary$TX_VCT_TYPE_DESCR_FR), horiz = F)
barplot(table(acc_reduce_summary$TX_SEX_DESCR_FR), horiz = F,las=2)
barplot(table(acc_reduce_summary$TX_MUNTY_DESCR_FR), horiz = T,las=2)
barplot(table(acc_reduce_summary$TX_PROV_DESCR_FR), horiz = T,cex.names = 0.8, las=2)

which(acc_reduce_summary$TX_SEX_DESCR_FR=="Femmes")


mydata <- data.frame(Barplot1=rbinom(5,16,0.6), Barplot2=rbinom(5,16,0.25),
                     Barplot3=rbinom(5,5,0.25), Barplot4=rbinom(5,16,0.7))
barplot(as.matrix(mydata), main="Interesting", ylab="Total", beside=TRUE, 
        col=terrain.colors(5))

legend(13, 12, c("Label1","Label2","Label3","Label4","Label5"), cex=0.6, 
       fill=terrain.colors(5))






Comune <- "Malines"

acc_reduce_summary <- acc_reduce %>% 
  filter(TX_MUNTY_DESCR_FR == c(Comune)) %>% 
  separate(DT_DAY,into = c("year","month","day")) %>% 
  mutate(year, class("numeric")) %>% 
  group_by(year, TX_AGE_CLS_DESCR_FR)  %>%  #, TX_MUNTY_DESCR_FR
  summarize( "count_MS_DEAD" = sum(MS_DEAD), 
               "count_MS_DEAD_30_DAYS" = sum(MS_DEAD_30_DAYS),
               "count_MS_MORY_INJ" = sum(MS_MORY_INJ),
               "count_MS_SERLY_INJ" = sum(MS_SERLY_INJ))  


summarise(acc_reduce_summary)

acc_reduce_summary$year <- as.numeric(as.character(acc_reduce_summary$year))

summarise(acc_reduce_summary)

ggplot(data = acc_reduce_summary) + geom_line(aes( acc_reduce_summary$year, acc_reduce_summary$count_MS_DEAD), colour="green")

# we can get frequency tables across two groups
View(counts) <- table(acc_reduce_summary$`TX_AGE_CLS_DESCR_FR`,acc_reduce_summary$count_MS_SERLY_INJ)

# you can plot these results either stacked or side-by-side with barplot()



# you can turn these counts into proportions
proportions <- prop.table(counts)

# now plot these results in a similar manner as above
barplot()





#ggplot(data = acc_reduce_summary, aes(x=TX_AGE_CLS_DESCR_FR)) + geom_histogram()

ggplot(data = mpg, aes(x = hwy)) + geom_histogram()

class(acc_reduce_summary$year)

class(acc_reduce_summary)

plot(Year_,Count_Inj, main = "Dead count in St.Josse",sub = "Road Accidents") 
par(new=TRUE)
lines(Year_,Count_dead, col="green")


hist(table(filter(acc_reduce_summary, TX_MUNTY_DESCR_FR == "Saint-Josse-ten-Noode")), breaks = 2 )


=======

library(googleVis)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

setwd("C:/Users/gabriele.a/Documents/R/")

file_list <- list.files(pattern = "*.txt")

print(file_list)

rm (acc_temp, acc)

for (file_ in file_list) {
  
  if (!exists("acc")){
    acc <- read_delim(file = file_ ,delim = "|",col_names = TRUE) 
  }
  
  if (exists("acc")){
  
  acc_temp <- read_delim(file = file_ ,delim = "|",col_names = TRUE)  
  acc <- rbind(acc, acc_temp)
  rm (acc_temp)
  
  }

}

# acc_2015 <- read_delim("TF_ACCIDENTS_VICTIMS_2015.txt",delim = "|",col_names = TRUE)

acc_reduce <- acc %>% select(-ends_with("NL")) 

# summary of numeber of victims by year and comune
View(head(acc_reduce))

acc_reduce_summary <- acc_reduce %>% 
  separate(DT_DAY,into = c("year","month","day")) %>% 
  mutate(year, class("numeric")) %>% 
  group_by(year, TX_AGE_CLS_DESCR_FR)  %>%  #, TX_MUNTY_DESCR_FR
  arrange(desc(TX_AGE_CLS_DESCR_FR)) 

barplot(table(acc_reduce_summary$TX_AGE_CLS_DESCR_FR), horiz = F)
barplot(table(acc_reduce_summary$TX_LIGHT_COND_DESCR_FR), horiz = F)
barplot(table(acc_reduce_summary$TX_VCT_TYPE_DESCR_FR), horiz = F)
barplot(table(acc_reduce_summary$TX_SEX_DESCR_FR), horiz = F,las=2)
barplot(table(acc_reduce_summary$TX_MUNTY_DESCR_FR), horiz = T,las=2)
barplot(table(acc_reduce_summary$TX_PROV_DESCR_FR), horiz = T,cex.names = 0.8, las=2)

which(acc_reduce_summary$TX_SEX_DESCR_FR=="Femmes")


mydata <- data.frame(Barplot1=rbinom(5,16,0.6), Barplot2=rbinom(5,16,0.25),
                     Barplot3=rbinom(5,5,0.25), Barplot4=rbinom(5,16,0.7))
barplot(as.matrix(mydata), main="Interesting", ylab="Total", beside=TRUE, 
        col=terrain.colors(5))

legend(13, 12, c("Label1","Label2","Label3","Label4","Label5"), cex=0.6, 
       fill=terrain.colors(5))






Comune <- "Malines"

acc_reduce_summary <- acc_reduce %>% 
  filter(TX_MUNTY_DESCR_FR == c(Comune)) %>% 
  separate(DT_DAY,into = c("year","month","day")) %>% 
  mutate(year, class("numeric")) %>% 
  group_by(year, TX_AGE_CLS_DESCR_FR)  %>%  #, TX_MUNTY_DESCR_FR
  summarize( "count_MS_DEAD" = sum(MS_DEAD), 
               "count_MS_DEAD_30_DAYS" = sum(MS_DEAD_30_DAYS),
               "count_MS_MORY_INJ" = sum(MS_MORY_INJ),
               "count_MS_SERLY_INJ" = sum(MS_SERLY_INJ))  


summarise(acc_reduce_summary)

acc_reduce_summary$year <- as.numeric(as.character(acc_reduce_summary$year))

summarise(acc_reduce_summary)

ggplot(data = acc_reduce_summary) + geom_line(aes( acc_reduce_summary$year, acc_reduce_summary$count_MS_DEAD), colour="green")

# we can get frequency tables across two groups
View(counts) <- table(acc_reduce_summary$`TX_AGE_CLS_DESCR_FR`,acc_reduce_summary$count_MS_SERLY_INJ)

# you can plot these results either stacked or side-by-side with barplot()



# you can turn these counts into proportions
proportions <- prop.table(counts)

# now plot these results in a similar manner as above
barplot()





#ggplot(data = acc_reduce_summary, aes(x=TX_AGE_CLS_DESCR_FR)) + geom_histogram()

ggplot(data = mpg, aes(x = hwy)) + geom_histogram()

class(acc_reduce_summary$year)

class(acc_reduce_summary)

plot(Year_,Count_Inj, main = "Dead count in St.Josse",sub = "Road Accidents") 
par(new=TRUE)
lines(Year_,Count_dead, col="green")


hist(table(filter(acc_reduce_summary, TX_MUNTY_DESCR_FR == "Saint-Josse-ten-Noode")), breaks = 2 )


>>>>>>> fc94cc93a90ebc7f3203980f2c3759931c1e07b4
View(head(acc_reduce))