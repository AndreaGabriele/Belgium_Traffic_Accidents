

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(readxl)
library(httr)
library(lubridate)

setwd("C:/Users/gabriele.a/Documents/R/")
 
## Download data from StatBel and append data in a Dataframe
# url with MetaData on Column Names
url_list <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2016_tcm326-283523.xlsx"
url_list[2] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2015_tcm326-283522.xlsx"
url_list[3] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2014_tcm326-283521.xlsx"
url_list[4] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2013_tcm326-283460.xlsx"
url_list[5] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2012_tcm326-283547.xlsx"
url_list[6] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2011_tcm326-283546.xlsx"
url_list[7] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2010_tcm326-283545.xlsx"
url_list[8] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2009_tcm326-283544.xlsx"
url_list[9] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2008_tcm326-283543.xlsx"
url_list[10] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2007_tcm326-283542.xlsx"
url_list[11] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2006_tcm326-283541.xlsx"
url_list[12] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2005_tcm326-283540.xlsx"
url_list[13] <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_2002_tcm326-283539.xlsx"



rm (df)
    
for (url in url_list){
  tf <- tempfile(fileext = ".xlsx")
  print(url)
  GET(url, write_disk(tf))
  
  if (!exists("df", mode = "list")){
    df <- read_excel(path = tf,sheet = 1, range = NULL, col_names = TRUE,
                    col_types = NULL, trim_ws = TRUE, skip = 0, n_max = Inf )
  }
  
  else if (exists("df", mode = "list")){
    
    df_temp <- read_excel(path = tf,sheet = NULL, range = NULL, col_names = TRUE,
                         col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf )
    df <- rbind(df, df_temp)
    rm (df_temp)
    }
  
  file.remove(tf)  

}



# use read metadata for columheaders
url <- "http://statbel.fgov.be/fr/binaries/TF_ACCIDENTS_VICTIMS_META_tcm326-283604.xlsx"
tf <- tempfile(fileext = ".xlsx")
print(url)
GET(url, write_disk(tf)) 
df_headers <- read_excel(path = tf,sheet = 1, range = NULL, col_names = TRUE, col_types = NULL, trim_ws = TRUE, skip = 0, n_max = Inf )
file.remove(tf)  

# Remove the Dutch Colums
df_reduce <- df %>% select(-ends_with("NL")) 
df_headers_reduce <- df_headers %>% filter(!grepl("_NL",NAME)) 
View(df_headers_reduce)
#Rename the Colums using metadata
#names(df_reduce) <- df_headers_reduce$LABEL
#df_reduce_ <- df_reduce %>% select(-starts_with("Business"))

rm(df_headers, df_headers_reduce, tf, url,url_list)
# View(head(df_reduce))

class(df_reduce)


### Comparison Accidents by Gender
df_reduce_summary <- df_reduce %>% 
  group_by(year(DT_DAY), CD_SEX) %>% 
  summarise(sum(MS_VCT),
            sum(MS_SLY_INJ),
            sum(MS_SERLY_INJ),
            sum(MS_DEAD_30_DAYS),
            sum(MS_DEAD))


ggplot(df_reduce_summary, aes(x=df_reduce_summary$`year(DT_DAY)`,y=`sum(MS_DEAD)`,fill=CD_SEX)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  scale_x_continuous(breaks = df_reduce_summary$`year(DT_DAY)`) +
  scale_y_continuous(breaks = seq(0, 900, 50)) +
  labs(x = "Year",y = "Number Dead", title = "Car Accident Dead by Gender in Belgium") +
  theme(axis.text.x=element_text(size = 9, colour = "black", angle = 90, hjust = 0, vjust = 0.5),
        axis.text.y=element_text(size = 9, colour = "black", angle = 0, hjust = 0, vjust = 0.5)) +
scale_fill_manual("Gender",values=c("orange","red","grey"))


### Difference between Male / Female Victimes
df_reduce_summary <- df_reduce %>% 
  filter(TX_VCT_TYPE_DESCR_FR!= "Autre victime" & TX_VCT_TYPE_DESCR_FR!= "Non disponible") %>% 
  group_by(CD_SEX, TX_VCT_TYPE_DESCR_FR) %>% 
  summarise(sum(MS_VCT),
            sum(MS_SLY_INJ),
            sum(MS_SERLY_INJ),
            sum(MS_DEAD_30_DAYS),
            sum(MS_DEAD))

ggplot(df_reduce_summary, aes(x=TX_VCT_TYPE_DESCR_FR,y=`sum(MS_DEAD)`,fill=CD_SEX)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  scale_y_continuous(breaks = seq(0, 7500, 500)) +
  labs(x = "Victime type",y = "Number Dead", title = "Role in the Accident by Gender in Belgium") +
  theme(axis.text.x=element_text(size = 9, colour = "black", angle = 0, hjust = 0.5, vjust = 0),
        axis.text.y=element_text(size = 9, colour = "black", angle = 0, hjust = 0, vjust = 0)) +
  scale_fill_manual("Gender",values=c("orange","red","grey"))

### Conclusions More man dye of car accident than women do if the man is the driver 
### one can infer that if the drive has a pannager the acidents are less


### Age Distribution of accidents
df_reduce_summary <- df_reduce %>% 
  filter(TX_VCT_TYPE_DESCR_FR == "Conducteur ou piéton" & MS_DEAD != 0) %>% 
  group_by(TX_DAY_OF_WEEK_DESCR_FR,CD_SEX,CD_AGE_CLS) %>% 
  summarise(sum(MS_DEAD))

ggplot(df_reduce_summary, aes(x=CD_AGE_CLS,y=df_reduce_summary$`sum(MS_DEAD)`,fill=CD_SEX)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  facet_wrap(~df_reduce_summary$TX_DAY_OF_WEEK_DESCR_FR, scales = 'free_x') +
  scale_y_continuous(breaks = seq(0, 230, 10)) +
  scale_x_continuous(breaks = seq(18, 90, 2)) +
  labs(x = "Victime Age",y = "Number Dead", title = "Age distibution by Day of Week")
### Conclsions only by the age of 55 the % accidents of man becomes similar to the one of Females

### Age Distrubution 
tmp <- df_reduce %>% 
  filter((TX_VCT_TYPE_DESCR_FR == "Conducteur ou piéton" | 
         TX_VCT_TYPE_DESCR_FR == "Passager") & 
         MS_DEAD != 0 &
         CD_SEX != "NA")

ggplot(tmp, aes(x = tmp$CD_AGE_CLS, color = tmp$CD_SEX)) +
  geom_histogram(aes(y=..count..), bins = 80,  position="dodge",  na.rm = FALSE) +
  facet_wrap(~tmp$TX_VCT_TYPE_DESCR_FR, scales = 'free_x') +
  geom_density(stat = "count", position="identity", alpha=1, size = 1.1) +
  scale_y_continuous(breaks = seq(0, 230, 10)) +
  scale_x_continuous(limits = c(18,90), breaks = seq(18, 90, 5)) +
  labs(x = "Victime Age",y = "Number Dead", title = "Age distibution") + 
  scale_colour_manual("Gender",values=c("orange","red"))

##Latitude longitute per comune PostCode - Comune - Long - Latitude
url <- ("https://raw.githubusercontent.com/jief/zipcode-belgium/master/zipcode-belgium.csv")
lat_long <-read.csv(url, header=F)
names(lat_long) <- c("PostCode","Comune","Longitude", "Latitude")
View(lat_long)

##NIS code into Post Code per comune 
url <- ("http://ckan-001.corve.openminds.be/storage/f/2013-06-06T12%3A17%3A06.134Z/gemeentecodes.csv")
NIS_to_Post_code <- read.delim(url, header=T, sep = ";")
View(NIS_to_Post_code)

# select from data base only the comunes we have postcode for
df_reduce_ <- df_reduce %>% 
  subset(df_reduce$CD_MUNTY_REFNIS != NIS_to_Post_code$NIS.code) 

df_reduce_$CD_MUNTY_REFNIS <- as.integer(df_reduce_$CD_MUNTY_REFNIS)


#remove duplicates in the new data tables
NIS_to_Post_code_ <- NIS_to_Post_code[,1:2]
NIS_to_Post_code_ <- NIS_to_Post_code_[which(!duplicated(NIS_to_Post_code_[ ,1])==TRUE),1:2]
lat_long_ <- lat_long[which(!duplicated(lat_long[,1])==T),c(1,3,4)]

# Add the ISO nomenclature per Province
library(ISOcodes)
data("ISO_3166_2")
ISO_3166_2_BE <- ISO_3166_2[grep("BE-", ISO_3166_2$Code), ]
ISO_3166_2_BE <- ISO_3166_2_BE[order(ISO_3166_2_BE$Name),]

ISO_Region <- c("Antwerp",	"Province d'Anvers")
ISO_Region <- rbind(ISO_Region,c("Walloon Brabant",	"Province de Brabant wallon"))
ISO_Region <- rbind(ISO_Region,c("Hainaut",	"Province de Hainaut"))
ISO_Region <- rbind(ISO_Region,c("Liege",  "Province de Liège"))
ISO_Region <- rbind(ISO_Region,c("Limburg",  "Province de Limbourg"))
ISO_Region <- rbind(ISO_Region,c("Luxembourg",  "Province de Luxembourg"))
ISO_Region <- rbind(ISO_Region,c("Province of Namur",  "Province de Namur"))
ISO_Region <- rbind(ISO_Region,c("East Flanders",  "Province de Flandre orientale"))
ISO_Region <- rbind(ISO_Region,c("Flemish Brabant",  "Province de Brabant flamand"))
ISO_Region <- rbind(ISO_Region,c("West Flanders",  "Province de Flandre occidentale"))
ISO_Region<- as.data.frame(ISO_Region)
names(ISO_Region) <- c("ISO_Name", "Name")



#Join Tables together
rm(df_reduce_LatLong)
df_reduce_LatLong <- df_reduce_ %>% 
                    left_join(NIS_to_Post_code_, by = c("CD_MUNTY_REFNIS" = "NIS.code")) %>% 
                    left_join(lat_long_, by = c("Postcode" = "PostCode")) %>% 
                    left_join(ISO_Region, by = c("TX_PROV_DESCR_FR" = "Name"))

names(df_reduce_LatLong)[names(df_reduce_LatLong) == "Latitude"] <- "lat"
names(df_reduce_LatLong)[names(df_reduce_LatLong) == "Longitude"] <- "long"


df_reduce_LatLong_ <- df_reduce_LatLong %>% 
  group_by(TX_ADM_DSTR_DESCR_FR)  %>% 
  summarise(count_dead = sum(MS_DEAD), 
            lat = round(mean(lat),1), 
            long = round(mean(long),1)
            ) %>% 
  ungroup()

df_reduce_LatLong_$latlong<-paste(df_reduce_LatLong_$lat,df_reduce_LatLong_$long, sep=":")


####  df_reduce_LatLong has now latitude and longitudes per accidents
# df_reduce_LatLong <- df_reduce_LatLong_[!is.na(df_reduce_LatLong_$ISO_Name),]
df_reduce_LatLong_ <- df_reduce_LatLong_[!is.na(df_reduce_LatLong_$long),]


library(googleVis)

Map_ <- gvisGeoChart(df_reduce_LatLong_, "latlong", 
                      colorvar='count_dead', hovervar='TX_ADM_DSTR_DESCR_FR',
                      options=list(region=      "BE", 
                                   dataMode=    "Marker",
                                   #displayMode= "provinces",
                                   resolution="provinces"
                                   )
                   )

Table_ <- gvisTable(df_reduce_LatLong_[,1:2], 
               options=list(height=300))

GT <- gvisMerge(Map_,Table_, horizontal=TRUE) 

plot(GT)


# and in Brussels which comunes are more affected by car accidents? 
# use ggmap

library(ggmap)
# load the data
tartu_housing <- read.csv("data/tartu_housing_xy_wgs84_a.csv", sep = ";")

# Download the base map
tartu_map_g_str <- get_map(location = "tartu", zoom = 13)
# Draw the heat map
ggmap(tartu_map_g_str, extent = "device") + geom_density2d(data = tartu_housing, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = tartu_housing, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

qmap(location = 'Bruxelles', zoom = 10, maptype = 'hybrid')



plot(GeoStates)

### Comparison Accidents by Geography
## plotting Deads by Region
df_reduce_summary <- df_reduce %>% 
  group_by(year(DT_DAY),TX_RGN_DESCR_FR ) %>% 
  summarise(sum(MS_VCT),
            sum(MS_SLY_INJ),
            sum(MS_SERLY_INJ),
            sum(MS_DEAD_30_DAYS),
            sum(MS_DEAD))
df_reduce_summary <- arrange(df_reduce_summary,desc(`sum(MS_DEAD)`))

ggplot(df_reduce_summary, aes(x=TX_RGN_DESCR_FR,y=`sum(MS_DEAD)`,fill=TX_RGN_DESCR_FR)) +
  facet_grid(~df_reduce_summary$`year(DT_DAY)`, scales = 'free_x') +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## plotting Deads by Province
df_reduce_summary <- df_reduce %>% 
  group_by(year(DT_DAY),TX_PROV_DESCR_FR ) %>% 
  summarise(sum(MS_VCT),
            sum(MS_SLY_INJ),
            sum(MS_SERLY_INJ),
            sum(MS_DEAD_30_DAYS),
            sum(MS_DEAD))
df_reduce_summary <- arrange(df_reduce_summary,desc(`sum(MS_DEAD)`))

ggplot(df_reduce_summary, aes(x=df_reduce_summary$`year(DT_DAY)`,y=`sum(MS_DEAD)`,group = df_reduce_summary$TX_PROV_DESCR_FR)) +
  geom_line(aes(colour = TX_PROV_DESCR_FR, position = "stack")) +
  geom_point(aes(colour = TX_PROV_DESCR_FR, position = "stack"))  + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(df_reduce_summary, aes(x=df_reduce_summary$`year(DT_DAY)`,y=`sum(MS_SERLY_INJ)`,group = df_reduce_summary$TX_PROV_DESCR_FR)) +
  geom_line(aes(colour = TX_PROV_DESCR_FR, position = "stack")) +
  geom_point(aes(colour = TX_PROV_DESCR_FR, position = "stack"))






barplot(table(df_reduce$CD_AGE_CLS), horiz = F)
barplot(table(acc_reduce_summary$TX_LIGHT_COND_DESCR_FR), horiz = F)
barplot(table(acc_reduce_summary$TX_VCT_TYPE_DESCR_FR), horiz = F)
barplot(table(acc_reduce_summary$TX_SEX_DESCR_FR), horiz = F,las=2)
barplot(table(acc_reduce_summary$TX_MUNTY_DESCR_FR), horiz = T,las=2)
barplot(table(acc_reduce_summary$TX_PROV_DESCR_FR), horiz = T,cex.names = 0.8, las=2)

which(df_reduce_summary$sex=="Femmes")

head(df_reduce)













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