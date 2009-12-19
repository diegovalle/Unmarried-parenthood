#A quick an dirty program to calculate the unmarried parenthood rate
#in Mexico and compare it with the Hispanic rate in the US.
#Diego Valle-Jones http://www.diegovalle.net
library(ggplot2)
library(Hmisc)

#Change this as appropiate
wd <- "C:/Documents and Settings/Diego/My Documents/docs/personal/Math/births/unmarried"
setwd(wd)

#Cleanup the csv file from the INEGI
cleandata <- function(births, ne = T) {
  if (ne) {
    births <- subset(births, births$Year.of.Birth != "Total" &
                     births$Year.of.Birth != "No especificado" &
                     births$Marital.Status != "Total" &
                     births$Marital.Status != "No especificado" )
  }
  else {
    births <- subset(births, births$Year.of.Birth != "Total" &
                 births$Year.of.Birth != "No especificado" &
                 births$Marital.Status != "Total" 
                 )
  }
  births$Year.of.Birth <- as.numeric(as.numeric(gsub('[[:alpha:]]', '',
                                                   births$Year.of.Birth)))
  births <- subset(births, births$Year.of.Birth >= 1985)  
  births[is.na(births)] <- 0
  births$Tot <- apply(births[,3:ncol(births)],1, function(x) sum(x))
  
  #We only use the data up to 2006 (not 2008) because parents
  #sometimes are 'lazy' about getting their
  #child a birth certificate (e.g Santiago Creel)
  births2006 <- subset(births, births$Year.of.Birth <= 2006)
  births2006
}

#############################################################################
#Chart for percentage of births to unmarried mothers in Mexico
#and for US Hispanics
############################################################################
#Data file with all births *registered* in Mexico in a given year
#data source: Estadísticas Vitales INEGI
births<-read.csv("unmarried.csv.gz")
births2006 <- cleandata(births)
unmarried.df <- ddply(births2006, .(Year.of.Birth), 
                      function(df) sum(df$Tot[c(1,3,4,5,6)])/sum(df$Tot))

#Data for Hispanics in the US from
#http://www.cdc.gov/nchs/data/nvsr/nvsr57/nvsr57_12.pdf
hispanic <- c(29.5,31.6, 32.6, 34, 35.5, 36.7, 38.5, 39.1, 40, 43.1, 40.8, 40.7, 
40.9, 41.6, 42.2, 42.7, 42.5, 43.5, 45,46.4,48,49.9)/100           #51.3
unmarried.df$hispanic <- hispanic
colnames(unmarried.df) <- c("Year.of.Birth", "Mexico", "US.Hispanics")
ggplot(melt(unmarried.df,id="Year.of.Birth"), 
      aes(Year.of.Birth,value, group=variable, color=variable)) +
      geom_line(size=1) +
      scale_y_continuous(formatter="percent") + 
      opts(title="Percentage of Births to Unmarried Mothers in Mexico")
dev.print(png,"Births to unmarried mothers.png", width=900, height=600)

##############################################################################
#Chart of percentage of births by marital status
#############################################################################
births<-read.csv("unmarried.csv.gz")
births2006 <- cleandata(births, ne = F)

percentages.df <- merge(births2006, ddply(births2006, .(Year.of.Birth), 
                function(df) sum(df$Tot)), by="Year.of.Birth")
percentages.df$per <- percentages.df$Tot / percentages.df$V1
percentages.df$plot<-c(2,1,1,3,3,3,2)

percentages.df$Marital.Status <- factor(percentages.df$Marital.Status)
levels(percentages.df$Marital.Status) <- c("Married", "Divorced",
     "Not Specified", "Separated", "Single", "Living Together",  "Widowed" 
      )

ggplot(percentages.df, aes(x = Year.of.Birth, y = per, group = Marital.Status, 
                          color = Marital.Status)) +
  geom_line(size=1) +  scale_y_continuous(formatter="percent")+
  facet_grid(plot ~ .,scales = "free")  +
  opts(strip.background = theme_rect(fill = NA, colour = NA), 
       strip.text.y = theme_text(size = 0)) +
  scale_colour_brewer(name="Marital\nStatus",breaks=c("Married",
     "Living Together", "Single", "Not Specified","Separated","Widowed", 
     "Divorced" ))  +
  xlab("Year of Birth") + ylab("Percent") +
  opts(title="Percentage of Births by Marital Status in Mexico")
ggsave(file="Percentage of Births by Marital Status in Mexico.png", dpi = 100)  


##############################################################################
#chart comparing Mexico with other OCDE countries
#Data refer to 2006 for Mexico; 2005 for Australia and Canada; and, 2006 
#for Iceland, Korea, Japan, New Zealand, Portugal, the United Kingdom and 
#the United States
#http://www.oecd.org/dataoecd/50/62/42294003.xls
###############################################################################
countries<-c("Korea", "Japan", "Greece", "Cyprus", "Mexico", "Switzerland", 
"Poland", "Italy", "Canada", "Malta", "Spain", "Slovak Republic", 
"Lithuania", "Germany", "Luxembourg", "Portugal", "Australia", 
"Ireland", "Czech Republic", "Hungary", "Austria", "United States", 
"Belgium", "Netherlands", "Finland", "Latvia", "United Kingdom", 
"Denmark", "New Zealand", "Slovenia", "Bulgaria", "France", "Norway", 
"Sweden", "Estonia", "Iceland")
unmmx <- 49.0    #(2006 for mexico) there's a mistake and it's not 10%
perunm <- c(1.5, 2.1, 5, 5.6, unmmx, 16.2, 19.5, 20.7, 24.5, 24.9, 28.4, 28.8, 
29.2, 30, 30.7, 31.6, 32.2, 33.2, 34.5, 37.5, 38.2, 38.5, 39, 
39.7, 40.6, 43, 43.7, 46.1, 47.2, 48.1, 50.2, 50.4, 54.5, 54.7, 
58.1, 65.6) /100
unmrt.df <- data.frame(countries,perunm)
unmrt.df$countries <- with(unmrt.df,reorder(countries,perunm))
qplot(data = unmrt.df, x = countries ,y = perunm, geom = "point") + 
  coord_flip() +
  opts(legend.position = "none") +
  xlab("") + ylab("") +
  opts(title="Proportion of births outside marriage") +  
  scale_y_continuous(formatter="percent")
dev.print(png, file="Proportion of births outside marriage (2007).png", 
          width=500, height=680)