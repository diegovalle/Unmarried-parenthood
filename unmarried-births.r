#A quick an dirty program to calculate the unmarried parenthood rate
#in Mexico and compare it with the Hispanic rate in the US.
#Diego Valle-Jones http://www.diegovalle.net
library(ggplot2)
library(Hmisc)

#Change this
setwd("C:/Documents and Settings/Diego/My Documents/docs/personal/Math/births/unmarried")


#Data file with all births *registered* in Mexico in a given year
#data source: Estadísticas Vitales INEGI
births<-read.csv("unmarried.csv.gz")
births <- subset(births, births$Year.of.Birth != "Total" &
                 births$Year.of.Birth != "No especificado" &
                 births$Marital.Status != "Total" &
                 births$Marital.Status != "No especificado" )
births$Year.of.Birth <- as.numeric(as.numeric(gsub('[[:alpha:]]', '',births$Year.of.Birth)))
births <- subset(births, births$Year.of.Birth >= 1985)  

births[is.na(births)] <- 0
births$Tot <- apply(births[,3:ncol(births)],1, function(x) sum(x))

#We only use the data up to 2006 (not 2008) because parents
#sometimes are lazy about getting their
#child a birth certificate
#http://www.google.com.mx/search?hl=en&client=firefox-a&rls=org.mozilla%3Aen-US%3Aofficial&q=santiago+creel+birth+certificate&btnG=Search&aq=f&oq=
births2006 <- subset(births, births$Year.of.Birth <= 2006)

unmarried.df <- ddply(births2006, .(Year.of.Birth), function(df) sum(df$Tot[c(1,3,4,5,6)])/sum(df$Tot))

#Data for Hispanics in the US from
#http://www.cdc.gov/nchs/data/nvsr/nvsr57/nvsr57_12.pdf
hispanic <- c(29.5,31.6, 32.6, 34, 35.5, 36.7, 38.5, 39.1, 40, 43.1, 40.8, 40.7, 
40.9, 41.6, 42.2, 42.7, 42.5, 43.5, 45,46.4,48,49.9)/100           #,49.9,51.3
unmarried.df$hispanic <- hispanic
colnames(unmarried.df) <- c("Year.of.Birth", "Mexico", "US.Hispanics")
ggplot(melt(unmarried.df,id="Year.of.Birth"), 
      aes(Year.of.Birth,value, group=variable, color=variable)) +
      geom_line() +
      scale_y_continuous(formatter="percent") +
      theme_bw()
dev.print(png,"Births to unmarried mothers.png", width=900, height=600)

