setwd("C://Users//Cherch//DataScience//project")

#install.packages("openxlsx")
require("openxlsx")
require("tidyverse")
require(devtools)
library(dplyr)


protocol<-read.xlsx("../project/BoxOffice - Data Retrieval Protocol.xlsx", sheet = "protocol")

head(protocol)

rownames(protocol) <- str_trim(protocol$Feature.name)

load("../data/BoxOffice_ff.RData")

##########################################################################################
# Function that print plot per variable type
##########################################################################################
doEDA <- function(data, column_name) {
  
  val.type <- str_trim(protocol[column_name, 'Value.type'])
  data.type <- str_trim(protocol[column_name, 'Data.type'])
  
  
  cat(sprintf("column name: %s\n", column_name))
  cat(sprintf("Data.Type: %s\n", val.type))
  
  summary(data[[column_name]])
  
  
  if (!is.na(val.type) & val.type == "Numeric") {
    val.min <-   as.numeric(str_trim(protocol$Min[i]))
    val.max <- as.numeric(str_trim(protocol$Max[i]))
    
    

    plot(data[[column_name]])
    
    plot(data[['revenue']] ~ data[[column_name]], xlab=column_name)
    
    #if more than 35 unique numbers
    if (protocol[column_name,"Unique.count"] > 35) {
      #if differencce between the min and max is bigger than 1000 present log
      if ((val.max - val.min) > 1000) {
        hist(log(data[[column_name]]+1))
      } else {
        hist(data[[column_name]])
      }
    } else {
      barplot(table(data[[column_name]]))
    }
    
    
    # if differencce between the min and max is bigger than 1000 present log
    delta<-(val.max - val.min)
    if (!is.na(delta) & delta > 1000) {
      boxplot(log(data[column_name]+1),main=column_name)
    } else {
      boxplot(data[column_name],main=column_name)
    }
    
    scatter.smooth(as.numeric(unlist(data[[column_name]])) ~ as.numeric(unlist(data['movie_id'])), main=column_name, xlab="movies",ylab=column_name, family="symmetric",
                   lpars =list(col = "red", lwd = 2, lty = 2))
    
    scatter.smooth(data[[column_name]] ~ data[['movie_id']], main=column_name, xlab="movies",ylab=column_name, family="symmetric",
                   lpars =list(col = "red", lwd = 2, lty = 2))
    
  } else if (!is.na(val.type) & val.type == "Categorical") {
    
    if (!is.na(data.type) & data.type != "Text") {
      table(data[[column_name]])
      
      barplot(table(data[[column_name]]))
      #plot(data[['revenue']] ~ data[[column_name]], xlab=column_name)
    }
    
    ggplot(data)+
      geom_density(aes(log(data[['revenue']]), group=data[[column_name]], color=data[[column_name]]))
  }
}

##########################################################################################
# clean the movies - replace all the NULL values with Na
##########################################################################################
cmovies<-movies
for (n in 2:nrow(protocol)){
  val<-str_trim(protocol$Null[n])

  feature<-str_trim(protocol$Feature.name[n])
  
  # set the NULL value to be Na
  if (!is.na(val) & (val == "0" || val == "1")) {
    cmovies[feature]<- na_if(cmovies[feature], as.numeric(val))
  }
}

##########################################################################################
# print the summary and graphs per variable on cmovies 
##########################################################################################
# Run in loop over the parameters and plot graph based on the variable type



#remove depart_Lighting_female as it has all the values = 0
cmovies<-cmovies[-grep('depart_Lighting_female', names(cmovies))]
protocol<-protocol[-grep('depart_Lighting_female', rownames(protocol)), ]

#remove movie_id from protocol
cmovies<-cmovies[-grep('sw_collection', names(cmovies))]
protocol<-protocol[-grep('sw_collection', rownames(protocol)), ]

#remove movie_id from protocol
protocol<-protocol[-grep('movie_id', rownames(protocol)), ]

for (n in rownames(protocol)){
  doEDA(cmovies, n)
}

##########################################################################################
# Correlation Graph 
##########################################################################################


# list all the numeric variables
numerics<-str_trim(protocol$Feature.name[protocol$Value.type == "Numeric"])


# list all the cat variables 
categoricals<-str_trim(protocol$Feature.name[protocol$Value.type == "Categorical"])

#remove movie_id
categoricals<-categoricals[-grep('movie_id', categoricals)]

#remove original_languages
categoricals<-categoricals[-grep('original_language', categoricals)]

#remove runtime_cat
categoricals<-categoricals[-grep('runtime_cat', categoricals)]

#cmovies$original_language_num<-factor(cmovies$original_language)
#cmovies$original_language_num<-as.numeric(levels(cmovies$original_language_num))[cmovies$original_language_num]
#cmovies$runtime_cat<-factor(cmovies$runtime_cat)

#install.packages("corrplot")
library(corrplot)

corr<-cor(cmovies[numerics], method = "pearson", use = "complete.obs")

corrplot(corr, method="circle")


corr<-cor(cmovies[categoricals], method = "spearman", use = "complete.obs")

corrplot(corr, method="circle")



both<-c(numerics, categoricals)

corr<-cor(cmovies[both], method = "spearman", use = "complete.obs")

corrplot(corr, method="circle")



##########################################################################################
# Missing Values heatmap
##########################################################################################
missingMatrix <- function(data) {
  vn <- names(data)
  missdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    mv <- ifelse(is.na(data[[v]]),1,0)
    missdata[v] <- mv
  }
  missdata$row1 <- NULL
  return(missdata)
}

miss<-missingMatrix(cmovies)

options(repr.plot.width = 4, repr.plot.height = 4)
library(naniar)
vis_miss(cmovies[numerics])

vis_miss(cmovies[categoricals])

##########################################################################################
# Outliers: checking distribution with and without outliers
# Outliers: checking distribution with and without outliers agains revenue
##########################################################################################

# list all the numeric variables
numerics<-str_trim(protocol$Feature.name[protocol$Value.type == "Numeric"])

outlierMatrix <- function(data,threshold=1.5) {
  vn <- names(data)
  outdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    if(is.numeric(data[[v]])) {
      med<- median(data[[v]], na.rm = T)
      outlow <- quantile(data[[v]],probs = 0.25,na.rm = T) 
      outhigh <- quantile(data[[v]],probs = 0.75, na.rm = T)
      irq_level <- (outhigh - outlow) * threshold
      outlow <- outlow - irq_level
      outhigh <- outhigh +  irq_level
      mv <- ifelse(data[[v]] < outlow | data[[v]] > outhigh, 1, 0)
      outdata[v] <- mv
    } else {
      mv <- rep(0,nrow(data))
    }
  }
  outdata$row1 <- NULL
  return(outdata)
}

out<-outlierMatrix(cmovies,threshold = 2.0)

#cmovies with cleaned outliers
ocmovies<-cmovies

options(repr.plot.width = 8, repr.plot.height = 8)
par(mfrow=c(2,2))
for(v in numerics) {
  #look on variable with some variability
  if (protocol[v,"Unique.count"] > 35) {
    print(v)
  
    ##############################
    #Handle outliers
    ##############################
    
    #drop outlier value (replace by NA)
    ocmovies[which(out[v] == 1), v]<-NA

    hist(cmovies[[v]], freq = FALSE, xlab = v,  main = "With Outliers")
    lines(density(cmovies[[v]], na.rm = TRUE))
    
    hist(ocmovies[[v]], freq = FALSE,xlab = v,  main = "Without Outliers")
    lines(density(ocmovies[[v]], na.rm = TRUE))
    
    plot(y = cmovies$revenue, x = cmovies[[v]], pch = 16, cex = 1.3, col = "blue", main = "Distribution against Revenure(with Outliers)", xlab = v, ylab = "revenue")
    abline(lm(cmovies$revenue ~ cmovies[[v]]))

    plot(y = ocmovies$revenue, x = ocmovies[[v]], pch = 16, cex = 1.3, col = "blue", main = "Distribution against Revenure(with Outliers)", xlab = v, ylab = "revenue")
    abline(lm(ocmovies$revenue ~ ocmovies[[v]]))
    
  }
}
par(mfrow=c(1,1))

##########################################################################################
# Misssing: for each variable checking distribution with and without missing against all the other varaibles
##########################################################################################

miss<-missingMatrix(ocmovies)

for(v in numerics) {
  ggplot(ocmovies) +
    geom_density(aes(log(ocmovies$revenue), group=miss[v], color=miss[v] + 1))
}

ggplot(ocmovies) +
  geom_density(aes(log(ocmovies$revenue)))

##########################################################################################
# Misssing: Create a table with all the variable that have missing values and explain how missings were created (MCAR and etc.)
##########################################################################################

##########################################################################################
# Misssing: Do imputation for each variable according to the appropriate technic
##########################################################################################

