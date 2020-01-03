setwd("C://Users//Cherch//DataScience//project")

#install.packages("openxlsx")
require("openxlsx")
require("tidyverse")
require(devtools)
library(dplyr)


protocol<-read.xlsx("../project/BoxOffice - Data Retrieval Protocol.xlsx", sheet = "protocol")
load("../data/BoxOffice_ff.RData")

##########################################################################################
# Function that print plot per variable type
##########################################################################################
doEDA <- function(data, i) {
  
  column_name <- str_trim(protocol$Feature.name[i])
  val.type <- str_trim(protocol$Value.type[i])
  
  cat(sprintf("Data.Type: %s\n", val.type))
  
  summary(data[column_name])
  
  summary(protocol$Value.type)
  
  if (!is.na(val.type) & val.type == "Numeric") {
    val.min <-   as.numeric(str_trim(protocol$Min[i]))
    val.max <- as.numeric(str_trim(protocol$Max[i]))
    
    
    plot(data[column_name])
    
    plot(movies$revenue ~ as.numeric(unlist(data[column_name])), xlab=column_name)
    
    #if differencce between the min and max is bigger than 1000 present log
    if ((val.max - val.min) > 1000) {
      hist(log(data[column_name]+1))
    } else {
      hist(data[column_name])
    }
    
    # if differencce between the min and max is bigger than 1000 present log
    if ((val.max - val.min) > 1000) {
      boxplot(log(data[column_name]+1),main=column_name)
    } else {
      boxplot(data[column_name],main=column_name)
    }
    
    scatter.smooth(data[column_name] ~ movies$movie_id, main=column_name, xlab="movies",ylab=column_name, family="symmetric",
                   lpars =list(col = "red", lwd = 2, lty = 2))
    
  } else if (!is.na(val.type) & val.type == "Categorical") {
    barplot(prop.table(data[column_name]))
    
    table(data[column_name])
    
    plot(movies$revenue ~ as.numeric(unlist(movies[column_name])), xlab=column_name)
    
    ggplot(data)+
      geom_density(aes(log(revenue), group=data[column_name], color=data[column_name]))
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
for (n in 2:5) {#nrow(protocol)){
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


categoricals<-categoricals[-grep('original_language', categoricals)]
categoricals<-categoricals[-grep('runtime_cat', categoricals)]

cmovies$original_language_num<-factor(cmovies$original_language)
cmovies$original_language_num<-as.numeric(levels(cmovies$original_language_num))[cmovies$original_language_num]

cmovies$runtime_cat<-factor(cmovies$runtime_cat)

#install.packages("corrplot")
library(corrplot)

corr<-cor(cmovies[numerics], method = "pearson", use = "complete.obs")

corrplot(corr, method="circle")


corr<-cor(cmovies[categoricals], method = "spearman", use = "complete.obs")

corrplot(corr, method="circle")



both<-c(numerics, categoricals)

corr<-cor(cmovies[both], method = "spearman", use = "complete.obs")

corrplot(corr, method="circle")

corr1<-corr[abs(corr) > 0.5]

dim(corr1)

##########################################################################################
# Missing Values heatmap
##########################################################################################

##########################################################################################
# Outliers: checking distribution with and without outliers
##########################################################################################

##########################################################################################
# Outliers: checking distribution with and without outliers agains revenue
##########################################################################################

##########################################################################################
# Handle outliers
##########################################################################################

##########################################################################################
# Misssing: for each variable checking distribution with and without outliers against all the other varaibles
##########################################################################################

##########################################################################################
# Misssing: Create a table with all the variable that have missing values and explain how missings were created (MCAR and etc.)
##########################################################################################

##########################################################################################
# Misssing: Do imputation for each variable according to the appropriate technic
##########################################################################################

