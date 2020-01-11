#home 
setwd("C://Users//Cherch//DataScience//project")

#work 
#setwd("C://bb//DataScience//project")



#install.packages("openxlsx")
#install.packages("caret")

require("caret")
require("openxlsx")
require("tidyverse")
require(devtools)
library(dplyr)
library(car)



protocol<-read.xlsx("../project/BoxOffice - Data Retrieval Protocol.xlsx", sheet = "protocol")

head(protocol)

rownames(protocol) <- str_trim(protocol$Feature.name)

load("../data/BoxOffice_ff.RData")

##########################################################################################
# Function that print plot per variable type
##########################################################################################
doEDA <- function(data, column_name) {
  
  options(repr.plot.width = 16, repr.plot.height = 16)
  par(mfrow=c(2,2))
  
  
  val.type <- str_trim(protocol[column_name, 'Value.type'])
  data.type <- str_trim(protocol[column_name, 'Data.type'])
  
  
  cat(sprintf("column name: %s\n", column_name))
  cat(sprintf("Data.Type: %s\n", val.type))
  
  summary(data[[column_name]])
  
  
  if (!is.na(val.type) & val.type == "Numeric") {
    val.min <-   as.numeric(str_trim(protocol[column_name, 'Min']))
    val.max <- as.numeric(str_trim(protocol[column_name, 'Max']))
    
    

    plot(data[[column_name]], ylab = column_name)
    
    plot(data[['revenue']] ~ data[[column_name]], xlab=column_name, ylab = "revenue")
    
    #if more than 35 unique numbers
    if (protocol[column_name,"Unique.count"] > 35) {
      #if differencce between the min and max is bigger than 1000 present log
      if ((val.max - val.min) > 1000) {
        hist(log(data[[column_name]]+1), main = paste("log of ", column_name), xlab = paste("log of ", column_name))
      } else {
        hist(data[[column_name]], main = column_name, xlab = column_name)
      }
      
      scatter.smooth(data[[column_name]] ~ data[['movie_id']], main=column_name, xlab="movies",ylab=column_name, family="symmetric",
                     lpars =list(col = "red", lwd = 2, lty = 2))
    } else {
      barplot(table(data[[column_name]]), main = column_name)
    }
    
    
    # if differencce between the min and max is bigger than 1000 present log
    delta<-(val.max - val.min)
    if (!is.na(delta) & delta > 1000) {
      boxplot(log(data[column_name]+1),main=column_name)
    } else {
      boxplot(data[column_name],main=column_name)
    }
    
  } else if (!is.na(val.type) & val.type == "Categorical") {
    
    if (!is.na(data.type) & data.type != "Text") {
      table(data[[column_name]])
      
      barplot(table(data[[column_name]]), main = column_name)
      #plot(data[['revenue']] ~ data[[column_name]], xlab=column_name)
    }
    
    ggplot(data)+
      geom_density(aes(log(data[['revenue']]), group=data[[column_name]], color=data[[column_name]]))
  }
  par(mfrow=c(1,1))
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


#remove movie_id from protocol
protocol<-protocol[-grep('movie_id', rownames(protocol)), ]

for (n in rownames(protocol)){
  doEDA(cmovies, n)
  
  Sys.sleep(10)
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

#install.packages("corrplot")
library(corrplot)

corr<-cor(cmovies[numerics], method = "pearson", use = "complete.obs")

corrplot(corr, method="circle")


corr<-cor(cmovies[categoricals], method = "spearman", use = "complete.obs")

corrplot(corr, method="circle")

###################################
#remove highly correlated variables
##################################
numerics_wo_revenue<-numerics[-grep('revenue', numerics)]

df2 <- cor(cmovies[numerics_wo_revenue],method = "pearson", use = "complete.obs")
high_correlated_variable <- findCorrelation(df2, cutoff=0.6) # remove with correlation bigger than cutoff

paste(numerics_wo_revenue[c(high_correlated_variable)], "was removed due to high correlation")

#reduced movies
rcmovies<-cmovies[ , !(names(cmovies) %in% numerics_wo_revenue[high_correlated_variable])]

#reduced numerics
rnumerics<-numerics[! numerics %in% numerics_wo_revenue[c(high_correlated_variable)]]


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

miss<-missingMatrix(rcmovies)

options(repr.plot.width = 4, repr.plot.height = 4)
library(naniar)
vis_miss(rcmovies[rnumerics])

vis_miss(rcmovies[categoricals])

##########################################################################################
# Outliers: checking distribution with and without outliers
# Outliers: checking distribution with and without outliers agains revenue
##########################################################################################

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

out<-outlierMatrix(movies,threshold = 1.5)


ocmovies<-movies


options(repr.plot.width = 16, repr.plot.height = 16)
par(mfrow=c(1,3))
for(v in numerics) {
  #look on variable with some variability
    
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, v, 
         cex = 1.6, col = "black")
    
    hist(movies[[v]], freq = FALSE, xlab = v,  main = "With Outliers")
    
    #barplot(table(movies[[v]]))
    
    dev.new(width=5, height=4)
    scatterplot(movies[['revenue']] ~ movies[[v]] | out[[v]], 
                xlab="Revenue", ylab=v,
                main=paste(v, "before outliers cleaup"))
    abline(lm(ocmovies$revenue ~ movies[[v]]), col = 'green')
    
      
      
    
    ##############################
    #Handle outliers
    ##############################

    
    if (protocol[v,"Outlier.treatment"] == "Leave"){
      
      print("Do nothing")
      
    } else if (protocol[v,"Outlier.treatment"] == "Null"){
      
      #drop outlier value (replace by NA)
      ocmovies[which(out[v] == 1), v]<-NA
      
    } else if (protocol[v,"Outlier.treatment"] == "Log"){
      
      ocmovies[[v]]<-log(ocmovies[[v]])
      
    } else if (protocol[v,"Outlier.treatment"] == "Mean"){
      
      ocmovies[which(out[v] == 1), v]<-mean(ocmovies[[v]])
      
    } else if (protocol[v,"Outlier.treatment"] == "Sqrt"){
      
      ocmovies[[v]]<-sqrt(ocmovies[[v]])
      
    } else if (protocol[v,"Outlier.treatment"] == "DropVar"){
      
      ocmovies[,! names(ocmovies) == v]
    }
    
    
    print(protocol[v,"Notes"])
    
    
    
    
    
    #hist(ocmovies[[v]], freq = FALSE,xlab = v,  main = "Without Outliers")
    #lines(density(ocmovies[[v]], na.rm = TRUE))
    
    #plot(y = rcmovies$revenue, x = rcmovies[[v]], pch = 16, cex = 1.3, col = "blue", main = "Distribution against Revenure(with Outliers)", xlab = v, ylab = "revenue")
    #abline(lm(rcmovies$revenue ~ rcmovies[[v]]))

    #plot(y = ocmovies$revenue, x = ocmovies[[v]], pch = 16, cex = 1.3, col = "blue", main = "Distribution against Revenure(without Outliers)", xlab = v, ylab = "revenue")
    #abline(lm(ocmovies$revenue ~ ocmovies[[v]]))
  
    #print(paste("T-Test of",v))
    #res<-t.test(ocmovies[[v]], rcmovies[[v]])

    #print(res)
    
}
par(mfrow=c(1,1))

##########################################################################################
#getMissingness <- function (data, getRows = FALSE) {
##########################################################################################

getMissingness <- function (data, getRows = FALSE) {
  require(dplyr)
  l <- nrow(data)
  vn <- names(data)
  nadf <- data
  cnt <- NULL
  miss <- function(x) return(sum(is.na(x)))
  for (n in vn) {
    nadf[[n]] <- ifelse(is.na(nadf[[n]]) == T, 1, 0)
    cnt <- rbind(cnt, data.frame(n, sum(nadf[[n]])))
  }
  names(cnt) <- c("var", "na.count")
  cnt$rate <- round((cnt$na.count/nrow(nadf)) * 100, 1)
  nadf$na.cnt <- 0
  nadf$na.cnt <- rowSums(nadf)
  cnt <- cnt %>% dplyr::arrange(desc(na.count)) %>% dplyr::filter(na.count > 
                                                                    0)
  totmiss <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::tally()
  idx <- NULL
  msg <- (paste("This dataset has ", as.character(totmiss), 
                " (", as.character(round(totmiss/nrow(data) * 100, 1)), 
                "%)", " complete rows. Original data has ", nrow(data), 
                " rows.", sep = ""))
  if (getRows == TRUE & totmiss != 0) {
    nadf$rn <- seq_len(nrow(data))
    idx <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::select(rn)
  }
  print(list(head(cnt, n = 10), msg))
  return(list(missingness = cnt, message = msg, rows = idx$rn))
}

##########################################################################################
# Misssing: for each variable checking distribution with and without missing against all the other varaibles
##########################################################################################


missingness<-getMissingness(ocmovies)

df<-missingness$missingness %>% filter(rate >= 37.0)
high_missing_variables<-as.character(df$var)

#remove variables with more than X of missings
df<-missingness$missingness %>% filter(rate < 37.0)
df<-df %>% filter(rate > 0)


missing_variables<-as.character(df$var)

miss<-missingMatrix(ocmovies[missing_variables])


lmiss <- lapply(miss, as.logical)


for(v in missing_variables) {
  for(j in missing_variables) {
    print(paste(v,j))
    
    if (protocol[v, "Value.type"] == "Numeric" & protocol[j, "Value.type"] == "Numeric" & 
        protocol[v, "Data.type"] != "Boolean" & protocol[j, "Data.type"] != "Boolean")
    {
      val.min <-   as.numeric(str_trim(protocol[v, 'Min']))
      val.max <- as.numeric(str_trim(protocol[v, 'Max']))
      
      
      if (v!=j)
      {
        if (val.max> 1000000) {
          is_missing<-lmiss[[j]]
          p<-ggplot(ocmovies) +
            geom_density(aes(log(ocmovies[[v]]), group=is_missing, color=is_missing), size = 1) +
            scale_x_continuous(name = paste("Log of", v)) +
            ggtitle(paste("Density plot of", v, "with and without missing", j)) 
          print(p)
        }
        else
        {
          is_missing<-lmiss[[j]]
          p<-ggplot(ocmovies) +
            geom_density(aes(ocmovies[[v]], group=is_missing, color=is_missing), size = 1) +
            scale_x_continuous(name = v) +
            ggtitle(paste("Density plot of", v, "with and without missing", j)) 
          print(p)
          
        }
      }
    }
  }
}

##########################################################################################
# Misssing: Create a table with all the variable that have missing values and explain how missings were created (MCAR and etc.)
##########################################################################################


require(MissMech)

miss1 <- TestMCARNormality(data=as.matrix(rcmovies[missing_variables[10:34]]), del.lesscases = 10)


#TestMCARNormality(data=as.matrix(movies[numerics_wo_revenue]), del.lesscases = 1)


dd<-protocol[missing_variables,] %>% filter(Value.type == 'Numeric') 

ff<-str_trim(dd$Feature.name)

TestMCARNormality(data=as.matrix(movies[ff[1:15]]), del.lesscases = 1)

nummovies<-movies[, (names(movies) %in% numerics)]

TestMCARNormality(data=as.matrix(nummovies[, !(names(nummovies) %in% high_missing_variables)]), del.lesscases = 1)






##########################################################################################
# Misssing: Do imputation for each variable according to the appropriate technic
##########################################################################################

library(mice)
init = mice(movies[missing_variables], maxit=0) 
meth = init$method
predM = init$predictorMatrix

predM
set.seed(103)
imputed = mice(movies[missing_variables], method=meth, predictorMatrix=predM, m=5)


n <- 300
p <- 5
r <- 0.3
mu <- rep(0, p)
sigma <- r * (matrix(1, p, p) - diag(1, p))+ diag(1, p)
set.seed(110)
eig <- eigen(sigma)
sig.sqrt <- eig$vectors %*%  diag(sqrt(eig$values)) %*%  solve(eig$vectors)
sig.sqrt <- (sig.sqrt + sig.sqrt) / 2
y <- matrix(rnorm(n * p), nrow = n) %*%  sig.sqrt
tmp <- y
for (j in 2:p){
  y[tmp[, j - 1] > 0.8, j] <- NA 
}
out <- TestMCARNormality(data = y, alpha =0.1)
print(out)
