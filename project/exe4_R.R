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

load("../data/BoxOffice_ff_with_ouliers.RData")

#save(ocmovies,file="../data/BoxOffice_ff_with_ouliers.RData")

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


#install.packages("corrplot")
library(corrplot)

corr<-cor(ocmovies[numerics], method = "pearson", use = "complete.obs")

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

miss<-missingMatrix(ocmovies)

options(repr.plot.width = 4, repr.plot.height = 4)
library(naniar)
vis_miss(ocmovies[numerics])

vis_miss(ocmovies[categoricals])

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
##########################################################################################
outlierMatrixWinsorizing <- function(data, v, threshold=1.5) {
  if(is.numeric(data[[v]])) {
    med<- median(data[[v]], na.rm = T)
    outlow <- quantile(data[[v]],probs = 0.25,na.rm = T)
    outhigh <- quantile(data[[v]],probs = 0.75, na.rm = T)
    irq_level <- (outhigh - outlow) * threshold
    outlow <- outlow - irq_level
    outhigh <- outhigh +  irq_level

    data[data[[v]] < outlow, v]<-outlow

    data[data[[v]] > outhigh, v]<-outhigh

  } else {
    mv <- rep(0,nrow(data))
  }

  return(data)
}

##########################################################################################

movies_threshold<-2.0

out<-outlierMatrix(movies,threshold = movies_threshold)


ocmovies<-movies

options(repr.plot.width = 16, repr.plot.height = 16)
for(v in numerics) {
  #look on variable with some variability


  par(mfrow=c(2,2))
  #options(repr.plot.width = 8, repr.plot.height = 4)

  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, v,
       cex = 1.6, col = "black")

  hist(movies[[v]], freq = FALSE, xlab = v,  main = "With Outliers")

  barplot(table(movies[[v]]))

  
  scatter.smooth(movies[['revenue']] ~ movies[[v]], main=v, xlab=v ,ylab="revenue", family="symmetric",
                 lpars =list(col = "green", lwd = 2, lty = 2), col=out[[v]]+1)




  ##############################
  #Handle outliers
  ##############################

  mes<-sprintf("Outliers of %s will behandled via %s method,
            the reason is \
            %s",v,
            protocol[v, "Outlier.treatment"],
            protocol[v, "Outlier.Notes"])



  if (protocol[v,"Outlier.treatment"] == "Leave"){

    par(mfrow=c(1,2))
    
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, mes,
         cex = 1.6, col = "black")
    
    #options(repr.plot.width = 8, repr.plot.height = 4)
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste("Do nothing for", v),
         cex = 1.6, col = "black")

  } else if (protocol[v,"Outlier.treatment"] == "Null"){

    #drop outlier value (replace by NA)
    ocmovies[which(out[v] == 1), v]<-NA


    par(mfrow=c(2,2))
    
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, mes,
         cex = 1.6, col = "black")
    
    #options(repr.plot.width = 8, repr.plot.height = 8)
    hist(ocmovies[[v]], freq = FALSE, xlab = v,  main = "without Outliers")

    barplot(table(ocmovies[[v]]))

    
    scatter.smooth(ocmovies[['revenue']] ~ ocmovies[[v]], main=paste(v, "after outliers cleaup"), xlab=v ,ylab="revenue", family="symmetric",
                   lpars =list(col = "green", lwd = 2, lty = 2))
    

  } else if (protocol[v,"Outlier.treatment"] == "Log"){

    ocmovies[[v]]<-log(movies[[v]] + 1)


    par(mfrow=c(2,2))
    
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, mes,
         cex = 1.6, col = "black")
    
    hist(ocmovies[[v]], freq = FALSE, xlab = v,  main = "without Outliers")

    barplot(table(ocmovies[[v]]))

    scatter.smooth(ocmovies[['revenue']] ~ ocmovies[[v]], main=paste(v, "after outliers cleaup"), xlab=v ,ylab="revenue", family="symmetric",
                   lpars =list(col = "green", lwd = 2, lty = 2))
    

  } else if (protocol[v,"Outlier.treatment"] == "Sqrt"){

    ocmovies[[v]]<-sqrt(movies[[v]] + 1)

    par(mfrow=c(2,2))
    
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, mes,
         cex = 1.6, col = "black")
    
    hist(ocmovies[[v]], freq = FALSE, xlab = v,  main = "without Outliers")

    barplot(table(ocmovies[[v]]))

    scatter.smooth(ocmovies[['revenue']] ~ ocmovies[[v]], main=paste(v, "after outliers cleaup"), xlab=v ,ylab="revenue", family="symmetric",
                   lpars =list(col = "green", lwd = 2, lty = 2))
    

  } else if (protocol[v,"Outlier.treatment"] == "Winsorizing"){

    ocmovies<-outlierMatrixWinsorizing(movies, v, movies_threshold)


    par(mfrow=c(2,2))
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, mes,
         cex = 1.6, col = "black")
    
    hist(ocmovies[[v]], freq = FALSE, xlab = v,  main = "without Outliers")

    barplot(table(ocmovies[[v]]))

    scatter.smooth(ocmovies[['revenue']] ~ ocmovies[[v]], main=paste(v, "after outliers cleaup"), xlab=v ,ylab="revenue", family="symmetric",
                   lpars =list(col = "green", lwd = 2, lty = 2))
    

  } else if (protocol[v,"Outlier.treatment"] == "Categorize"){

    cat_num<-protocol[v,"Categories.num"]
    cat_num_vec<-unlist(strsplit(cat_num, ","))


    cat_names<-protocol[v,"Categories.names"]
    cat_names_vec<-unlist(strsplit(cat_names, ","))

    ocmovies[[v]]<-cut(movies[[v]], breaks = as.numeric(cat_num_vec), labels = cat_names_vec,
                       right = FALSE)

    st<-sprintf("Numeric variable %s Was categorized, values were replaced with %s breaks \
                  with following names %s", v, cat_num, cat_names)

    par(mfrow=c(2,2))
    
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, mes,
         cex = 1.6, col = "black")
    
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, st,
         cex = 1.6, col = "black")
    
    plot(ocmovies[[v]], xlab = v)
  }

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

par(mfrow=c(10,10))


for(v in missing_variables) {
  for(j in missing_variables) {
    

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
        }
        else
        {
          is_missing<-lmiss[[j]]
          p<-ggplot(ocmovies) +
            geom_density(aes(ocmovies[[v]], group=is_missing, color=is_missing), size = 1) +
            scale_x_continuous(name = v) +
            ggtitle(paste("Density plot of", v, "with and without missing", j))
    
        }
      }
    }
  }
}

##########################################################################################
# Misssing: Create a table with all the variable that have missing values and explain how missings were created (MCAR and etc.)
##########################################################################################

missingness<-getMissingness(ocmovies)


#remove variables with more than X of missings
df<-missingness$missingness %>% filter(rate < 10.0)
df<-df %>% filter(rate > 1.0)


missing_variables<-as.character(df$var)

#movies w/o high-missing variables
mmovies<-ocmovies[missing_variables]

dim(mmovies)

m<-getMissingness(mmovies)

as.character(m$missingness$var)

#miss<-missingMatrix(rcmovies)

options(repr.plot.width = 4, repr.plot.height = 4)
library(naniar)

vis_miss(mmovies)


#Variable with missingness less than 1% will be resolved via observations removal
#depart_Sound"                 "actor1_movies_5y_cnt"         "depart_Production_female"     "depart_Visual_Effects_female"
# [5] "depart_Crew"                  "depart_Custom_Mkup"           "depart_Lighting"              "depart_Art_female"
# [9] "depart_Camera_female"         "depart_Crew_female"           "depart_Custom_Mkup_female"    "depart_Directing_female"
#[13] "depart_Editing_female"        "depart_Sound_female"          "depart_Writing_female"        "depart_Art"
#[17] "depart_Camera"                "depart_Lighting_female"       "actor2_movies_5y_cnt"         "director_movies_5y_cnt"
#[21] "actor0_movies_5y_cnt"         "release_date"                 "release_year"                 "release_month"
#[25] "release_day"                  "seasonality"                  "countries_cnt"

# according to vis_miss it's clear this is not MCAR/MAT - following categorical variables should be fix via adding a new category (missing)
#"sw_female_actor0"      "sw_male_actor0"        "sw_female_actor2"      "sw_male_actor2"        "sw_female_actor1" "sw_male_actor0"
#
#"actor0_prev_revenue" is a Numerics variable (37,32% missingness) - need to thing what to do with it



#miss1 <- TestMCARNormality(data=mmovies, del.lesscases = 3)


for (n in 1:1) {
  print(n)
  mm<-as.matrix(mmovies[n:12])
  miss1 <- TestMCARNormality(data=mm, del.lesscases = 1)
  print(miss1)
  options(repr.plot.width = 6, repr.plot.height = 8)
  boxplot(miss1)

}

mmovies.numeric <- mmovies[,sapply(mmovies, is.numeric)]
dim(mmovies.numeric)

names(mmovies.numeric)
getMissingness(mmovies.numeric)






require(MissMech)


for (n in 1:19) {
  print(n)
  mm<-as.matrix(mmovies.numeric[n:(n+4)])
  miss1 <- TestMCARNormality(data=mm, del.lesscases = 1)
  print(miss1)
}



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
init = mice(ocmovies, maxit=0)
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


missingness<-getMissingness(mmovies)

df<-missingness$missingness %>% filter(rate < 1.0)
low_missing_variables<-as.character(df$var)

DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA), z=c(NA, 33, 22))
DF[!is.na(DF$y),]

missingness<-getMissingness(ocmovies)

df<-missingness$missingness %>% filter(rate < 1.0)
low_missing_variables<-as.character(df$var)

ocmovies[!is.na(ocmovies[low_missing_variables]),]

#####################################################################
missingness<-getMissingness(ocmovies)

df<-missingness$missingness %>% filter(rate >= 37.0)
high_missing_variables<-as.character(df$var)

high_missing_variables_wo_revenue<-high_missing_variables[-grep('\\brevenue\\b', high_missing_variables)]

mmovies<-ocmovies[, !(names(ocmovies) %in% high_missing_variables_wo_revenue)]

vis_miss(mmovies[1:round(ncol(mmovies)/2)])

vis_miss(mmovies[round(ncol(mmovies)/2): ncol(mmovies)])



