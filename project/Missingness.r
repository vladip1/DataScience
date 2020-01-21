
options(warn=-1)

#home
setwd("C://Users//Cherch//DataScience//project")

#work
#setwd("C://bb//DataScience//project")


require("caret")
require("openxlsx")
require("tidyverse")
require(devtools)
library(dplyr)
library(car)
library(naniar)


protocol<-read.xlsx("../project/BoxOffice - Data Retrieval Protocol.xlsx", sheet = "protocol")

head(protocol)

rownames(protocol) <- str_trim(protocol$Feature.name)

load("../data/BoxOffice_ff.RData")

load("../data/BoxOffice_ff_with_ouliers.RData")

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

# list all the numeric variables
numerics<-str_trim(protocol$Feature.name[protocol$Value.type == "Numeric"])

# list all the numeric variables
categorics<-str_trim(protocol$Feature.name[protocol$Value.type == "Categorical"])


par(mfrow=c(1,2))
options(repr.plot.width = 16, repr.plot.height = 16)

vis_miss(ocmovies[numerics])

vis_miss(ocmovies[categorics])


getMissingness <- function (data, getRows = FALSE, print_rows = 10) {
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
  print(list(head(cnt, n = print_rows), msg))
  return(list(missingness = cnt, message = msg, rows = idx$rn))
}


#check the missingness
missingness<-getMissingness(ocmovies, getRows = TRUE, print_rows = 100)




missingness<-getMissingness(ocmovies)

df<-missingness$missingness %>% filter(rate < 1.0)

low_missing_variables<-as.character(df$var)

mmovies<-ocmovies %>% drop_na(low_missing_variables)

options(repr.plot.width = 16, repr.plot.height = 16)

vis_miss(mmovies[1:round(ncol(mmovies)/2)])

vis_miss(mmovies[round(ncol(mmovies)/2): ncol(mmovies)])

df<-protocol %>% filter(Data.type == "Boolean") %>% select(Feature.name)
booleans<-str_trim(as.character(df$Feature.name))


for(n in booleans){
  levels<-c("None", "Exist")
  mmovies[[n]] <- cut(mmovies[[n]], breaks = c(0,1,2), labels = levels,  right = FALSE)
}



factors_columns<-names(mmovies)[ sapply(mmovies, is.factor)]

for (n in factors_columns){
  s<-sum(is.na(mmovies[[n]]))
  if (s > 0){
    levels <- levels(mmovies[[n]])
    levels[length(levels) + 1] <- "Missing"

    # refactor mmovies[[n]] to include "Missing" as a factor level
    # and replace NA with "None"
    mmovies[[n]] <- factor(mmovies[[n]], levels = levels)

    mmovies[[n]][is.na(mmovies[[n]])] <- "Missing"

    print(n)

  }
}


miss<-missingMatrix(mmovies)

lmiss <- lapply(miss, as.logical)

counter<-0

missing_numerics<-NULL

options(repr.plot.width = 8, repr.plot.height = 8)

for(j in names(mmovies)) {

    is_missing<-lmiss[[j]]

  for(v in names(mmovies)) {


    if (v!=j &
        !is.na(mmovies[[v]]) & !is.na(mmovies[[j]]) &
        is.numeric(mmovies[[v]]) &
        is.numeric(mmovies[[j]]) &
        !all(is_missing == FALSE) &
        !is.na(protocol[v, "Value.type"]) &
        !is.na(protocol[v, "Data.type"]) &
        !is.na(protocol[j, "Value.type"]) &
        !is.na(protocol[j, "Data.type"]) &
        protocol[v, "Value.type"] == "Numeric" & protocol[j, "Value.type"] == "Numeric" &
        protocol[v, "Data.type"] != "Boolean" & protocol[j, "Data.type"] != "Boolean"){

        print(counter)
        counter<-counter + 1

        missing_numerics<-c(missing_numerics, j)

        val.min <-min(mmovies[[v]], na.rm = TRUE)
        val.max <-max(mmovies[[v]], na.rm = TRUE)

        if (val.max> 1000000) {
            p<-ggplot(mmovies) +
            geom_density(aes(log(mmovies[[v]]), group=is_missing, color=is_missing), size = 1) +
            scale_x_continuous(name = paste("Log of", v)) +
            ggtitle(paste("Density plot of", v, "with and without missing", j))

            #print(p)

        }
        else{
            p<-ggplot(mmovies) +
            geom_density(aes(mmovies[[v]], group=is_missing, color=is_missing), size = 1) +
            scale_x_continuous(name = v) +
            ggtitle(paste("Density plot of", v, "with and without missing", j))

            #print(p)

        }

        if (counter == 10)
        {
            break
        }
    }
  }
}

##########################################################
#list all the variables that need to be handled
##########################################################

missing_numerics<-unique(missing_numerics)

final_missing_numerics<-missing_numerics

#budget cannot be imputated due to bad distribution of missing values on "revenue"
final_missing_numerics<-setdiff(final_missing_numerics, "budget")

#budget
f.var<-"budget"
mmovies[[f.var]]<-log(mmovies[[f.var]] + 1)

hist(mmovies[[f.var]])

summary(mmovies[[f.var]])

fbreaks<-c(0,11,16,20)
flabels<-c("Small", "Medium", "Large")
mmovies[[f.var]]<-cut(mmovies[[f.var]], breaks = fbreaks, labels = flabels,
                      right = FALSE)


#popularity cannot be imputated due to bad distribution of missing values on "revenue", on "depart_Art", on "depart_Production"
final_missing_numerics<-setdiff(final_missing_numerics, "popularity")

#runtime looks okay and can be imputated

#revenue looks okay and can be imputated - not sure we shoudl do it since it's a dependent variable and missing part should be predicted -
#removing from imputation population
final_missing_numerics<-setdiff(final_missing_numerics, "revenue")

#keyword_cnt looks okayt and can be imputated

#producers_cnt cannot be impuatted due to bad distribution of missing values on "budget", "producers_cnt", "revenue", "depart_Art", "depart_Writing"
final_missing_numerics<-setdiff(final_missing_numerics, "producers_cnt")


#actor0_movies_cnt cannot be impuatted due to bad distribution of missing values on "actor0_movies_5y_cnt", "actor0_prev_revenue"
final_missing_numerics<-setdiff(final_missing_numerics, "actor0_movies_cnt")

#actor1_movies_cnt cannot be impuatted due to bad distribution of missing values on "actor1_movies_5y_cnt", "actor1_prev_revenue"
final_missing_numerics<-setdiff(final_missing_numerics, "actor1_movies_cnt")

#actor2_movies_cnt cannot be impuatted due to bad distribution of missing values on actor1_movies_cnt, actor2_movies_cnt, actor2_prev_revenue
final_missing_numerics<-setdiff(final_missing_numerics, "actor2_movies_cnt")

#actor0_prev_revenue looks okay and can be imputated

#actor1_prev_revenue looks okay and can be imputated

#actor2_prev_revenue looks okay and can be imputated

#depart_Directing cannot be impuatted due to bad distribution of missing values on 'depart_Art', "depart_Production"
final_missing_numerics<-setdiff(final_missing_numerics, "depart_Directing")

#depart_Editing cannot be impuatted due to bad distribution of missing values on 'depart_Art', "depart_Production"
final_missing_numerics<-setdiff(final_missing_numerics, "depart_Editing")

#depart_Production cannot be impuatted due to bad distribution of missing values on 'depart_Art', "depart_Editing"
final_missing_numerics<-setdiff(final_missing_numerics, "depart_Production")

#depart_Writing looks okay and can be imputated



##########################################################
#try to find MCAR variables
##########################################################

mm<-mmovies[final_missing_numerics]

vis_miss(mm)

require(MissMech)

miss1 <- TestMCARNormality(data=mm, del.lesscases = 6)


#remove all the observations missing in the revenue missing

final_missing_numerics<-c("revenue", "budget", final_missing_numerics)
mm1<-mmovies[final_missing_numerics]

mm2<-mm1 %>% drop_na("revenue")

dim(mm2)
miss2 <- TestMCARNormality(data=mm2, del.lesscases = 1)

vis_miss(mm2)

final_missing_numerics<-setdiff(final_missing_numerics, c("actor0_prev_revenue", "actor1_prev_revenue", "actor2_prev_revenue"))
mm3<-mmovies[final_missing_numerics]

mm3.1<-mm3 %>% drop_na("revenue")

miss3 <- TestMCARNormality(data=mm3, del.lesscases = 3)


final_missing_numerics<-setdiff(final_missing_numerics, c("budget", "revenue"))

mm4<-mmovies[final_missing_numerics]
miss4 <- TestMCARNormality(data=mm4, del.lesscases = 1)

vis_miss(mm3)


##########################################################
#those that cannot be inputated - turn to factor
##########################################################


#something
fbreaks<-c(1,2,3)
flabels<-c("a", "b")
f.var<-"some"
mmovies[[f.var]]<-cut(mmovies[[f.v]], breaks = fbreaks, labels = flabels,
                   right = FALSE)

#something
fbreaks<-c(1,2,3)
flabels<-c("a", "b")
f.var<-"some"
mmovies[[f.var]]<-cut(mmovies[[f.v]], breaks = fbreaks, labels = flabels,
                      right = FALSE)

#something
fbreaks<-c(1,2,3)
flabels<-c("a", "b")
f.var<-"some"
mmovies[[f.var]]<-cut(mmovies[[f.v]], breaks = fbreaks, labels = flabels,
                      right = FALSE)


##########################################################
#Turn Category variables to factors and add misisng level
#########################################################


df<-protocol %>% filter(Value.type == "Categorical") %>% select(Feature.name)
variables.cat<-str_trim(as.character(df$Feature.name))

factors_columns<-names(mmovies)[ sapply(mmovies, is.factor)]

#find all the category variables yet to be factoprs
fc<-setdiff(variables.cat, factors_columns)

for (n in fc)
{
  mmovies[[n]]<-factor(mmovies[[n]])
}

factors_columns<-names(mmovies)[ sapply(mmovies, is.factor)]


for (n in factors_columns){
  s<-sum(is.na(mmovies[[n]]))
  if (s > 0){
    print(n)
    levels <- levels(mmovies[[n]])
    levels[length(levels) + 1] <- "Missing"

    # refactor mmovies[[n]] to include "Missing" as a factor level
    # and replace NA with "None"
    mmovies[[n]] <- factor(mmovies[[n]], levels = levels)

    mmovies[[n]][is.na(mmovies[[n]])] <- "Missing"

    print(n)

  }
}

# All the variables are missing-free
#########################################################################








head(mmovies)

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

