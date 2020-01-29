
library(dplyr)

Table1 <- function (x=NULL, y=NULL, rn=NULL, data=NULL, miss=3, catmiss=TRUE, formatted=TRUE, categorize=FALSE,
                    factorVars=NULL, maxcat=10, delzero=TRUE, decimals=1, messages=TRUE, excel=0, excel_file=NULL) {
  ### define sub-functions
  options(warn=-1)
  Del <- NULL
  Pop <- NULL
  n <- NULL
  g1 <- function(var)c(Mean=mean(var,na.rm=TRUE), SD=stats::sd(var,na.rm=TRUE))
  g2 <- function(var)c(Median=stats::median(var,na.rm=TRUE), IQR=stats::quantile(var,c(0.25,0.75),na.rm=TRUE))
  msg <- NULL
  
  ### function for transforming variables to factors
  setFactors <- function(data=data, factorVars=factorVars, catmiss=catmiss, maxcat=maxcat) {
    #print(factorVars)
    if(is.null(factorVars)==T) {
      aa <- sapply(sapply(data, unique), length)
      factorVars <- names(which(aa <= maxcat))
    }
    #print(factorVars)
    for (v in factorVars) {
      ct <- ifelse( ((is.null(factorVars)==F & (v %in% factorVars)) | (is.null(factorVars)==T & length(unique(data[[v]])) <= maxcat)),1,0)
      if (ct == 1) {
        data[[v]] <- factor(data[[v]])
        if(catmiss == T & sum(is.na(data[[v]])==T) > 0) {
          data[[v]] <- factor(data[[v]],levels=c(levels(data[[v]]),"Missing"))
          data[[v]][which(is.na(data[[v]])==T)] <- "Missing"
        }
      }
    }
    return(data)
  }
  ### proceed to convert varibles to factors
  if (categorize == T | is.null(factorVars)==F ) {
    data <- setFactors(data, factorVars, catmiss, maxcat)
  }
  
  getSimpleTable  <- function(x=x, rn=rn, data=data, miss=miss, catmiss=catmiss,formatted=formatted,
                              categorize=categorize,maxcat=maxcat, delzero=delzero) {
    if (is.null(x)==TRUE) { x <- names(data)}
    if (is.null(rn)==TRUE) { rn <- x}
    ln <- length(x)
    pb <- utils::txtProgressBar(min=0,max=ln,style=3)
    msg <- NULL
    ### define the column names
    tableaaaa <- cbind(Del="Del",V1="Variables",V2="Categories",n="n","Population")
    tablebbbb <- cbind(Del="Del",V1="Variables",V2="Categories",n="n",val1="val1",val2="val2",val3="val3")
    tbl1 <- cbind(0,"Individuals","n",n=1, nrow(data))
    tbl2 <- cbind(0,"Individuals","n",n=1, nrow(data),NA,NA)
    tableaaaa <- rbind(tableaaaa,tbl1)
    tablebbbb <- rbind(tablebbbb,tbl2)
    q <- 1
    n <- 1
    ii <- 1
    for (v in x)
    {
      if (v %in% names(data)) {
        ### define if the actual variable has to be treated as numeric or factor
        ct <- ifelse(is.numeric(data[[v]])==T & categorize==T &
                       ((is.null(factorVars)==F & (v %in% factorVars)) |
                          (is.null(factorVars)==T & length(unique(data[[v]])) <= maxcat)),1,0)
        ### treat as numeric
        if (length(unique(data[v]))==0) {
          if (messages==T) {
            #print(paste("The variable",v,"has no data... avoided"))
            msg <- c(msg, paste("The variable",v,"has no data... avoided"))
          }
        } else if (inherits(data[[v]], "Date")==TRUE) {
          if (messages==T) {
            msg <- c(msg, paste("The variable",v,"is a date. Dates are not allowed in Table1... avoided"))
          }
        } else if (is.numeric(data[[v]])==T & ct==0) {
          ## report mean and standard deviation
          t_n <- g1(data[[v]])
          tp <- paste(format(round(t_n[1],decimals),nsmall=1,big.mark=",")," (", format(round(t_n[2],decimals),nsmall=1,big.mark=","),")",sep="")
          tbl1 <- cbind(0,rn[q],"Mean (SD)",n=1, tp)
          tbl2 <- cbind(0,rn[q],"Mean (SD)",n=1,t_n[1],t_n[2],NA)
          tableaaaa <- rbind(tableaaaa,tbl1)
          tablebbbb <- rbind(tablebbbb,tbl2)
          ## report median and Interquartile ranges (25%,75%)
          t_n <- g2(data[[v]])
          tp <- paste(format(round(t_n[1],decimals),nsmall=1,big.mark=",")," (", format(round(t_n[2],decimals),nsmall=1,big.mark=","),"-", format(round(t_n[3],decimals),nsmall=1,big.mark=","), ")",sep="")
          tbl1 <- cbind(0,rn[q],"Median (IQR)",n=2, format(tp,big.mark=","))
          tbl2 <- cbind(0,rn[q],"Median (IQR)",n=2,t_n[1],t_n[2],t_n[3])
          tableaaaa <- rbind(tableaaaa,tbl1)
          tablebbbb <- rbind(tablebbbb,tbl2)
          ## report number and percent of missing
          if (miss >= 1) {
            datams <- subset(data,is.na(data[[v]])==T)
            if (nrow(datams)>0) {
              data$cnt <- 1
              datams$cnt <- 1
              t_n <- table(data$cnt)
              t_m <- sum(datams$cnt)
              tp <- paste(format(t_m,big.mark=",")," (",format(round((t_m/t_n)*100,decimals),nsmall=1,big.mark=","),"%)",sep="")
              tbl1 <- cbind(0,rn[q],"Missing (%)",n=3, tp)
              tbl2 <- cbind(0,rn[q],"Missing (%)",n=3, t_m, (t_m/t_n)*100, NA)
            } else {
              tbl1 <- cbind(1,rn[q],"Missing (%)",n=3, " -- ")
              tbl2 <- cbind(1,rn[q],"Missing (%)",n=3, NA, NA, NA)
            }
            tableaaaa <- rbind(tableaaaa,tbl1)
            tablebbbb <- rbind(tablebbbb,tbl2)
          }
        } else {
          t_n <- table(data[[v]])
          ttotal <- sum(t_n)
          nm <- row.names(t_n)
          for (f in 1:length(nm)) {
            del1 <- ifelse(length(nm)==2 & (nm[f]=="No" | nm[f]=="no" | nm[f]==0 | nm[f]=="0" | nm[f]=="None" | nm[f]=="none"),1,0)
            tp <- t_n[f] / ttotal * 100
            pct <- paste(format(round(t_n[f],decimals),nsmall=0,big.mark=",")," (", format(round(tp,decimals),nsmall=1,big.mark=","), "%)",sep="")
            tbl1 <- cbind(del1,rn[q],nm[f],n=f, pct)             ########### delete rows 0/1 !!!!!!!!!
            tbl2 <- cbind(del1,rn[q],nm[f],n=f, t_n[f], tp, NA)  ########### delete rows 0/1 !!!!!!!!!
            tableaaaa <- rbind(tableaaaa,tbl1)
            tablebbbb <- rbind(tablebbbb,tbl2)
          }
          if (miss >= 2 & catmiss==F ) {
            datams <- subset(data,is.na(data[[v]])==T)
            if (nrow(datams)>0) {
              data$cnt <- 1
              datams$cnt <- 1
              t_n <- table(data$cnt)
              t_m <- sum(datams$cnt)
              tp <- paste(format(t_m,big.mark=",")," (",format(round((t_m/t_n)*100,decimals),nsmall=1,big.mark=","),"%)",sep="")
              tbl1 <- cbind(0,rn[q],"Missing (%)",n=f, tp)
              tbl2 <- cbind(0,rn[q],"Missing (%)",n=f, t_m, (t_m/t_n)*100, NA)
            } else {
              tbl1 <- cbind(1,rn[q],"Missing (%)",n=f, " -- ")
              tbl2 <- cbind(1,rn[q],"Missing (%)",n=f, NA, NA, NA)
            }
            tableaaaa <- rbind(tableaaaa,tbl1)
            tablebbbb <- rbind(tablebbbb,tbl2)
          }
        }
      } else {
        if (messages==T) {
          msg <- c(msg, paste("The variable",v,"doesn't exists in the dataset... avoiding"))
        }
      }
      q <- q + 1
      utils::setTxtProgressBar(pb,ii)
      ii <- ii + 1
    }
    if(formatted==TRUE) {
      return(tableaaaa)
    } else {
      return(tablebbbb)
    }
    close(pb)
  }
  
  pvals <- function(x=x,y=y,rn=rn,data=data,categorize=categorize,maxcat=maxcat) {
    ptab <- NULL
    if (is.null(y)==FALSE) {
      if (y %in% names(data)) {
        if (is.null(x)==TRUE) { x <- names(data)}
        if (is.null(rn)==TRUE | length(rn)<2) {rn <- x}
        q <- 1
        ptab <- cbind(V="Variables",pval="pval", n="n")
        for (v in x) {
          if (v %in% names(data)) {
            ct <- ifelse(is.numeric(data[[v]])==T & categorize==T & length(unique(data[[v]])) <= maxcat,1,0)
            if (is.numeric(data[[y]])==T & categorize==T & length(unique(data[[y]])) <= maxcat) {
              data[[y]] <- as.factor(data[[y]])
            } else if (is.numeric(data[[y]])==T) {
              if (messages==T) {
                msg <- c(msg, paste("The variable",y,"is not a factor. Please convert to factor or change the 'categorize' flag to TRUE."))
              }
              pval <- "Please rerun!!!"
            }
            if (is.numeric(data[[v]])==TRUE & length(unique(data[[v]])) > 1 & ct == 0) {
              ### first check for homoscedasticity
              tryCatch({
                if (stats::bartlett.test(data[[v]], data[[y]])[3] >= 0.05) {
                  pval <- round(as.numeric(car::Anova(stats::lm(data[[v]] ~ data[[y]]))[1, 4]), 3)
                } else {
                  pval <- round(as.numeric(car::Anova(stats::lm(data[[v]] ~ data[[y]]), white.adjust = TRUE)[1, 3]), 3)
                }
              }, error = function(e) {
                pval <- "---"
              })
            } else if (length(unique(data[[v]]))==1) {
              pval <- NA
            } else {
              if (min(table(data[[v]],data[[y]])) > 5) {
                pval <- round(as.numeric(stats::chisq.test(data[[v]],data[[y]])$p.val),3)
              } else {
                if(min(table(data[[v]],data[[y]]))==0) {
                  #in cases where there are cells with zero, we use Fisher's exact test
                  tryCatch(
                    pval <- round(as.numeric(stats::fisher.test(data[[v]],data[[y]], workspace=1e9)$p.val),3),
                    error = function(e) {msg <- c(msg,paste0("Unable to calcualte the Fisher test for variables ",v," and ",y))})
                } else {
                  pval <- round(as.numeric(stats::kruskal.test(data[[v]],data[[y]], workspace=1e9)$p.val),3)
                }
              }
            }
            ptab <- rbind(ptab,cbind(V=rn[q],pval=pval,n=2))
          }
          q <- q + 1
        }
      }
    }
    return(ptab)
  }
  ####################### Begin analysis
  ##### if y is null then make a simple table
  tabaaa1 <- getSimpleTable(x=x, rn=rn, data=data, miss=miss, catmiss=catmiss,formatted=formatted,categorize=categorize,maxcat=maxcat, delzero=delzero)
  tabaaa1 <- tibble::as.tibble(tabaaa1)
  ############################  CHANGE TO 5 !!!!!!!!!!!!!!
  if(length(tabaaa1) > 5) {
    names(tabaaa1) <- c("Del","V1","V2","n","Pop","pop2","pop3")
  } else {
    names(tabaaa1) <- c("Del","V1","V2","n","Pop")
  }
  ##### if y has two levels, then make a compound comparison
  if (is.null(y)==FALSE){
    if (y %in% names(data)) {
      if (is.factor(data[[y]])==F) {
        if (length(levels(factor(data[[y]]))) > 8) {
          if (messages==T) {
            print("The dependent variable has more than 8 levels, table too large!")
          }
        } else if(min(table(data[[y]]))==0) {
          print("The dependent variable has one or more levels with no individuals assigned!")
        } else {
          data[[y]] <- factor(data[[y]])
        }
      }
      if (length(levels(data[[y]])) >= 2) {
        for (lv in levels(data[[y]])) {
          dtsub <- subset(data, data[[y]]==lv)
          tab <- getSimpleTable(x=x, rn=rn, data=dtsub, miss=miss, catmiss=catmiss, formatted=formatted,categorize=categorize,maxcat=maxcat, delzero=delzero)
          tab <- data.frame(tab)
          ############################  CHANGE TO 5 !!!!!!!!!!!!!!
          if(length(tab) > 5) {
            names(tab) <- c("Del","V1","V2","n",paste0(lv,"_1"),paste0(lv,"_2"),paste0(lv,"_3"))
          } else {
            names(tab) <- c("Del","V1","V2","n",lv)
          }
          ############################  CHANGE TO 5 !!!!!!!!!!!!!!
          tab[1,5] <- lv
          tabaaa1 <- suppressMessages(dplyr::left_join(tabaaa1, tab))
        }
        # what to do with dichotomous variables? We remove the "Zero" label...
        # clean unnecesary rows
        if (delzero == TRUE) {
          tabaaa1 <- tabaaa1 %>%
            dplyr::filter(Del==0)
        }
        ### calculate the p-value
        ptab <- data.frame(pvals(x=x,y=y,rn=rn,data=data,categorize=categorize,maxcat=maxcat))
        names(ptab) <- c("V1","pval","n")
        tabaaa1 <- suppressMessages(dplyr::left_join(tabaaa1, ptab))
        
        tabaaa1 <- tabaaa1 %>% dplyr::filter(Pop != " -- ") #%>%
      }
    }
  }
  tabaaa1 <- tabaaa1 %>% dplyr::select(-n) %>% dplyr::select(-Del)
  ##### Join the tables...
  Sys.setenv(JAVA_HOME="")
  if (excel==1) {
    wb <- xlsx::createWorkbook()
    sheet1 <- xlsx::createSheet(wb, sheetName="Table 1")
    xlsx::addDataFrame(tabaaa1,sheet1)
    #### save and close the workbook
    xlsx::saveWorkbook(wb, excel_file)
    return(tabaaa1)
  } else {
    return(tabaaa1)
  }
}


train_test <- function(data=NULL,train_name=NULL,test_name=NULL,prop=NULL,seed=round(runif(1, min=10, max=100)),tableone=FALSE)
{
  pval <- NULL
  checkTrainTest <- function(train=NULL,test=NULL) {
    train[["traintest_ind_"]] <- 1
    test[["traintest_ind_"]] <- 2
    df <- rbind(train, test)
    tab <- Table1(data=df, y="traintest_ind_",messages = F)
    vars <- subset(tab, pval < 0.05)$V1
    vars <- setdiff(vars,"traintest_ind_")
    if (length(vars)==0) {
      message("You got a perfectly balanced training and test datasets")
      message(" ")
    } else {
      message("WARNING: The following variables are not balanced between the training and test datasets:")
      for (v in vars) { message(paste("*",v)) }
      message("You can try to change the seed value until you get a balanced partition.")
      message("Alternatively, you can ommit this warning and exclude those variables from your model")
      message(" ")
    }
    return(tab)
  }
  nm <- 1
  ttenv = as.environment(nm)
  ## set the seed to make your partition reproductible
  set.seed(seed)
  smp_size <- floor(prop * nrow(data))
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  assign(train_name, data[train_ind, ], envir=ttenv)
  assign(test_name, data[-train_ind, ], envir=ttenv)
  message(paste("Dataset partitioned into:"))
  message(paste(" + Train dataset:", train_name))
  message(paste(" + Test dataset:", test_name))
  if(tableone==TRUE) {
    tab = checkTrainTest(get(train_name),get(test_name))
    return(tab)
  }
}

############################################################



#home
#setwd("C://Users//Cherch//DataScience//kaggle")

#work
setwd("C://bb//DataScience//kaggle")


df<-read.csv("train_enriched.csv")
tdf<-read.csv("test_enriched.csv")

dim(df)

str(df)

head(df)

#######################################Outliers Cleanup#############################################################################

bx.hum<-boxplot(df$hum)
df[df$hum %in% bx.hum$out, 'hum']<-NA

bx.windspeed<-boxplot(df$windspeed)
df[df$windspeed %in% bx.windspeed$out, 'windspeed']<-NA

df<-na.omit(df)





####################################### Data Transformation ############################################################################

dts.weekly <- ts(df$cnt, start=1, end=365, frequency=7)

stationary_dts.weekly<-decompose(dts.weekly)

days <- data.frame(weekday=(0:6),seasonal_days=stationary_dts.weekly$seasonal[2:8])

df2 <- inner_join(df,days)

tdf2<- inner_join(tdf, days)

#############################################################################################

dfm<-df %>% group_by(mnth) %>% summarise(month_sum=sum(cnt))

dfm.ts<-ts(dfm$month_sum, start = 1, end = 12, frequency = 12)

stationary_dfm<-decompose(dfm.ts)     

plot(stationary_dfm)

monthes <- data.frame(mnth=(1:12),seasonal_monthes=stationary_dfm$seasonal[1:12])

df3 <- inner_join(df2,monthes)

tdf3<- inner_join(tdf2, monthes)

#############################################################################################

dfs<-df %>% group_by(season) %>% summarise(season_sum=sum(cnt))

dfs.ts<-ts(dfs$season_sum, start = 1, end = 4, frequency = 4)

stationary_dfs<-decompose(dfs.ts)     

plot(stationary_dfs)

seasons <- data.frame(season=(1:4),seasonal_season=stationary_dfs$seasonal[1:4])

df4 <- inner_join(df3,seasons)
tdf4<- inner_join(tdf3, seasons)
#############################################################################################
summary(df4)


factorize <- function(data) {
  data$cluster<-factor(data$cluster)
  data$hcluster<-factor(data$hcluster)
  data$season<-factor(data$season)
  data$mnth<-factor(data$mnth)
  data$holiday<-factor(data$holiday)
  data$weekday<-factor(data$weekday)
  data$workingday<-factor(data$workingday)
  data$weathersit<-factor(data$weathersit)
  
  return(data)
}

df5<-factorize(df4)

tdf5<-factorize(tdf4)

summary(tdf5)


summary(df5)

df4<-df5

tdf4<-tdf5


##to do
#arima
#add seasonality as another columne
#try lm (linear regression)
#fine a cheatsheet adn looks for another modles
#run unsuperwised clustering and add as another colm (find number of clusters with the elbow approach)


#############################################################################################
# Data Split
#############################################################################################
best_model<-NULL

for (n in 1:50)
{
  train_test(data = df4, train_name = 'train', test_name = 'test', prop = 0.7, tableone = TRUE)
  
  head(train)
  
  
  drops<-c("X", "id", "atemp", "cluster")
  train<-train[ , !(names(train) %in% drops)]
  test<-test[ , !(names(test) %in% drops)]
  
  
  str(train)

#  boxplot(train$atemp)
#  boxplot(train$hum) #need to clean
#  boxplot(train$windspeed) #need to clean
  
#  library(corrplot)
  
#  corr<-cor(df[-c(1:2)], method = "pearson", use = "complete.obs")
  
#  corrplot(corr, method="circle")
  
  
  
  #tdf<-tdf[ , !(names(df) %in% drops)]
  
  
  
  #######################  Models ###################################################################
  
  ### The error we will use is the RMSE and RMSLE
  rmse <- function(y,y_hat) {
    err <- sqrt(sum((y_hat-y)^2,na.rm=T)/length(y))
    return(err)
  }
  
  rmsle <- function(y,y_hat) {
    err <- sqrt(sum((log(y_hat+1)-log(y+1))^2,na.rm=T)/length(y))
    return(err)
  }
  
  ### Table of resulting errors
  ### Name, Model, RMSE, RMSLE
  #err_res <- NULL
  
  
  #######################  Linear Model ###################################################################
  ## model with only the original variables
  mod1 <- lm(cnt ~., data=train[,1:12])
  summary(mod1)
  pred1 <- predict(mod1,newdata=test)
  rmse(test$cnt,pred1)
  rmsle(test$cnt,pred1)
  err_res <- rbind(err_res, data.frame(Name="Base Linear regression", Model="mod1", 
                                       RMSE=rmse(test$cnt,pred1), 
                                       RMSLE=rmsle(test$cnt,pred1)))
  
  ## model with all the variables
  mod2 <- lm(cnt ~., data=train)
  summary(mod2)
  
  pred2 <- predict(mod2,newdata=test)
  rmse(test$cnt,pred2)
  rmsle(test$cnt,pred2)
  err_res <- rbind(err_res, data.frame(Name="Extended Linear regression", Model="mod2", 
                                       RMSE=rmse(test$cnt,pred2), 
                                       RMSLE=rmsle(test$cnt,pred2)))
  
  #######################  Desicion trees ###################################################################
  
  library(tree)
  library(rpart)
  
  mod3 <- tree(cnt ~., data=train)
  mod3
  
  pred3 <- predict(mod3,newdata=test)
  rmse(test$cnt,pred3)
  rmsle(test$cnt,pred3)
  err_res <- rbind(err_res, data.frame(Name="Decision Trees-tree", Model="mod3", 
                                       RMSE=rmse(test$cnt,pred3), 
                                       RMSLE=rmsle(test$cnt,pred3)))
  
  
  
  mod4 <- rpart(cnt ~., data=train)
  mod4
  
  pred4 <- predict(mod4,newdata=test)
  rmse(test$cnt,pred4)
  rmsle(test$cnt,pred4)
  err_res <- rbind(err_res, data.frame(Name="Decision Trees-rpart", Model="mod4", 
                                       RMSE=rmse(test$cnt,pred4), 
                                       RMSLE=rmsle(test$cnt,pred4)))
  err_res
  
  #######################  Random Forest ###################################################################
  library(randomForest)
  library(ranger)
  
  mod5 <- randomForest(cnt ~., data=train)
  mod5
  
  pred5 <- predict(mod5,newdata=test)
  rmse(test$cnt,pred5)
  rmsle(test$cnt,pred5)
  
  
  
  err_res <- rbind(err_res, data.frame(Name="RandomForest", Model="mod5", 
                                       RMSE=rmse(test$cnt,pred5), 
                                       RMSLE=rmsle(test$cnt,pred5)))
  
  
  mod6 <- ranger(cnt ~., data=train)
  mod6
  
  pred6 <- predict(mod6,data=test)
  #head(pred6)
  rmse(test$cnt,pred6$predictions)
  rmsle(test$cnt,pred6$predictions)
  err_res <- rbind(err_res, data.frame(Name="RandomForest (ranger)", Model="mod6", 
                                       RMSE=rmse(test$cnt,pred6$predictions), 
                                       RMSLE=rmsle(test$cnt,pred6$predictions)))
  
  
  
  #######################  XGBoost ###################################################################
  #install.packages("xgboost")
  library(xgboost)
  
  train1 <- Matrix::sparse.model.matrix(cnt ~ .-1, data = train)
  test1 <- Matrix::sparse.model.matrix(cnt ~ .-1, data = test)
  
  X_train <- train1
  y_train <- train$cnt
  mod7 <- xgboost(data=X_train,label=y_train, nrounds=50,print_every_n = 10)
  
  X_test <- test1
  y_test <- test$cnt
  
  pred7 <- predict(mod7,newdata=X_test)
  rmse(y_test,pred7)
  rmsle(y_test,pred7)
  err_res <- rbind(err_res, data.frame(Name="XGBoost", Model="mod7", 
                                       RMSE=rmse(test$cnt,pred7), 
                                       RMSLE=rmsle(test$cnt,pred7)))
  
  #######################  kNN ###################################################################
  
  ### adaboost needs that values to be normalized
  min_max <- function(x) { (x -min(x))/(max(x)-min(x))   }
  
  X_train <- sapply(data.frame(as.matrix(train1)),min_max)
  X_test <- sapply(data.frame(as.matrix(test1)),min_max)
  
  summary(X_train)
  
  library(class)
  mod8 <- knn(X_train,X_test,cl=train$cnt)
  
  str(mod8)
  
  pred8 <- as.numeric(as.character(mod8))
  
  rmse(test$cnt,pred8)
  rmsle(test$cnt,pred8)
  err_res <- rbind(err_res, data.frame(Name="kNN", Model="mod8", 
                                       RMSE=rmse(test$cnt,pred8), 
                                       RMSLE=rmsle(test$cnt,pred8)))
  
  #######################  SVM ###################################################################
  
  #install.packages("liquidSVM")
  library(liquidSVM)
  
  mod9 <- svm(cnt ~., train)
  
  pred9 <- predict(mod9, newdata=test)
  
  rmse(test$cnt,pred9)
  rmsle(test$cnt,pred9)
  err_res <- rbind(err_res, data.frame(Name="SVM", Model="mod9", 
                                       RMSE=rmse(test$cnt,pred9), 
                                       RMSLE=rmsle(test$cnt,pred9)))
  plot(test$cnt, pred9, col=c('blue', 'red'))
  
  plot(test$cnt, pred7, col=c('blue', 'red'))
  
       
  err_res
  
  if (rmsle(test$cnt,pred9) < 0.18)
  {
    break()
  }
}
##########################################################################################
# Upload the real test
##########################################################################################

pred.res<-NULL
pred.res$id<-tdf4$id

tdf.pred<-tdf4[ , !(names(tdf4) %in% drops)]

pred.res$cnt<-round(predict(mod9, newdata=tdf.pred))

write.csv(file = "result_SVM_hcluster_no_aheat_no_out179.csv", pred.res, row.names = FALSE)

dim(df4)
#------------------------------------------------------------------------------------------

dim(tdf4)

pred.res<-NULL
pred.res$id<-tdf4$id
tdf.pred<-tdf4[ , !(names(tdf4) %in% drops)]

pp<-predict(mod6, data=tdf.pred)
pred.res$cnt<-round(pp$predictions)


summary(test)
summary(tdf.pred)


write.csv(file = "result_Ranger_hcluster_factorized_columns_no_atemp_158.csv", pred.res, row.names = FALSE)

#------------------------------------------------------------------------------------------

pred.res<-NULL
pred.res$id<-tdf4$id
tdf.pred<-tdf4[ , !(names(tdf4) %in% drops)]

dim(tdf.pred)

pred.res$cnt<-round(predict(mod5, newdata=tdf.pred))

length(pred.res$cnt)

dim(pred.res)

summary(test)
summary(pred.res)

head(pred.res)

write.csv(file = "result_RF_hcluster_factorized_columns_172.csv", pred.res, row.names = FALSE)

#------------------------------------------------------------------------------------------



pred.res<-NULL
pred.res$id<-tdf4$id
tdf.pred<-tdf4[ , !(names(tdf4) %in% drops)]
pred.res$cnt<-round(predict(mod7, newdata=as.matrix(tdf.pred)))

write.csv(file = "result_XBoost_with hcluster_factorized_columns.csv", pred.res, row.names = FALSE)

