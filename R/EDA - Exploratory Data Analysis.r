library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

con <- dbConnect(RSQLite::SQLite(),"../data/BoxOffice.db")

movies <- dbReadTable(con,"movies")
head(movies)

summary(movies)

### frequencies of original language
table(movies$original_language)


### differences between the average revenue by language?
movies %>% 
    group_by(original_language) %>% 
    summarise(revenue_avg=mean(revenue,na.rm=T), revenue_sd=sd(revenue,na.rm=T),n=n()) %>% 
    arrange(desc(n))

ggplot(data=movies)+
geom_density(aes(log(revenue), group=original_language, color=original_language))

movies <- movies %>% mutate(orig_lang_en=ifelse(original_language == "english",original_language,'other'))

movies <- movies %>% 
    mutate(orig_lang2=ifelse(original_language %in% c("en","fr","in","ru","es","ja"),original_language,'other'))

### differences between the average revenue by language?
movies %>% 
    group_by(orig_lang2) %>% 
    summarise(revenue_avg=mean(revenue,na.rm=T), revenue_sd=sd(revenue,na.rm=T),n=n(), q=max(revenue,na.rm=T)) %>% 
    arrange(desc(n))

ggplot(data=movies) +
    geom_density(aes(log(revenue), group=orig_lang2, color=orig_lang2))

### popularity
ggplot(data=movies) +
    geom_density(aes(popularity))

### popularity
ggplot(data=movies) +
    geom_point(aes(x=id, y=popularity)) +
    geom_hline(yintercept = 25,color="red")

### popularity
ggplot(data=movies) +
    geom_boxplot(aes(y = popularity))

plot(movies$revenue ~ movies$popularity)

# release_date
library(lubridate)



movies$release_date <- mdy(movies$release_date)

summary(movies)


c(min(movies$release_date,na.rm=T),max(movies$release_date,na.rm=T))

table(year(movies$release_date))

movies$release_date <- as_date(ifelse(year(movies$release_date)>2020,
       ymd(as.character(((year(movies$release_date)-100)*10000) + (month(movies$release_date)*100) + day(movies$release_date))),
       movies$release_date))

c(min(movies$release_date,na.rm=T),max(movies$release_date,na.rm=T))

table(year(movies$release_date))
plot(table(year(movies$release_date)),ylab="Number of movies",xlab="year",main="Movies by year")

table(month(movies$release_date))
plot(table(month(movies$release_date)), ylab="Number of movies",xlab="month",main="Movies by month")
abline(h=mean(table(month(movies$release_date)),na.rm=T),col="red")

table(day(movies$release_date))
plot(table(day(movies$release_date)), ylab="Number of movies",xlab="day of the month",main="Movies by day of the month")
abline(h=mean(table(day(movies$release_date)),na.rm=T),col="red")

movie.time <- movies %>% 
    mutate(mv_year=year(release_date),mv_month=month(release_date)) %>%
    group_by(mv_year,mv_month) %>%
    summarise(n=n(),revenue_mean=mean(revenue,na.rm=T)) %>%
    select(mv_year,mv_month,n,revenue_mean) %>%
    filter(between(mv_year,1985,2016))

movie.time$revenue_mean <- ifelse(is.na(movie.time$revenue_mean),0,movie.time$revenue_mean)

movie.ts <- ts(movie.time$revenue_mean, start = c(1995, 1), end=c(2016,12), frequency = 12)

#ggplot() +
#    geom_line(aes(revenue_mean))


movie.ts

plot(movie.ts)

plot(decompose(movie.ts))

movie.dec <- decompose(movie.ts)


movie.dec$seasonal

montly_trend <- data.frame(month=c(1:12),season=movie.dec$seasonal[1:12])
montly_trend

movie.dec$trend

## runtime
summary(movies$runtime)
table(movies$runtime)
plot(table(movies$runtime))

### there are 21 with zero th at must be transformed to NA
movies$runtime_cat <- factor(ifelse(movies$runtime==0, NA, ifelse(movies$runtime<94,1,ifelse(movies$runtime>=118,3,2))),
                             levels=c(1,2,3),labels=c("Short","medium","large"))
summary(movies$runtime_cat)


### status
table(movies$status)

# tagline
length(unique(movies$tagline))
head(movies$tagline,10)

dbListTables(con)

actors <- dbReadTable(con,"movie_cast")
head(actors)

actors_dim <- dbReadTable(con,"actors_dim")
head(actors_dim)

summary(actors)

table(actors$order)

famous <- actors %>% filter(order<5) %>% group_by(id) %>% tally() %>% arrange(desc(n)) %>% filter(n>=30)
head(famous)
nrow(famous)

famous_sw <- famous
famous_sw$sw_famous <- 1
famous_sw$n <- NULL

famous <- left_join(famous, actors_dim[,1:3])
summary(famous)
head(famous,20)

actors <- left_join(actors, actors_dim[,1:3])
head(actors)

actors <- left_join(actors, famous_sw)
actors$sw_famous[which(is.na(actors$sw_famous)==TRUE)] <- 0
head(actors)

### order by sex
actors %>% filter(order < 3) %>% group_by(order, gender) %>% tally()

actor_gender <- actors %>% 
    group_by(movie_id) %>% 
    mutate(male=ifelse(gender==2,1,0),female=ifelse(gender==1,1,0),unk=ifelse(gender==0,1,0)) %>%
    summarize(male=sum(male,na.rm=T),female=sum(female,na.rm=T), unk=sum(unk,na.rm=T),n=n(),
              male_pct=sum(male,na.rm=T)/n(),female_pct=sum(female,na.rm=T)/n(), unk_pct=sum(unk,na.rm=T)/n(),
              sw_famous=max(sw_famous),famous_cnt=sum(sw_famous))

head(actor_gender)

summary(actor_gender)

productors <- dbReadTable(con,"movie_produtors")
head(productors)

productor_cnt <- productors %>% group_by(id) %>% tally() %>% arrange(desc(n))
head(productor_cnt)

summary(productor_cnt)

productor_cnt$productor_cat <- factor(ifelse(productor_cnt$n>50,1,
                                      ifelse(productor_cnt$n>5,2,3)),
                                      levels=c(1,2,3),labels=c("Star","medium","low"))
summary(productor_cnt)
                                      

crew <- dbReadTable(con,"movie_crew")
head(crew)

crew %>% group_by(job) %>% tally() %>% arrange(desc(n))

crew_cnt <- crew %>% group_by(id) %>% tally() %>% arrange(desc(n))
head(crew_cnt)

crew_cnt$crew_cat <- factor(ifelse(crew_cnt$n>50,1,
                            ifelse(crew_cnt$n>5,2,3)),
                            levels=c(1,2,3),labels=c("Star","medium","low"))
summary(crew_cnt)

crew_jobs <- crew %>% group_by(movie_id,job) %>% tally() %>%
    group_by(movie_id) %>% summarise(crew_count=n())
head(crew_jobs)

lang <- dbReadTable(con,"movie_languages")
head(lang)

lang_cnt <- lang %>% group_by(movie_id) %>% summarise(lang_n=n())
head(lang_cnt)
summary(lang_cnt)

table(lang_cnt$lang_n)
barplot(table(lang_cnt$lang_n),main="Number of languages by movies",xlab="# languages",ylab="frequency")

countries <- dbReadTable(con,"movie_countries")
head(countries)

country_cnt <- countries %>% group_by(movie_id) %>% tally()
head(country_cnt)

table(country_cnt$n)

country_cnt$country_num <- factor(ifelse(country_cnt$n > 4,5,country_cnt$n), 
                                  levels=c(1,2,3,4,5),
                                  labels=c("1","2","3","4","5+"))
summary(country_cnt)

genre <- dbReadTable(con,"movies_genres")
genre2 <- dbReadTable(con,"genres_dim")
genre <- left_join(genre,genre2)
gncat <- genre2$name
rm(genre2)
head(genre)

gncat

#gncat <- paste("genre",gsub(" ", "_", gncat),sep="_")
#gncat

genre %>% group_by(name) %>% tally()

genre %>% filter(name=="TV Movie")

movies %>% filter(id==1694)

genre$name[which(genre$name=="TV Movie")] <- "Science Fiction"
gncat <- gncat[1:length(gncat)-1]

genre %>% group_by(name) %>% tally()

genre2 <- genre
for(x in gncat) {
    categ <- paste("genre",gsub(" ", "_", x),sep="_")
    genre2[categ] <- ifelse(genre2$name==x,1,0)
}
head(genre2)

collect <- dbReadTable(con,"movie_collection")
collect2 <- dbReadTable(con,"collection_dim")
collect <- left_join(collect,collect2)
head(collect)

dim(collect2)
head(collect2)

collect %>% filter(is.na(name)==FALSE) %>% tally()

keywd <- dbReadTable(con,"movie_keywords")
keywd2 <- dbReadTable(con,"keywords_dim")
keywd <- left_join(keywd,keywd2)
head(keywd)

## number of movies by keyword
keywd %>% group_by(name) %>% tally() %>% arrange(desc(n)) %>% head(30)



library(wordcloud)
kw <- keywd %>% group_by(name) %>% tally() %>% arrange(desc(n))
wordcloud(words = kw$name, freq = kw$n, min.freq = 1,           
             max.words=200, random.order=FALSE, rot.per=0.35,            
             colors=brewer.pal(8, "Dark2"))


## number of keywords by movie
keywd %>% group_by(movie_id) %>% tally() %>% arrange(desc(n)) %>% head(20)

scatter.smooth(keywd %>% group_by(movie_id) %>% tally() %>% arrange(desc(n)))

keywd %>% filter(movie_id==1794) %>% select(name)

movies %>% filter(id == 1794)

summary(movies$revenue)
hist(movies$revenue)

hist(log(movies$revenue+1))

hist(sqrt(movies$revenue+1))

summary(movies$budget)
hist(movies$budget)

hist(log(movies$budget+1))

hist(sqrt(movies$budget+1))

### Top ten by year...
top10 <- movies %>% mutate(yr=year(release_date)) %>% select(id,yr, revenue) %>% group_by(yr) %>% 
    arrange(yr,desc(revenue)) %>%top_n(10,revenue)
tail(top10,11)

top10$top10 <- 1
movies <- left_join(movies, top10[,c(1,4)])

movies$top10[which(is.na(movies$top10)==TRUE)] <- 0

table(movies$top10)

## top10 vs other vars
ggplot(data=movies) +
    geom_density(aes(x=budget, group=top10, color=top10))

boxplot(movies$budget ~ movies$top10)

ggplot(data=movies) +
    geom_density(aes(x=popularity, group=top10, color=top10))

ggplot(data=movies) +
    geom_density(aes(x=runtime, group=top10, color=top10))
