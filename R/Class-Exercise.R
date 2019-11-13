a<-seq(10,20,2)

aa<-c('q','w','e','r')

aaa<-letters[seq(2,20,3)]

class(aa)

f<-c(1,0,0,1,1)

ff<-factor(f, levels=c(0,1), labels=c("YES", "NO"))


ff

summary(ff)

help(objects)


objects()

ls()


Rank<-1:5
Peak<-c(1,1,3,4,5)
Title<-c("Avatar","Titanic","Star Wars:The Force Awakens","Avengers:Infinity War 1", "Jurassic World")
Gross<-c(2121221,123213213,123213,213213213,2132133)
Year<-c(2009,1997, 2015, 2018, 2015)

df <- data.frame(Rank,
                Peak,
                  Title,
                   Gross,
                   Year,
                   stringsAsFactors = F)


df[2,3]

df[2,'Titanic']


sqrt(df$mpg)
log(df$disp)
df$wt^3

s1<-c("age", "gender", "height", "weight")

ss<-paste("+", s1, sep=":")
s1<-paste("+", s1, collapse = ";")


length(ss)
length(s1)




m1 <- matrix(c(4,7,-8,3,0,-2,1,-5,12,-3,6,9), ncol=4)

colMeans(m1)
rowMeans(m1)

mean(m1[1,])
mean(m1[2,])
mean(m1[3,])

mean(m1[,1])
mean(m1[,2])
mean(m1[,3])
mean(m1[,4])

mean(m1)

?log(10)
exp(1)



for (x in length(LETTERS):1)
{
  print(LETTERS[x])
}

repeat{
  x<-round(runif(n=1, min=0, max=10),0)  ## 10 random numbers between 0 and 1
  
  if (x != 8) {print(x)} else {break}
  
}



while (TRUE) {
  
  x<-round(runif(n=1, min=0, max=10),0)  ## 10 random numbers between 0 and 1
  
  if (x != 8) {print(x)} else {break}
  
}

for (z in 1:100000)
{
  x<-round(runif(n=1, min=0, max=10),0)  ## 10 random numbers between 0 and 1
  
  if (x != 8) {print(x)} else {break}
  
}

a<-c('well', 'you', 'merged', 'vectors', 'one')
b<-c('done', 'have', 'two', 'into', 'phrase')

paste(a,b,collapse = " ")


res<-NULL
for (x in 1:length(a))
{
  res<-c(res, a[x], b[x])
  
}
res

require("dplyr")
require("nycflights13")
if (require("nycflights13")) {
  carriers <- group_by(flights, carrier)
  summarise(carriers, n())
  mutate(carriers, n = n())
  filter(carriers, n() < 100)
}

require(dplyr)

library(dplyr)

nasa1 <- as_tibble(nasa)

class(nasa1)

nasa1 %>%
  filter(29.56< lat & lat <= 33.09 & -110.93 < long & long <= -90.55) %>%
  mutate(temp_ratio = temperature/surftemp) %>%
  group_by(year) %>% 
  summarise(pressure_average = mean(pressure,na.rm=TRUE),
            pressure_sd = sd(pressure,na.rm=TRUE),
            temp_ratio_average = mean(pressure,na.rm=TRUE),
            temp_ratio_sd = sd(pressure,na.rm=TRUE),
            ozone_average = mean(ozone,na.rm=TRUE),
            ozone_sd = sd(ozone,na.rm=TRUE)) %>%
  arrange(desc(ozone_average))
            

mysw <- starwars %>%
  group_by(homeworld) %>% 
  mutate(male = ifelse(gender == "male",1,0),
         female = ifelse(gender == "female", 1,0),
         nogender = ifelse(is.na(gender)==T,1,0),
         attack_of_clones = ifelse("Attack of the Clones" %in% films,1,0)) %>%
  filter(attack_of_clones == 1) %>%
  summarise(height_min=min(height,na.rm=TRUE),
            height_mean=mean(height,na.rm=TRUE),
            height_max=max(height,na.rm=TRUE),
            mass_min=min(mass,na.rm=TRUE),
            mass_mean=mean(mass,na.rm=TRUE),
            mass_max=max(mass,na.rm=TRUE),
            males = sum(male, na.rm=TRUE),
            females = sum(female, na.rm=TRUE),
            nogender = sum(nogender, na.rm=TRUE),
            num_individuals=n()) %>%
  arrange(desc(num_individuals))




