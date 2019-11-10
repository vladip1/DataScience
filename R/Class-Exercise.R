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

test<-1:10000




res<-NULL
for (x in 1:length(a))
{
  res<-c(res, a[x], b[x])
  
}
res







