#install.packages("xlsReadWrite")
#require(xlsReadWrite)
#xls.getshlib()
#df = read.xls("myfile.xls", sheet = 1)

install.packages("RODBC")
require(RODBC)
conn = odbcConnectExcel("../proj/BoxOffice - Data Retrieval Protocol.xlsx")
sqlTables(conn)$TABLE_NAME # show all sheets
df = sqlFetch(conn, "Sheet1") # read a sheet
df = sqlQuery(conn, "select * from [Sheet1 $]") # read a sheet (alternative SQL sintax)
close(conn) # close the connection to the file


install.packages("XLConnect")
require("tidyverse")

require(XLConnect)

wb = loadWorkbook("../project/BoxOffice - Data Retrieval Protocol.xlsx")

protocol = readWorksheet(wb, sheet = "protocol", header = TRUE)

head(protocol)


val_types <- protocol$Value.type

dim(protocol)

booleanEDA <- function(data, fun_xlab) {
  
  print(summary(data))
  
  barplot(prop.table(table(data)),main=fun_xlab,xlab=fun_xlab)
  
  print(table(data))
  
  ggplot(data=movies)+
    geom_density(aes(log(revenue), group=data, color=data))
}

head(protocol)

categoricalEDA <- function(data) {
  summary(movies$sw_lang_en)
  barplot(prop.table(table(movies$sw_lang_en)))
  table(movies$sw_lang_en)
  plot(movies$revenue ~ movies$sw_lang_en)
  ggplot(data=movies)+
    geom_density(aes(log(revenue), group=movies$sw_lang_en, color=movies$sw_lang_en))
}


numericEDA <- function(data, column_name) {

    print(column_name)
    print(summary(data[column_name]))
  
    cat(sprintf("Data.Type: %s\n", protocol$Data.type[str_trim(protocol$Feature.name) == column_name]))
    cat(sprintf("Unique.count: %s\n", protocol$Unique.count[str_trim(protocol$Feature.name) == column_name]))
    

    if (protocol$Data.type[str_trim(protocol$Feature.name) == column_name] == "Decimal" ||
        protocol$Data.type[str_trim(protocol$Feature.name) == column_name] == "Integer") 
    {
      print("plot")
      plot(movies$revenue ~ as.numeric(unlist(movies[column_name])), xlab=column_name)
      
      print("boxplot")
      boxplot(data[column_name])
    } 
    else
    {
      print("barplot")
      barplot(prop.table(table(data[column_name])))
    }
    
    if (strtoi(str_trim(protocol$Unique.count[str_trim(protocol$Feature.name) == column_name])) < 10) {
      print("density")
      ggplot(data)+
        geom_density(aes(log(revenue), group=data[column_name], color=data[column_name]))
    }

}


numericEDA(movies, "budget")
numericEDA(movies, "genre_drama")

plot(movies$revenue ~ as.numeric(unlist(movies[column_name])))

typeof(movies$revenue)

  
for (n in 1:2) {#nrow(protocol)){
  if (protocol$Value.type[n] == "Numeric") {
    print("NUM")
    numericEDA(movies, str_trim(protocol$Feature.name[n]))
    #print(protocol$Feature.name[n])
  } else if (protocol$Value.type[n] == "Categorical") {
    print("CAT")
  }
}

movies["budget"]
    


summary(df)

df$Value.type


load("../data/BoxOffice_ff.RData")

summary(movies)

head(movies)



