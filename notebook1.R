library(openxlsx)
library(tidyverse) #fill is part of tidyr
library(dplyr)

#reading main df
df_data = read.xlsx(xlsxFile = "Assignment_1_data.xlsx", sheet = "data")
df_type = read.xlsx(xlsxFile = "Assignment_1_data.xlsx", sheet = "type")

#sample
df_sample = df_data %>% slice(1:100)
write.csv(df_sample,"sample_100.csv")

#check for columns with nan values
na_cols = apply(is.na(df_sample), 2, any)
print(colnames(df_sample)[na_cols])


#check for codes of all column names
codes = read.xlsx(xlsxFile = "Data.xlsx", sheet = "Sheet1")
codes_tbls_simple = codes%>%fill(Description)
#getting description list
codes_keys = codes%>%fill(Description)%>%group_keys(Description)

for (keys in as.list(codes_keys)$Description) {
  #getting all column names grouped by description key as list
  tbls = codes_tbls_simple[codes_tbls_simple$Description==keys,]
  var_cols=as.list(tbls$Name)
  for (i in 1:length(var_cols)) {
    #getting all size of combinations from 1 to total columns within a description 
    com = combn(var_cols, i)
    num_com=dim(com)[2]
    for (j in 1:num_com) {
      #individual combination of all possible sizes within description
      inv_com =com[,j]
      print(inv_com)
    }
  }
}