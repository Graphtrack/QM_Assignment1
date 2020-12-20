library(openxlsx)
library(tidyverse) #fill is part of tidyr
library(dplyr)
library(caret)
library(modelsummary)

#function to import selected columns of data frame
Takedf = function(df,x,y)
{
  all_cols = c(x,y)
  df_abc = df %>% select(all_cols)
  return(df_abc)
}

#function to drop NA
Nadrop = function(df)
{
  df_abc = df %>% drop_na()
  return(df_abc)
}



#function to regression
Regress = function(df,x,y,ci)
{
  model_list=c()
  for (key in y) {
    #getting all column names grouped by description key as list
    for (i in 1:length(x)) {
      #getting all size of combinations from 1 to total columns within a description 
      com = combn(x, i)
      num_com=dim(com)[2]
      for (j in 1:num_com) {
        #individual combination of all possible sizes within description
        inv_com =com[,j]
        lm_formula=paste0(key,'~',inv_com)
        models = lm(formula = as.formula(lm_formula), data = df)
        if (summary(models)$adj.r.squared > ci){
          model_list = append(model_list,models)
          print(summary(models)) 
          print(key)
        }
        
      }
    }
  }
  return(model_list)
}


df_input = read.xlsx(xlsxFile = "Assignment_1_data.xlsx", sheet = "data")

df_filter = df_input %>% filter(stateid2 %in% c(102,105,101,203,206,207))
#df_sample = read.csv("sample_100.csv")



x = c("hhed5m","hhed5f","hhed5adult","urban","stateid2", "ro5")

y = c("income")

df_select = Takedf(df_filter,x,y)
df_select = df_select %>% mutate_all(na_if," ")
if("metro6" %in% x){
  df_select[["metro6"]][is.na(df_select[["metro6"]])] = 0
}
df_select = Nadrop(df_select)

#numerical values to be taken categorical
df_select[,c("urban","stateid2")]= lapply(df_select[,c("urban","stateid2")],factor)

#one hot encoding
dmy = dummyVars(" ~. ", data = df_select)
final_df = data.frame(predict(dmy, newdata = df_select))
head(final_df)

x_new = colnames(final_df)[!colnames(final_df) %in% y]
print(x_new)

cobn = Regress(final_df,x_new,y,0.3)
#modelsummary(cobn, output = 'table.docx')




#check for columns with nan values
#na_cols = apply(is.na(df_select), 2, any)
#print(colnames(df_sample)[na_cols])


#check for codes of all column names
#codes = read.xlsx(xlsxFile = "Data.xlsx", sheet = "Sheet1")
#codes_tbls_simple = codes%>%fill(Description)
#getting description list
#codes_keys = codes%>%fill(Description)%>%group_keys(Description)
