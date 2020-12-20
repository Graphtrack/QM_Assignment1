#install.packages(c("openxlsx","tidyverse","dplyr","caret","modelsummary","broom.mixed","plyr"))
library(openxlsx)
library(tidyverse) #fill is part of tidyr
library(dplyr)
library(caret)

p = function(..., sep='_') {
  paste(..., sep=sep, collapse=sep)
}

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
  setwd("./results")
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
          print(paste(key,p(inv_com)))
          print(summary(models)$adj.r.squared)
          sink(paste(key,p(inv_com), ".csv"))
          print(summary(models)) 
          sink()
        }
        
      }
    }
  }
  setwd("..")
  return()
}



main_func = function(df_select,x,y,num_cat_cols,ci){
  
  df_select = Takedf(df_input,x,y) #change to filtered if applying filters
  df_select = df_select %>% mutate_all(na_if," ")
  if("metro6" %in% x){
    df_select[["metro6"]][is.na(df_select[["metro6"]])] = 0
  }
  df_select = Nadrop(df_select)
  
  #numerical values to be taken categorical
  if (length(num_cat_cols)>0)
  {
    df_select[,num_cat_cols]= lapply(df_select[,num_cat_cols],factor)
    
  }
  
  
  #one hot encoding
  dmy = dummyVars(" ~. ", data = df_select)
  final_df = data.frame(predict(dmy, newdata = df_select))
  
  x_new = colnames(final_df)[!colnames(final_df) %in% y]
  
  
  cobn = Regress(final_df,x_new,y,ci)
  
  
}




x = c("urban","stateid2","Npersons","Nchildren","Nteens","ro3","ro5","ed5")

y = c("income")

num_cat_cols = c("stateid2","urban","ro3")

df_input = read.xlsx(xlsxFile = "Regression1Data.xlsx", sheet = "Sheet1")
#df_filtered = df_input %>% filter()

main_func(df_input,x,y,num_cat_cols,0.08943)