####################################################
## This is part of the code I wrote to process and analyze data for one of my academic paper, part of the code is deliberately removed.
##
## Background: Appliance of Nitrogen fertilizer during crop production process can lead to environmental risk. Global trade, however, enables countries 
##             to increase agricultural products consumption without paying for environmental cost. Such trade links can either have negative or positive 
##             influence on global nitrogen pollution. We thus try to measure this effect in our paper.
##             This algorithm is used to manipulate trade data and calculate the most important indicator variable -- Global Nitrogen Savings.
## Input: global crop trade data from 1986 to 2015, production data and Nitrogen balance data during same period.
## Main Output: adjusted trade result (in STEP 3) and calculated saving result (in STEP 4).
## Last edit by Ruoshui, at Dec.29th, 2019.
####################################################

library(dplyr)
library(Matrix)
library(MASS)
library(tidyr)

raw_data <- read.csv('/total_trade_data_use.csv')
raw_data <- raw_data %>% 
  select(Element,Crop,Partner.Country.Code,Reporter.Country.Code,Value,Year) %>%
  mutate_at(.vars=c(2,3,4,6),.fun=as.factor) %>%
  mutate_at(.vars=5,.fun=as.numeric) %>%
  rename_at(.vars=c(1:6),~c('Element','Crop','coun1','coun2','Trade','Year'))
# check
# length(unique(raw_data$Crop))



######## STEP 1 ########
######## Use import data to fill export data ########
# Due to limitation of raw data, different numbers can be reported by importer and exporter of the same trade link,
raw_im <- raw_data %>% 
  filter(Element=='Import Quantity') %>% 
  select(Crop,coun1,coun2,Trade,Year) %>%
  rename(Exporter=coun1,Importer=coun2)

raw_ex <- raw_data %>% 
  filter(Element=='Export Quantity') %>% 
  select(Crop,coun1,coun2,Trade,Year) %>%
  rename(Importer=coun1,Exporter=coun2)

# This step is to merge exporter database and importer database: 
# for those trade links which have reported data from both sides, we uniformly choose export data; 
# for those which have only one-side data, the value is recorded in final trade database.
result <- raw_ex %>% 
  full_join(raw_im,by=c('Exporter','Importer','Crop','Year')) %>% 
  mutate(Trade=ifelse(is.na(Trade.x),Trade.y,Trade.x)) %>%
  select(Exporter,Importer,Crop,Year,Trade) %>%
  mutate_at(.vars=c(1,2),.fun=as.factor)



######## STEP 2 ########
######## Calculate equivalent trade data ########
# Countries can export both primary crops (like soybeans) and secondary products (like soy sauce). 
# This step is to calculate equivalent trade of primary products.
trans_factor <- read.csv('/fao_tcf.csv')
trans_factor <- trans_factor %>%
  rename_at(.vars=c(1:5),~c('Category','level1','tf1','Crop','tf2')) %>%
  mutate_at(.vars=c(1,2,4),.fun=as.factor) %>%
  mutate_at(.vars=c(3,5),.fun=as.numeric)

# First level calculation: third level products trade data transform to secondary products.
me1 <- result %>% 
  left_join(tran[,c('Crop','tf2','level1')], by='Crop') %>% 
  mutate_at(.vars=3,.funs=as.factor) %>%
  mutate(equivalent.trade1=Trade/tf2) %>% 
  mutate(equivalent.trade1=if_else(is.na(equivalent.trade1),Trade,equivalent.trade1)) %>%
  mutate(level1=if_else(is.na(level1),Crop,level1)) %>%
  select(Exporter,Importer,Year,level1,equivalent.trade1) %>%
  group_by(.dots=c('Exporter','Importer','Year','level1')) %>%
  summarise(Trade=sum(equivalent.trade1)) %>%
  data.frame()

trans_factor2 <- trans_factor %>%
  rename('level0'=Category, 'Crop'=level1,'level2'=Crop) %>%
  select(level0, Crop, tf1) %>%
  distinct()

# Second level calculation: secondary products trade data transform to primary products.
me2 <- me1 %>%
  rename('Crop'=level1) %>%
  left_join(tran2, by='Crop') %>%
  mutate_at(.vars=4,.funs=as.factor) %>%
  mutate(equivalent.trade0=Trade/tf1) %>%
  mutate(equivalent.trade0=if_else(is.na(equivalent.trade0),Trade,equivalent.trade0)) %>%
  mutate(level0=ifelse(is.na(level0),Crop,level0)) %>%
  select(Exporter,Importer,Year,level0,equivalent.trade0) %>%
  group_by(.dots=c('Exporter','Importer','Year','level0')) %>%
  summarise(Equivalent.trade=sum(equivalent.trade0)) %>%
  rename('Crop'=level0) %>%
  data.frame()

#write.csv(me2,'/Ultimate_trade_data.csv')



######## STEP 3 ########
######## Adjust trade data using origin-tracing algorithm ########
# Countries can import primary crops and then export secondary crops after processing.
# This small algorithm is to deal with 're-export' problems and assign crop trade to its origin production country.
df_pro <- read.csv('/Production_estimate.csv')
df_trade <- read.csv('/Ultimate_trade_data.csv')
df_pro <- df_pro %>%
  select(Exporter,Year,Crop,Production) %>%
  filter(!is.na(Production), Production!=0) %>%
  mutate_at(.vars=c(1:3),.fun=as.factor)
df_trade <- df_trade %>%
  select(Exporter,Importer,Year,Crop,Equivalent.trade) %>%
  mutate_at(.vars=c(1:4),.fun=as.factor)  


# Function used to pre-process trade data
make_matrix <- function(df_trade,df_pro,category,year){
  pro_data <- filter(df_pro,Crop==category,Year==year)
  country_list <- unique(pro_data$Exporter)
  pro_list=pro_data[,c('Exporter','Production')]
  trade_data <- filter(df_trade,Crop==category,Year==year,Exporter %in% country_list,Importer %in% country_list)
  
  num <- length(country_list)
  check_trade <- dim(trade_data)[1]
  if (num!=0 & check_trade !=0){
    tra_matrix <- matrix(0,nrow=num,ncol=num,dimnames=list(country_list,country_list))
    for (i in seq(nrow(trade_data))){
      tra_matrix[as.character(trade_data[i,]$Importer),as.character(trade_data[i,]$Exporter)]=trade_data[i,]$Equivalent.trade
    }
  }  
  else {tra_matrix <- matrix(0,nrow=0,ncol=0)}
  return (list(tra_matrix, country_list, pro_list))
}


# Function used to update production data
# Deal with (export) excess (domestic production + import)
trans_pro <- function(tra_matrix, country_list, pro_list){
  pro_list['Production2']=0
  for (country in country_list){
    productions <- filter(pro_list, Exporter==country)$Production
    imports <- sum(tra_matrix[country,])
    exports <- sum(tra_matrix[,country])
    DMI <- productions + imports
    pro_list[pro_list$Exporter==country,]$Production2=if_else(DMI<exports,exports,productions)
  }
  pro_list <- select(pro_list,Production2)
  return (pro_list)
}


# Function used to apply Origin-tracing algorithm
# More detail see Kastner et.al.(2011)
clean_estimate <- function(pro_list,tra_matrix,export.form= "ij",estimator="kastner.method",rule="equal",rounding=0){
  if (is.null(pro_list)) stop("Production data must be provided")
  if (is.null(tra_matrix)) stop("Trade data must be provied")
  if (dim(pro_list)[1] != dim(tra_matrix)[1]) stop("Dimensions are not comformable")
  n <- dim(pro_list)[1]
  pro_list <- as.matrix(pro_list)
  tra_matrix <- as.matrix(tra_matrix)
  if(export.form =="ij"){
    Z <- tra_matrix
  } else {
    Z <- t(tra_matrix)
  } 

  imports <- rowSums(Z)
  exports <- colSums(Z)
  x <- pro_list + rowSums(Z)
  pro_hat <- diag(as.vector(pro_list), n)
  # first estimate
  R_1 <- pro_hat + Z
  R_1 <- round(R_1,2)
  x_hat <- diag(as.vector(x),n)
  x_hat_reciprocal <- diag(1/as.vector(x),n)
  A <- Z %*% x_hat_reciprocal
  
  # second estimate
  R_2 <- pro_hat + A %*% pro_hat + A %*% (x_hat- pro_hat)
  if (rule =="equal"){
    # consumption share, equal distribution between consumption and exports
    c_hat <- (x- colSums(Z))/x
    R_1 <- round(R_1 * as.vector(c_hat),rounding)
    R_1 <- array(data = R_1,dimnames = dimnames(tra_matrix),dim=dim(tra_matrix))
    R_2 <- round(R_2 * as.vector(c_hat),rounding)
    R_2 <- array(data = R_2,dimnames = dimnames(tra_matrix),dim=dim(tra_matrix))
    x <- array(x,dim = dim(x),dimnames = list(dimnames(tra_matrix)[[1]],"total consumption"))
    exports <- array(exports,dim = dim(as.matrix(exports)),dimnames = list(dimnames(tra_matrix)[[1]],"exports"))
    imports <- array(imports,dim = dim(as.matrix(imports)),dimnames = list(dimnames(tra_matrix)[[1]],"imports"))
  }
  if(estimator=="kastner.method"){
    print("Decomposing bilateral trade flows using Kastner method")
    list(total_comsum <- x,
         trade_flows <- R_2,
         exports <- exports,
         imports <- imports)
  }else{
    print("Rough solution is used")
    list(total_comsum <- x,
         trade_flows <- R.1,
         exports <- exports,
         imports <- imports)
  } 
}


# Implement functions and aggregate calculation results
# Results stored in variable "trade_result"
crop_list=levels(df_trade$Crop)
year_list <- as.character(c(1986:2015))
trade_result <- data.frame(Importer=0,Exporter=0,Trade=0,Crop=0,Year=0)
trade_result <- trade_result[-1,]
for (year in year_list){
  # print(year)
  for (category in crop_list){
    # print(category)
    result1 <- make_matrix(df_trade, df_pro, category, year)
    tra_matrix <- result1[[1]]
    country_list <- result1[[2]]
    pro_list <- result1[[3]]
    print(dim(tra_matrix)[1])
    if (dim(tra_matrix)[1] != 0){
      pro_list2 <- trans_pro(tra_matrix,country_list,pro_list)
      result2 <- clean_estimate(pro_list2,tra_matrix,export.form= "ij",estimator="kastner.method",rule="equal",rounding=0)
      adjust_trade <- result2[[2]]
      cou <- colnames(adjust_trade)
      adjust_trade <- adjust_trade %>% 
        as.data.frame() %>%
        mutate(Importer=cou) %>%
        gather(Exporter,Trade,-Importer) %>%
        filter(!is.na(Trade), Trade!=0) %>%
        mutate(Crop=category, Year=year)
      trade_result <- rbind(trade_result,adjust_trade)
    }
    next
  }
}
write.csv(trade_result,'/adjust_trade_data.csv')



######## STEP 4 ########
######## Calculate indicator variable ########
trade_data <- read.csv('/each_trade.csv')
trade_data <- select(trade_data,Importer,Exporter,Trade,Crop,Year)
# check
# length(unique(trade_data[,'Year']))

pro_data <- read.csv('/each_pro.csv')
pro_data <- pro_data %>%
  select(Exporter,Year,Crop,Production) %>%
  filter(!is.na(Production), Production!=0)

df_Nin <- read.csv('/Nin_sum.csv')
df_Nyeild <- read.csv('/Nyeild_sum.csv')
df_Nin <- df_Nin %>%
  select(Country, Crop, Nin, Year) %>%
  rename('Exporter'=Country)
df_Nyeild <- df_Nyeild %>%
  select(Country, Crop, Nyeild, Year) %>%
  rename('Exporter'=Country)


# Function used to join and calculate
# Final calculation result stored in variable "saving_result"
calculate <- function(trade_data,pro_data){
  ex_merge <- trade_data %>%
    inner_join(pro_data, by=c('Exporter','Crop','Year')) %>%
    inner_join(df_Nin, by=c('Exporter','Crop','Year')) %>%
    left_join(df_Nyeild,by=c('Exporter','Crop','Year')) %>%
    rename_at(.vars=c(6:8),~c('Ex_Production','Ex_Nin','Ex_Nyeild'))
    
  colnames(pro_data) <- c('Importer','Year','Crop','Production')
  colnames(df_Nin) <- c('Importer','Crop','Nin','Year')
  colnames(df_Nyeild) <- c('Importer','Crop','Nyeild','Year')
  
  im_merge <- ex_merge %>%
    inner_join(pro_data, by=c('Importer','Crop','Year')) %>%
    inner_join(df_Nin, by=c('Importer','Crop','Year')) %>%
    left_join(df_Nyeild,by=c('Importer','Crop','Year')) %>%
    rename_at(.vars=c(9:11),~c('Im_Production','Im_Nin','Im_Nyeild'))
  
  calculation_result <- im_merge %>%
    mutate(Exporter_loss=if_else(((Ex_Nin-Ex_Nyeild)*Trade/Ex_Production)<0,0,(Ex_Nin-Ex_Nyeild)*Trade/Ex_Production)) %>%
    mutate(Hypo_loss_of_importer=if_else(((Im_Nin-Im_Nyeild)*Trade/Im_Production)<0,0,(Im_Nin-Im_Nyeild)*Trade/Im_Production)) %>%
    mutate(Saving=Hypo_loss_of_importer-Exporter_loss)
  
  return(calculation_result)
}
saving_result=calculate(trade_data,pro_data)

