setwd("E:/DATA ANALYTICS JOURNEY/R Edvancer/PROJECT 2 RETAIL")
store_train=read.csv("store_train.csv",stringsAsFactors = F)
store_test=read.csv("store_test.csv",stringsAsFactors = F)

store_test$store=NA

store_train$data='train'
store_test$data='test'
store_all=rbind(store_train,store_test)
library(dplyr)
glimpse(store_all)

##if you look at variables storecode n statealpha, you will notice that it has lot more unique
#values,having none of the individual values having frequency higher than a good number , we'll drop those
#vars.
store_all=store_all %>% select(-storecode-state_alpha)

#We'll create dummies for rest
char_logical=sapply(store_all,is.character)
cat_cols=names(store_all)[char_logical]
cat_cols=cat_cols[!(cat_cols %in% c('data','store'))]
cat_cols

library(dplyr)

for(col in cat_cols){
  store_all=CreateDummies(store_all,col,50)
}

CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

store_all=store_all[!((is.na(store_all$store)) & store_all$data=='train'), ]
for(col in names(store_all)){
  if(sum(is.na(store_all[,col]))>0 & !(col %in% c("data","store"))){
    store_all[is.na(store_all[,col]),col]=mean(store_all[store_all$data=='train',col],na.rm=T)
  }
}

store_train=store_all %>% filter(data=='train') %>% select(-data)
store_test=store_all %>% filter(data=='test') %>% select(-data,-store)

#lets remove vars which have redundant information first on the basis of vif
for_vif=lm(store~.-Id,data=store_train)
library(car)
sort(vif(for_vif),decreasing = T)[1:3]

summary(for_vif)
## i removed aroostookCounty and 3 others as it has NA in estimates when i ran summary function
#now i can do VIF removal successfully
for_vif=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
           -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
           -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea,data=store_train)
library(car)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
           -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
           -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0,data=store_train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
           -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
           -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2,data=store_train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
           -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
           -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3,data=store_train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
           -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
           -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-State,data=store_train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
           -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
           -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1,data=store_train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
           -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
           -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1-State,data=store_train)
sort(vif(for_vif),decreasing = T)[1:3]

##lets build our model now that all VIFs are below 5 with removing high p-values
rm(for_vif)
fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1-State,data=store_train)
fit=step(fit)

summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty
       -state_alpha_IL,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty
       -state_alpha_IL-state_alpha_WI,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty
       -state_alpha_IL-state_alpha_WI-state_alpha_NY,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty
       -state_alpha_IL-state_alpha_WI-state_alpha_NY-countyname_WorcesterCounty,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty
       -state_alpha_IL-state_alpha_WI-state_alpha_NY-countyname_WorcesterCounty-state_alpha_SD,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty
       -state_alpha_IL-state_alpha_WI-state_alpha_NY-countyname_WorcesterCounty-state_alpha_SD
       -CouSub,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty
       -state_alpha_IL-state_alpha_WI-state_alpha_NY-countyname_WorcesterCounty-state_alpha_SD
       -CouSub-country,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty
       -state_alpha_IL-state_alpha_WI-state_alpha_NY-countyname_WorcesterCounty-state_alpha_SD
       -CouSub-country-storecode_METRO12620N23019,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty
       -state_alpha_IL-state_alpha_WI-state_alpha_NY-countyname_WorcesterCounty-state_alpha_SD
       -CouSub-country-storecode_METRO12620N23019-countyname_MiddlesexCounty,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty
       -state_alpha_IL-state_alpha_WI-state_alpha_NY-countyname_WorcesterCounty-state_alpha_SD
       -CouSub-country-storecode_METRO12620N23019-countyname_MiddlesexCounty-state_alpha_MT,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty
       -state_alpha_IL-state_alpha_WI-state_alpha_NY-countyname_WorcesterCounty-state_alpha_SD
       -CouSub-country-storecode_METRO12620N23019-countyname_MiddlesexCounty-state_alpha_MT
       -countyname_AroostookCounty,data=store_train)
summary(fit)

fit=lm(store~.-Id-Areaname_AroostookCountyME-storecode_NCNTY23003N23003
       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-state_alpha_FL-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore-state_alpha_CT-state_alpha_VA-state_alpha_OK-state_alpha_KS
       -state_alpha_NC-state_alpha_IA-state_alpha_TX-state_alpha_MO-state_alpha_NE
       -state_alpha_KY-state_alpha_MN-state_alpha_MI-state_alpha_MS-state_alpha_PA
       -state_alpha_AR-state_alpha_OH-countyname_WashingtonCounty-countyname_FranklinCounty
       -state_alpha_IL-state_alpha_WI-state_alpha_NY-countyname_WorcesterCounty-state_alpha_SD
       -CouSub-country-storecode_METRO12620N23019-countyname_MiddlesexCounty-state_alpha_MT
       -countyname_AroostookCounty-state_alpha_AL,data=store_train)
summary(fit)

##-----------predict ur model on test data-----------------------------------
test.probs=predict(fit,newdata=store_test,type='response')
write.csv(test.probs,'Omkar_Sawant_P2_part2.csv',row.names = F)

##------------------------------------Quiz----------------------------------------------------------------------------------
#(1) - what is the total sales (sum of all sales) of Supermarket Type1 in area Kennebec County, ME? 
glimpse(store_train)
store_train %>%
  filter(Areaname =="Kennebec County") %>%
  filter(store_Type =="Supermarket Type1") %>%
  summarise(Sales = sum(sales0,sales1,sales2,sales3,sales4))

#Ans - 38680

#(2) - Should storecode be included in building models?
#Ans - No

#(3) -  should country be treated as numeric type or character?
#Ans - character

#(4) - Find out number of unique categories of variable Areaname
unique(store_train$Areaname)

#Ans - 1891

#(5) - For store type grocery store what is the response rate ? 
#[ what % of obs have response value as 1 ]  Round off to two decimal digits. 

store_train %>%
  filter(store_Type =="Grocery Store") %>%
  filter(store ==0)

#Ans - 42.13
  
#(6) - Do all the sales variable follow normal distribution?
library(ggplot2)
ggplot(store_train,aes(x=sales0,y))+geom_histogram()

#Ans - No

#(7) - Number of outliers for total sales based on following limits (q1-1.5*IQR, q3+1.5*IQR)?
#Ans - NA

#(8) - which store type has maximum variance in total sales?
#Ans - Grocery Store

#(9) - How many dummies will you create for variable state_alpha?
unique(store_train$state_alpha)
#Ans - 54  as unique so n-1 variables  will be created by you

#(10) - What should be the type of categorical variable when using the function randomForest?
#Ans - NA (maybe nominal/ ordinal)
