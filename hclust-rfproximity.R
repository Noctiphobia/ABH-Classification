if (!require("caret")){
  install.packages("caret")
}
library("caret")
library(Matrix)
if (!require('pROC')){
  install.packages('pROC')
}
library('pROC')
library(xgboost)
library(caret)
library(glmnet)

#zbior treningowy (ten co link do niego wygasl) zamieniony na csv-ke
df = read.csv("data.csv", sep=',', header=TRUE)
train = df[, c(1:71,which(colnames(df) %in% c('Sale.success', 'Contacted.by.CC')))]
#usuwanie ID
ids = which(grepl(".*id.*",colnames(train),ignore.case=T))
tokenids = which(colnames(train) %in% c('calculation_token', "salesforce_lead"))
ids = c(ids, tokenids)
#zamiana integer -> factor
train2 = train[,-ids]
train2$etx_fuel_code = as.factor(train2$etx_fuel_code)
train2$etx_model_code = as.factor(train2$etx_model_code)
train2$protection_scope = as.factor(train2$protection_scope)
train2$child_carriage_frequency = as.factor(train2$child_carriage_frequency)
train2$used_abroad = as.factor(train2$used_abroad)
train2$theft_protection_installation = as.factor(train2$theft_protection_installation)
train2$theft_protection_device_1 = as.factor(train2$theft_protection_device_1)
train2$theft_protection_device_2 = as.factor(train2$theft_protection_device_2)
train2$is_damaged = as.factor(train2$is_damaged)
train2$leasing = as.factor(train2$leasing)
train2$calc_complete = as.factor(train2$calc_complete)
train2$calc_finished = as.factor(train2$calc_finished)
train2$phone_yes = as.integer(train2$phone_yes)
train2$terms_acceptance = as.factor(train2$terms_acceptance)
train2$step = as.factor(train2$step)
train2$sent_to_sf = as.factor(train2$sent_to_sf)
train2$phone_lookup_status = as.factor(train2$phone_lookup_status)
train2$Sale.success = as.factor(train2$Sale.success)
#poprawienie dat
d0 <- as.Date(0, origin="1899-12-30", tz='UTC')

train2$created_at = sapply(as.character(train2$created_at), function(rd){
  if (is.na(rd) | nchar(rd)<5) {NA} else {
    ssplit = strsplit(rd,' ')[[1]]
    a = strsplit(ssplit[1],'-')[[1]]
    aa = as.Date(paste0(which(month.abb==a[2]),'/',a[1],'/',a[3]),'%m/%d/%y')
    numericdate = as.numeric(aa-d0)
    b = strsplit(ssplit[2],':')[[1]]
    dayseconds = 24*60*60
    numerator = as.numeric(b[1])*3600+as.numeric(b[2])*60+as.numeric(b[3])
    numericdate + (numerator/dayseconds)
  }
})

train2$registration_date = sapply(as.character(train2$registration_date), function(rd){
  if (nchar(rd)<5) {NA} else {
    a = strsplit(strsplit(rd,' ')[[1]],'-')[[1]]
    aa = as.Date(paste0(which(month.abb==a[2]),'/',a[1],'/',a[3]),'%m/%d/%y')
    as.numeric(aa-d0)
  }
})

train2$insurance_start_date = sapply(as.character(train2$insurance_start_date), function(rd){
  if (nchar(rd)<5) {NA} else {
    a = strsplit(strsplit(rd,' ')[[1]],'-')[[1]]
    aa = as.Date(paste0(which(month.abb==a[2]),'/',a[1],'/',a[3]),'%m/%d/%y')
    as.numeric(aa-d0)
  }
})

train2$form_finished_at = sapply(as.character(train2$form_finished_at), function(rd){
  if (is.na(rd) | nchar(rd)<5) {NA} else {
    ssplit = strsplit(rd,' ')[[1]]
    a = strsplit(ssplit[1],'-')[[1]]
    aa = as.Date(paste0(which(month.abb==a[2]),'/',a[1],'/',a[3]),'%m/%d/%y')
    numericdate = as.numeric(aa-d0)
    b = strsplit(ssplit[2],':')[[1]]
    dayseconds = 24*60*60
    numerator = as.numeric(b[1])*3600+as.numeric(b[2])*60+as.numeric(b[3])
    numericdate + (numerator/dayseconds)
  }
})

train2$offer_first_at = sapply(as.character(train2$offer_first_at), function(rd){
  if (is.na(rd) | nchar(rd)<5) {NA} else {
    ssplit = strsplit(rd,' ')[[1]]
    a = strsplit(ssplit[1],'-')[[1]]
    aa = as.Date(paste0(which(month.abb==a[2]),'/',a[1],'/',a[3]),'%m/%d/%y')
    numericdate = as.numeric(aa-d0)
    b = strsplit(ssplit[2],':')[[1]]
    dayseconds = 24*60*60
    numerator = as.numeric(b[1])*3600+as.numeric(b[2])*60+as.numeric(b[3])
    numericdate + (numerator/dayseconds)
  }
})

train2$offer_last_at = sapply(as.character(train2$offer_last_at), function(rd){
  if (is.na(rd) | nchar(rd)<5) {NA} else {
    ssplit = strsplit(rd,' ')[[1]]
    a = strsplit(ssplit[1],'-')[[1]]
    aa = as.Date(paste0(which(month.abb==a[2]),'/',a[1],'/',a[3]),'%m/%d/%y')
    numericdate = as.numeric(aa-d0)
    b = strsplit(ssplit[2],':')[[1]]
    dayseconds = 24*60*60
    numerator = as.numeric(b[1])*3600+as.numeric(b[2])*60+as.numeric(b[3])
    numericdate + (numerator/dayseconds)
  }
})

train2 = train2[,-which(colnames(train2) == "created_at_date")]

#zamiana character->factor
#dodanie poziomu NA do factorow
for (i in 1:length(train2)) {
  if (is.character(train2[,i])){
    train2[,i] = as.factor(train2[,i])
  }
  if (is.factor(train2[,i])){
    train2[,i] = addNA(train2[,i])
  }
}

#usuwanie predyktorow z wariancja bliska 0
nzv = nearZeroVar(train2,freqCut=99) #bardzo duży cutoff - 99/1 (by nie obcinać zbyt dużo)
train3 = train2[,-nzv]

#imputacja
numericpreds = which(sapply(train3,class) %in% c('integer','numeric'))
has_NA = sapply(numericpreds,function(npred)any(is.na(train3[,npred])))
numericpreds = numericpreds[has_NA]
for (numericpred in numericpreds){
  is_na_col = is.na(train3[,numericpred])
  train3[which(is.na(train3[,numericpred])),numericpred] = as.integer(mean(na.omit(train3[,numericpred])))
  train3 = data.frame(train3, is_na_col)
  colnames(train3)[ncol(train3)] = paste0("was_na_", colnames(train3)[numericpred])
}
#korelacje
correlated = c("phone_yes","phone_no","phone_acceptance")
traininds = sapply(correlated,function(cc)which(colnames(train3)==cc))
train4 = train3[,-traininds]

postalCodeToRegion = function (vector){
  vector = as.character(vector)
  codes = sapply(vector,function(v){
    if (is.na(v) | (substr(v,3,3) != '-') | (nchar(v) != 6)){
      NA
    } else {
      aa = substr(v,1,2)
      a = substr(aa,1,1)
      if (a == '0'){
        if (aa %in% c('00','01','02','03','04','05')){
          'Warsaw'
        } else {
          'Warsaw Region'
        }
      }else if (a == '1'){
        if (aa == '10'){
          'Olsztyn'
        } else if (aa=='15'){
          'Bialystok'
        } else {
          'Olsztyn Region'
        }
      }else if (a == '2'){
        if (aa == '20'){
          'Lublin'
        } else if (aa=='25'){
          'Kielce'
        } else if (aa=='26'){
          'Radom'
        } else {
          'Lublin Region'
        }
      }else if (a == '3'){
        if (aa %in% c('30','31')){
          'Krakow'
        } else if (aa=='35'){
          'Rzeszow'
        } else {
          'Krakow Region'
        }
      }else if (a == '4'){
        if (aa =='40'){
          'Katowice'
        } else if (aa=='45'){
          'Opole'
        } else {
          'Katowice Region'
        }
      }else if (a == '5'){
        if (aa %in% c('50','51','52','53','54')){
          'Wroclaw'
        } else {
          'Wroclaw Region'
        }
      }else if (a == '6'){
        if (aa %in% c('60','61')){
          'Poznan'
        } else if (aa=='65'){
          'Zielona Gora'
        } else {
          'Poznan Region'
        }
      }else if (a == '7'){
        if (aa %in% c('70','71')){
          'Szczecin'
        } else if (aa=='75'){
          'Koszalin'
        } else {
          'Szczecin Region'
        }
      }else if (a == '8'){
        if (aa=='80'){
          'Gdansk'
        } else if (aa=='81'){
          'Gdynia'
        } else if (aa=='87'){
          'Torun'
        }  else if (aa=='85'){
          'Bydgoszcz'
        } else {
          'Gdansk Region'
        }
      }else if (a == '9'){
        if (aa %in% c('90','91','92','93','94')){
          'Lodz'
        } else {
          'Lodz Region'
        }
      }else {NA}
    }
  })
  fcodes = as.factor(codes)
  fcodes = addNA(fcodes)
  fcodes
}

train4$night_parking_place_postal_code = postalCodeToRegion (train4$night_parking_place_postal_code)
train4$day_parking_place_postal_code = postalCodeToRegion (train4$day_parking_place_postal_code)
train4$main_driver_postal_code = postalCodeToRegion (train4$main_driver_postal_code)

##
##INZYNIERIA CECH
##

#na podstawie nowych danych, tylko te cechy mozna wprowadzic:
train4$timeWaiting = (train4$offer_last_after)-(train4$offer_first_after)
train4$formFillingTime = (train4$form_finished_at) - (train4$created_at)
#imputacja formFillingTime
train4[is.na(train4$formFillingTime),]$formFillingTime = mean(train4[!is.na(train4$formFillingTime),]$formFillingTime)
#powyzsza wyrzuciu blad bo zaimputowano juz wartosci
train4$hurryTime = (train4$insurance_start_date - train4$created_at)

#usuniete przez nearzerovariance:
train4$ac_offer_min_val = train2$ac_offer_min_val
train4$ac_offer_min_val[is.na(train4$ac_offer_min_val)] = 0

train4$ocacqty = train4$oc_offers_qty + train4$ac_offers_qty
train4$ocacminval = train4$oc_offer_min_val + train4$ac_offer_min_val
train4$ocacratio = (train4$oc_offer_min_val) / (train4$ac_offer_min_val)
train4$ocacratio[train4$ac_offer_min_val==0] = 0

#day of week created_at
train4$createdDoW = as.factor((as.integer(train4$created_at) + 6)%%7)

#ramka ktora bedzie podstawa dla emptydf
train_full = train4

#usuniecie niektorych kolumn z datami z treningu (beda one bardzo szkodliwe dla modelu)
train4$form_finished_at = NULL
train4$created_at = NULL
train4$insurance_start_date = NULL
train4$offer_last_at = NULL
train4$offer_first_at = NULL

#czy nie ma NA w modelu?
any(sapply(train4,function(cc)any(is.na(cc))))

library(Matrix)
md = sparse.model.matrix(Sale.success~.,train4)
dim(md)

library(xgboost)
xgtrain = xgb.DMatrix(md,label=as.integer(as.character(train4$Sale.success)))

modelxg = xgb.train(data=xgtrain,nrounds=100,objective="binary:logistic")

xgi = xgb.importance(colnames(xgtrain),modelxg)

head(xgi,100)
head(xgi,100)
#feature selection
feats = head(xgi$Feature,50)

md2 = md[,feats]
  dim(md2)

set.seed(997)
sa = sample(1:157827,10000,replace=F)
md3 = md2[sa,]
rfdata = as.matrix(md3)
#usuniecie NA level z sale.success
resp = as.factor(as.integer(as.character(train4$Sale.success)))[sa]
mean(as.integer(resp))
library(randomForest)
modelrf = randomForest(rfdata,y=resp,ntree=150,proximity = T)
#proximity
dim(modelrf$proximity)

library(class)
d = as.dist(modelrf$proximity)
hc = hclust(d,method="complete")
plot(hc)
x = cutree(hc,h=0.98)
unique(x)
#4 klastry

#uczenie klasyfikatora na klastrach:
#dodanie etykiety Sale.success
clust_data = as.data.frame(as.matrix(md3))
clust_data$Sale.success = as.numeric(as.character(train4$Sale.success))[sa]
#wymog xgboost - klasy musza byc 0,1,2,3 a nie 1,2,3,4
xg_clust_data = xgb.DMatrix(as.matrix(clust_data),label=(x-1))
xg_clust_model = xgb.train(data=xg_clust_data,nrounds=100,objective="multi:softmax",num_class=4)
xgi_clust = xgb.importance(colnames(xg_clust_data),xg_clust_model)
xgi_clust
which(xgi_clust$Feature=='Sale.success')
clust_data_label=clust_data
clust_data_label$cluster = x
#boxploty (z tego nic nie wynika)
for (feat in head(xgi_clust$Feature,5)){
  boxplot(formula=as.formula(paste0(feat,"~cluster")),data=clust_data_label)
}
library(dplyr)
head(xgi_clust$Feature,30)
#klaster 2 ma najwieksza szanse na sprzedaz
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(Sale.success))
#klastry 3 i 4 maja najwieksze hurryTime (najdalej od okresu rozpoczecia ubezpieczenia)
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(hurryTime))
#klaster 3 ma zdecydowanie najdluzsze formFillingTime, pozostale - bardzo szybko
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(formFillingTime))
boxplot(formFillingTime~cluster,data=clust_data_label)
#klaster 1 ma najwieksze wartosci samochodow i najwiekszy mileage
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(car_worth))
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(mileage))
#klaster 2 to ludzie starsi srednio o rok niz klaster 4
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(main_driver_age))
boxplot(main_driver_age~cluster,data=clust_data_label[clust_data_label$main_driver_age%in%18:100,])
#klaster 1 i 4 to najdawniej zarejestrowane samochody
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(registration_date))
#klaster 3 to zdecydowanie najdrozsze ubezpieczenia OC
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(oc_offer_min_val))
#klaster 2 to ludzie ktorzy najdluzej czekali na ostatnia oferte
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(timeWaiting))
#klaster 1 to najstarsze buy year, 4 to najnowsze
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(buy_year))
#klaster 2 trzyma samochody najczesciej w garazu
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(night_parking_placeindividual_garage))
#klaster 1 i 4 najregularniej korzysta z samochodow (malo istotne)
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(use_frequencyregularly))
#klaster 4 to ludzie sprawdzajacy najmniej ofert, klaster 2/1 to ludzie najczesciej
clust_data_label %>% group_by(cluster) %>% summarise(meancol = mean(went_to_partners))

prop.table(table(clust_data_label$went_to_partners,clust_data_label$cluster),2)
summary(as.factor(clust_data_label$cluster))
#klaster 3 - "mysliciele" - ludzie dlugo sie zastanawiaja, pewnie bo maja drogie ubezpieczenia OC, choc tansze samochody. 
#najmniej im sie spieszy. srednia liczba. przewaznie mlodsi
#klaster 1 - najwieksza grupa klientow. przewaznie starsze samochody, sprawdzaja w miare duzo ofert (went_to_partners).
#klaster 2 - srednia liczba klientow. najwieksza szansa na sprzedaz. trzymaja samochod najczesciej w garazu.
#klaster 4 - najmniejszy klaster. ludzie sprawdzajacy najmniej ofert, nie spieszy im sie, najmlodsi


t.test(clust_data_label$formFillingTime[clust_data_label$cluster==1],clust_data_label$formFillingTime[clust_data_label$cluster==3])

sapply(clust_data_label,function(cc){
  a1 = cc[clust_data_label$cluster==1]
  a2 = cc[clust_data_label$cluster==2]
  a3 = cc[clust_data_label$cluster==3]
  a4 = cc[clust_data_label$cluster==4]
  min(c(a1,a2,a3,a4))/max(c(a1,a2,a3,a4))
})
