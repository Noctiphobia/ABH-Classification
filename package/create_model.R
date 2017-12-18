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
numericpreds = which(sapply(train3,class)=='integer')
has_NA = sapply(numericpreds,function(npred)any(is.na(train3[,npred])))
numericpreds = numericpreds[has_NA]
for (numericpred in numericpreds){
	train3[which(is.na(train3[,numericpred])),numericpred] = as.integer(mean(na.omit(train3[,numericpred])))
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

##
##MODEL
##

#datasety
train_contacted = train4[train4$Contacted.by.CC == 1, -which(colnames(train4) == 'Contacted.by.CC')]
train_not_contacted = train4[train4$Contacted.by.CC == 0, -which(colnames(train4) == 'Contacted.by.CC')]

sparse_contacted = sparse.model.matrix(Sale.success~.,train_contacted, drop.unused.levels = F)
sparse_not_contacted = sparse.model.matrix(Sale.success~.,train_not_contacted, drop.unused.levels = F)
contacted_y = as.numeric(as.character(train_contacted$Sale.success))
not_contacted_y = as.numeric(as.character(train_not_contacted$Sale.success))
#pierwszy model
xgtrain_con = xgb.DMatrix(sparse_contacted,label=contacted_y)
xgtrain_ncon = xgb.DMatrix(sparse_not_contacted,label=not_contacted_y)
model_con = xgb.train(data=xgtrain_con,nrounds=150,objective="binary:logistic")
model_ncon = xgb.train(data=xgtrain_ncon,nrounds=150,objective="binary:logistic")

#selekcja zmiennych
xgi_c = xgb.importance(colnames(xgtrain_con),model_con)
xgi_n = xgb.importance(colnames(xgtrain_ncon),model=model_ncon)

#head 40 - empirycznie pokazane we wczesniejszym etapie
features_contacted = head(xgi_c$Feature,40)
features_not_contacted = head(xgi_n$Feature,40)

#drugi model - na selekcji
sparse_contacted_final = sparse_contacted[,features_contacted]
sparse_not_contacted_final= sparse_not_contacted[,features_not_contacted]

xgtrain_con_final = xgb.DMatrix(sparse_contacted_final,label=contacted_y)
xgtrain_ncon_final = xgb.DMatrix(sparse_not_contacted_final,label=not_contacted_y)

#optymalizacja hiperparametrow
#przeniesienie z poprzedniego modelu
besttune = list(nrounds=50,max_depth=1,eta=0.3,gamma=0,colsample_bytree=0.8,min_child_weight=1,subsample=1)

#model:
model_con = xgb.train(data=xgtrain_con_final,params= besttune,nrounds=50,objective="binary:logistic")
model_ncon= xgb.train(data=xgtrain_ncon_final,params= besttune,nrounds=50,objective="binary:logistic")

##
##ZAPIS
##

#zapis modeli
rawmodel_contacted = model_con$raw
save(rawmodel_contacted, file="./package/scoreABH/data/rawmodel_contacted.rda")

rawmodel_not_contacted = model_ncon$raw
save(rawmodel_not_contacted, file="./package/scoreABH/data/rawmodel_not_contacted.rda")

#zapis imputacji
numericcols = which(sapply(train_full,is.numeric))
numericmeans = sapply(train_full[,numericcols], function(x){mean(na.omit(x))})
save(numericmeans, file="./package/scoreABH/data/numericmeans.rda")

#schema:
emptydf = train_full[numeric(0),-which(colnames(train_contacted)=='Sale.success')]
save(emptydf, file="./package/scoreABH/data/emptydf.rda")

#wybrane predyktory
save(features_contacted, file="./package/scoreABH/data/features_contacted.rda")
save(features_not_contacted, file="./package/scoreABH/data/features_not_contacted.rda")
