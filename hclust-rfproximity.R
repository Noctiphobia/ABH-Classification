#dziala dla ABH
load("./ABH-Classification/ABH.rda")
preds = strsplit(
  "LastCall__c	calculation_id	calculation_token	etx_make_name	etx_model_name	etx_fuel_code	production_year	etx_model_code	vehicle_id	protection_scope	kind	usage_type	use_frequency	child_carriage_frequency	mileage	yearly_mileage	used_abroad	night_parking_place	night_parking_place_postal_code	day_parking_place	day_parking_place_postal_code	theft_protection_installation	theft_protection_device_1	theft_protection_device_2	origin	buy_year	registration_date	is_damaged	leasing	car_worth	main_driver_postal_code	main_driver_age	main_driver_gender	insurance_start_date	phone_exists	calc_complete	calc_incomplete	calc_finished	phone_yes	phone_no	calculation_type	terms_acceptance	phone_acceptance	phone_accepted	step	created_at	created_at_date	affiliation_id	user_id	salesforce_lead	sent_to_sf	sf_r_purchasing	calculation_state	oc_offers_qty	oc_offer_min_val	ac_offers_qty	ac_offer_min_val	b2c_leads_sent	form_finished_at	offer_first_at	offer_last_at	offer_first_after	offer_last_after	phone_lookup_status	utm_campaign	utm_content	utm_medium	utm_source	pkb_transform_ver	went_to_partners	contact_requests	Acc_Agreement_call__c	Acc_Agreement_marketing__c	Acc_Agreement_newsletter__c	Acc_Birthdate__c	Acc_PhoneHLRStatus__c	Account__r.AccountSource	Account__r.AgreementCall__pc	Account__r.AgreementMarketing__pc	Account__r.AgreementNewsletter__pc	Account__r.BillingCity	Account__r.BillingPostalCode	Account__r.CreatedById	Account__r.CreatedDate	Account__r.Gender__pc	Account__r.Id	Account__r.IsDeleted	Account__r.IsPersonAccount	Account__r.LegacyID__c	Account__r.OwnerId	Account__r.PersonBirthdate	Account__r.PersonContactId	Account__r.PhoneHLRStatus__pc	Account__r.PhoneNATStatus__pc	Account__r.RecordTypeId	Account__r.Salutation	Account__r.Type	CalculationToken__c	CarEngineCapacity__c	CarFuelType__c	CarMake__c	CarModel__c	CarYear__c	CpcPaid__c	CreatedBy.Alias	CreatedBy.CreatedDate	CreatedById	CreatedDate	Id	IsDeleted	Mpc__c	PolicyStartDate__c	ProductScope__c	Source__c	WentToPartner__c	dont_have_this_car__c	TelehubStatus__c","\t")[[1]]
indices = sapply(preds, function(pred){which(colnames(ABH)==pred)})
labelindex = which(colnames(ABH)=='Sale.success')
ABHnoID = ABH[,c(unlist(indices),labelindex)]

colnames(ABHnoID)

ABHnoID$calculation_id=NULL
ABHnoID$calculation_token=NULL
ABHnoID$affiliation_id=NULL
ABHnoID$sent_to_sf=NULL
ABHnoID$Id = NULL
ABHnoID$Status__c=NULL
ABHnoID$StatusType__c=NULL
ABHnoID$SystemModstamp=NULL
ABHnoID$Last.call=NULL
ABHnoID$LastCall__c=NULL
ABHnoID$Account__r.CreatedDate = NULL

ABHnoID$mileage = as.numeric(ABHnoID$mileage)
ABHnoID$yearly_mileage = as.numeric(ABHnoID$yearly_mileage)
ABHnoID$car_worth = as.numeric(ABHnoID$car_worth)
ABHnoID$main_driver_age = as.integer(ABHnoID$main_driver_age)
ABHnoID$oc_offers_qty = as.integer(ABHnoID$oc_offers_qty)
ABHnoID$oc_offer_min_val = as.integer(ABHnoID$oc_offer_min_val)
ABHnoID$ac_offers_qty = as.integer(ABHnoID$ac_offers_qty)
ABHnoID$ac_offer_min_val = as.integer(ABHnoID$ac_offer_min_val)
ABHnoID$offer_first_after = as.integer(ABHnoID$offer_first_after)
ABHnoID$offer_last_after = as.integer(ABHnoID$offer_last_after)
ABHnoID$contact_requests = as.integer(ABHnoID$contact_requests)

ABHnoID$registration_date = as.numeric(ABHnoID$registration_date)
ABHnoID$insurance_start_date = as.numeric(ABHnoID$insurance_start_date)
ABHnoID$created_at = as.numeric(ABHnoID$created_at)
ABHnoID$form_finished_at = as.numeric(ABHnoID$form_finished_at)
ABHnoID$offer_first_at = as.numeric(ABHnoID$offer_first_at)
ABHnoID$offer_last_at = as.numeric(ABHnoID$offer_last_at)
ABHnoID$Account__r.PersonBirthdate = as.numeric(ABHnoID$Account__r.PersonBirthdate)
ABHnoID$CreatedDate = as.numeric(ABHnoID$CreatedDate)
ABHnoID$PolicyStartDate__c = as.numeric(ABHnoID$PolicyStartDate__c)

#change chars to factors
for (i in 1:length(ABHnoID)) {
  if (is.character(ABHnoID[,i])){
    ABHnoID[,i] = as.factor(ABHnoID[,i])
    ABHnoID[,i] = addNA(ABHnoID[,i])
  }
}

library(caret)
nzv = nearZeroVar(ABHnoID)
nzv
train3 = ABHnoID[,-nzv]


numericpreds = which(sapply(train3,class) %in% c('integer','numeric'))
has_NA = sapply(numericpreds,function(npred)any(is.na(train3[,npred])))
numericpreds = numericpreds[has_NA]
for (numericpred in numericpreds){
  train3[which(is.na(train3[,numericpred])),numericpred] = as.integer(mean(na.omit(train3[,numericpred])))
}

#korelacje
correlated = c("phone_yes","phone_no","phone_acceptance")
traininds = sapply(correlated,function(cc)which(colnames(train3)==cc))
train4 = train3[,-28]

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

numericpreds = which(sapply(train4,class) %in% c('integer','numeric'))
has_NA = sapply(numericpreds,function(npred)any(is.na(train4[,npred])))
numericpreds = numericpreds[has_NA]
for (numericpred in numericpreds){
  train4[which(is.na(train4[,numericpred])),numericpred] = as.integer(mean(na.omit(train4[,numericpred])))
}

##
##INZYNIERIA CECH
##

#na podstawie nowych danych, tylko te cechy mozna wprowadzic:
train4$timeWaiting = (train4$offer_last_after)-(train4$offer_first_after)
train4$formFillingTime = (train4$form_finished_at) - (train4$created_at)
train4$hurryTime = (train4$insurance_start_date - train4$created_at)

#usuniete przez nearzerovariance:
train4$ac_offer_min_val = ABH$ac_offer_min_val
train4$ac_offer_min_val[is.na(train4$ac_offer_min_val)] = 0

train4$oc_offer_min_val = ABH$oc_offer_min_val
any(is.na(train4$ac_offers_qty))
numericpreds = which(sapply(train4,class) %in% c('integer','numeric'))
has_NA = sapply(numericpreds,function(npred)any(is.na(train4[,npred])))
numericpreds = numericpreds[has_NA]
for (numericpred in numericpreds){
  train4[which(is.na(train4[,numericpred])),numericpred] = as.integer(mean(na.omit(train4[,numericpred])))
}

train4$ocacqty = train4$oc_offers_qty + train4$ac_offers_qty
train4$ocacminval = train4$oc_offer_min_val + train4$ac_offer_min_val
train4$ocacratio = (train4$oc_offer_min_val) / (train4$ac_offer_min_val)
train4$ocacratio[train4$ac_offer_min_val==0] = 0

#day of week created_at
train4$createdDoW = as.factor((as.integer(train4$created_at) + 6)%%7)


#usuniecie niektorych kolumn z datami z treningu (beda one bardzo szkodliwe dla modelu)
train4$form_finished_at = NULL
train4$created_at = NULL
train4$insurance_start_date = NULL
train4$offer_last_at = NULL
train4$offer_first_at = NULL

#czy nie ma NA w modelu?
any(sapply(train4,function(cc)any(is.na(cc))))
train4$Sale.success = ABH$Sale.success
library(Matrix)
md = sparse.model.matrix(Sale.success~.,train4)
library(xgboost)
xgtrain = xgb.DMatrix(md,label=ABH$Sale.success)

modelxg = xgb.train(data=xgtrain,nrounds=100,objective="binary:logistic")

xgi = xgb.importance(colnames(xgtrain),modelxg)
head(xgi,20)
#feature selection
feats = head(xgi$Feature,50)

md2 = md[,feats]
  dim(md2)

set.seed(997)
sa = sample(1:93646,10000,replace=F)
md3 = md2[sa,]
rfdata = as.matrix(md3)
resp = as.factor(ABH$Sale.success)[sa]
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
?cutree
dim(md2)
colnames(md2)
hist(sapply(unique(x), function(xx){mean(md2[x==xx,3])}))
plot(md2[,16],md2[,20],col=x)
mean(md2[,13])
mean(group2[,13])
