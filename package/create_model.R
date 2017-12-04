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
library(dummies)

df = read.csv("Model_decyzyjny_zbiór_treningowy.csv", header=TRUE)
labelindex = which(colnames(df)=='Sale.success')
train = df[, c(1:69, labelindex)]
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
train2$phone_yes = as.factor(train2$phone_yes)
train2$terms_acceptance = as.factor(train2$terms_acceptance)
train2$step = as.factor(train2$step)
train2$sent_to_sf = as.factor(train2$sent_to_sf)
train2$phone_lookup_status = as.factor(train2$phone_lookup_status)
train2$Sale.success = as.factor(train2$Sale.success)
#poprawienie dat
Sys.setlocale("LC_TIME", "C")
get_timestamp_datetime = function(x) {
	as.integer(as.POSIXct(strptime(as.character(x), "%d-%b-%y %H:%M:%S")))
}
get_timestamp_date = function(x) {
	as.integer(as.POSIXct(strptime(as.character(x), "%d-%b-%y")))
}
train2$created_at = get_timestamp_datetime(train2$created_at)
train2$registration_date = get_timestamp_datetime(train2$registration_date)
train2$insurance_start_date = get_timestamp_date(train2$insurance_start_date)
train2$form_finished_at = get_timestamp_datetime(train2$form_finished_at)
train2$offer_first_at = get_timestamp_datetime(train2$offer_first_at)
train2$offer_last_at = get_timestamp_datetime(train2$offer_last_at)
train2 = train2[,-which(colnames(train2) == "created_at_date")]

#dodanie poziomu NA do factorow
for (i in 1:length(train2)) {
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
numerics = which(sapply(train3,class)%in%c('numeric','integer'))
cr = cor(train3[,numerics])
correlated = findCorrelation(cr,cutoff= 0.8,names=TRUE)
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

#lib
#funkcje do rysowania ROC

tpr = function(t=0.5,pred,ty){
	predh = as.integer(pred>t)
	sum(predh[which(ty==1)])/sum(ty)
}

fpr = function(t=0.5,pred,ty){
	predh = as.integer(pred>t)
	sum(predh[-which(ty==1)])/(length(ty)-sum(ty))
}

#funkcja
drawROC = function(pred,ty,title="ROC"){
	if (is.factor(ty)){ty = as.integer(ty)} 
	t = seq(0.001,0.999,0.001)
	ty = ty[!is.na(pred)]
	pred = pred[!is.na(pred)]
	dx = sapply(t, function(tt){
		fpr(t=tt,pred,ty)
	})
	dy = sapply(t, function(tt){
		tpr(t=tt,pred,ty)
	})
	plot(dx,dy,xlim=c(0,1),ylim=c(0,1),type="l",col=1,xaxs="i",yaxs="i")
	title(main=title)
	abline(0,1,col=2)
}

ROCcoords = function(pred,ty,title="ROC"){
	if (is.factor(ty)){ty = as.integer(ty)} 
	t = seq(0.0001,0.9999,0.0001)
	
	dx = sapply(t, function(tt){
		fpr(t=tt,pred,ty)
	})
	dy = sapply(t, function(tt){
		tpr(t=tt,pred,ty)
	})
	return (data.frame(dx,dy))
}

#funkcje do lift
drawLIFT = function(pred,ty,title="LIFT"){
	if (is.factor(ty)){ty = as.integer(ty)} 
	t = 1:100
	sort_pred = order(pred,decreasing=T)
	n = length(pred)
	
	dx = 1:100
	
	dy = sapply(dx, function(x){
		m = as.integer((n*x)/100)
		if (m==0){
			NA
		}else{
			dct = sum(ty[head(sort_pred,m)])/sum(ty)
			dct/(x/100)
		}
	})
	
	dx = dx[!is.na(dy)]
	dy = dy[!is.na(dy)]
	
	plot(dx,dy,xlim=c(0,100),type="l",col=1,xaxs="i",yaxs="i")
	title(main=title)
	abline(1,0,col=2)
}

getLIFTS = function(pred,ty){
	if (is.factor(ty)){ty = as.integer(ty)} 
	t = 1:100
	sort_pred = order(pred,decreasing=T)
	n = length(pred)
	
	dx = c(5,10)
	
	dy = sapply(dx, function(x){
		m = as.integer((n*x)/100)
		if (m==0){
			NA
		}else{
			dct = sum(ty[head(sort_pred,m)])/sum(ty)
			dct/(x/100)
		}
	})
	
	return(data.frame(lift5=dy[1],lift10=dy[2]))
}

LIFTcoords = function(pred,ty){
	if (is.factor(ty)){ty = as.integer(ty)} 
	t = 1:100
	sort_pred = order(pred,decreasing=T)
	n = length(pred)
	
	dx = 1:100
	
	dy = sapply(dx, function(x){
		m = as.integer((n*x)/100)
		if (m==0){
			NA
		}else{
			dct = sum(ty[head(sort_pred,m)])/sum(ty)
			dct/(x/100)
		}
	})
	
	dx = dx[!is.na(dy)]
	dy = dy[!is.na(dy)]
	return (data.frame(dx,dy))
}

#hiperparametry

xgboosthiper = function (datax,datay,predictors=40,obj='binary:logistic'){
	
	n = nrow(datax)
	trx = datax
	try = datay
	
	xgsf = xgb.DMatrix(trx,label=try)
	
	#selekcja zmiennych ('predictors' most important)
	xg1=  xgboost(xgsf, nrounds=100, objective = obj,verbose=0)
	imp = xgb.importance(colnames(trx),xg1)
	sec = head(imp$Feature,predictors)
	inds = sapply(sec,function(sc)which(colnames(trx)==sc))
	ptrx = trx[,inds]
	
	#optymalizacja hiperparametrow
	sa = sample(1:nrow(ptrx),n/5,replace=F)
	optrx = as.matrix(ptrx[sa,])
	
	xgFitControl = trainControl (method="cv",number = 4) #4fold
	xgxo = optrx
	xgyo = as.factor(try[sa])
	xgModel = train(xgxo,xgyo,method = "xgbTree",trControl = xgFitControl)
	
	return (xgModel$bestTune)
}

#kombajn działający dla fold, dokonujący selekcji zmiennych na każdym foldzie i optymalizujący hiperparametry
#zwraca uśrednione AUC, LIFT5 i LIFT10

xgboostmeasure = function (datax,datay,k=10, rk=5, obj= 'binary:logistic',predictors=40){
	retdf = data.frame(AUC=rep(0,k),LIFT5=rep(0,k),LIFT10=rep(0,k))
	set.seed(997)   
	tune = list(nrounds=50,max_depth=1,eta=0.3,gamma=0,colsample_bytree=0.8,min_child_weight=1,subsample=0.75)
	
	xgsf = xgb.DMatrix(datax,label=datay)
	
	#selekcja zmiennych ('predictors' most important)
	xg1=  xgboost(xgsf, nrounds=100, objective = obj,verbose=0)
	imp = xgb.importance(colnames(datax),xg1)
	sec = head(imp$Feature,predictors)
	inds = sapply(sec,function(sc)which(colnames(datax)==sc))
	pdatax = datax[,inds]    
	
	set.seed(997)
	
	for (j in 1:rk){
		sequence = sample(1:nrow(datax),nrow(datax),replace=F)
		for (i in 1:k){
			n = nrow(datax)
			nf = as.integer(nrow(datax)/k)
			tf = ((i-1)*nf+1):(i*nf) #indeksy zb. testowego
			
			trx = pdatax[sequence[-tf],]
			try = datay[sequence[-tf]]
			tsx = pdatax[sequence[tf],]
			tsy = datay[sequence[tf]]
			
			
			#trenowanie modelu
			rtrx = xgb.DMatrix(trx,label=try)
			model =  xgboost(params= tune, data = rtrx, nrounds=tune$nrounds, 
											 objective = obj,verbose=0)
			
			#predykcja
			rtsx = xgb.DMatrix(tsx)
			predy = predict(model,rtsx,type="response")
			
			rauc = pROC::auc(tsy,predy)
			retdf[i,1] = rauc
			lifts = getLIFTS(predy,tsy)
			retdf[i,2] = lifts$lift5
			retdf[i,3] = lifts$lift10
			print(paste0("Arrange nr ",j," Fold nr ",i,", AUC ROC= ",rauc,", LIFT5= ",lifts$lift5,", LIFT10= ",lifts$lift10))
		}
	}
	return (retdf)
}

mdtrain4 = sparse.model.matrix(Sale.success~.,train4, drop.unused.levels = F)
trainy = train4$Sale.success

xg4scores = xgboostmeasure(datax=mdtrain4,datay=as.numeric(trainy)-1)

numericcols = which(sapply(train4,is.numeric))
numericmeans = sapply(train4[,numericcols], mean)
save(numericmeans, file="numericmeans.rda")

emptydf = train4[numeric(0),-which(colnames(train4) == "Sale.success")]
save(emptydf, file="emptydf.rda")

rawmodel = xgb.load("xgboost.model")$raw
save(rawmodel, file="model.rda")