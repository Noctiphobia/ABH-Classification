#skrypt do analizy jakosci obecnego klasyfikatora
#dane przetwarzane to to samo co w create_model.R oczywiscie

#ladowanie plikow rda
load('./package/scoreABH/data/emptydf.rda')
load('./package/scoreABH/data/numericmeans.rda')
load('./package/scoreABH/data/rawmodel_contacted.rda')
load('./package/scoreABH/data/rawmodel_not_contacted.rda')
load('./package/scoreABH/data/features_contacted.rda')
load('./package/scoreABH/data/features_not_contacted.rda')


#w tym miejscu zaladuj do environment plik get_score.R
#errory dla funkcji data nie maja znaczenia, i tak wszystko juz jest

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

#ladowanie zbioru treningowego
dfs = read.csv("data.csv", sep=",", header=TRUE)
#symulacja prawdziwych daych, czyli usuwanie kontekstu w postaci leveli:
factors = which(sapply(dfs,is.factor))
for (i in factors){
  dfs[,i] = as.character(dfs[,i])
}
#ustalilismy, ze daty beda numeryczne. nasz zbior jednakze tego nie ma, wiec trzeba zamienic zgodnie z create_model.R
#poprawienie dat
d0 <- as.Date(0, origin="1899-12-30", tz='UTC')

dfs$created_at = sapply(as.character(dfs$created_at), function(rd){
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

dfs$registration_date = sapply(as.character(dfs$registration_date), function(rd){
  if (nchar(rd)<5) {NA} else {
    a = strsplit(strsplit(rd,' ')[[1]],'-')[[1]]
    aa = as.Date(paste0(which(month.abb==a[2]),'/',a[1],'/',a[3]),'%m/%d/%y')
    as.numeric(aa-d0)
  }
})

dfs$insurance_start_date = sapply(as.character(dfs$insurance_start_date), function(rd){
  if (nchar(rd)<5) {NA} else {
    a = strsplit(strsplit(rd,' ')[[1]],'-')[[1]]
    aa = as.Date(paste0(which(month.abb==a[2]),'/',a[1],'/',a[3]),'%m/%d/%y')
    as.numeric(aa-d0)
  }
})

dfs$form_finished_at = sapply(as.character(dfs$form_finished_at), function(rd){
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

dfs$offer_first_at = sapply(as.character(dfs$offer_first_at), function(rd){
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

dfs$offer_last_at = sapply(as.character(dfs$offer_last_at), function(rd){
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


labels = dfs$Sale.success
dfs = dfs[,-which(colnames(dfs)=="Sale.success")]

##
##sprawdzanie czy model decyzyjny podejmuje decyzje
##

#zmodyfikowane get score dla calej ramki danych
get_score_dataframe <- function(data) {
  newdf = emptydf
  #wektor nazw atrybutow danych
  dnames = colnames(data)
  #wektor nazw atrybutow w modelu
  mnames = names(newdf)
  
  j=1
  newlist = list()
  
  for (i in 1:ncol(data)) {
    if (!any(dnames[i] == mnames)){
      next
    }
    #indeks w newdf
    model_index = which(dnames[i]==mnames)

    if (any(is.na(data[,i])) && dnames[i] %in% names(numericmeans)){
      numeric_index = which(names(numericmeans)==dnames[i])
      tmp = numeric(nrow(data))
      tmp[!is.na(data[,i])] = data[!is.na(data[,i]),i]
      tmp[is.na(data[,i])] = numericmeans[numeric_index]
      newlist[[j]] = tmp
      names(newlist)[j] = mnames[model_index]
      j=j+1
    }
    else{
        if (grepl("postal", dnames[i])){
          newlist[[j]] = postalCodeToRegion(data[,i])
          names(newlist)[j] = mnames[model_index]
          j=j+1
        }
        else if (is.factor(newdf[,model_index])){
          newlist[[j]] = addNA(data[,i])
          names(newlist)[j] = mnames[model_index]
          j = j+1
        }
        else{
          newlist[[j]] = as.numeric(data[,i])
          names(newlist)[j] = mnames[model_index]
          j = j+1
        }
    }
  }
  
  #feature engineering
  newlist$timeWaiting = (newlist$offer_last_after)-(newlist$offer_first_after)
  newlist$formFillingTime = (data$form_finished_at) - (data$created_at)
  newlist$hurryTime = (data$insurance_start_date - data$created_at)
  #oc/ac
  newlist$ocacqty = newlist$oc_offers_qty + newlist$ac_offers_qty
  newlist$ocacminval = newlist$oc_offer_min_val + newlist$ac_offer_min_val
  newlist$ocacratio = (newlist$oc_offer_min_val) / (newlist$ac_offer_min_val)
  newlist$ocacratio[newlist$ac_offer_min_val==0] = 0
  newlist$createdDoW = as.factor((as.integer(data$created_at) + 6)%%7)

  newdf = as.data.frame(newlist)
  #prowizoryczna imputacja
  newdf$formFillingTime[is.na(newdf$formFillingTime)] = mean(newdf$formFillingTime[!is.na(newdf$formFillingTime)])
  
  md = stats::model.matrix(~., data=newdf)
  md_con = md[,features_contacted]
  md_ncon =md[,features_not_contacted]

  xg_con = xgb.DMatrix(md_con)
  xg_ncon = xgb.DMatrix(md_ncon)
  
  score_contacted = xgboost:::predict.xgb.Booster(model_contacted, xg_con)
  score_not_contacted = xgboost:::predict.xgb.Booster(model_not_contacted,xg_ncon)
  result = score_contacted - score_not_contacted
  multiplier = 1 # TODO: multiplier zalezny od potencjalnych zarobkow
  result * multiplier
}

#sprawdzenie klasyfikatora
#uwaga, potrzeba 5.6 GB ramu (policzy sie w moment, tylko trzeba tyle zapewnic)
scores = get_score_dataframe(dfs)
cor(scores,labels)
hist(scores[labels==1],freq=F,xlim=c(-1,1),ylim=c(0,2))
hist(scores[labels==0],add=TRUE,freq=F,col=rgb(1,0,0,0.5))

##
#xgboost explainer
##

install.packages("devtools") 
library(devtools) 
install_github("AppliedDataSciencePartners/xgboostExplainer")

library(xgboostExplainer)
traindata_con = to_model_con_dataset(dfs,labels)
explainer_con = buildExplainer(model_contacted,traindata_con, type="binary", base_score = 0.5)
traindata_ncon = to_model_not_con_dataset(dfs,labels)
explainer_ncon = buildExplainer(model_not_contacted,traindata_ncon, type="binary", base_score = 0.5)

#save(explainer_con,file="explainer_con.rda")
#save(explainer_ncon,file="explainer_ncon.rda")

features_contacted
features_not_contacted
#dlaczego model daje niskie scoringi sprzedanym?
inds = which(scores<(-0.90) & labels==1)

p_con = explainPredictions(model_contacted, explainer_con, traindata_con)
p_ncon = explainPredictions(model_not_contacted, explainer_ncon, traindata_ncon)

####### form filling time, wplyw
formfillingtime = (dfs$form_finished_at-dfs$created_at)
plot(formfillingtime, as.numeric(p_con$formFillingTime), cex=0.4, pch=16, 
     xlab = "FormFillingTime", ylab = "Impact on log-odds",col=labels+1) #red - success
hist(formfillingtime,breaks=seq(0,7000,100))
#not contacted
plot(formfillingtime, as.numeric(p_ncon$formFillingTime), cex=0.4, pch=16, 
     xlab = "FormFillingTime", ylab = "Impact on log-odds",col=labels+1) #red - success
#jak to przeanalizowac - nie wiem
#######
utm_sourcecc_mfind = as.integer(dfs$utm_source=='cc-mfind')
plot(utm_sourcecc_mfind, as.numeric(p_con$`utm_sourcecc-mfind`), cex=0.4, pch=16, 
     xlab = "FormFillingTime", ylab = "Impact on log-odds",col=labels+1) #red - success
#######
plot(dfs$insurance_start_date, as.numeric(p_con$insurance_start_date), cex=0.4, pch=16, 
     xlab = "FormFillingTime", ylab = "Impact on log-odds",col=labels+1) #red - success
plot(dfs$insurance_start_date, as.numeric(p_ncon$insurance_start_date), cex=0.4, pch=16, 
     xlab = "FormFillingTime", ylab = "Impact on log-odds",col=labels+1) #red - success
