#skrypt do analizy jakosci obecnego klasyfikatora
#dane przetwarzane to to samo co w create_model.R oczywiscie

#ladowanie plikow rda
load('./package/scoreABH/data/emptydf.rda')
load('./package/scoreABH/data/numericmeans.rda')
load('./package/scoreABH/data/rawmodel_contacted.rda')
load('./package/scoreABH/data/rawmodel_not_contacted.rda')

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

#ladowanie zbioru treningowego
dfs = read.csv("data.csv", sep=",", header=TRUE)
#symulacja prawdziwych daych, czyli usuwanie kontekstu w postaci leveli:
factors = which(sapply(dfs,is.factor))
for (i in factors){
  dfs[,i] = as.character(dfs[,i])
}
#ustalilismy, ze daty beda numeryczne. nasz zbior jednakze tego nie ma, wiec trzeba zamienic zgodnie z create_model.R
Sys.setlocale("LC_TIME", "C")
get_timestamp_datetime = function(x) {
  as.integer(as.POSIXct(strptime(as.character(x), "%d-%b-%y %H:%M:%S")))
}
get_timestamp_date = function(x) {
  as.integer(as.POSIXct(strptime(as.character(x), "%d-%b-%y")))
}

dfs$created_at = get_timestamp_datetime(dfs$created_at)
dfs$registration_date = get_timestamp_datetime(dfs$registration_date)
dfs$insurance_start_date = get_timestamp_date(dfs$insurance_start_date)
dfs$form_finished_at = get_timestamp_datetime(dfs$form_finished_at)
dfs$offer_first_at = get_timestamp_datetime(dfs$offer_first_at)
dfs$offer_last_at = get_timestamp_datetime(dfs$offer_last_at)

labels = dfs$Sale.success
dfs = dfs[,-which(colnames(df)=="Sale.success")]

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
  newlist$formFillingTime = (newlist$form_finished_at) - (newlist$created_at)
  #oc/ac
  newlist$ocacqty = newlist$oc_offers_qty + newlist$ac_offers_qty
  newlist$ocacminval = newlist$oc_offer_min_val + newlist$ac_offer_min_val
  newlist$ocacratio = (newlist$oc_offer_min_val) / (newlist$ac_offer_min_val)
  newlist$ocacratio[newlist$ac_offer_min_val==0] = 0

  newdf = as.data.frame(newlist)
  
  md = stats::model.matrix(~., data=newdf)
  md_con = md[,features_contacted]
  md_ncon =md[,features_not_contacted]

  xg_con = xgb.DMatrix(md_con)
  xg_ncon = xgb.DMatrix(md_ncon)
  
  score_contacted = xgboost:::predict.xgb.Booster(model_con, xg_con)
  score_not_contacted = xgboost:::predict.xgb.Booster(model_ncon,xg_ncon)
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

#jest jakies rozdzielenie nawet, ale trzeba to ulepszac
