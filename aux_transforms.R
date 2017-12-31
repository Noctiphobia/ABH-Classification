#transformuj dataset do postaci do trenowania

to_model_con_dataset = function(data,lbl){
  md = get_md_set(data)
  md_con = md[,features_contacted]
  xgb.DMatrix(md_con,label=lbl)
}

to_model_not_con_dataset = function(data,lbl){
  md = get_md_set(data)
  md_ncon =md[,features_not_contacted]
  xgb.DMatrix(md_ncon,label=lbl)
}

to_model_con_dataset_test = function(data){
  md = get_md_set(data)
  print(dim(md))
  md_ncon =md[,features_contacted]
  xgb.DMatrix(md_ncon)
}

to_model_not_con_dataset_test = function(data){
  md = get_md_set(data)
  md_ncon =md[,features_not_contacted]
  xgb.DMatrix(md_ncon)
}

get_md_set = function(data){
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
  newlist$hurryTime = (newlist$insurance_start_date - newlist$created_at)
  #oc/ac
  newlist$ocacqty = newlist$oc_offers_qty + newlist$ac_offers_qty
  newlist$ocacminval = newlist$oc_offer_min_val + newlist$ac_offer_min_val
  newlist$ocacratio = (newlist$oc_offer_min_val) / (newlist$ac_offer_min_val)
  newlist$ocacratio[newlist$ac_offer_min_val==0] = 0
  newlist$createdDoW = as.factor((as.integer(newlist$created_at) + 6)%%7)
  
  
  newdf = as.data.frame(newlist)
  
  stats::model.matrix(~., data=newdf)
}