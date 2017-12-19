data("rawmodel_contacted")
data("rawmodel_not_contacted")
data("numericmeans")
data("emptydf")
data("features_contacted")
data("features_not_contacted")

model_contacted = xgboost::xgb.load(rawmodel_contacted)
model_not_contacted = xgboost::xgb.load(rawmodel_not_contacted)

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

#' calculate score
#'
#' @param ... parametry modelu
#'
#' @return numeric
#' @export
get_score <- function(...) {
	parameters = data.frame(...)
	newdf = emptydf

	#wektor nazw atrybutow danych
	dnames = colnames(parameters)
	#wektor nazw atrybutow w modelu
	mnames = colnames(newdf)

	for (i in 1:length(parameters)) {
		if (!any(dnames[i] == mnames)){
			next
		}
		#indeks w newdf
		model_index = which(dnames[i]==mnames)

		if (is.na(parameters[,i]) && dnames[i] %in% names(numericmeans)){
			numeric_index = which(names(numericmeans)==dnames[i])
			newdf[1,model_index] = numericmeans[numeric_index]
		}
		else{
			if (grepl("postal", dnames[i]))
				newdf[1,model_index] = postalCodeToRegion(parameters[,i])
			else{
				newdf[1,model_index] = ifelse(is.numeric(newdf[,model_index]), as.numeric(as.character(parameters[,i])), as.character(parameters[,i]))
			}
		}
	}
	#!dopisywac nowe features jesli cos nowego dojdzie
	new_feats = c("timeWaiting","formFillingTime",'hurryTime',"ocacqty","ocacminval","ocacratio","createdDoW")

	#imputacja brakujacych w requescie atrybutow
	not_delivered =  mnames[which(!(mnames %in% c(dnames,new_feats)))]
	if (length(not_delivered)>0){
		for (feat in not_delivered){
			if (is.numeric(newdf[,feat])){
				numeric_index = which(names(numericmeans)==feat)
				newdf[1,feat] = numericmeans[numeric_index]
			} else if (is.factor(newdf[,feat])){
				newdf[1,feat] = NA #dla kazdego factora jest level NA
			}
		}
	}

	#feature engineering - nie wymaga imputacji z powodu powyzszej petli
	newdf$timeWaiting = (newdf$offer_last_after)-(newdf$offer_first_after)
	newdf$formFillingTime = (newdf$form_finished_at) - (newdf$created_at)
	newdf$hurryTime = (newdf$insurance_start_date - newdf$created_at)
	#oc/ac
	newdf$ocacqty = newdf$oc_offers_qty + newdf$ac_offers_qty
	newdf$ocacminval = newdf$oc_offer_min_val + newdf$ac_offer_min_val
	newdf$ocacratio = (newdf$oc_offer_min_val) / (newdf$ac_offer_min_val)
	newdf$ocacratio[newdf$ac_offer_min_val==0] = 0
	#inna obsluga by nie usuwac istniejacego factora
	newdf[1,"createdDoW"] = as.character((as.integer(newdf$created_at) + 6)%%7)

	md = stats::model.matrix(~., data=newdf)
	mc = md[,features_contacted]
	mn = md[,features_not_contacted]
	md_con = t(mc) #model.matrix tworzy zwykly wektor kolumnowy, wymagana transpozycja
	md_ncon =t(mn)

	xg_con = xgboost::xgb.DMatrix(md_con)
	xg_ncon = xgboost::xgb.DMatrix(md_ncon)

	score_contacted = xgboost:::predict.xgb.Booster(model_contacted, xg_con)
	score_not_contacted = xgboost:::predict.xgb.Booster(model_not_contacted,xg_ncon)

	result = score_contacted - score_not_contacted
	#zabezpieczenie na (ekstremalny) przypadek nieznanych poziomow czynnika
	if (length(result)>1)
		result=result[1]

	multiplier = 1 # TODO: multiplier zalezny od potencjalnych zarobkow
	result * multiplier
}
