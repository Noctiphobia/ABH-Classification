data("rawmodel")
data("numericmeans")
data("emptydf")
xgbmodel = xgboost::xgb.load(rawmodel)

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
get_score <- function(etx_make_name, etx_model_name, etx_fuel_code, production_year, etx_model_code, 
											protection_scope, kind, use_frequency, child_carriage_frequency, mileage, 
											yearly_mileage, used_abroad, night_parking_place, night_parking_place_postal_code, day_parking_place, 
											day_parking_place_postal_code, theft_protection_installation, theft_protection_device_1, theft_protection_device_2, origin, 
											buy_year, registration_date, car_worth, main_driver_postal_code, main_driver_age, 
											main_driver_gender, insurance_start_date, phone_exists, phone_yes, phone_no, 
											step, oc_offer_min_val, ac_offers_qty, b2c_leads_sent, offer_last_at, 
											offer_first_after, offer_last_after, phone_lookup_status, utm_campaign, utm_content, 
											utm_medium, utm_source, ...) {
	parameters = as.list(environment(), all=TRUE)
	parameters = parameters[-length(parameters)]
	newdf = emptydf
	for (i in 1:length(newdf)) {
		newdf[1,i] = NA
	}
	for (i in 1:length(parameters)) {
		if (is.na(parameters[[i]]) && names(parameters)[i] %in% names(numericmeans)) 
			newdf[1,names(parameters)[i]] = numericmeans[names(parameters)[i]]
		else 
			if (names(parameters)[i] %in% colnames(newdf)) {
				if (grepl("postal", names(parameters)[i])) 
					newdf[1,names(parameters)[i]] = postalCodeToRegion(parameters[[i]])
				else
					newdf[1,names(parameters)[i]] = ifelse(is.numeric(newdf[,names(parameters)[i]]), as.numeric(parameters[[i]]), parameters[[i]])
			}
	}
	md = stats::model.matrix(~., data=newdf)
	xgboost:::predict.xgb.Booster(xgbmodel, md)
}


