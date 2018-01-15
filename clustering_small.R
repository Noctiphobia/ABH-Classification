load("ABH.rda")
library(randomForest)
library(dplyr)
library(mclust)

isBigCity = function (vector){
	vector = as.character(vector)
	codes = sapply(vector,function(v){
		if (is.na(v) | (substr(v,3,3) != '-') | (nchar(v) != 6)){
			NA
		} else {
			aa = substr(v,1,2)
			a = substr(aa,1,1)
			label = if (a == '0'){
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
			}else {'Region'}
			!grepl('Region', label)
		}
	})
	 codes
}
options(scipen=999) # zeby nie uzywal naukowej notacji
standard_preds = c("Sale.success", "car_worth", "production_year", "main_driver_age")
df = ABH[,standard_preds]
df$Sale.success = as.factor(df$Sale.success)
df[,"is_main_driver_male"] = (ABH$main_driver_gender == "male")
df[,"is_frequently_used"] = (ABH$use_frequency == "regularly")
df[,"is_private_usage"] = (ABH$usage_type == "private")
df[,"hurry_time"] = (ABH$insurance_start_date - ABH$created_at_date)
df[,"form_filling_time"] = (ABH$form_finished_at - ABH$created_at) * 24 * 60 # w minutach
df[,"is_big_city"] = isBigCity(ABH$main_driver_postal_code)
df$car_worth = as.numeric(df$car_worth)
df$production_year = as.numeric(df$production_year)

df_no_na = df[apply(df, 1, function(x){!any(is.na(x))}),]
df_no_na = df_no_na[df_no_na$main_driver_age <= 100 & df_no_na$main_driver_age >= 18,] # sensowny wiek kierowcy
df_no_na = df_no_na[df_no_na$form_filling_time >= 4,] #wypelnienie na pewno zajmie chociaz 2 minuty
df_no_na = df_no_na[df_no_na$hurry_time > 0,] #wieksza szansa na falszywe wyniki niz cos sensownego

df_s = as.data.frame(scale(cbind(as.numeric(df_no_na[,1]) - 1,df_no_na[,-1])))
colnames(df_s)[1] = "Sale.success"

clust = Mclust(df_s[,-1])
x = clust$classification

df_mclust = df_no_na
df_mclust[,"cluster"] = x

df_mclust %>% group_by(cluster) %>% summarise(count=n())

df_mclust %>% group_by(cluster) %>% summarise(count = n(), Sale.success = mean(as.numeric(Sale.success) - 1), car_worth = mean(car_worth), production_year = mean(production_year), main_driver_age = mean(main_driver_age),
																				 is_main_driver_male = mean(is_main_driver_male), is_frequently_used = mean(is_frequently_used), is_private_usage = mean(is_private_usage), 
																				 hurry_time = mean(hurry_time), form_filling_time = mean(form_filling_time), is_big_city = mean(is_big_city))


km = kmeans(df_s[,-1], 4)
x = km$cluster

df_kmeans = df_no_na
df_kmeans[,"cluster"] = x

df_kmeans %>% group_by(cluster) %>% summarise(count=n())

df_kmeans %>% group_by(cluster) %>% summarise(count = n(), Sale.success = mean(as.numeric(Sale.success) - 1), car_worth = mean(car_worth), production_year = mean(production_year), main_driver_age = mean(main_driver_age),
																							is_main_driver_male = mean(is_main_driver_male), is_frequently_used = mean(is_frequently_used), is_private_usage = mean(is_private_usage), 
																							hurry_time = mean(hurry_time), form_filling_time = mean(form_filling_time), is_big_city = mean(is_big_city))

