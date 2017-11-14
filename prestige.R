normalize_makes = function(data) {
	res = trimws(tolower(data))
	usamakes = endsWith(res, "(usa)") | endsWith(res, "(bmw)")
	usamakes = usamakes & !is.na(usamakes)
	res[usamakes] = trimws(substr(res[usamakes], 1, nchar(res[usamakes]) - 5))
	res
}

makes_to_prestige = function(data) {
	normalized = normalize_makes(data)
	prestige = list()
	prestige[[1]] = c("buggati", "ferrari", "lamborghini", "mclaren", "koenigsegg", "noble", "aston martin")
	prestige[[2]] = c("tesla", "porsche", "bentley", "rolls-royce", "maybach")
	prestige[[3]] = c("jaguar", "land rover", "rover", "hummer", "jeep", "mercedes-benz", "bmw", "audi", "lexus", "infiniti", "maserati", "cadillac", "acura", "alfa romeo", "lincoln")
	prestige[[4]] = c("buick", "dodge", "lotus", "volvo","de lorean", "plymouth", "mini", "navara")
	prestige[[5]] = c("opel", "chrysler",  "lancia", "skoda", "hyundai", "toyota", "lancia",  "chevrolet", "ford", "suzuki", "zafira", "qashai", "daihatsu", "smart", "maruti-suzuki", "volkswagen",  "saab", "nissan", "honda", "seat", "subaru", "ssangyong", "mitsubishi", "kia", "citroën", "suzuki", "gwm", "renault", "peugeot", "pontiac", "mazda")
	prestige[[6]] = c("fiat", "tata","dacia","eagle")
	prestige[[7]] = c("moskwicz", "fso", "daewoo-fso/fso","daewoo", "lada", "trabant", "gaz", "fsc", "fsm", "waz lada", "syrena", "wartburg", "uaz", "lti")
	res =(7 - sapply(normalized, FUN = function(y) {
		res = which(sapply(prestige, FUN = function(x) {y %in% x}))
		ifelse(length(res) == 0, 5, res)
	}))
	names(res) = NULL
	res
}

train4[,"prestige"] = makes_to_prestige(train4$etx_make_name)
train4$etx_model_code = NULL
train4$etx_model_name = NULL
train4$etx_make_name = NULL
train4$CarMake__c = NULL
train4$CarModel__c = NULL