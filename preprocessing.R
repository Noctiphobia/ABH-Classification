load('ABH.rda')
#remove IDs
ABHnoID = cbind(ABH[,!grepl(".*id.*", names(ABH), ignore.case = T)], ABH$MpcPaid__c)
names(ABHnoID)[134] = "MpcPaid_c"
names(ABHnoID)[127] = "y"
#remove 'modified date' database columns
ABHnoID = ABHnoID[,!grepl(".*modified.*", names(ABHnoID), ignore.case = T)]
#remove 'token' columns
ABHnoID = ABHnoID[,!grepl(".*token*", names(ABHnoID), ignore.case = T)]
#remove columns with one value
cols = apply(ABHnoID,2,function(x){length(unique(x))})
colnames2 = names(cols[cols>1])
colnames2 = colnames2[-which(colnames2=='phone_no')]
colnames2 = colnames2[-which(colnames2=='Account__r.Salutation')]
colnames2 = colnames2[-which(colnames2=='phone_acceptance')]
ABHnoID = ABHnoID[,colnames2]


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
ABHnoID$TotalCarCollection__c = as.integer(ABHnoID$TotalCarCollection__c)

ABHnoID$registration_date = as.numeric(ABHnoID$registration_date)
ABHnoID$insurance_start_date = as.numeric(ABHnoID$insurance_start_date)
ABHnoID$created_at = as.numeric(ABHnoID$created_at)
ABHnoID$created_at_date = as.numeric(ABHnoID$created_at_date)
ABHnoID$form_finished_at = as.numeric(ABHnoID$form_finished_at)
ABHnoID$offer_first_at = as.numeric(ABHnoID$offer_first_at)
ABHnoID$offer_last_at = as.numeric(ABHnoID$offer_last_at)
ABHnoID$Account__r.CreatedDate = as.numeric(ABHnoID$Account__r.CreatedDate)
ABHnoID$Account__r.PersonBirthdate = as.numeric(ABHnoID$Account__r.PersonBirthdate)
ABHnoID$CreatedDate = as.numeric(ABHnoID$CreatedDate)
ABHnoID$PolicyStartDate__c = as.numeric(ABHnoID$PolicyStartDate__c)
ABHnoID$LastCall__c = as.numeric(ABHnoID$LastCall__c)
ABHnoID$SystemModstamp = as.numeric(ABHnoID$SystemModstamp)

#change chars to factors
for (i in 1:length(ABHnoID)) {
  if (is.character(ABHnoID[,i])){
    ABHnoID[,i] = as.factor(ABHnoID[,i])
    ABHnoID[,i] = addNA(ABHnoID[,i])
  }
}

#drop columns based on correlation
cor2 = cor(apply(ABHnoID,2, as.numeric))

corx = character(0)
cory = character(0)
correlation = numeric(0)
for (i in 1:length(ABHnoID)) {
  for (j in 1:i) {
    if (i != j) {
      corx[length(corx) + 1] = rownames(cor2)[i]
      cory[length(cory) + 1] = colnames(cor2)[j]
      correlation[length(correlation) + 1] = cor2[i,j]
    }
  }
}

corres = na.omit(data.frame(corx, cory, correlation))
corresfiltered = corres[corres$correlation > 0.5,]
corresfiltered = corresfiltered[order(corresfiltered$correlation, decreasing = T),]

dropped = c("CreatedDate", "created_at", "created_at_date", "Last.call", "Telehub", "phone_yes", "Account__r.CreatedDate", "phone_lookup_status", "Sale.success", "TotalCarCollection__c", "etx_model_code")
ABHnoID = ABHnoID[,!(names(ABHnoID) %in% dropped)]
names(ABHnoID)

