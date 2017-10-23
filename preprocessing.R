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
ABHnoID = ABHnoID[,colnames2)]
#change chars to factors
for (i in 1:length(ABHnoID)) {
  if (is.character(ABHnoID[,i]))
    ABHnoID[,i] = as.factor(ABHnoID[,i])
}


