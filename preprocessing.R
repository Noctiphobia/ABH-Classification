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
ABHnoID = ABHnoID[,names(cols[cols>1])]
#change chars to factors
for (i in 1:length(ABHnoID)) {
  if (is.character(ABHnoID[,i]))
    ABHnoID[,i] = as.factor(ABHnoID[,i])
}


