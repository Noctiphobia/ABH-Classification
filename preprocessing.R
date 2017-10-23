load('ABH.rda')
ABHnoID = cbind(ABH[,!grepl(".*id.*", names(ABH), ignore.case = T)], ABH$MpcPaid__c)
names(ABHnoID)[134] = "MpcPaid_c"
names(ABHnoID)[127] = "y"
ABHnoID = ABHnoID[,!grepl(".*modified.*", names(ABHnoID), ignore.case = T)]
ABHnoID = ABHnoID[,!grepl(".*token*", names(ABHnoID), ignore.case = T)]
cols = apply(ABHnoID,2,function(x){length(unique(x))})

#usuwanie atrybutow o jednej lub mniej wartosciach
colnames2 = names(cols[cols>1])
