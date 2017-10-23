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
  if (is.character(ABHnoID[,i])) {
    ABHnoID[,i] = as.factor(ABHnoID[,i])
    ABHnoID[,i] = addNA(ABHnoID[,i])
  }
}
library(caret)
cor2 = cor(apply(ABHnoID,2, as.numeric), use = "pairwise.complete.obs")

corx = character(0)
cory = character(0)
correlation = numeric(0)
for (i in 1:length(ABHnoID)) {
  for (j in 1:length(ABHnoID)) {
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
