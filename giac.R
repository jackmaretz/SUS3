train = read.csv("train.csv", header = TRUE)
test =  read.csv("test.csv",header = TRUE)

variabili = colnames(train)[14:39]
train_giac= train[colnames(train)[14:39]]
train_giac$TARG_TOT= train$TARG_TOT
sum(is.na(train_giac))
colnames(train_giac[sapply(train_giac,anyNA)])



library(corrplot)
corrplot(cor(train_giac))

table(train_giac$TARG_TOT,train_giac$FIND_NUM_MEN_RES)

(is.na(train_giac$FIND_NUM_MEN_RES))

x=apply(is.na(train_giac),1,sum)
table(x,train_giac$TARG_TOT)
train_giac=(na.omit(train_giac))

require(randomForest)
fit=randomForest(factor(TARG_TOT)~., data=train_giac,importance=TRUE)
(VI_F=(importance(fit)))
varImpPlot(fit)
str(varImpPlot(fit))
attr(importance(fit),"dimnames")[[1]][order(importance(fit))]

attr(varImpPlot(fit),"dimnames")[[1]][order(varImpPlot(fit))]


VI_F[order(VI_F)]
library(ranger)
m0= ranger(TARG_TOT~.,data=train_giac,importance="permutation",num.trees=200,write.forest=FALSE)
variable=sort(m0$variable.importance,decreasing=T)[1:10]
dati=train_giac[,c(attr(variable,'names'),'TARG_TOT')]
colnames(dati)

# GGPAIRS -----------------------------------------------------------------
library(corrplot)
corrplot(cor(dati))
