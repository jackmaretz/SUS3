dati<-read.csv('dataset_stima.csv.txt',header=T)
dativ<-read.csv('dataset_previsione.csv.txt',header=T)
dativ$y<-rep(0,nrow(dativ)) # we add the response variable column so that we can bind train and test to work on a single database  
colnames(dati)[colnames(dati)%in%'TARG_TOT']<-'y' #rename target variable
dativ<-dativ[,c(74,1:73)]



######################################
###### QUI APPLICHIAMO TUTTO PREPROCESS
dati<-rbind(dati,dativ) 
#######################################
#BILANCIAMENTO DATASET
dati1<- dati[dati$y==1, ]
dati0<- dati[dati$y[1:39943]==0, ]
set.seed(12345)
acaso <- sample(1:nrow(dati0), nrow(dati1))
datib<-rbind(dati1,dati0[acaso,])
table(datib$y)


#######FUNZIONI#############################

factordummy<-function(data)
{
  for(i in 1:ncol(data))
  {
    if(length(unique(data[,i]))<3)
    {
      data[,i]<-as.factor(data[,i])
      cat(colnames(data)[i],', ',sep='')
    }
  }
  return(data)
}

frequenze   <- function(x,cumul=FALSE,head=NULL)
{
  mult <- 100
  f <- if(is.null(head)) table(x) else {table(x)[head]}
  f<-f[!is.na(f)]
  t <- sum(f)
  pcg <- round((f/t)*mult,2)
  F <- cumsum(f)
  PCG <- round((F/t)*mult,2)
  nf <- names(f)
  n <- length(f)
  f <- append(f,t)
  pcg <- append(pcg,sum(pcg))
  nf <- append(nf,length(nf))
  
  F <- append(F,NA)
  PCG <- append(PCG,NA)
  
  FX <- cbind(format(nf),format(f),format(pcg))
  if (cumul) FX <- cbind(format(names(f)),format(f),format(pcg),format(F),format(PCG))
  nch <- nchar(FX[1,])+2
  r <- ""
  nii <- 3
  if (cumul) nii <- 5
  for (i in 1:nii) {
    r <- paste(r,"+",sep="")
    for (j in 1:nch[i]) r <- paste(r,"-",sep="")
  }
  riga <- paste(r,"+",sep="")
  
  abcd <- c("x ","n ","f ")
  if (cumul) abcd <- c("x ","n ","f ","N ","F ")
  
  r <- ""
  for (i in 1:nii) {
    r <- paste(r," ",sep="")
    for (j in 1:(nch[i]-2)) r <- paste(r," ",sep="")
    r <- paste(r,abcd[i],sep="")
  }
  intesta <- r
  
  cat(intesta,"\n")
  cat(riga,"\n")
  for (i in 1:n) {
    r <- paste("|",FX[i,1],"|",FX[i,2],"|",FX[i,3],"|")
    if (cumul) r <- paste("|",FX[i,1],"|",FX[i,2],"|",FX[i,3],"|",FX[i,4],"|",FX[i,5],"|")
    cat(r,"\n")
  }
  cat(riga,"\n")
  if (!cumul) {
    r <- paste(" ",FX[n+1,1]," ",FX[n+1,2]," ",FX[n+1,3]," ")
    cat(r,"\n")
  }
  cat("Osservazioni mancanti:",sum(is.na(x)),"\n\n")
}



# resto -------------------------------------------------------------------


colnames(dativ)
dim(dati)
colnames(dati)
#############  FACTOR DUMMY #############
dati<-factordummy(dati)    
# dati<-dati[,1:13]
# dati<-dati[,-2]

frequenze(dati$y)
summary(dati$y)

colnames(dati)

#grep(66691,dati[,2])


#Xb<-as.data.frame(model.matrix(y~.,data=datib)[,-1])
#yb<-as.numeric(datib$y)-1



# Xbv<-as.data.frame(model.matrix(y~.,data=datibv)[,-1])
# ybv<-as.numeric(datibv$y)-1
# Xbv2<-as.data.frame(model.matrix(y~.,data=datibv2)[,-1])
# ybv2<-as.numeric(datibv2$y)-1
anyNA(datib) #ci sono NA. > ocome li trattuamo?
#VARIABILI NON FATTORIALI
# summary(datib[,c(sapply(datib,class)!='factor')])
# nonfattori<-which(sapply(datib,class)!='factor')
# nonfattori
# length(nonfattori)
# attr(nonfattori,'names')
#######
# dati<-Xb[!is.na(Xb[,20]),]
# colnames(dati) #dataser dummizzato 
# dim(dati)
# dati<-dati[!is.na(dati[,21]),]
# dati<-dati[!is.na(dati[,45]),]
# dati = Xb
# dati$dummy40<-rep(0,nrow(dati))
# colnames(dati)
# dati$dummy40[is.na(dati[,40])]<-1 #xk
# table(dati$dummy40)
# table(is.na(dati[,40]))
# dati[,40]<-NULL
# dati$dummy4<-0
# dati$dummy4[is.na(dati$ANZ_BAN)]<-1  #xk
# dati[,4]<-NULL
# frequenze(dati$ANZ_PROF)
# dati$ANZ_PROF[dati$ANZ_PROF>70]<-NA    #xk
# frequenze(dati$COD_RES)
# anyNA(dati)

######


# PREPROCESSING -----------------------------------------------------------

varNA = colnames(dati)[ apply(dati, 2, anyNA) ]

countNA =array()
n=1

for (i in varNA) {
  cat(i,": with::",sum(is.na(dati[i])), "-NA-___ with::", dim(unique(dati[i]))[1],"unique values \n")
  countNA[n]=dim(unique(dati[i]))[1]
  print(n)
  n=n+1
}

summary(dati[,varNA])

na = data.frame(varNA=countNA,row.names = varNA)


na
barplot(height = na$varNA,names.arg = row.names(na),horiz = T,cex.names = 0.5)


# replace tutti na con media ----------------------------------------------

NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
dati[varNA] <- lapply(dati[varNA], NA2mean)

anyNA(dati)
#fatto



# replace na in dati bilanciati -------------------------------------------
#BILANCIAMENTO DATASET
dati1<- dati[dati$y==1, ]
dati0<- dati[dati$y[1:39943]==0, ]
set.seed(12345)
acaso <- sample(1:nrow(dati0), nrow(dati1))

################################################

# qui ci fittiamo il modello ----------------------------------------------

###############################################
datib<-rbind(dati1,dati0[acaso,])
table(datib$y)  #dati bilanciati
###############################################



frequenze(datib$y)
dim(datib)


# e' ora di testare i modelli ---------------------------------------------

#devtools::install_github("Laurae2/Laurae",force=TRUE)


# xgboost -----------------------------------------------------------------

X<-model.matrix(y~.,data=datib[,-2])[,-1]
dim(datib)
colnames(X)
dim(X)
library(data.table)
X<-as.data.table(X)
y<-as.numeric(dati[1:39943,1])-1
yv<-as.numeric(dati[-c(1:39943),1])-1

dtrain <-
  xgb.DMatrix(data = Laurae::DT2mat(X[1:39943,]), label = y)
dtest <-
  xgb.DMatrix(data = Laurae::DT2mat(X[-c(1:39943),]), label = yv)

set.seed(11111)
cv <-
  xgb.cv(
    params = list(
      nthread = 4,
      # More threads if you feel so
      eta = 0.10,
      max_depth = 6,
      booster = "gbtree",colsample_bytree=0.6
    ),
    objective = "binary:logistic",
    eval_metric = "error",
    nrounds = 10000,
    early_stopping_rounds = 50,
    data = dtrain,
    nfold = 3,
    verbose = 1
  )

bot$sendMessage('cv xgb va')

niter <- cv$best_iteration
xgbtempo <-
  round(system.time(
    xgb <-
      xgb.train(
        params = list(
          nthread = 30,
          # More threads if you feel so
          eta = 0.10,
          max_depth = 6,
          booster = "gbtree",colsample_bytree=0.3
        ),
        objective = "binary:logistic",
        eval_metric = "error",
        nrounds = niter,
        data = dtrain,
        verbose = 1
      )
  )[3] / 60);xgbtempo
p6<-predict(xgb,newdata=dtest)
class(p6)
s6<-dati[-c(1:39943),][order(p6,decreasing=T)[1:1e4],2]

write.table(s6,file='s6',row.names = F,col.names = F)

