library(data.table)
library(FeatureHashing)
library(xgboost)
library(dplyr)
library(Matrix)
setwd("D:/Data Science/Red Hat")
cat(Sys.time())
cat("Reading data\n")



groups <- fread("FoldGroups.csv")

folded <- list()
folded[[1]] <- which(groups$group == 0)
folded[[2]] <- which(groups$group == 1)
folded[[3]] <- which(groups$group == 2)
folded[[4]] <- which(groups$group == 3)
folded[[5]] <- which(groups$group == 4)


train=fread('act_train.csv', verbose = FALSE) %>% as.data.frame() #added verbose for silent
test=fread('act_test.csv', verbose = FALSE) %>% as.data.frame() #added verbose for silent


#people data frame
people=fread('people.csv') %>% as.data.frame()


cat(Sys.time())
cat("Processing data\n")

people$char_1<-NULL #unnecessary duplicate to char_2
names(people)[2:length(names(people))]=paste0('people_',names(people)[2:length(names(people))])

p_logi <- names(people)[which(sapply(people, is.logical))]
for (col in p_logi) set(people, j = col, value = as.numeric(people[[col]]))

people$people_group_1[people$people_group_1 %in% names(which(table(people$people_group_1)==1))]='group unique'


d1 <- merge(train, people, by = "people_id", all.x = T)
d2 <- merge(test, people, by = "people_id", all.x = T)
Y <- d1$outcome
d1$outcome <- NULL


row.train=nrow(train)
gc(verbose=FALSE)

D=rbind(d1,d2)

laurae1=fread('Date_4800457_0295614_out.csv', verbose = FALSE) %>% as.data.frame() #added verbose for silenhead(laurae1)
names(laurae1)="laurae1"
laurae2=fread('Date_4498974_0362636_out.csv', verbose = FALSE) %>% as.data.frame() #added verbose for silent
names(laurae2)="laurae2"

D=cbind(D,laurae1,laurae2)
D$i=1:dim(D)[1]

test_activity_id=test$activity_id
rm(train,test,d1,d2);gc(verbose=FALSE)


char.cols=c('activity_category','people_group_1',
            'char_1','char_2','char_3','char_4','char_5','char_6','char_7','char_8','char_9','char_10',
            'people_char_2','people_char_3','people_char_4','people_char_5','people_char_6','people_char_7','people_char_8','people_char_9')
for (f in char.cols) {
  if (class(D[[f]])=="character") {
    levels <- unique(c(D[[f]]))
    D[[f]] <- as.numeric(factor(D[[f]], levels=levels))
  }
}

head(D,20)
dim(D)

factorizer <- function(df, has_date = TRUE) {
  for (i in colnames(df)[1:ifelse(has_date == TRUE, ncol(df)-1, ncol(df))]) {
    df[[i]] <- as.factor(df[[i]])
  }
  return(df)
}

D$date <- as.Date(D$date, format = "%Y-%m-%d")
D$date <- as.numeric(D$date)
D$date <- D$date - min(D$date) + 1
data=D[,c("char_5","char_7","date")]
data <- factorizer(data, has_date=TRUE)
new_data <- sparse.model.matrix(~.^2, data = data)
threshold <- new_data@p[2:length(new_data@p)] - new_data@p[1:(length(new_data@p)-1)]
new_data=new_data[, threshold >= (417 + 424)]
dim(new_data)

D$people_date <- as.Date(D$people_date, format = "%Y-%m-%d")
D$people_date <- as.numeric(D$people_date)
D$people_date <- D$people_date - min(D$people_date) + 1
data2=D[,c("char_5","char_7","people_date")]
data2 <- factorizer(data2, has_date=TRUE)
new_data2 <- sparse.model.matrix(~.^2, data = data2) 
threshold2 <- new_data2@p[2:length(new_data2@p)] - new_data2@p[1:(length(new_data2@p)-1)]
new_data2=new_data2[, threshold2 >= (417 + 424)]
dim(new_data2)

D.sparse=
  cBind(sparseMatrix(D$i,D$activity_category),
        #sparseMatrix(D$i,D$people_group_1),
        sparseMatrix(D$i,D$char_1),
        sparseMatrix(D$i,D$char_2),
        sparseMatrix(D$i,D$char_3),
        sparseMatrix(D$i,D$char_4),
        sparseMatrix(D$i,D$char_5),
        sparseMatrix(D$i,D$char_6),
        sparseMatrix(D$i,D$char_7),
        sparseMatrix(D$i,D$char_8),
        sparseMatrix(D$i,D$char_9),
        sparseMatrix(D$i,D$people_char_2),
        sparseMatrix(D$i,D$people_char_3),
        sparseMatrix(D$i,D$people_char_4),
        sparseMatrix(D$i,D$people_char_5),
        sparseMatrix(D$i,D$people_char_6),
        sparseMatrix(D$i,D$people_char_7),
        sparseMatrix(D$i,D$people_char_8),
        sparseMatrix(D$i,D$people_char_9)
        #,sparseMatrix(D$i,as.integer(as.factor(D$min_activity_category)))
        #,sparseMatrix(D$i,as.integer(as.factor(D$max_activity_category)))
        
        #,sparseMatrix(D$i,new_data)
        #,sparseMatrix(D$i,D$Freq)
        #,sparseMatrix(D$i,as.integer(as.factor(D$min_date)))
        #,sparseMatrix(D$i,as.integer(as.factor(D$max_date)))
        #,sparseMatrix(D$i,as.integer(as.factor(D$people_id2)))
        #,sparseMatrix(D$i,as.integer(as.factor(D$activity_id2)))
        #,sparseMatrix(D$i,as.integer(as.factor(D$min_activity_category)))
        #,sparseMatrix(D$i,as.integer(as.factor(D$max_activity_category)))
        #,sparseMatrix(D$i,as.integer(as.factor(D$sum_activity_category)))
        #,sparseMatrix(D$i,as.integer(as.factor(D$min_char_10)))
        #,sparseMatrix(D$i,as.integer(as.factor(D$max_char_10)))
        #,sparseMatrix(D$i,as.integer(as.factor(D$sum_char_10)))
        #,sparseMatrix(D$i,as.integer(as.factor(D$sum_activity_id2)))
  )


dim(D.sparse)
D.sparse=
  cBind(D.sparse,
        D$people_char_10,
        D$people_char_11,
        D$people_char_12,
        D$people_char_13,
        D$people_char_14,
        D$people_char_15,
        D$people_char_16,
        D$people_char_17,
        D$people_char_18,
        D$people_char_19,
        D$people_char_20,
        D$people_char_21,
        D$people_char_22,
        D$people_char_23,
        D$people_char_24,
        D$people_char_25,
        D$people_char_26,
        D$people_char_27,
        D$people_char_28,
        D$people_char_29,
        D$people_char_30,
        D$people_char_31,
        D$people_char_32,
        D$people_char_33,
        D$people_char_34,
        D$people_char_35,
        D$people_char_36,
        D$people_char_37,
        D$people_char_38
        ,new_data
        ,new_data2
        #,new_data3
        #,new_data4
        #,new_data5
        #,new_data4
        ,D$laurae1
        ,D$laurae2
  )
dim(D.sparse)

##########




cat(Sys.time())
cat("Unmerging train/test sparse data\n")

train.sparse=D.sparse[1:row.train,]
test.sparse=D.sparse[(row.train+1):nrow(D.sparse),]




dtrain  <- xgb.DMatrix(train.sparse, label = Y)
gc(verbose=FALSE)

dtest  <- xgb.DMatrix(test.sparse)
gc(verbose=FALSE)

rm(D, new_data, new_data2, train.sparse, test.sparse, D.sparse, data, data2, groups, laurae1, laurae2, people, char.cols, col, f, levels, p_logi, row.train, threshold, threshold2, factorizer, test_activity_id)


# gblinear, eta=0.05 [41]	train-auc:0.989393+0.000237	test-auc:0.978462+0.003249
# gblinear, eta=0.005 [411]	train-auc:0.989383+0.000237	test-auc:0.978463+0.003237

# gc(verbose = FALSE)
# set.seed(11111)
# m1 <- xgb.cv(data = dtrain,
#              param,
#              nrounds = 50000,
#              early_stopping_rounds = 100,
#              folds = folded)

out_preds <- numeric(2197291 + 498687)

param <- list(objective = "binary:logistic", 
              eval_metric = "auc",
              booster = "gbtree", 
              eta = 0.05,
              max_depth = 11,
              subsample = 1.00,
              colsample_bytree = 1.00)

param <- list(objective = "binary:logistic", 
              eval_metric = "auc",
              booster = "gblinear", 
              eta = 0.05)

for (i in 1:5) {
  
  slice_train <- dtrain[setdiff(1:2197291, folded[[i]])]
  slice_test <- dtrain[folded[[i]]]
  print(slice_train)
  print(slice_test)
  
  gc(verbose = FALSE)
  set.seed(11111)
  my_model <- xgb.train(data = slice_train,
                        param,
                        nrounds = 100000,
                        early_stopping_rounds = 10,
                        watchlist = list(train = slice_train, test = slice_test))
  gc(verbose = FALSE)
  out_preds[folded[[i]]] <- predict(my_model, slice_test, ntreelimit = my_model$best_iteration)
  gc(verbose = FALSE)
  out_preds[2197292:2695978] <- (predict(my_model, dtest, ntreelimit = my_model$best_iteration) / 5) + out_preds[2197292:2695978]
  gc(verbose = FALSE)
  
}

write.csv(out_preds, file="laurae_noleak.csv", row.names=FALSE)
