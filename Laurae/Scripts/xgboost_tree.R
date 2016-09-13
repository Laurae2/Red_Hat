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

#reducing group_1 dimension
people$people_group_1[people$people_group_1 %in% names(which(table(people$people_group_1)==1))]='group unique'


#reducing char_10 dimension
#unique.char_10=
#  rbind(
#    select(train,people_id,char_10),
#    select(test,people_id,char_10)) %>% group_by(char_10) %>% 
#  summarize(n=n_distinct(people_id)) %>% 
#  filter(n==1) %>% 
#  select(char_10) %>%
#  as.matrix() %>% 
#  as.vector()

#train$char_10[train$char_10 %in% unique.char_10]='type unique'
#test$char_10[test$char_10 %in% unique.char_10]='type unique'

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





###uncomment this for CV run
# set.seed(120)
# unique_p <- unique(d1$people_id)
# valid_p  <- unique_p[sample(1:length(unique_p), 40000)]
# valid <- which(d1$people_id %in% valid_p)
# model <- (1:length(d1$people_id))[-valid]

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
#new indicators
#D=merge(D,as.data.frame(D %>% group_by(people_id) %>% summarise(min_activity_category=min(activity_category))))
#D=merge(D,as.data.frame(D %>% group_by(people_id) %>% summarise(max_activity_category=max(activity_category))))

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

#data3=D[,c("char_10","activity_category","people_date","date")]
#new_data3 <- sparse.model.matrix(~.^2, data = data3) 
#threshold3 <- new_data3@p[2:length(new_data3@p)] - new_data3@p[1:(length(new_data3@p)-1)]
#new_data3=new_data3[, threshold3 >= 417]
#dim(new_data3)

#data4=D[,c("people_group_1","people_date","date")]
#new_data4 <- sparse.model.matrix(~.^2, data = data4) 
#threshold3 <- new_data4@p[2:length(new_data4@p)] - new_data4@p[1:(length(new_data4@p)-1)]
#new_data4=new_data4[, threshold3 >= 417]
#dim(new_data4)

#data5=D[,c("char_4","char_6","date","char_5","char_7","people_date")]
#new_data5 <- sparse.model.matrix(~.^2, data = data5) 
#threshold3 <- new_data4@p[2:length(new_data4@p)] - new_data4@p[1:(length(new_data4@p)-1)]
#new_data4=new_data4[, threshold3 >= 417]
#dim(new_data5)

D.sparse=
  cBind(sparseMatrix(D$i,D$activity_category),
        sparseMatrix(D$i,D$people_group_1),
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
#test.sparse=D.sparse[(row.train+1):nrow(D.sparse),]



# Hash train to sparse dmatrix X_train + LibSVM/SVMLight format



cat(Sys.time())
cat("Making data for SVMLight format\n")

# LibSVM format if you use Python / etc. ALWAYS USEFUL


# TOO LONG

cat("Creating SVMLight format\n")
dtrain  <- xgb.DMatrix(train.sparse, label = Y)
gc(verbose=FALSE)
cat("Exporting SVMLight format\n")
#xgb.DMatrix.save(dtrain, "dtrain.data")
gc(verbose=FALSE)
#rm(dtrain) #avoid getting through memory limits
#gc(verbose=FALSE)
cat("Zipping SVMLight\n")
#zip("dtrain.data.zip", "dtrain.data", flags = "-m9X", extras = "", zip = Sys.getenv("R_ZIPCMD", "zip"))
#file.remove("dtrain.data")

cat(Sys.time())
cat("File size of train in SVMLight: ", file.size("dtrain.data.zip"), "\n", sep = "")

cat("Creating SVMLight format\n")
#                                 dtest  <- xgb.DMatrix(test.sparse)
gc(verbose=FALSE)
cat("Exporting SVMLight format\n")
#xgb.DMatrix.save(dtest, "dtest.data")
gc(verbose=FALSE)
cat("Zipping SVMLight\n")
#zip("dtest.data.zip", "dtest.data", flags = "-m9X", extras = "", zip = Sys.getenv("R_ZIPCMD", "zip"))
#file.remove("dtest.data")
cat(Sys.time())
cat("File size of test in SVMLight: ", file.size("dtest.data.zip"), "\n", sep = "")


#cat("Re-creating SVMLight format\n")
#dtrain  <- xgb.DMatrix(train.sparse, label = Y) #recreate train sparse to run under the memory limit of 8589934592 bytes
gc(verbose=FALSE)


param <- list(objective = "binary:logistic", 
              eval_metric = "auc",
              booster = "gbtree", 
              eta = 0.05,
              subsample = 0.86,
              colsample_bytree = 0.92,
              colsample_bylevel = 0.9,
              min_child_weight = 0,
              max_depth = 11)

rm(D, new_data, new_data2, train.sparse, D.sparse, data, data2, groups, laurae1, laurae2, people, char.cols, col, f, levels, p_logi, row.train, threshold, factorizer, test_activity_id)


# gblinear, eta=0.05 [41]	train-auc:0.989393+0.000237	test-auc:0.978462+0.003249
# gblinear, eta=0.005 [411]	train-auc:0.989383+0.000237	test-auc:0.978463+0.003237

gc(verbose = FALSE)
set.seed(11111)
m1 <- xgb.cv(data = dtrain,
             param,
             nrounds = 50000,
             early_stopping_rounds = 100,
             folds = folded)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ STOP HERE
quit()


# [1877]	train-auc:0.989992+0.000365	test-auc:0.963346+0.008329
# Still didn't stop converging


# [889]	train-auc:0.984136+0.000477	test-auc:0.957983+0.008339 
# [890]	train-auc:0.984143+0.000478	test-auc:0.957988+0.008340 
# [891]	train-auc:0.984159+0.000469	test-auc:0.958015+0.008327 
# [892]	train-auc:0.984169+0.000472	test-auc:0.958020+0.008326 
# [893]	train-auc:0.984178+0.000470	test-auc:0.958023+0.008327 
# [894]	train-auc:0.984192+0.000479	test-auc:0.958039+0.008315 
# [895]	train-auc:0.984200+0.000480	test-auc:0.958045+0.008316 
# [896]	train-auc:0.984205+0.000483	test-auc:0.958048+0.008317 
# [897]	train-auc:0.984208+0.000480	test-auc:0.958072+0.008282 
# [898]	train-auc:0.984218+0.000481	test-auc:0.958100+0.008285 
# [899]	train-auc:0.984224+0.000481	test-auc:0.958104+0.008284 
# [900]	train-auc:0.984232+0.000481	test-auc:0.958107+0.008283 
# [901]	train-auc:0.984241+0.000484	test-auc:0.958110+0.008282 
# [902]	train-auc:0.984250+0.000484	test-auc:0.958116+0.008283 
# [903]	train-auc:0.984259+0.000482	test-auc:0.958121+0.008277 
# [904]	train-auc:0.984265+0.000483	test-auc:0.958125+0.008277 
# [905]	train-auc:0.984275+0.000485	test-auc:0.958130+0.008276 
# [906]	train-auc:0.984284+0.000485	test-auc:0.958135+0.008273 
# [907]	train-auc:0.984297+0.000488	test-auc:0.958146+0.008280 
# [908]	train-auc:0.984305+0.000487	test-auc:0.958149+0.008279 
# [909]	train-auc:0.984314+0.000486	test-auc:0.958153+0.008280 
# [910]	train-auc:0.984323+0.000485	test-auc:0.958163+0.008282 
# [911]	train-auc:0.984339+0.000481	test-auc:0.958182+0.008277 
# [912]	train-auc:0.984349+0.000481	test-auc:0.958185+0.008279 
# [913]	train-auc:0.984356+0.000479	test-auc:0.958188+0.008278 
# [914]	train-auc:0.984364+0.000482	test-auc:0.958189+0.008276 
# [915]	train-auc:0.984384+0.000496	test-auc:0.958210+0.008271 
# [916]	train-auc:0.984393+0.000495	test-auc:0.958228+0.008256 
# [917]	train-auc:0.984399+0.000495	test-auc:0.958237+0.008256 
# [918]	train-auc:0.984409+0.000493	test-auc:0.958252+0.008239 
# [919]	train-auc:0.984415+0.000492	test-auc:0.958257+0.008239 
# [920]	train-auc:0.984420+0.000488	test-auc:0.958264+0.008239 
# [921]	train-auc:0.984440+0.000474	test-auc:0.958270+0.008241 
# [922]	train-auc:0.984449+0.000473	test-auc:0.958278+0.008233 
# [923]	train-auc:0.984458+0.000474	test-auc:0.958281+0.008235 
# [924]	train-auc:0.984465+0.000477	test-auc:0.958292+0.008235 
# [925]	train-auc:0.984471+0.000475	test-auc:0.958298+0.008234 
# [926]	train-auc:0.984476+0.000474	test-auc:0.958302+0.008232 
# [927]	train-auc:0.984485+0.000473	test-auc:0.958322+0.008232 
# [928]	train-auc:0.984494+0.000472	test-auc:0.958327+0.008232 
# [929]	train-auc:0.984500+0.000472	test-auc:0.958331+0.008233 
# [930]	train-auc:0.984509+0.000473	test-auc:0.958335+0.008235 
# [931]	train-auc:0.984519+0.000471	test-auc:0.958348+0.008229 
# [932]	train-auc:0.984526+0.000470	test-auc:0.958353+0.008228 
# [933]	train-auc:0.984532+0.000474	test-auc:0.958358+0.008225 
# [934]	train-auc:0.984538+0.000474	test-auc:0.958362+0.008225 
# [935]	train-auc:0.984554+0.000471	test-auc:0.958375+0.008232 
# [936]	train-auc:0.984562+0.000472	test-auc:0.958380+0.008229 
# [937]	train-auc:0.984575+0.000470	test-auc:0.958385+0.008229 
# [938]	train-auc:0.984593+0.000480	test-auc:0.958394+0.008231 
# [939]	train-auc:0.984598+0.000481	test-auc:0.958399+0.008231 
# [940]	train-auc:0.984607+0.000484	test-auc:0.958406+0.008230 
# [941]	train-auc:0.984614+0.000484	test-auc:0.958405+0.008237 
# [942]	train-auc:0.984631+0.000501	test-auc:0.958420+0.008240 
# [943]	train-auc:0.984634+0.000503	test-auc:0.958433+0.008243 
# [944]	train-auc:0.984638+0.000500	test-auc:0.958439+0.008240 
# [945]	train-auc:0.984648+0.000498	test-auc:0.958448+0.008246 
# [946]	train-auc:0.984654+0.000499	test-auc:0.958453+0.008248 
# [947]	train-auc:0.984663+0.000500	test-auc:0.958459+0.008249 
# [948]	train-auc:0.984669+0.000500	test-auc:0.958462+0.008250 
# [949]	train-auc:0.984677+0.000499	test-auc:0.958483+0.008247 
# [950]	train-auc:0.984685+0.000501	test-auc:0.958496+0.008233 
# [951]	train-auc:0.984694+0.000501	test-auc:0.958500+0.008233 
# [952]	train-auc:0.984701+0.000499	test-auc:0.958512+0.008225 
# [953]	train-auc:0.984706+0.000498	test-auc:0.958516+0.008226 
# [954]	train-auc:0.984721+0.000509	test-auc:0.958547+0.008220 
# [955]	train-auc:0.984731+0.000512	test-auc:0.958553+0.008221 
# [956]	train-auc:0.984739+0.000511	test-auc:0.958563+0.008224 
# [957]	train-auc:0.984748+0.000509	test-auc:0.958576+0.008221 
# [958]	train-auc:0.984755+0.000508	test-auc:0.958579+0.008222 
# [959]	train-auc:0.984762+0.000507	test-auc:0.958582+0.008223 
# [960]	train-auc:0.984781+0.000513	test-auc:0.958590+0.008226 
# [961]	train-auc:0.984790+0.000513	test-auc:0.958613+0.008223 
# [962]	train-auc:0.984797+0.000514	test-auc:0.958617+0.008223 
# [963]	train-auc:0.984803+0.000515	test-auc:0.958625+0.008223 
# [964]	train-auc:0.984809+0.000515	test-auc:0.958628+0.008224 
# [965]	train-auc:0.984818+0.000517	test-auc:0.958631+0.008223 
# [966]	train-auc:0.984828+0.000522	test-auc:0.958642+0.008224 
# [967]	train-auc:0.984835+0.000523	test-auc:0.958647+0.008225 
# [968]	train-auc:0.984841+0.000524	test-auc:0.958653+0.008225 
# [969]	train-auc:0.984847+0.000524	test-auc:0.958653+0.008226 
# [970]	train-auc:0.984855+0.000524	test-auc:0.958652+0.008226 
# [971]	train-auc:0.984864+0.000525	test-auc:0.958665+0.008230 
# [972]	train-auc:0.984871+0.000528	test-auc:0.958669+0.008231 
# [973]	train-auc:0.984882+0.000523	test-auc:0.958681+0.008240 
# [974]	train-auc:0.984889+0.000524	test-auc:0.958685+0.008240 
# [975]	train-auc:0.984895+0.000525	test-auc:0.958691+0.008237 
# [976]	train-auc:0.984900+0.000525	test-auc:0.958696+0.008238 
# [977]	train-auc:0.984912+0.000525	test-auc:0.958708+0.008244 
# [978]	train-auc:0.984916+0.000523	test-auc:0.958715+0.008244 
# [979]	train-auc:0.984923+0.000523	test-auc:0.958727+0.008243 
# [980]	train-auc:0.984931+0.000522	test-auc:0.958734+0.008238 
# [981]	train-auc:0.984938+0.000521	test-auc:0.958736+0.008238 
# [982]	train-auc:0.984943+0.000521	test-auc:0.958741+0.008239 
# [983]	train-auc:0.984950+0.000519	test-auc:0.958745+0.008240 
# [984]	train-auc:0.984958+0.000519	test-auc:0.958749+0.008243 
# [985]	train-auc:0.984958+0.000520	test-auc:0.958749+0.008240 
# [986]	train-auc:0.984965+0.000521	test-auc:0.958756+0.008238 
# [987]	train-auc:0.984975+0.000519	test-auc:0.958771+0.008222 
# [988]	train-auc:0.984983+0.000517	test-auc:0.958779+0.008222 
# [989]	train-auc:0.985016+0.000492	test-auc:0.958686+0.008427 
# [990]	train-auc:0.985031+0.000485	test-auc:0.958696+0.008429 
# [991]	train-auc:0.985042+0.000494	test-auc:0.958704+0.008427 
# [992]	train-auc:0.985050+0.000496	test-auc:0.958709+0.008428 
# [993]	train-auc:0.985054+0.000496	test-auc:0.958710+0.008429 
# [994]	train-auc:0.985061+0.000496	test-auc:0.958713+0.008432 
# [995]	train-auc:0.985069+0.000494	test-auc:0.958725+0.008434 
# [996]	train-auc:0.985076+0.000494	test-auc:0.958737+0.008440 
# [997]	train-auc:0.985081+0.000493	test-auc:0.958744+0.008439 
# [998]	train-auc:0.985088+0.000493	test-auc:0.958747+0.008439 
# [999]	train-auc:0.985094+0.000493	test-auc:0.958752+0.008438 
# [1000]	train-auc:0.985103+0.000491	test-auc:0.958757+0.008435 
# [1001]	train-auc:0.985109+0.000489	test-auc:0.958759+0.008434 
# [1002]	train-auc:0.985113+0.000491	test-auc:0.958760+0.008432 
# [1003]	train-auc:0.985119+0.000492	test-auc:0.958764+0.008433 
# [1004]	train-auc:0.985125+0.000491	test-auc:0.958772+0.008435 
# [1005]	train-auc:0.985134+0.000492	test-auc:0.958788+0.008427 
# [1006]	train-auc:0.985140+0.000493	test-auc:0.958794+0.008423 
# [1007]	train-auc:0.985146+0.000493	test-auc:0.958798+0.008422 
# [1008]	train-auc:0.985154+0.000492	test-auc:0.958814+0.008430 
# [1009]	train-auc:0.985162+0.000494	test-auc:0.958820+0.008434 
# [1010]	train-auc:0.985172+0.000495	test-auc:0.958832+0.008423 
# [1011]	train-auc:0.985179+0.000497	test-auc:0.958834+0.008423 
# [1012]	train-auc:0.985182+0.000497	test-auc:0.958841+0.008415 
# [1013]	train-auc:0.985189+0.000497	test-auc:0.958845+0.008414 
# [1014]	train-auc:0.985194+0.000497	test-auc:0.958847+0.008412 
# [1015]	train-auc:0.985201+0.000494	test-auc:0.958855+0.008404 
# [1016]	train-auc:0.985208+0.000494	test-auc:0.958859+0.008401 
# [1017]	train-auc:0.985213+0.000493	test-auc:0.958865+0.008405 
# [1018]	train-auc:0.985220+0.000489	test-auc:0.958878+0.008411 
# [1019]	train-auc:0.985227+0.000486	test-auc:0.958886+0.008401 
# [1020]	train-auc:0.985237+0.000485	test-auc:0.958904+0.008409 
# [1021]	train-auc:0.985240+0.000484	test-auc:0.958908+0.008410 
# [1022]	train-auc:0.985248+0.000484	test-auc:0.958912+0.008411 
# [1023]	train-auc:0.985256+0.000483	test-auc:0.958929+0.008406 
# [1024]	train-auc:0.985265+0.000485	test-auc:0.958934+0.008408 
# [1025]	train-auc:0.985288+0.000470	test-auc:0.958955+0.008378 
# [1026]	train-auc:0.985297+0.000469	test-auc:0.958958+0.008376 
# [1027]	train-auc:0.985305+0.000471	test-auc:0.958967+0.008374 
# [1028]	train-auc:0.985311+0.000472	test-auc:0.959000+0.008369 
# [1029]	train-auc:0.985317+0.000470	test-auc:0.959002+0.008368 
# [1030]	train-auc:0.985326+0.000469	test-auc:0.959015+0.008375 
# [1031]	train-auc:0.985329+0.000468	test-auc:0.959016+0.008375 
# [1032]	train-auc:0.985339+0.000469	test-auc:0.959024+0.008373 
# [1033]	train-auc:0.985347+0.000469	test-auc:0.959036+0.008373 
# [1034]	train-auc:0.985355+0.000468	test-auc:0.959042+0.008374 
# [1035]	train-auc:0.985364+0.000472	test-auc:0.959056+0.008375 
# [1036]	train-auc:0.985365+0.000469	test-auc:0.959055+0.008376 
# [1037]	train-auc:0.985370+0.000469	test-auc:0.959060+0.008375 
# [1038]	train-auc:0.985376+0.000471	test-auc:0.959068+0.008381 
# [1039]	train-auc:0.985380+0.000472	test-auc:0.959075+0.008378 
# [1040]	train-auc:0.985390+0.000470	test-auc:0.959081+0.008380 
# [1041]	train-auc:0.985399+0.000467	test-auc:0.959085+0.008380 
# [1042]	train-auc:0.985406+0.000466	test-auc:0.959094+0.008380 
# [1043]	train-auc:0.985412+0.000464	test-auc:0.959103+0.008382 
# [1044]	train-auc:0.985421+0.000466	test-auc:0.959113+0.008386 
# [1045]	train-auc:0.985428+0.000465	test-auc:0.959121+0.008389 
# [1046]	train-auc:0.985434+0.000466	test-auc:0.959130+0.008390 
# [1047]	train-auc:0.985448+0.000459	test-auc:0.959136+0.008391 
# [1048]	train-auc:0.985454+0.000459	test-auc:0.959142+0.008389 
# [1049]	train-auc:0.985461+0.000458	test-auc:0.959149+0.008392 
# [1050]	train-auc:0.985466+0.000455	test-auc:0.959158+0.008400 
# [1051]	train-auc:0.985473+0.000453	test-auc:0.959166+0.008402 
# [1052]	train-auc:0.985479+0.000453	test-auc:0.959168+0.008401 
# [1053]	train-auc:0.985486+0.000454	test-auc:0.959182+0.008406 
# [1054]	train-auc:0.985505+0.000442	test-auc:0.959179+0.008418 
# [1055]	train-auc:0.985513+0.000442	test-auc:0.959185+0.008418 
# [1056]	train-auc:0.985519+0.000441	test-auc:0.959187+0.008418 
# [1057]	train-auc:0.985528+0.000438	test-auc:0.959195+0.008408 
# [1058]	train-auc:0.985547+0.000433	test-auc:0.959215+0.008382 
# [1059]	train-auc:0.985553+0.000433	test-auc:0.959219+0.008383 
# [1060]	train-auc:0.985559+0.000433	test-auc:0.959224+0.008382 
# [1061]	train-auc:0.985570+0.000441	test-auc:0.959227+0.008381 
# [1062]	train-auc:0.985577+0.000442	test-auc:0.959229+0.008391 
# [1063]	train-auc:0.985583+0.000442	test-auc:0.959236+0.008393 
# [1064]	train-auc:0.985589+0.000442	test-auc:0.959239+0.008394 
# [1065]	train-auc:0.985593+0.000443	test-auc:0.959251+0.008375 
# [1066]	train-auc:0.985599+0.000446	test-auc:0.959251+0.008377 
# [1067]	train-auc:0.985613+0.000450	test-auc:0.959261+0.008382 
# [1068]	train-auc:0.985631+0.000443	test-auc:0.959273+0.008388 
# [1069]	train-auc:0.985637+0.000442	test-auc:0.959278+0.008388 
# [1070]	train-auc:0.985644+0.000441	test-auc:0.959283+0.008388 
# [1071]	train-auc:0.985654+0.000448	test-auc:0.959287+0.008385 
# [1072]	train-auc:0.985669+0.000444	test-auc:0.959292+0.008387 
# [1073]	train-auc:0.985676+0.000448	test-auc:0.959296+0.008386 
# [1074]	train-auc:0.985685+0.000447	test-auc:0.959301+0.008385 
# [1075]	train-auc:0.985687+0.000445	test-auc:0.959304+0.008386 
# [1076]	train-auc:0.985692+0.000446	test-auc:0.959311+0.008386 
# [1077]	train-auc:0.985704+0.000441	test-auc:0.959309+0.008398 
# [1078]	train-auc:0.985715+0.000440	test-auc:0.959310+0.008404 
# [1079]	train-auc:0.985733+0.000432	test-auc:0.959322+0.008403 
# [1080]	train-auc:0.985739+0.000432	test-auc:0.959323+0.008402 
# [1081]	train-auc:0.985745+0.000432	test-auc:0.959327+0.008400 
# [1082]	train-auc:0.985755+0.000433	test-auc:0.959329+0.008401 
# [1083]	train-auc:0.985760+0.000434	test-auc:0.959332+0.008401 
# [1084]	train-auc:0.985767+0.000435	test-auc:0.959334+0.008402 
# [1085]	train-auc:0.985774+0.000439	test-auc:0.959340+0.008401 
# [1086]	train-auc:0.985782+0.000440	test-auc:0.959347+0.008393 
# [1087]	train-auc:0.985788+0.000439	test-auc:0.959353+0.008389 
# [1088]	train-auc:0.985796+0.000437	test-auc:0.959359+0.008389 
# [1089]	train-auc:0.985802+0.000439	test-auc:0.959376+0.008398 
# [1090]	train-auc:0.985805+0.000435	test-auc:0.959379+0.008396 
# [1091]	train-auc:0.985813+0.000435	test-auc:0.959389+0.008402 
# [1092]	train-auc:0.985817+0.000438	test-auc:0.959393+0.008403 
# [1093]	train-auc:0.985828+0.000433	test-auc:0.959418+0.008363 
# [1094]	train-auc:0.985833+0.000432	test-auc:0.959424+0.008362 
# [1095]	train-auc:0.985841+0.000431	test-auc:0.959433+0.008364 
# [1096]	train-auc:0.985849+0.000431	test-auc:0.959439+0.008364 
# [1097]	train-auc:0.985855+0.000430	test-auc:0.959446+0.008359 
# [1098]	train-auc:0.985860+0.000431	test-auc:0.959460+0.008348 
# [1099]	train-auc:0.985865+0.000428	test-auc:0.959462+0.008350 
# [1100]	train-auc:0.985882+0.000417	test-auc:0.959467+0.008350 
# [1101]	train-auc:0.985887+0.000420	test-auc:0.959472+0.008346 
# [1102]	train-auc:0.985893+0.000420	test-auc:0.959476+0.008346 
# [1103]	train-auc:0.985907+0.000422	test-auc:0.959491+0.008334 
# [1104]	train-auc:0.985913+0.000423	test-auc:0.959498+0.008335 
# [1105]	train-auc:0.985920+0.000421	test-auc:0.959506+0.008336 
# [1106]	train-auc:0.985927+0.000420	test-auc:0.959562+0.008344 
# [1107]	train-auc:0.985933+0.000420	test-auc:0.959563+0.008342 
# [1108]	train-auc:0.985939+0.000422	test-auc:0.959564+0.008341 
# [1109]	train-auc:0.985957+0.000436	test-auc:0.959570+0.008342 
# [1110]	train-auc:0.985962+0.000437	test-auc:0.959572+0.008341 
# [1111]	train-auc:0.985969+0.000436	test-auc:0.959576+0.008342 
# [1112]	train-auc:0.985974+0.000434	test-auc:0.959581+0.008346 
# [1113]	train-auc:0.985994+0.000422	test-auc:0.959580+0.008344 
# [1114]	train-auc:0.986000+0.000423	test-auc:0.959585+0.008341 
# [1115]	train-auc:0.986007+0.000423	test-auc:0.959591+0.008341 
# [1116]	train-auc:0.986010+0.000424	test-auc:0.959603+0.008342 
# [1117]	train-auc:0.986016+0.000423	test-auc:0.959608+0.008339 
# [1118]	train-auc:0.986037+0.000441	test-auc:0.959616+0.008344 
# [1119]	train-auc:0.986044+0.000440	test-auc:0.959621+0.008345 
# [1120]	train-auc:0.986049+0.000441	test-auc:0.959625+0.008345 
# [1121]	train-auc:0.986054+0.000440	test-auc:0.959630+0.008346 
# [1122]	train-auc:0.986063+0.000443	test-auc:0.959633+0.008347 
# [1123]	train-auc:0.986071+0.000445	test-auc:0.959647+0.008354 
# [1124]	train-auc:0.986079+0.000446	test-auc:0.959657+0.008347 
# [1125]	train-auc:0.986083+0.000447	test-auc:0.959664+0.008344 
# [1126]	train-auc:0.986090+0.000447	test-auc:0.959677+0.008324 
# [1127]	train-auc:0.986092+0.000444	test-auc:0.959680+0.008325 
# [1128]	train-auc:0.986098+0.000443	test-auc:0.959685+0.008325 
# [1129]	train-auc:0.986103+0.000445	test-auc:0.959691+0.008324 
# [1130]	train-auc:0.986109+0.000444	test-auc:0.959696+0.008325 
# [1131]	train-auc:0.986115+0.000445	test-auc:0.959702+0.008326 
# [1132]	train-auc:0.986124+0.000453	test-auc:0.959708+0.008323 
# [1133]	train-auc:0.986127+0.000457	test-auc:0.959715+0.008321 
# [1134]	train-auc:0.986134+0.000458	test-auc:0.959716+0.008317 
# [1135]	train-auc:0.986139+0.000458	test-auc:0.959710+0.008336 
# [1136]	train-auc:0.986147+0.000460	test-auc:0.959713+0.008336 
# [1137]	train-auc:0.986153+0.000461	test-auc:0.959726+0.008341 
# [1138]	train-auc:0.986159+0.000458	test-auc:0.959727+0.008340 
# [1139]	train-auc:0.986167+0.000457	test-auc:0.959731+0.008339 
# [1140]	train-auc:0.986175+0.000458	test-auc:0.959734+0.008339 
# [1141]	train-auc:0.986183+0.000459	test-auc:0.959747+0.008342 
# [1142]	train-auc:0.986191+0.000457	test-auc:0.959751+0.008340 
# [1143]	train-auc:0.986195+0.000455	test-auc:0.959757+0.008341 
# [1144]	train-auc:0.986202+0.000457	test-auc:0.959761+0.008341 
# [1145]	train-auc:0.986208+0.000461	test-auc:0.959778+0.008343 
# [1146]	train-auc:0.986213+0.000462	test-auc:0.959785+0.008344 
# [1147]	train-auc:0.986219+0.000462	test-auc:0.959790+0.008345 
# [1148]	train-auc:0.986225+0.000462	test-auc:0.959792+0.008345 
# [1149]	train-auc:0.986231+0.000462	test-auc:0.959799+0.008343 
# [1150]	train-auc:0.986236+0.000460	test-auc:0.959803+0.008344 
# [1151]	train-auc:0.986241+0.000460	test-auc:0.959800+0.008356 
# [1152]	train-auc:0.986246+0.000461	test-auc:0.959814+0.008350 
# [1153]	train-auc:0.986248+0.000461	test-auc:0.959817+0.008348 
# [1154]	train-auc:0.986253+0.000461	test-auc:0.959822+0.008344 
# [1155]	train-auc:0.986260+0.000460	test-auc:0.959824+0.008343 
# [1156]	train-auc:0.986264+0.000461	test-auc:0.959828+0.008343 
# [1157]	train-auc:0.986270+0.000460	test-auc:0.959833+0.008342 
# [1158]	train-auc:0.986276+0.000461	test-auc:0.959836+0.008343 
# [1159]	train-auc:0.986282+0.000462	test-auc:0.959840+0.008344 
# [1160]	train-auc:0.986287+0.000462	test-auc:0.959846+0.008342 
# [1161]	train-auc:0.986291+0.000458	test-auc:0.959849+0.008342 
# [1162]	train-auc:0.986297+0.000459	test-auc:0.959852+0.008343 
# [1163]	train-auc:0.986301+0.000460	test-auc:0.959858+0.008343 
# [1164]	train-auc:0.986308+0.000460	test-auc:0.959866+0.008340 
# [1165]	train-auc:0.986342+0.000441	test-auc:0.959879+0.008340 
# [1166]	train-auc:0.986350+0.000443	test-auc:0.959903+0.008346 
# [1167]	train-auc:0.986356+0.000441	test-auc:0.959917+0.008328 
# [1168]	train-auc:0.986363+0.000440	test-auc:0.959923+0.008325 
# [1169]	train-auc:0.986365+0.000441	test-auc:0.959929+0.008327 
# [1170]	train-auc:0.986371+0.000441	test-auc:0.959933+0.008327 
# [1171]	train-auc:0.986382+0.000448	test-auc:0.959945+0.008333 
# [1172]	train-auc:0.986386+0.000447	test-auc:0.959952+0.008337 
# [1173]	train-auc:0.986392+0.000446	test-auc:0.959958+0.008331 
# [1174]	train-auc:0.986396+0.000448	test-auc:0.959970+0.008337 
# [1175]	train-auc:0.986413+0.000442	test-auc:0.959981+0.008342 
# [1176]	train-auc:0.986422+0.000440	test-auc:0.959994+0.008347 
# [1177]	train-auc:0.986433+0.000429	test-auc:0.960006+0.008342 
# [1178]	train-auc:0.986435+0.000426	test-auc:0.960008+0.008342 
# [1179]	train-auc:0.986441+0.000426	test-auc:0.960012+0.008341 
# [1180]	train-auc:0.986444+0.000424	test-auc:0.960016+0.008341 
# [1181]	train-auc:0.986450+0.000425	test-auc:0.960016+0.008340 
# [1182]	train-auc:0.986454+0.000425	test-auc:0.960021+0.008336 
# [1183]	train-auc:0.986459+0.000424	test-auc:0.960025+0.008337 
# [1184]	train-auc:0.986473+0.000420	test-auc:0.960032+0.008338 
# [1185]	train-auc:0.986484+0.000415	test-auc:0.960039+0.008348 
# [1186]	train-auc:0.986495+0.000408	test-auc:0.960054+0.008326 
# [1187]	train-auc:0.986500+0.000409	test-auc:0.960063+0.008327 
# [1188]	train-auc:0.986517+0.000400	test-auc:0.960071+0.008330 
# [1189]	train-auc:0.986530+0.000395	test-auc:0.960078+0.008333 
# [1190]	train-auc:0.986535+0.000395	test-auc:0.960080+0.008332 
# [1191]	train-auc:0.986541+0.000394	test-auc:0.960095+0.008319 
# [1192]	train-auc:0.986551+0.000388	test-auc:0.960102+0.008303 
# [1193]	train-auc:0.986555+0.000388	test-auc:0.960114+0.008301 
# [1194]	train-auc:0.986558+0.000390	test-auc:0.960122+0.008291 
# [1195]	train-auc:0.986563+0.000389	test-auc:0.960128+0.008291 
# [1196]	train-auc:0.986568+0.000388	test-auc:0.960132+0.008293 
# [1197]	train-auc:0.986575+0.000388	test-auc:0.960137+0.008295 
# [1198]	train-auc:0.986584+0.000384	test-auc:0.960147+0.008302 
# [1199]	train-auc:0.986590+0.000386	test-auc:0.960155+0.008303 
# [1200]	train-auc:0.986595+0.000387	test-auc:0.960159+0.008303 
# [1201]	train-auc:0.986601+0.000386	test-auc:0.960174+0.008308 
# [1202]	train-auc:0.986607+0.000385	test-auc:0.960178+0.008310 
# [1203]	train-auc:0.986612+0.000384	test-auc:0.960183+0.008309 
# [1204]	train-auc:0.986618+0.000385	test-auc:0.960191+0.008314 
# [1205]	train-auc:0.986625+0.000383	test-auc:0.960194+0.008316 
# [1206]	train-auc:0.986632+0.000382	test-auc:0.960205+0.008320 
# [1207]	train-auc:0.986638+0.000383	test-auc:0.960209+0.008319 
# [1208]	train-auc:0.986643+0.000384	test-auc:0.960213+0.008320 
# [1209]	train-auc:0.986645+0.000381	test-auc:0.960214+0.008320 
# [1210]	train-auc:0.986650+0.000381	test-auc:0.960217+0.008319 
# [1211]	train-auc:0.986654+0.000379	test-auc:0.960220+0.008317 
# [1212]	train-auc:0.986660+0.000379	test-auc:0.960226+0.008313 
# [1213]	train-auc:0.986666+0.000376	test-auc:0.960229+0.008315 
# [1214]	train-auc:0.986672+0.000376	test-auc:0.960231+0.008314 
# [1215]	train-auc:0.986677+0.000376	test-auc:0.960234+0.008313 
# [1216]	train-auc:0.986684+0.000373	test-auc:0.960241+0.008316 
# [1217]	train-auc:0.986685+0.000375	test-auc:0.960248+0.008306 
# [1218]	train-auc:0.986697+0.000391	test-auc:0.960267+0.008305 
# [1219]	train-auc:0.986704+0.000388	test-auc:0.960275+0.008302 
# [1220]	train-auc:0.986710+0.000387	test-auc:0.960278+0.008299 
# [1221]	train-auc:0.986713+0.000388	test-auc:0.960278+0.008297 
# [1222]	train-auc:0.986718+0.000386	test-auc:0.960279+0.008300 
# [1223]	train-auc:0.986728+0.000383	test-auc:0.960288+0.008299 
# [1224]	train-auc:0.986734+0.000383	test-auc:0.960294+0.008295 
# [1225]	train-auc:0.986744+0.000383	test-auc:0.960296+0.008297 
# [1226]	train-auc:0.986749+0.000384	test-auc:0.960302+0.008297 
# [1227]	train-auc:0.986755+0.000383	test-auc:0.960307+0.008289 
# [1228]	train-auc:0.986760+0.000384	test-auc:0.960311+0.008289 
# [1229]	train-auc:0.986765+0.000384	test-auc:0.960314+0.008290 
# [1230]	train-auc:0.986768+0.000383	test-auc:0.960317+0.008289 
# [1231]	train-auc:0.986774+0.000382	test-auc:0.960319+0.008288 
# [1232]	train-auc:0.986778+0.000382	test-auc:0.960326+0.008287 
# [1233]	train-auc:0.986784+0.000381	test-auc:0.960329+0.008287 
# [1234]	train-auc:0.986789+0.000381	test-auc:0.960341+0.008293 
# [1235]	train-auc:0.986798+0.000379	test-auc:0.960347+0.008286 
# [1236]	train-auc:0.986802+0.000379	test-auc:0.960351+0.008282 
# [1237]	train-auc:0.986807+0.000377	test-auc:0.960358+0.008285 
# [1238]	train-auc:0.986812+0.000377	test-auc:0.960364+0.008286 
# [1239]	train-auc:0.986816+0.000377	test-auc:0.960365+0.008288 
# [1240]	train-auc:0.986821+0.000377	test-auc:0.960368+0.008287 
# [1241]	train-auc:0.986827+0.000379	test-auc:0.960370+0.008286 
# [1242]	train-auc:0.986835+0.000377	test-auc:0.960373+0.008291 
# [1243]	train-auc:0.986839+0.000376	test-auc:0.960376+0.008292 
# [1244]	train-auc:0.986842+0.000377	test-auc:0.960378+0.008292 
# [1245]	train-auc:0.986848+0.000376	test-auc:0.960388+0.008291 
# [1246]	train-auc:0.986850+0.000376	test-auc:0.960391+0.008292 
# [1247]	train-auc:0.986860+0.000382	test-auc:0.960397+0.008293 
# [1248]	train-auc:0.986862+0.000380	test-auc:0.960402+0.008293 
# [1249]	train-auc:0.986868+0.000379	test-auc:0.960409+0.008296 
# [1250]	train-auc:0.986872+0.000379	test-auc:0.960413+0.008296 
# [1251]	train-auc:0.986877+0.000378	test-auc:0.960418+0.008293 
# [1252]	train-auc:0.986883+0.000378	test-auc:0.960422+0.008291 
# [1253]	train-auc:0.986886+0.000383	test-auc:0.960450+0.008281 
# [1254]	train-auc:0.986895+0.000388	test-auc:0.960457+0.008282 
# [1255]	train-auc:0.986900+0.000389	test-auc:0.960464+0.008285 
# [1256]	train-auc:0.986904+0.000389	test-auc:0.960466+0.008285 
# [1257]	train-auc:0.986910+0.000388	test-auc:0.960471+0.008285 
# [1258]	train-auc:0.986914+0.000388	test-auc:0.960475+0.008285 
# [1259]	train-auc:0.986920+0.000387	test-auc:0.960483+0.008274 
# [1260]	train-auc:0.986924+0.000388	test-auc:0.960486+0.008276 
# [1261]	train-auc:0.986932+0.000388	test-auc:0.960489+0.008275 
# [1262]	train-auc:0.986951+0.000398	test-auc:0.960492+0.008274 
# [1263]	train-auc:0.986956+0.000397	test-auc:0.960498+0.008263 
# [1264]	train-auc:0.986963+0.000397	test-auc:0.960505+0.008266 
# [1265]	train-auc:0.986968+0.000397	test-auc:0.960511+0.008267 
# [1266]	train-auc:0.986973+0.000396	test-auc:0.960514+0.008268 
# [1267]	train-auc:0.986978+0.000399	test-auc:0.960517+0.008270 
# [1268]	train-auc:0.986985+0.000399	test-auc:0.960521+0.008272 
# [1269]	train-auc:0.986987+0.000399	test-auc:0.960524+0.008269 
# [1270]	train-auc:0.986992+0.000399	test-auc:0.960538+0.008273 
# [1271]	train-auc:0.986998+0.000401	test-auc:0.960544+0.008273 
# [1272]	train-auc:0.987000+0.000400	test-auc:0.960545+0.008272 
# [1273]	train-auc:0.987003+0.000397	test-auc:0.960550+0.008264 
# [1274]	train-auc:0.987010+0.000399	test-auc:0.960561+0.008267 
# [1275]	train-auc:0.987017+0.000402	test-auc:0.960560+0.008275 
# [1276]	train-auc:0.987022+0.000402	test-auc:0.960578+0.008270 
# [1277]	train-auc:0.987029+0.000400	test-auc:0.960586+0.008264 
# [1278]	train-auc:0.987035+0.000399	test-auc:0.960594+0.008264 
# [1279]	train-auc:0.987041+0.000400	test-auc:0.960604+0.008264 
# [1280]	train-auc:0.987045+0.000399	test-auc:0.960609+0.008264 
# [1281]	train-auc:0.987049+0.000400	test-auc:0.960611+0.008265 
# [1282]	train-auc:0.987055+0.000401	test-auc:0.960609+0.008271 
# [1283]	train-auc:0.987059+0.000400	test-auc:0.960615+0.008274 
# [1284]	train-auc:0.987066+0.000401	test-auc:0.960621+0.008276 
# [1285]	train-auc:0.987075+0.000397	test-auc:0.960622+0.008275 
# [1286]	train-auc:0.987079+0.000396	test-auc:0.960626+0.008273 
# [1287]	train-auc:0.987086+0.000397	test-auc:0.960630+0.008275 
# [1288]	train-auc:0.987090+0.000397	test-auc:0.960636+0.008275 
# [1289]	train-auc:0.987095+0.000397	test-auc:0.960643+0.008275 
# [1290]	train-auc:0.987101+0.000394	test-auc:0.960650+0.008279 
# [1291]	train-auc:0.987108+0.000397	test-auc:0.960654+0.008280 
# [1292]	train-auc:0.987112+0.000402	test-auc:0.960665+0.008266 
# [1293]	train-auc:0.987119+0.000400	test-auc:0.960672+0.008272 
# [1294]	train-auc:0.987123+0.000401	test-auc:0.960673+0.008275 
# [1295]	train-auc:0.987129+0.000400	test-auc:0.960693+0.008237 
# [1296]	train-auc:0.987135+0.000400	test-auc:0.960695+0.008238 
# [1297]	train-auc:0.987139+0.000400	test-auc:0.960709+0.008214 
# [1298]	train-auc:0.987146+0.000401	test-auc:0.960735+0.008229 
# [1299]	train-auc:0.987152+0.000400	test-auc:0.960739+0.008227 
# [1300]	train-auc:0.987161+0.000397	test-auc:0.960747+0.008228 
# [1301]	train-auc:0.987165+0.000397	test-auc:0.960743+0.008242 
# [1302]	train-auc:0.987170+0.000396	test-auc:0.960746+0.008239 
# [1303]	train-auc:0.987177+0.000396	test-auc:0.960750+0.008240 
# [1304]	train-auc:0.987181+0.000396	test-auc:0.960754+0.008239 
# [1305]	train-auc:0.987187+0.000396	test-auc:0.960760+0.008242 
# [1306]	train-auc:0.987192+0.000397	test-auc:0.960767+0.008245 
# [1307]	train-auc:0.987197+0.000396	test-auc:0.960768+0.008245 
# [1308]	train-auc:0.987202+0.000397	test-auc:0.960772+0.008244 
# [1309]	train-auc:0.987206+0.000398	test-auc:0.960774+0.008244 
# [1310]	train-auc:0.987212+0.000401	test-auc:0.960798+0.008212 
# [1311]	train-auc:0.987217+0.000401	test-auc:0.960802+0.008213 
# [1312]	train-auc:0.987222+0.000400	test-auc:0.960805+0.008213 
# [1313]	train-auc:0.987228+0.000401	test-auc:0.960813+0.008216 
# [1314]	train-auc:0.987262+0.000387	test-auc:0.960834+0.008199 
# [1315]	train-auc:0.987266+0.000387	test-auc:0.960839+0.008198 
# [1316]	train-auc:0.987290+0.000363	test-auc:0.960831+0.008222 
# [1317]	train-auc:0.987321+0.000385	test-auc:0.960854+0.008219 
# [1318]	train-auc:0.987326+0.000385	test-auc:0.960856+0.008219 
# [1319]	train-auc:0.987336+0.000386	test-auc:0.960866+0.008215 
# [1320]	train-auc:0.987340+0.000386	test-auc:0.960870+0.008215 
# [1321]	train-auc:0.987346+0.000386	test-auc:0.960872+0.008216 
# [1322]	train-auc:0.987352+0.000387	test-auc:0.960877+0.008215 
# [1323]	train-auc:0.987361+0.000392	test-auc:0.960884+0.008214 
# [1324]	train-auc:0.987365+0.000392	test-auc:0.960888+0.008213 
# [1325]	train-auc:0.987372+0.000393	test-auc:0.960896+0.008210 
# [1326]	train-auc:0.987374+0.000396	test-auc:0.960897+0.008206 
# [1327]	train-auc:0.987377+0.000395	test-auc:0.960899+0.008206 
# [1328]	train-auc:0.987383+0.000396	test-auc:0.960909+0.008206 
# [1329]	train-auc:0.987389+0.000395	test-auc:0.960916+0.008208 
# [1330]	train-auc:0.987391+0.000397	test-auc:0.960921+0.008211 
# [1331]	train-auc:0.987395+0.000394	test-auc:0.960923+0.008210 
# [1332]	train-auc:0.987402+0.000392	test-auc:0.960925+0.008209 
# [1333]	train-auc:0.987409+0.000393	test-auc:0.960932+0.008202 
# [1334]	train-auc:0.987414+0.000393	test-auc:0.960935+0.008201 
# [1335]	train-auc:0.987420+0.000393	test-auc:0.960936+0.008202 
# [1336]	train-auc:0.987426+0.000395	test-auc:0.960944+0.008201 
# [1337]	train-auc:0.987431+0.000393	test-auc:0.960948+0.008201 
# [1338]	train-auc:0.987435+0.000393	test-auc:0.960952+0.008202 
# [1339]	train-auc:0.987438+0.000392	test-auc:0.960954+0.008202 
# [1340]	train-auc:0.987443+0.000393	test-auc:0.960966+0.008200 
# [1341]	train-auc:0.987454+0.000386	test-auc:0.960969+0.008201 
# [1342]	train-auc:0.987459+0.000385	test-auc:0.960972+0.008211 
# [1343]	train-auc:0.987462+0.000385	test-auc:0.960975+0.008212 
# [1344]	train-auc:0.987467+0.000385	test-auc:0.960977+0.008213 
# [1345]	train-auc:0.987471+0.000385	test-auc:0.960979+0.008213 
# [1346]	train-auc:0.987476+0.000385	test-auc:0.960981+0.008213 
# [1347]	train-auc:0.987479+0.000385	test-auc:0.960985+0.008214 
# [1348]	train-auc:0.987509+0.000379	test-auc:0.961024+0.008231 
# [1349]	train-auc:0.987516+0.000375	test-auc:0.961018+0.008251 
# [1350]	train-auc:0.987525+0.000370	test-auc:0.961032+0.008241 
# [1351]	train-auc:0.987531+0.000369	test-auc:0.961041+0.008237 
# [1352]	train-auc:0.987535+0.000368	test-auc:0.961045+0.008237 
# [1353]	train-auc:0.987540+0.000368	test-auc:0.961061+0.008232 
# [1354]	train-auc:0.987562+0.000388	test-auc:0.961069+0.008232 
# [1355]	train-auc:0.987566+0.000386	test-auc:0.961068+0.008232 
# [1356]	train-auc:0.987570+0.000386	test-auc:0.961068+0.008231 
# [1357]	train-auc:0.987594+0.000409	test-auc:0.961087+0.008241 
# [1358]	train-auc:0.987600+0.000410	test-auc:0.961091+0.008240 
# [1359]	train-auc:0.987606+0.000409	test-auc:0.961136+0.008236 
# [1360]	train-auc:0.987611+0.000409	test-auc:0.961138+0.008236 
# [1361]	train-auc:0.987632+0.000430	test-auc:0.961169+0.008230 
# [1362]	train-auc:0.987642+0.000428	test-auc:0.961188+0.008221 
# [1363]	train-auc:0.987646+0.000428	test-auc:0.961191+0.008222 
# [1364]	train-auc:0.987650+0.000428	test-auc:0.961193+0.008223 
# [1365]	train-auc:0.987654+0.000428	test-auc:0.961197+0.008223 
# [1366]	train-auc:0.987658+0.000427	test-auc:0.961203+0.008226 
# [1367]	train-auc:0.987664+0.000427	test-auc:0.961207+0.008226 
# [1368]	train-auc:0.987671+0.000426	test-auc:0.961214+0.008228 
# [1369]	train-auc:0.987675+0.000427	test-auc:0.961221+0.008218 
# [1370]	train-auc:0.987679+0.000429	test-auc:0.961225+0.008219 
# [1371]	train-auc:0.987683+0.000429	test-auc:0.961233+0.008224 
# [1372]	train-auc:0.987690+0.000429	test-auc:0.961234+0.008224 
# [1373]	train-auc:0.987695+0.000429	test-auc:0.961242+0.008224 
# [1374]	train-auc:0.987700+0.000430	test-auc:0.961245+0.008228 
# [1375]	train-auc:0.987723+0.000406	test-auc:0.961242+0.008241 
# [1376]	train-auc:0.987728+0.000402	test-auc:0.961245+0.008239 
# [1377]	train-auc:0.987734+0.000401	test-auc:0.961247+0.008238 
# [1378]	train-auc:0.987739+0.000400	test-auc:0.961249+0.008238 
# [1379]	train-auc:0.987746+0.000399	test-auc:0.961258+0.008242 
# [1380]	train-auc:0.987751+0.000399	test-auc:0.961263+0.008235 
# [1381]	train-auc:0.987760+0.000395	test-auc:0.961263+0.008235 
# [1382]	train-auc:0.987768+0.000398	test-auc:0.961273+0.008230 
# [1383]	train-auc:0.987774+0.000398	test-auc:0.961280+0.008231 
# [1384]	train-auc:0.987778+0.000397	test-auc:0.961282+0.008231 
# [1385]	train-auc:0.987780+0.000397	test-auc:0.961284+0.008231 
# [1386]	train-auc:0.987786+0.000397	test-auc:0.961291+0.008231 
# [1387]	train-auc:0.987790+0.000398	test-auc:0.961295+0.008230 
# [1388]	train-auc:0.987795+0.000398	test-auc:0.961301+0.008232 
# [1389]	train-auc:0.987800+0.000398	test-auc:0.961301+0.008227 
# [1390]	train-auc:0.987804+0.000398	test-auc:0.961303+0.008225 
# [1391]	train-auc:0.987810+0.000398	test-auc:0.961309+0.008223 
# [1392]	train-auc:0.987814+0.000397	test-auc:0.961311+0.008223 
# [1393]	train-auc:0.987819+0.000396	test-auc:0.961318+0.008226 
# [1394]	train-auc:0.987825+0.000395	test-auc:0.961325+0.008225 
# [1395]	train-auc:0.987832+0.000397	test-auc:0.961335+0.008226 
# [1396]	train-auc:0.987834+0.000396	test-auc:0.961336+0.008226 
# [1397]	train-auc:0.987840+0.000395	test-auc:0.961341+0.008227 
# [1398]	train-auc:0.987851+0.000394	test-auc:0.961340+0.008224 
# [1399]	train-auc:0.987857+0.000395	test-auc:0.961341+0.008223 
# [1400]	train-auc:0.987861+0.000394	test-auc:0.961344+0.008224 
# [1401]	train-auc:0.987864+0.000393	test-auc:0.961344+0.008224 
# [1402]	train-auc:0.987868+0.000394	test-auc:0.961347+0.008222 
# [1403]	train-auc:0.987874+0.000395	test-auc:0.961353+0.008222 
# [1404]	train-auc:0.987881+0.000395	test-auc:0.961356+0.008222 
# [1405]	train-auc:0.987886+0.000394	test-auc:0.961361+0.008213 
# [1406]	train-auc:0.987889+0.000394	test-auc:0.961364+0.008213 
# [1407]	train-auc:0.987891+0.000395	test-auc:0.961366+0.008213 
# [1408]	train-auc:0.987895+0.000395	test-auc:0.961369+0.008213 
# [1409]	train-auc:0.987899+0.000396	test-auc:0.961374+0.008210 
# [1410]	train-auc:0.987906+0.000395	test-auc:0.961377+0.008211 
# [1411]	train-auc:0.987911+0.000395	test-auc:0.961380+0.008209 
# [1412]	train-auc:0.987915+0.000397	test-auc:0.961388+0.008202 
# [1413]	train-auc:0.987918+0.000395	test-auc:0.961389+0.008203 
# [1414]	train-auc:0.987923+0.000395	test-auc:0.961397+0.008191 
# [1415]	train-auc:0.987926+0.000393	test-auc:0.961400+0.008190 
# [1416]	train-auc:0.987934+0.000398	test-auc:0.961406+0.008196 
# [1417]	train-auc:0.987939+0.000398	test-auc:0.961411+0.008196 
# [1418]	train-auc:0.987943+0.000398	test-auc:0.961414+0.008194 
# [1419]	train-auc:0.987952+0.000403	test-auc:0.961417+0.008195 
# [1420]	train-auc:0.987958+0.000404	test-auc:0.961425+0.008192 
# [1421]	train-auc:0.987962+0.000403	test-auc:0.961437+0.008195 
# [1422]	train-auc:0.987966+0.000403	test-auc:0.961439+0.008195 
# [1423]	train-auc:0.987974+0.000400	test-auc:0.961442+0.008194 
# [1424]	train-auc:0.987979+0.000401	test-auc:0.961452+0.008195 
# [1425]	train-auc:0.987984+0.000401	test-auc:0.961466+0.008196 
# [1426]	train-auc:0.987988+0.000402	test-auc:0.961475+0.008193 
# [1427]	train-auc:0.988001+0.000394	test-auc:0.961489+0.008199 
# [1428]	train-auc:0.988006+0.000393	test-auc:0.961492+0.008198 
# [1429]	train-auc:0.988010+0.000391	test-auc:0.961497+0.008189 
# [1430]	train-auc:0.988015+0.000392	test-auc:0.961502+0.008190 
# [1431]	train-auc:0.988018+0.000392	test-auc:0.961506+0.008188 
# [1432]	train-auc:0.988026+0.000390	test-auc:0.961507+0.008188 
# [1433]	train-auc:0.988028+0.000391	test-auc:0.961511+0.008188 
# [1434]	train-auc:0.988032+0.000390	test-auc:0.961518+0.008190 
# [1435]	train-auc:0.988036+0.000391	test-auc:0.961519+0.008189 
# [1436]	train-auc:0.988040+0.000390	test-auc:0.961521+0.008189 
# [1437]	train-auc:0.988045+0.000390	test-auc:0.961527+0.008187 
# [1438]	train-auc:0.988067+0.000412	test-auc:0.961539+0.008194 
# [1439]	train-auc:0.988070+0.000411	test-auc:0.961540+0.008193 
# [1440]	train-auc:0.988074+0.000410	test-auc:0.961543+0.008193 
# [1441]	train-auc:0.988076+0.000407	test-auc:0.961545+0.008192 
# [1442]	train-auc:0.988080+0.000406	test-auc:0.961551+0.008190 
# [1443]	train-auc:0.988085+0.000406	test-auc:0.961561+0.008194 
# [1444]	train-auc:0.988091+0.000407	test-auc:0.961563+0.008195 
# [1445]	train-auc:0.988103+0.000404	test-auc:0.961562+0.008193 
# [1446]	train-auc:0.988122+0.000400	test-auc:0.961570+0.008197 
# [1447]	train-auc:0.988128+0.000401	test-auc:0.961574+0.008197 
# [1448]	train-auc:0.988133+0.000401	test-auc:0.961577+0.008196 
# [1449]	train-auc:0.988137+0.000399	test-auc:0.961582+0.008196 
# [1450]	train-auc:0.988141+0.000396	test-auc:0.961587+0.008199 
# [1451]	train-auc:0.988146+0.000399	test-auc:0.961603+0.008198 
# [1452]	train-auc:0.988155+0.000399	test-auc:0.961610+0.008200 
# [1453]	train-auc:0.988158+0.000400	test-auc:0.961613+0.008198 
# [1454]	train-auc:0.988180+0.000382	test-auc:0.961629+0.008184 
# [1455]	train-auc:0.988185+0.000380	test-auc:0.961641+0.008181 
# [1456]	train-auc:0.988190+0.000384	test-auc:0.961648+0.008184 
# [1457]	train-auc:0.988195+0.000384	test-auc:0.961651+0.008184 
# [1458]	train-auc:0.988198+0.000383	test-auc:0.961653+0.008183 
# [1459]	train-auc:0.988204+0.000385	test-auc:0.961656+0.008183 
# [1460]	train-auc:0.988208+0.000386	test-auc:0.961659+0.008182 
# [1461]	train-auc:0.988212+0.000385	test-auc:0.961662+0.008182 
# [1462]	train-auc:0.988216+0.000385	test-auc:0.961666+0.008180 
# [1463]	train-auc:0.988218+0.000384	test-auc:0.961672+0.008168 
# [1464]	train-auc:0.988223+0.000384	test-auc:0.961677+0.008171 
# [1465]	train-auc:0.988230+0.000381	test-auc:0.961681+0.008169 
# [1466]	train-auc:0.988232+0.000382	test-auc:0.961686+0.008166 
# [1467]	train-auc:0.988237+0.000382	test-auc:0.961704+0.008167 
# [1468]	train-auc:0.988241+0.000383	test-auc:0.961712+0.008168 
# [1469]	train-auc:0.988247+0.000383	test-auc:0.961718+0.008170 
# [1470]	train-auc:0.988251+0.000383	test-auc:0.961722+0.008172 
# [1471]	train-auc:0.988254+0.000384	test-auc:0.961724+0.008172 
# [1472]	train-auc:0.988260+0.000385	test-auc:0.961730+0.008167 
# [1473]	train-auc:0.988266+0.000386	test-auc:0.961735+0.008169 
# [1474]	train-auc:0.988270+0.000386	test-auc:0.961734+0.008175 
# [1475]	train-auc:0.988276+0.000389	test-auc:0.961737+0.008175 
# [1476]	train-auc:0.988279+0.000388	test-auc:0.961740+0.008174 
# [1477]	train-auc:0.988281+0.000388	test-auc:0.961744+0.008174 
# [1478]	train-auc:0.988284+0.000390	test-auc:0.961748+0.008175 
# [1479]	train-auc:0.988296+0.000389	test-auc:0.961761+0.008175 
# [1480]	train-auc:0.988300+0.000387	test-auc:0.961761+0.008176 
# [1481]	train-auc:0.988303+0.000386	test-auc:0.961760+0.008174 
# [1482]	train-auc:0.988311+0.000381	test-auc:0.961766+0.008174 
# [1483]	train-auc:0.988316+0.000381	test-auc:0.961767+0.008174 
# [1484]	train-auc:0.988319+0.000381	test-auc:0.961769+0.008174 
# [1485]	train-auc:0.988323+0.000379	test-auc:0.961765+0.008171 
# [1486]	train-auc:0.988328+0.000380	test-auc:0.961770+0.008169 
# [1487]	train-auc:0.988333+0.000380	test-auc:0.961785+0.008178 
# [1488]	train-auc:0.988337+0.000380	test-auc:0.961789+0.008179 
# [1489]	train-auc:0.988340+0.000380	test-auc:0.961790+0.008180 
# [1490]	train-auc:0.988345+0.000381	test-auc:0.961795+0.008182 
# [1491]	train-auc:0.988348+0.000381	test-auc:0.961799+0.008180 
# [1492]	train-auc:0.988353+0.000381	test-auc:0.961802+0.008181 
# [1493]	train-auc:0.988355+0.000382	test-auc:0.961810+0.008174 
# [1494]	train-auc:0.988359+0.000381	test-auc:0.961815+0.008174 
# [1495]	train-auc:0.988364+0.000382	test-auc:0.961814+0.008173 
# [1496]	train-auc:0.988370+0.000383	test-auc:0.961816+0.008173 
# [1497]	train-auc:0.988374+0.000383	test-auc:0.961821+0.008174 
# [1498]	train-auc:0.988376+0.000377	test-auc:0.961820+0.008172 
# [1499]	train-auc:0.988382+0.000378	test-auc:0.961821+0.008172 
# [1500]	train-auc:0.988386+0.000378	test-auc:0.961832+0.008169 
# [1501]	train-auc:0.988389+0.000378	test-auc:0.961837+0.008171 
# [1502]	train-auc:0.988393+0.000379	test-auc:0.961840+0.008172 
# [1503]	train-auc:0.988396+0.000375	test-auc:0.961843+0.008172 
# [1504]	train-auc:0.988399+0.000375	test-auc:0.961846+0.008170 
# [1505]	train-auc:0.988401+0.000379	test-auc:0.961847+0.008168 
# [1506]	train-auc:0.988406+0.000377	test-auc:0.961850+0.008169 
# [1507]	train-auc:0.988412+0.000381	test-auc:0.961852+0.008169 
# [1508]	train-auc:0.988415+0.000381	test-auc:0.961858+0.008169 
# [1509]	train-auc:0.988418+0.000382	test-auc:0.961859+0.008184 
# [1510]	train-auc:0.988422+0.000382	test-auc:0.961868+0.008186 
# [1511]	train-auc:0.988425+0.000382	test-auc:0.961872+0.008189 
# [1512]	train-auc:0.988437+0.000391	test-auc:0.961878+0.008189 
# [1513]	train-auc:0.988440+0.000391	test-auc:0.961880+0.008190 
# [1514]	train-auc:0.988445+0.000391	test-auc:0.961884+0.008191 
# [1515]	train-auc:0.988449+0.000390	test-auc:0.961890+0.008191 
# [1516]	train-auc:0.988453+0.000389	test-auc:0.961894+0.008191 
# [1517]	train-auc:0.988457+0.000389	test-auc:0.961909+0.008169 
# [1518]	train-auc:0.988461+0.000389	test-auc:0.961912+0.008166 
# [1519]	train-auc:0.988464+0.000389	test-auc:0.961915+0.008167 
# [1520]	train-auc:0.988495+0.000365	test-auc:0.961760+0.008473 
# [1521]	train-auc:0.988498+0.000365	test-auc:0.961763+0.008474 
# [1522]	train-auc:0.988502+0.000364	test-auc:0.961770+0.008476 
# [1523]	train-auc:0.988506+0.000364	test-auc:0.961773+0.008476 
# [1524]	train-auc:0.988511+0.000362	test-auc:0.961775+0.008477 
# [1525]	train-auc:0.988513+0.000362	test-auc:0.961781+0.008478 
# [1526]	train-auc:0.988517+0.000362	test-auc:0.961784+0.008474 
# [1527]	train-auc:0.988521+0.000362	test-auc:0.961795+0.008475 
# [1528]	train-auc:0.988525+0.000362	test-auc:0.961797+0.008474 
# [1529]	train-auc:0.988530+0.000363	test-auc:0.961798+0.008472 
# [1530]	train-auc:0.988534+0.000362	test-auc:0.961838+0.008460 
# [1531]	train-auc:0.988541+0.000358	test-auc:0.961838+0.008455 
# [1532]	train-auc:0.988545+0.000358	test-auc:0.961841+0.008454 
# [1533]	train-auc:0.988549+0.000359	test-auc:0.961844+0.008455 
# [1534]	train-auc:0.988553+0.000359	test-auc:0.961845+0.008456 
# [1535]	train-auc:0.988556+0.000358	test-auc:0.961850+0.008453 
# [1536]	train-auc:0.988559+0.000359	test-auc:0.961851+0.008453 
# [1537]	train-auc:0.988560+0.000362	test-auc:0.961850+0.008449 
# [1538]	train-auc:0.988564+0.000362	test-auc:0.961859+0.008448 
# [1539]	train-auc:0.988571+0.000363	test-auc:0.961864+0.008451 
# [1540]	train-auc:0.988574+0.000363	test-auc:0.961865+0.008449 
# [1541]	train-auc:0.988599+0.000360	test-auc:0.961883+0.008458 
# [1542]	train-auc:0.988603+0.000359	test-auc:0.961885+0.008459 
# [1543]	train-auc:0.988607+0.000359	test-auc:0.961890+0.008459 
# [1544]	train-auc:0.988611+0.000359	test-auc:0.961892+0.008459 
# [1545]	train-auc:0.988613+0.000361	test-auc:0.961890+0.008469 
# [1546]	train-auc:0.988616+0.000361	test-auc:0.961894+0.008469 
# [1547]	train-auc:0.988625+0.000355	test-auc:0.961891+0.008478 
# [1548]	train-auc:0.988648+0.000378	test-auc:0.961909+0.008488 
# [1549]	train-auc:0.988653+0.000377	test-auc:0.961912+0.008487 
# [1550]	train-auc:0.988657+0.000378	test-auc:0.961913+0.008487 
# [1551]	train-auc:0.988663+0.000375	test-auc:0.961931+0.008457 
# [1552]	train-auc:0.988666+0.000375	test-auc:0.961933+0.008457 
# [1553]	train-auc:0.988671+0.000374	test-auc:0.961941+0.008457 
# [1554]	train-auc:0.988675+0.000374	test-auc:0.961942+0.008457 
# [1555]	train-auc:0.988678+0.000375	test-auc:0.961945+0.008458 
# [1556]	train-auc:0.988683+0.000375	test-auc:0.961963+0.008464 
# [1557]	train-auc:0.988687+0.000376	test-auc:0.961965+0.008465 
# [1558]	train-auc:0.988692+0.000376	test-auc:0.961967+0.008464 
# [1559]	train-auc:0.988695+0.000375	test-auc:0.961979+0.008462 
# [1560]	train-auc:0.988699+0.000374	test-auc:0.961980+0.008463 
# [1561]	train-auc:0.988700+0.000377	test-auc:0.961985+0.008463 
# [1562]	train-auc:0.988707+0.000377	test-auc:0.961989+0.008464 
# [1563]	train-auc:0.988711+0.000376	test-auc:0.961994+0.008462 
# [1564]	train-auc:0.988722+0.000375	test-auc:0.961996+0.008461 
# [1565]	train-auc:0.988726+0.000374	test-auc:0.962002+0.008458 
# [1566]	train-auc:0.988730+0.000374	test-auc:0.962004+0.008458 
# [1567]	train-auc:0.988733+0.000375	test-auc:0.962008+0.008458 
# [1568]	train-auc:0.988737+0.000375	test-auc:0.962024+0.008463 
# [1569]	train-auc:0.988739+0.000375	test-auc:0.962025+0.008461 
# [1570]	train-auc:0.988745+0.000377	test-auc:0.962029+0.008462 
# [1571]	train-auc:0.988749+0.000376	test-auc:0.962035+0.008451 
# [1572]	train-auc:0.988753+0.000376	test-auc:0.962038+0.008449 
# [1573]	train-auc:0.988756+0.000375	test-auc:0.962041+0.008450 
# [1574]	train-auc:0.988760+0.000376	test-auc:0.962043+0.008450 
# [1575]	train-auc:0.988767+0.000374	test-auc:0.962047+0.008449 
# [1576]	train-auc:0.988770+0.000374	test-auc:0.962048+0.008450 
# [1577]	train-auc:0.988776+0.000369	test-auc:0.962058+0.008451 
# [1578]	train-auc:0.988781+0.000369	test-auc:0.962064+0.008449 
# [1579]	train-auc:0.988785+0.000369	test-auc:0.962067+0.008448 
# [1580]	train-auc:0.988789+0.000369	test-auc:0.962071+0.008449 
# [1581]	train-auc:0.988793+0.000368	test-auc:0.962073+0.008449 
# [1582]	train-auc:0.988796+0.000368	test-auc:0.962078+0.008449 
# [1583]	train-auc:0.988800+0.000369	test-auc:0.962079+0.008449 
# [1584]	train-auc:0.988803+0.000368	test-auc:0.962083+0.008448 
# [1585]	train-auc:0.988808+0.000368	test-auc:0.962093+0.008447 
# [1586]	train-auc:0.988817+0.000366	test-auc:0.962116+0.008413 
# [1587]	train-auc:0.988820+0.000366	test-auc:0.962126+0.008414 
# [1588]	train-auc:0.988825+0.000366	test-auc:0.962128+0.008415 
# [1589]	train-auc:0.988830+0.000364	test-auc:0.962137+0.008417 
# [1590]	train-auc:0.988834+0.000364	test-auc:0.962139+0.008416 
# [1591]	train-auc:0.988836+0.000366	test-auc:0.962142+0.008415 
# [1592]	train-auc:0.988839+0.000366	test-auc:0.962145+0.008417 
# [1593]	train-auc:0.988842+0.000366	test-auc:0.962148+0.008417 
# [1594]	train-auc:0.988844+0.000365	test-auc:0.962147+0.008415 
# [1595]	train-auc:0.988848+0.000365	test-auc:0.962152+0.008416 
# [1596]	train-auc:0.988850+0.000368	test-auc:0.962155+0.008416 
# [1597]	train-auc:0.988854+0.000368	test-auc:0.962163+0.008410 
# [1598]	train-auc:0.988857+0.000368	test-auc:0.962165+0.008411 
# [1599]	train-auc:0.988865+0.000367	test-auc:0.962160+0.008424 
# [1600]	train-auc:0.988868+0.000367	test-auc:0.962163+0.008423 
# [1601]	train-auc:0.988873+0.000368	test-auc:0.962166+0.008423 
# [1602]	train-auc:0.988881+0.000371	test-auc:0.962169+0.008424 
# [1603]	train-auc:0.988889+0.000362	test-auc:0.962173+0.008417 
# [1604]	train-auc:0.988893+0.000361	test-auc:0.962175+0.008418 
# [1605]	train-auc:0.988898+0.000361	test-auc:0.962180+0.008415 
# [1606]	train-auc:0.988901+0.000362	test-auc:0.962183+0.008414 
# [1607]	train-auc:0.988904+0.000361	test-auc:0.962187+0.008411 
# [1608]	train-auc:0.988918+0.000346	test-auc:0.962189+0.008410 
# [1609]	train-auc:0.988923+0.000346	test-auc:0.962197+0.008409 
# [1610]	train-auc:0.988929+0.000342	test-auc:0.962200+0.008406 
# [1611]	train-auc:0.988935+0.000341	test-auc:0.962241+0.008332 
# [1612]	train-auc:0.988939+0.000341	test-auc:0.962249+0.008320 
# [1613]	train-auc:0.988943+0.000341	test-auc:0.962254+0.008317 
# [1614]	train-auc:0.988945+0.000340	test-auc:0.962256+0.008317 
# [1615]	train-auc:0.988949+0.000340	test-auc:0.962258+0.008316 
# [1616]	train-auc:0.988952+0.000340	test-auc:0.962260+0.008316 
# [1617]	train-auc:0.988956+0.000339	test-auc:0.962263+0.008315 
# [1618]	train-auc:0.988962+0.000341	test-auc:0.962265+0.008315 
# [1619]	train-auc:0.988968+0.000339	test-auc:0.962272+0.008311 
# [1620]	train-auc:0.988975+0.000339	test-auc:0.962280+0.008312 
# [1621]	train-auc:0.988979+0.000340	test-auc:0.962282+0.008313 
# [1622]	train-auc:0.988984+0.000342	test-auc:0.962287+0.008314 
# [1623]	train-auc:0.988987+0.000337	test-auc:0.962294+0.008313 
# [1624]	train-auc:0.988990+0.000336	test-auc:0.962298+0.008313 
# [1625]	train-auc:0.988994+0.000336	test-auc:0.962301+0.008313 
# [1626]	train-auc:0.988999+0.000336	test-auc:0.962303+0.008311 
# [1627]	train-auc:0.989002+0.000337	test-auc:0.962307+0.008312 
# [1628]	train-auc:0.989005+0.000336	test-auc:0.962310+0.008309 
# [1629]	train-auc:0.989009+0.000337	test-auc:0.962325+0.008309 
# [1630]	train-auc:0.989016+0.000338	test-auc:0.962329+0.008309 
# [1631]	train-auc:0.989018+0.000338	test-auc:0.962330+0.008309 
# [1632]	train-auc:0.989022+0.000337	test-auc:0.962333+0.008310 
# [1633]	train-auc:0.989023+0.000340	test-auc:0.962337+0.008311 
# [1634]	train-auc:0.989026+0.000342	test-auc:0.962340+0.008310 
# [1635]	train-auc:0.989029+0.000342	test-auc:0.962341+0.008311 
# [1636]	train-auc:0.989032+0.000341	test-auc:0.962345+0.008312 
# [1637]	train-auc:0.989036+0.000341	test-auc:0.962346+0.008313 
# [1638]	train-auc:0.989039+0.000340	test-auc:0.962350+0.008308 
# [1639]	train-auc:0.989046+0.000343	test-auc:0.962351+0.008308 
# [1640]	train-auc:0.989049+0.000344	test-auc:0.962355+0.008310 
# [1641]	train-auc:0.989054+0.000343	test-auc:0.962358+0.008310 
# [1642]	train-auc:0.989058+0.000342	test-auc:0.962360+0.008310 
# [1643]	train-auc:0.989060+0.000342	test-auc:0.962362+0.008312 
# [1644]	train-auc:0.989067+0.000347	test-auc:0.962364+0.008313 
# [1645]	train-auc:0.989071+0.000347	test-auc:0.962370+0.008315 
# [1646]	train-auc:0.989075+0.000347	test-auc:0.962374+0.008313 
# [1647]	train-auc:0.989078+0.000347	test-auc:0.962378+0.008314 
# [1648]	train-auc:0.989081+0.000348	test-auc:0.962380+0.008315 
# [1649]	train-auc:0.989085+0.000347	test-auc:0.962314+0.008452 
# [1650]	train-auc:0.989089+0.000347	test-auc:0.962318+0.008453 
# [1651]	train-auc:0.989092+0.000348	test-auc:0.962326+0.008455 
# [1652]	train-auc:0.989093+0.000347	test-auc:0.962332+0.008452 
# [1653]	train-auc:0.989098+0.000347	test-auc:0.962335+0.008450 
# [1654]	train-auc:0.989102+0.000349	test-auc:0.962340+0.008448 
# [1655]	train-auc:0.989106+0.000349	test-auc:0.962350+0.008433 
# [1656]	train-auc:0.989109+0.000348	test-auc:0.962353+0.008434 
# [1657]	train-auc:0.989111+0.000350	test-auc:0.962353+0.008433 
# [1658]	train-auc:0.989114+0.000349	test-auc:0.962363+0.008416 
# [1659]	train-auc:0.989119+0.000351	test-auc:0.962368+0.008408 
# [1660]	train-auc:0.989118+0.000348	test-auc:0.962370+0.008407 
# [1661]	train-auc:0.989121+0.000348	test-auc:0.962372+0.008407 
# [1662]	train-auc:0.989123+0.000349	test-auc:0.962381+0.008415 
# [1663]	train-auc:0.989126+0.000349	test-auc:0.962384+0.008414 
# [1664]	train-auc:0.989129+0.000349	test-auc:0.962392+0.008418 
# [1665]	train-auc:0.989134+0.000348	test-auc:0.962397+0.008412 
# [1666]	train-auc:0.989137+0.000348	test-auc:0.962400+0.008413 
# [1667]	train-auc:0.989142+0.000347	test-auc:0.962402+0.008414 
# [1668]	train-auc:0.989144+0.000346	test-auc:0.962404+0.008415 
# [1669]	train-auc:0.989150+0.000343	test-auc:0.962409+0.008415 
# [1670]	train-auc:0.989154+0.000344	test-auc:0.962414+0.008418 
# [1671]	train-auc:0.989156+0.000343	test-auc:0.962416+0.008417 
# [1672]	train-auc:0.989160+0.000342	test-auc:0.962422+0.008420 
# [1673]	train-auc:0.989163+0.000343	test-auc:0.962427+0.008420 
# [1674]	train-auc:0.989167+0.000343	test-auc:0.962429+0.008419 
# [1675]	train-auc:0.989170+0.000343	test-auc:0.962430+0.008419 
# [1676]	train-auc:0.989173+0.000343	test-auc:0.962433+0.008419 
# [1677]	train-auc:0.989175+0.000346	test-auc:0.962438+0.008421 
# [1678]	train-auc:0.989180+0.000348	test-auc:0.962446+0.008420 
# [1679]	train-auc:0.989184+0.000349	test-auc:0.962458+0.008421 
# [1680]	train-auc:0.989188+0.000348	test-auc:0.962461+0.008419 
# [1681]	train-auc:0.989192+0.000348	test-auc:0.962463+0.008419 
# [1682]	train-auc:0.989201+0.000350	test-auc:0.962480+0.008423 
# [1683]	train-auc:0.989204+0.000350	test-auc:0.962487+0.008422 
# [1684]	train-auc:0.989212+0.000346	test-auc:0.962497+0.008405 
# [1685]	train-auc:0.989216+0.000346	test-auc:0.962499+0.008406 
# [1686]	train-auc:0.989221+0.000347	test-auc:0.962502+0.008406 
# [1687]	train-auc:0.989223+0.000347	test-auc:0.962507+0.008404 
# [1688]	train-auc:0.989227+0.000348	test-auc:0.962516+0.008409 
# [1689]	train-auc:0.989232+0.000345	test-auc:0.962519+0.008409 
# [1690]	train-auc:0.989237+0.000345	test-auc:0.962525+0.008405 
# [1691]	train-auc:0.989241+0.000344	test-auc:0.962528+0.008404 
# [1692]	train-auc:0.989242+0.000346	test-auc:0.962530+0.008403 
# [1693]	train-auc:0.989246+0.000347	test-auc:0.962529+0.008403 
# [1694]	train-auc:0.989250+0.000347	test-auc:0.962531+0.008403 
# [1695]	train-auc:0.989254+0.000347	test-auc:0.962537+0.008404 
# [1696]	train-auc:0.989258+0.000347	test-auc:0.962544+0.008397 
# [1697]	train-auc:0.989264+0.000349	test-auc:0.962546+0.008396 
# [1698]	train-auc:0.989267+0.000349	test-auc:0.962551+0.008394 
# [1699]	train-auc:0.989273+0.000348	test-auc:0.962557+0.008391 
# [1700]	train-auc:0.989279+0.000347	test-auc:0.962559+0.008391 
# [1701]	train-auc:0.989284+0.000346	test-auc:0.962563+0.008387 
# [1702]	train-auc:0.989287+0.000345	test-auc:0.962567+0.008386 
# [1703]	train-auc:0.989290+0.000345	test-auc:0.962573+0.008389 
# [1704]	train-auc:0.989293+0.000345	test-auc:0.962576+0.008390 
# [1705]	train-auc:0.989296+0.000343	test-auc:0.962579+0.008385 
# [1706]	train-auc:0.989305+0.000346	test-auc:0.962588+0.008373 
# [1707]	train-auc:0.989311+0.000350	test-auc:0.962595+0.008370 
# [1708]	train-auc:0.989314+0.000351	test-auc:0.962599+0.008370 
# [1709]	train-auc:0.989319+0.000351	test-auc:0.962607+0.008373 
# [1710]	train-auc:0.989322+0.000351	test-auc:0.962627+0.008373 
# [1711]	train-auc:0.989332+0.000359	test-auc:0.962634+0.008376 
# [1712]	train-auc:0.989341+0.000365	test-auc:0.962643+0.008375 
# [1713]	train-auc:0.989345+0.000365	test-auc:0.962648+0.008377 
# [1714]	train-auc:0.989346+0.000366	test-auc:0.962655+0.008374 
# [1715]	train-auc:0.989351+0.000366	test-auc:0.962659+0.008376 
# [1716]	train-auc:0.989356+0.000366	test-auc:0.962662+0.008376 
# [1717]	train-auc:0.989360+0.000366	test-auc:0.962675+0.008375 
# [1718]	train-auc:0.989365+0.000366	test-auc:0.962677+0.008376 
# [1719]	train-auc:0.989368+0.000365	test-auc:0.962682+0.008374 
# [1720]	train-auc:0.989380+0.000364	test-auc:0.962689+0.008371 
# [1721]	train-auc:0.989388+0.000368	test-auc:0.962693+0.008371 
# [1722]	train-auc:0.989391+0.000367	test-auc:0.962696+0.008370 
# [1723]	train-auc:0.989395+0.000367	test-auc:0.962700+0.008367 
# [1724]	train-auc:0.989397+0.000367	test-auc:0.962706+0.008366 
# [1725]	train-auc:0.989399+0.000364	test-auc:0.962708+0.008365 
# [1726]	train-auc:0.989403+0.000364	test-auc:0.962712+0.008363 
# [1727]	train-auc:0.989407+0.000363	test-auc:0.962714+0.008362 
# [1728]	train-auc:0.989412+0.000365	test-auc:0.962716+0.008363 
# [1729]	train-auc:0.989415+0.000366	test-auc:0.962720+0.008363 
# [1730]	train-auc:0.989420+0.000365	test-auc:0.962728+0.008366 
# [1731]	train-auc:0.989428+0.000357	test-auc:0.962741+0.008371 
# [1732]	train-auc:0.989431+0.000358	test-auc:0.962743+0.008371 
# [1733]	train-auc:0.989435+0.000357	test-auc:0.962747+0.008373 
# [1734]	train-auc:0.989438+0.000356	test-auc:0.962754+0.008373 
# [1735]	train-auc:0.989441+0.000355	test-auc:0.962762+0.008378 
# [1736]	train-auc:0.989444+0.000355	test-auc:0.962765+0.008380 
# [1737]	train-auc:0.989447+0.000355	test-auc:0.962769+0.008380 
# [1738]	train-auc:0.989449+0.000357	test-auc:0.962775+0.008371 
# [1739]	train-auc:0.989452+0.000356	test-auc:0.962776+0.008376 
# [1740]	train-auc:0.989456+0.000356	test-auc:0.962779+0.008376 
# [1741]	train-auc:0.989459+0.000355	test-auc:0.962782+0.008376 
# [1742]	train-auc:0.989460+0.000356	test-auc:0.962782+0.008375 
# [1743]	train-auc:0.989465+0.000354	test-auc:0.962787+0.008372 
# [1744]	train-auc:0.989468+0.000355	test-auc:0.962790+0.008372 
# [1745]	train-auc:0.989470+0.000355	test-auc:0.962794+0.008373 
# [1746]	train-auc:0.989473+0.000355	test-auc:0.962805+0.008374 
# [1747]	train-auc:0.989477+0.000355	test-auc:0.962813+0.008362 
# [1748]	train-auc:0.989480+0.000354	test-auc:0.962814+0.008362 
# [1749]	train-auc:0.989483+0.000354	test-auc:0.962820+0.008361 
# [1750]	train-auc:0.989487+0.000355	test-auc:0.962823+0.008361 
# [1751]	train-auc:0.989491+0.000354	test-auc:0.962827+0.008355 
# [1752]	train-auc:0.989496+0.000352	test-auc:0.962830+0.008355 
# [1753]	train-auc:0.989498+0.000352	test-auc:0.962834+0.008356 
# [1754]	train-auc:0.989502+0.000351	test-auc:0.962836+0.008355 
# [1755]	train-auc:0.989504+0.000351	test-auc:0.962838+0.008356 
# [1756]	train-auc:0.989507+0.000352	test-auc:0.962842+0.008351 
# [1757]	train-auc:0.989508+0.000354	test-auc:0.962843+0.008352 
# [1758]	train-auc:0.989517+0.000359	test-auc:0.962852+0.008350 
# [1759]	train-auc:0.989520+0.000359	test-auc:0.962853+0.008351 
# [1760]	train-auc:0.989523+0.000359	test-auc:0.962856+0.008351 
# [1761]	train-auc:0.989550+0.000392	test-auc:0.962849+0.008368 
# [1762]	train-auc:0.989553+0.000392	test-auc:0.962853+0.008370 
# [1763]	train-auc:0.989558+0.000391	test-auc:0.962854+0.008369 
# [1764]	train-auc:0.989562+0.000391	test-auc:0.962855+0.008368 
# [1765]	train-auc:0.989576+0.000401	test-auc:0.962879+0.008368 
# [1766]	train-auc:0.989582+0.000399	test-auc:0.962880+0.008368 
# [1767]	train-auc:0.989584+0.000400	test-auc:0.962886+0.008368 
# [1768]	train-auc:0.989587+0.000399	test-auc:0.962889+0.008366 
# [1769]	train-auc:0.989593+0.000398	test-auc:0.962892+0.008363 
# [1770]	train-auc:0.989596+0.000398	test-auc:0.962895+0.008361 
# [1771]	train-auc:0.989600+0.000397	test-auc:0.962898+0.008361 
# [1772]	train-auc:0.989604+0.000397	test-auc:0.962900+0.008365 
# [1773]	train-auc:0.989607+0.000397	test-auc:0.962903+0.008365 
# [1774]	train-auc:0.989610+0.000398	test-auc:0.962904+0.008365 
# [1775]	train-auc:0.989613+0.000399	test-auc:0.962910+0.008365 
# [1776]	train-auc:0.989617+0.000399	test-auc:0.962915+0.008365 
# [1777]	train-auc:0.989621+0.000399	test-auc:0.962923+0.008362 
# [1778]	train-auc:0.989627+0.000403	test-auc:0.962933+0.008364 
# [1779]	train-auc:0.989630+0.000402	test-auc:0.962938+0.008366 
# [1780]	train-auc:0.989631+0.000401	test-auc:0.962940+0.008366 
# [1781]	train-auc:0.989636+0.000402	test-auc:0.962941+0.008366 
# [1782]	train-auc:0.989639+0.000403	test-auc:0.962945+0.008367 
# [1783]	train-auc:0.989640+0.000404	test-auc:0.962946+0.008367 
# [1784]	train-auc:0.989643+0.000404	test-auc:0.962948+0.008367 
# [1785]	train-auc:0.989651+0.000400	test-auc:0.962954+0.008367 
# [1786]	train-auc:0.989654+0.000399	test-auc:0.962958+0.008366 
# [1787]	train-auc:0.989659+0.000401	test-auc:0.962969+0.008366 
# [1788]	train-auc:0.989662+0.000401	test-auc:0.962973+0.008366 
# [1789]	train-auc:0.989665+0.000401	test-auc:0.962980+0.008355 
# [1790]	train-auc:0.989669+0.000400	test-auc:0.962981+0.008355 
# [1791]	train-auc:0.989673+0.000400	test-auc:0.962985+0.008355 
# [1792]	train-auc:0.989674+0.000398	test-auc:0.962984+0.008355 
# [1793]	train-auc:0.989677+0.000398	test-auc:0.962990+0.008350 
# [1794]	train-auc:0.989680+0.000397	test-auc:0.962998+0.008341 
# [1795]	train-auc:0.989684+0.000397	test-auc:0.963005+0.008343 
# [1796]	train-auc:0.989687+0.000397	test-auc:0.963009+0.008343 
# [1797]	train-auc:0.989692+0.000397	test-auc:0.963011+0.008343 
# [1798]	train-auc:0.989696+0.000397	test-auc:0.963014+0.008346 
# [1799]	train-auc:0.989699+0.000396	test-auc:0.963017+0.008347 
# [1800]	train-auc:0.989702+0.000396	test-auc:0.963020+0.008346 
# [1801]	train-auc:0.989704+0.000396	test-auc:0.963022+0.008347 
# [1802]	train-auc:0.989708+0.000397	test-auc:0.963028+0.008349 
# [1803]	train-auc:0.989709+0.000395	test-auc:0.963028+0.008349 
# [1804]	train-auc:0.989713+0.000395	test-auc:0.963034+0.008347 
# [1805]	train-auc:0.989716+0.000395	test-auc:0.963037+0.008347 
# [1806]	train-auc:0.989718+0.000395	test-auc:0.963036+0.008351 
# [1807]	train-auc:0.989721+0.000395	test-auc:0.963047+0.008339 
# [1808]	train-auc:0.989726+0.000394	test-auc:0.963073+0.008340 
# [1809]	train-auc:0.989730+0.000395	test-auc:0.963081+0.008344 
# [1810]	train-auc:0.989732+0.000393	test-auc:0.963087+0.008344 
# [1811]	train-auc:0.989735+0.000392	test-auc:0.963090+0.008344 
# [1812]	train-auc:0.989739+0.000391	test-auc:0.963092+0.008345 
# [1813]	train-auc:0.989742+0.000391	test-auc:0.963094+0.008343 
# [1814]	train-auc:0.989745+0.000392	test-auc:0.963103+0.008333 
# [1815]	train-auc:0.989749+0.000392	test-auc:0.963106+0.008334 
# [1816]	train-auc:0.989752+0.000392	test-auc:0.963108+0.008333 
# [1817]	train-auc:0.989755+0.000392	test-auc:0.963110+0.008334 
# [1818]	train-auc:0.989757+0.000392	test-auc:0.963115+0.008334 
# [1819]	train-auc:0.989767+0.000382	test-auc:0.963117+0.008334 
# [1820]	train-auc:0.989771+0.000382	test-auc:0.963120+0.008336 
# [1821]	train-auc:0.989773+0.000382	test-auc:0.963120+0.008334 
# [1822]	train-auc:0.989776+0.000383	test-auc:0.963122+0.008334 
# [1823]	train-auc:0.989781+0.000379	test-auc:0.963127+0.008336 
# [1824]	train-auc:0.989787+0.000376	test-auc:0.963134+0.008339 
# [1825]	train-auc:0.989790+0.000375	test-auc:0.963136+0.008338 
# [1826]	train-auc:0.989793+0.000374	test-auc:0.963140+0.008339 
# [1827]	train-auc:0.989796+0.000374	test-auc:0.963142+0.008339 
# [1828]	train-auc:0.989797+0.000376	test-auc:0.963143+0.008338 
# [1829]	train-auc:0.989801+0.000376	test-auc:0.963147+0.008338 
# [1830]	train-auc:0.989806+0.000374	test-auc:0.963155+0.008342 
# [1831]	train-auc:0.989808+0.000377	test-auc:0.963169+0.008349 
# [1832]	train-auc:0.989810+0.000376	test-auc:0.963170+0.008348 
# [1833]	train-auc:0.989813+0.000377	test-auc:0.963172+0.008348 
# [1834]	train-auc:0.989816+0.000377	test-auc:0.963174+0.008348 
# [1835]	train-auc:0.989819+0.000377	test-auc:0.963177+0.008347 
# [1836]	train-auc:0.989822+0.000377	test-auc:0.963179+0.008348 
# [1837]	train-auc:0.989829+0.000376	test-auc:0.963183+0.008348 
# [1838]	train-auc:0.989832+0.000376	test-auc:0.963185+0.008348 
# [1839]	train-auc:0.989835+0.000376	test-auc:0.963188+0.008348 
# [1840]	train-auc:0.989837+0.000376	test-auc:0.963190+0.008347 
# [1841]	train-auc:0.989845+0.000382	test-auc:0.963195+0.008348 
# [1842]	train-auc:0.989848+0.000382	test-auc:0.963198+0.008348 
# [1843]	train-auc:0.989851+0.000381	test-auc:0.963200+0.008347 
# [1844]	train-auc:0.989855+0.000381	test-auc:0.963203+0.008347 
# [1845]	train-auc:0.989855+0.000381	test-auc:0.963203+0.008348 
# [1846]	train-auc:0.989858+0.000383	test-auc:0.963210+0.008344 
# [1847]	train-auc:0.989862+0.000384	test-auc:0.963213+0.008344 
# [1848]	train-auc:0.989865+0.000384	test-auc:0.963219+0.008344 
# [1849]	train-auc:0.989870+0.000384	test-auc:0.963221+0.008344 
# [1850]	train-auc:0.989872+0.000386	test-auc:0.963220+0.008342 
# [1851]	train-auc:0.989873+0.000384	test-auc:0.963219+0.008342 
# [1852]	train-auc:0.989889+0.000375	test-auc:0.963238+0.008309 
# [1853]	train-auc:0.989893+0.000375	test-auc:0.963242+0.008310 
# [1854]	train-auc:0.989900+0.000373	test-auc:0.963242+0.008319 
# [1855]	train-auc:0.989901+0.000371	test-auc:0.963245+0.008317 
# [1856]	train-auc:0.989904+0.000370	test-auc:0.963247+0.008318 
# [1857]	train-auc:0.989908+0.000370	test-auc:0.963252+0.008320 
# [1858]	train-auc:0.989912+0.000368	test-auc:0.963256+0.008322 
# [1859]	train-auc:0.989923+0.000379	test-auc:0.963267+0.008327 
# [1860]	train-auc:0.989929+0.000377	test-auc:0.963281+0.008335 
# [1861]	train-auc:0.989938+0.000383	test-auc:0.963290+0.008335 
# [1862]	train-auc:0.989941+0.000383	test-auc:0.963294+0.008336 
# [1863]	train-auc:0.989943+0.000382	test-auc:0.963295+0.008336 
# [1864]	train-auc:0.989947+0.000381	test-auc:0.963301+0.008334 
# [1865]	train-auc:0.989954+0.000375	test-auc:0.963308+0.008334 
# [1866]	train-auc:0.989953+0.000373	test-auc:0.963307+0.008334 
# [1867]	train-auc:0.989956+0.000373	test-auc:0.963308+0.008333 
# [1868]	train-auc:0.989960+0.000373	test-auc:0.963312+0.008336 
# [1869]	train-auc:0.989963+0.000373	test-auc:0.963315+0.008336 
# [1870]	train-auc:0.989965+0.000373	test-auc:0.963318+0.008336 
# [1871]	train-auc:0.989971+0.000368	test-auc:0.963319+0.008336 
# [1872]	train-auc:0.989974+0.000368	test-auc:0.963323+0.008337 
# [1873]	train-auc:0.989978+0.000368	test-auc:0.963327+0.008335 
# [1874]	train-auc:0.989982+0.000367	test-auc:0.963331+0.008330 
# [1875]	train-auc:0.989987+0.000365	test-auc:0.963337+0.008333 
# [1876]	train-auc:0.989989+0.000365	test-auc:0.963344+0.008328 
# [1877]	train-auc:0.989992+0.000365	test-auc:0.963346+0.008329
