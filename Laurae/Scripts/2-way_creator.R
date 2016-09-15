library(data.table)
library(FeatureHashing)
library(xgboost)
library(dplyr)
library(Matrix)
setwd("D:/Data Science/Red Hat")
cat(Sys.time())
cat("Reading data\n")


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


d1 <- merge(train, people, by = "people_id", all.x = T)
d2 <- merge(test, people, by = "people_id", all.x = T)
Y <- d1$outcome
d1$outcome <- NULL


row.train=nrow(train)
gc(verbose=FALSE)

D=rbind(d1,d2)

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
D$people_date <- as.Date(D$people_date, format = "%Y-%m-%d")
D$people_date <- as.numeric(D$people_date)
D$people_date <- D$people_date - min(D$people_date) + 1



# Create sparse quadratic features
# for RAM issues, char_10 is removed
#targets <- c("activity_category", "char_1", "char_2", "char_3", "char_4", "char_5", "char_6", "char_7", "char_8", "char_9", "people_char_2", "people_char_3", "people_char_4", "people_char_5", "people_char_6", "people_char_7", "people_char_8", "people_char_9")
targets <- c("char_1", "people_char_3", "char_2", "people_char_4", "people_char_7", "char_9", "char_8", "char_3", "char_7", "people_char_5", "people_char_9", "char_4", "char_5", "people_char_8", "activity_category", "people_char_6", "char_6", "people_char_2")



mega_sparse <- Matrix(matrix(nrow = 2695978, ncol = 0), sparse = TRUE)
mega_sparse <- as(mega_sparse, "dgCMatrix")
Dedup <- D[, targets]
Dedup <- factorizer(Dedup, has_date = FALSE)
#Dedup <- cbind(Dedup, D[, c("date", "people_date")])
gc()

# First interaction is skipped because it overflows in RAM

for (i in 1:(length(targets)-1)) {
  
  for (j in max((i+1), 3):length(targets)) {
    
    cat("Doing ", targets[i], " & ", targets[j], ". ", sep = "")
    mini_D <- Dedup[,c(targets[i], targets[j])]
    new_data <- sparse.model.matrix(~.^2, data = mini_D)
    threshold <- new_data@p[2:length(new_data@p)] - new_data@p[1:(length(new_data@p)-1)] # avoid overfitting - 90% statistical power
    cat("Columns kept: ", sum(threshold >= 417), " ", sep = "")
    new_data <- new_data[, threshold >= 417] # avoid overfitting - 90% statistical power
    cat(" but only ", length(grep(":", colnames(new_data))), " interactions. ", sep = "")
    new_data <- new_data[, grep(":", colnames(new_data))]
    if (ncol(new_data) > 0) {
      mega_sparse <- cbind(mega_sparse, new_data)
    }
    cat(" Total columns: ", ncol(mega_sparse), ".\n", sep = "")
    rm(new_data, mini_D)
    gc(verbose = FALSE)
    
  }
  
}



# SAVE OR LOAD - CHOOSE
#mega_sparse <- readRDS(file = "laurae_2way_interactions.data")
saveRDS(mega_sparse, file = "laurae_2way_interactions.data")



# your script for model
