# load requied libraries --------------------------------------------------
library(data.table)
setwd("D:/Data Science/Red Hat")


# load and transform people data ------------------------------------------
ppl <- fread("people.csv")

### Recode logic to numeric
p_logi <- names(ppl)[which(sapply(ppl, is.logical))]

for (col in p_logi) {
  set(ppl, j = col, value = as.numeric(ppl[[col]]))
}
rm(p_logi)

### transform date
ppl[,date := as.Date(as.character(date), format = "%Y-%m-%d")]

# load activities ---------------------------------------------------------

# read and combine
activs <- fread("act_train.csv")
TestActivs <- fread("act_test.csv")
TestActivs$outcome <- NA
activs <- rbind(activs,TestActivs)
rm(TestActivs)

# Extract only required variables
activs <- activs[, c("people_id","outcome","activity_id","date"), with = F]

# Merge people data into actvitities
d1 <- merge(activs, ppl, by = "people_id", all.x = T)

# Remember, remember the 5th of November and which is test
testset <- which(ppl$people_id %in% d1$people_id[is.na(d1$outcome)])
d1[, activdate := as.Date(as.character(date.x), format = "%Y-%m-%d")]

rm(activs)

d1_store <- d1[, c("people_id", "outcome", "activity_id", "activdate", "group_1"), with = FALSE]
#d1_index <- which(is.na(d1_store$outcome))
#d1_setdiff <- setdiff(1:2695978, d1_index)
d1_testset <- testset
#d1_unique <- which(!ppl$people_id %in% d1$people_id[is.na(d1$outcome)])
#d1_unique <- which(unique(d1_store$people_id[which(!is.na(d1_store$outcome))]) %in% ppl$people_id)
d1_activities <- d1_store$activity_id[which(!is.na(d1_store$outcome))]
d1_train <- d1_store[!is.na(d1_store$outcome)]
d1_test <- d1_store[is.na(d1_store$outcome)]
d1_unique <- which(ppl$people_id %in% unique(d1_train$people_id))

folds <- 20
repeated <- 10

my_folds <- list()
for (j in 1:repeated) {
  selection <- d1_unique
  set.seed(11111+j)
  for (i in 1:folds) {
    my_folds[[(j-1)*folds+i]] <- selection[sample(length(selection), floor(length(selection) / ((folds+1)-i)))]
    selection <- selection[!(selection %in% my_folds[[(j-1)*folds+i]])]
  }
}

test_out <- data.frame(activity_id = fread("leak_submission_NA.csv")$activity_id, outcome = numeric(498687), perfect = numeric(498687), filled = numeric(498687), minimum = numeric(498687), maximum = numeric(498687), std = numeric(498687), stringsAsFactors = FALSE)
test_table <- matrix(nrow = 498687, ncol = folds*repeated)
test_fill <- matrix(nrow = 498687, ncol = folds*repeated)
valid_out <- data.frame(activity_id = d1_activities, outcome = numeric(2197291), perfect = numeric(2197291), filled = numeric(2197291), minimum = numeric(2197291), maximum = numeric(2197291), std = numeric(2197291), stringsAsFactors = FALSE)
valid_table <- matrix(nrow = 2197291, ncol = repeated)
valid_fill <- matrix(nrow = 2197291, ncol = repeated)
diag <- data.frame(matrix(nrow = folds*repeated, ncol = 0))

# design function to interpolate unknown values ---------------------------

interpolateFun <- function(x){
  
  # Find all non-NA indexes, combine them with outside borders
  borders <- c(1, which(!is.na(x)), length(x) + 1)
  # establish forward and backward - looking indexes
  forward_border <- borders[2:length(borders)]
  backward_border <- borders[1:(length(borders) - 1)]
  
  # prepare vectors for filling
  forward_border_x <- x[forward_border]
  forward_border_x[length(forward_border_x)] <- abs(
    forward_border_x[length(forward_border_x) - 1] - 0.1
  ) 
  backward_border_x <- x[backward_border]
  backward_border_x[1] <- abs(forward_border_x[1] - 0.1)
  
  # generate fill vectors
  forward_x_fill <- rep(forward_border_x, forward_border - backward_border)
  backward_x_fill <- rep(backward_border_x, forward_border - backward_border)
  forward_x_fill_2 <- rep(forward_border, forward_border - backward_border) - 
    1:length(forward_x_fill)
  backward_x_fill_2 <- 1:length(forward_x_fill) -
    rep(backward_border, forward_border - backward_border)
  
  #linear interpolation
  vec <- (forward_x_fill + backward_x_fill)/2
  
  x[is.na(x)] <- vec[is.na(x)]
  return(x)
}

FastROC <- function(actual, predicted) {
  
  # y = actual
  # x = predicted
  x1 = predicted[actual == 1]
  n1 = as.numeric(length(x1))
  x2 = predicted[actual == 0]
  n2 = as.numeric(length(x2))
  r = rank(c(x1,x2))
  return((sum(r[1:n1]) - n1*(n1+1)/2) / (n1*n2))
  
}


for (i in 1:(folds*repeated)) {
  
  # mix stuff ---------------------------------------------------------------
  
  cat("[", format(Sys.time(), "%H:%M:%S"), " iteration ", sprintf("%03d", i), sep = "")
  set.seed(11111+i)
  sampling <- setdiff(d1_unique, my_folds[[i]])
  sampling_valid <- my_folds[[i]]
  d1 <- rbind(d1_train[!d1_train$people_id %in% ppl$people_id[sampling_valid]], d1_test)
  #d1 <- d1_store[!d1$people_id %in% ppl$people_id[sampling_valid]]
  #d1$outcome[d1$people_id %in% ppl$people_id[sampling_valid]] <- NA
  #sampling_valid <- d1$activity_id[setdiff(d1_setdiff, sampling)]
  
  
  # prepare grid for prediction ---------------------------------------------
  
  # Create all group_1/day grid
  minactivdate <- min(d1$activdate)
  maxactivdate <- max(d1$activdate)
  alldays <- seq(minactivdate, maxactivdate, "day")
  allCompaniesAndDays <- data.table(
    expand.grid(unique(
      d1$group_1[!d1$people_id %in% ppl$people_id[testset]]), alldays
    )
  )
  
  
  ## Nicer names
  colnames(allCompaniesAndDays) <- c("group_1","date.p")
  
  ## sort it
  setkey(allCompaniesAndDays,"group_1","date.p")
  
  ## What are values on days where we have data?
  meanbycomdate <- d1[
    !d1$people_id %in% ppl$people_id[testset],
    mean(outcome, na.rm = TRUE),
    by = c("group_1","activdate")
    ]
  
  ## Add them to full data grid
  allCompaniesAndDays <- merge(
    allCompaniesAndDays,
    meanbycomdate,
    by.x = c("group_1","date.p"), by.y = c("group_1","activdate"),
    all.x = T
  )
  
  
  
  
  # apply and submit --------------------------------------------------------
  
  allCompaniesAndDays[, filled := interpolateFun(V1), by = "group_1"]
  
  d1 <- d1_store
  
  d1 <- merge(
    d1,
    allCompaniesAndDays,
    all.x = T,all.y = F,
    by.x = c("group_1","activdate"),
    by.y = c("group_1","date.p")
  )
  
  
  ## Create prediction file and write
  testsetdt <- d1[
    d1$people_id %in% ppl$people_id[testset],
    c("activity_id", "filled", "outcome"), with = F
    ]
  testsetdt <- testsetdt[is.na(testsetdt$outcome), c("activity_id", "filled"), with = FALSE]
  test_fill[, i] <- as.numeric(!is.na(testsetdt$filled))
  
  validsetdt <- d1[
    d1$people_id %in% ppl$people_id[sampling_valid],
    c("activity_id", "filled", "outcome"), with = F
    ]
  validsetdt <- validsetdt[!is.na(validsetdt$outcome), c("activity_id", "filled", "outcome"), with = FALSE]
  valid_fill[which(d1_activities %in% validsetdt$activity_id), ((i-1) %/% folds + 1)] <- as.numeric(is.na(validsetdt$filled))
  cat(" - ", sprintf("%06d", nrow(validsetdt)), " obs]: ", sep = "")
  
  testsetdt[is.na(testsetdt$filled), filled := testsetdt[,mean(filled, na.rm = T)]]
  validsetdt[is.na(validsetdt$filled), filled := validsetdt[,mean(filled, na.rm = T)]]
  
  
  diag$valid_obs[i] <- nrow(validsetdt)
  diag$valid_AUC[i] <- FastROC(actual = validsetdt$outcome, predicted = validsetdt$filled)
  diag$valid_failed[i] <- sum(valid_fill[which(d1_activities %in% validsetdt$activity_id), ((i-1) %/% folds + 1)])
  diag$valid_failed_perc[i] <- diag$valid_failed[i] / nrow(validsetdt)
  diag$valid_correct_zero[i] <- sum((validsetdt$filled == 0) & (validsetdt$outcome == 0), na.rm = TRUE)
  diag$valid_correct_one[i] <- sum((validsetdt$filled == 1) & (validsetdt$outcome == 1), na.rm = TRUE)
  diag$valid_correct[i] <- diag$valid_correct_zero[i] + diag$valid_correct_one[i]
  diag$valid_correct_perc[i] <- diag$valid_correct[i] / nrow(validsetdt)
  diag$test_failed[i] <- sum(is.na(testsetdt$filled))
  diag$test_failed_perc[i] <- diag$test_failed[i] / 498687
  diag$test_perfect_zero[i] <- sum(testsetdt$filled == 0, na.rm = TRUE)
  diag$test_perfect_one[i] <- sum(testsetdt$filled == 1, na.rm = TRUE)
  diag$test_perfect[i] <- diag$test_perfect_zero[i] + diag$test_perfect_one[i]
  diag$test_perfect_perc[i] <- diag$test_perfect[i] / 498687
  test_table[, i] <- testsetdt$filled
  valid_table[which(d1_activities %in% validsetdt$activity_id), ((i-1) %/% folds + 1)] <- validsetdt$filled
  
  cat("AUC = ", sprintf("%6.4f", diag$valid_AUC[i]), " [Mean+SD: ", sprintf("%8.6f", mean(diag$valid_AUC[1:i])), "+", ifelse(i == 1, sprintf("%8.6f", 0.0), sprintf("%8.6f", sd(diag$valid_AUC[1:i]))), "] - Fail on ", sprintf("%06d", diag$valid_failed[i]), " (", sprintf("%05.2f", 100*diag$valid_failed_perc[i]), "%) obs [0=", sprintf("%06d", diag$valid_correct_zero[i]), ", 1=", sprintf("%06d", diag$valid_correct_one[i]), " => ", sprintf("%06d", diag$valid_correct[i]), " (", sprintf("%5.2f", 100*diag$valid_correct_perc[i]), "%) ]\n", sep = "")
  gc(verbose = FALSE)
  #cat(" -- ", format(Sys.time(), "%H:%M:%S"), "\n", sep = "")
  
}

test_out$outcome <- rowMeans(test_table, na.rm = TRUE)
gc(verbose=FALSE)
test_out$perfect <- rowMeans(((test_table == 0) | (test_table == 1))*1, na.rm = TRUE)
gc(verbose=FALSE)
test_out$filled <- rowMeans(test_fill, na.rm = TRUE)
gc(verbose=FALSE)
test_out$minimum <- apply(test_table, 1, function(x) {min(x, na.rm = TRUE)})
gc(verbose=FALSE)
test_out$maximum <- apply(test_table, 1, function(x) {max(x, na.rm = TRUE)})
gc(verbose=FALSE)
test_out$std <- apply(test_table, 1, function(x) {sd(x, na.rm = TRUE)})
gc(verbose=FALSE)

valid_out$outcome <- rowMeans(valid_table, na.rm = TRUE)
gc(verbose=FALSE)
valid_out$perfect <- rowMeans(((valid_table == 0) | (valid_table == 1))*1, na.rm = TRUE)
gc(verbose=FALSE)
valid_out$filled <- rowMeans(valid_fill, na.rm = TRUE)
gc(verbose=FALSE)
valid_out$minimum <- apply(valid_table, 1, function(x) {min(x, na.rm = TRUE)})
gc(verbose=FALSE)
valid_out$maximum <- apply(valid_table, 1, function(x) {max(x, na.rm = TRUE)})
gc(verbose=FALSE)
valid_out$std <- apply(valid_table, 1, function(x) {sd(x, na.rm = TRUE)})
gc(verbose=FALSE)

write.csv(test_out, "leak_200rsamp.csv", row.names = FALSE)
write.csv(test_out[, c(1, 2)], "leak_200rsamp_submit.csv", row.names = FALSE)

write.csv(cbind(valid_out, truth = d1_train$outcome, people = d1_train$people_id, group = d1_train$group_1, date = d1_train$activdate, rowId = 1:2197291), "valid_200rsamp.csv", row.names = FALSE)
write.csv(valid_fill, "valid_200rsamp_raw.csv")

leak_compare <- fread("leak_submission.csv")
plot(x = 1:498687, y = abs(test_out$outcome - leak_compare$outcome))
plot(x = 1:498687, y = test_out$std)

mini_learn <- d1_activities[which(abs(valid_out$outcome - d1_train$outcome) >= 0.5)] # 0.98 LB
write.csv(mini_learn, "leak_threshold50p.csv", row.names = FALSE)
