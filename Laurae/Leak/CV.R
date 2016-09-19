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

d1_store <- d1
d1_index <- which(is.na(d1_store$outcome))
d1_setdiff <- setdiff(1:2695978, d1_index)
test_out <- data.frame(activity_id = fread("leak_submission_NA.csv")$activity_id, outcome = numeric(498687), perfect = numeric(498687), filled = numeric(498687), minimum = numeric(498687), maximum = numeric(498687), std = numeric(498687))
test_table <- matrix(nrow = 498687, ncol = 200)
test_fill <- matrix(nrow = 498687, ncol = 200)

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



for (i in 1:200) {
  
  # mix stuff ---------------------------------------------------------------
  
  cat("[", format(Sys.time(), "%H:%M:%S"), " iteration ", sprintf("%03d", i), "]: ", sep = "")
  set.seed(11111+i)
  temp_sample <- c(sample(d1_setdiff, size = 2087426), d1_index)
  d1 <- d1_store[temp_sample, ]
  
  
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
    mean(outcome),
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
    c("activity_id","filled"), with = F
    ]
  test_fill[, i] <- as.numeric(!is.na(testsetdt$filled))
  
  cat("failed to predict ", sprintf("%06d", sum(is.na(testsetdt$filled))), " (", sprintf("%6.4f", sum(is.na(testsetdt$filled))/4986.87), "%) values. [0=", sprintf("%06d", sum(testsetdt$filled == 0, na.rm = TRUE)), " ; 1=", sprintf("%06d", sum(testsetdt$filled == 1, na.rm = TRUE)), " ; perfect: ", sprintf("%06d", sum(testsetdt$filled == 0, na.rm = TRUE) + sum(testsetdt$filled == 1, na.rm = TRUE)), " (", sprintf("%6.4f", (sum(testsetdt$filled == 0, na.rm = TRUE) + sum(testsetdt$filled == 1, na.rm = TRUE))/4986.87), "%) ]", sep = "")
  testsetdt[is.na(testsetdt$filled), filled := testsetdt[,mean(filled, na.rm = T)]]
  test_table[, i] <- testsetdt$filled
  gc(verbose = FALSE)
  cat(" -- ", format(Sys.time(), "%H:%M:%S"), "\n", sep = "")
  
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

write.csv(test_out, "leak_500rsamp.csv", row.names = FALSE)
write.csv(test_out[, c(1, 2)], "leak_500rsamp_submit.csv", row.names = FALSE)

leak_compare <- fread("leak_submission.csv")
plot(x = 1:498687, y = abs(test_out$outcome - leak_compare$outcome))
plot(x = 1:498687, y = test_out$std)
