data <- read.csv("portland_housing1.csv")



dim(data)
drop_cols <- c()
for (i in colnames(data)){
  if (sum(is.na(data[[i]])) > 4000){
    drop_cols <- c(drop_cols, i)
  }
}


data <- data[ , !(names(data) %in% drop_cols)]
dim(data)

#X<- data[, !(colnames(data) %in% c("price"))]
#y<- data[, (colnames(data) %in% c("price"))]

numeric_col <- c()
categorical_col <- c()

for (i in colnames(data)){
  if(class(data[[i]]) == 'character' || class(data[[i]]) == 'logical' )
  {categorical_col <- c(categorical_col,i)}
  else{
    numeric_col <- c(numeric_col,i)
  }
}

num_na_cols <- c()

for(i in numeric_col){
    if(sum(is.na(data[[i]]))>0){
      num_na_cols <- c(num_na_cols,i)
    }
  }


cat_na_col <- c()
for(i in categorical_col){
  if(sum(is.na(data[[i]]))>0){
    cat_na_col <- c(cat_na_col,i)
  }
}



for(i in numeric_col){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

calc_mode <- function(x){
  
  distinct_values <- unique(x)
  distinct_tabulate <- tabulate(match(x, distinct_values))
  distinct_values[which.max(distinct_tabulate)]
}


for(i in categorical_col){
  data[is.na(data[,i]), i] <- calc_mode(data[,i])
}

#for(i in categorical_col){
#  distinct_values <- unique(data[,i])
#  print(i)
#  print(distinct_values)
#}


#for (i in colnames(data)){
#  print(class(data[[i]]))
#  print(sum(is.na(data[[i]])))
#}


library(MASS)
library(caTools)
#install.packages("caTools")
library(broom)


split = sample.split(data$price, SplitRatio = 0.70)
train_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)
dim(train_set)
dim(test_set)

model  <- lm(price ~ ., data = train_set)

pred = predict(model,test_set)

plot(pred,test_set$price)






