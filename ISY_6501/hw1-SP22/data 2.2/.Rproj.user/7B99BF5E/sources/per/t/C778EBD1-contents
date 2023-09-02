
#2.2.1
library(kernlab)

#import data from txt.
data <- read.table("credit_card_data.txt", header = FALSE)

#define ksvm function. Call the linear equation to find the best C-value
model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type='C-svc',kernel='vanilladot',C=10000,scaled=TRUE)

# calculate a1…am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a
# calculate a0
a0 <- -model@b
a0

# see what the model predicts
pred <- predict(model,data[,1:10])
pred

# see what fraction of the model’s predictions match the actual classification
sum(pred == data[,11]) / nrow(data)

#Test result: 1. C=100, accuracy is 0.8639144. Then changed the C-value to C=50,000. Trying to see if I changed a larger c-value
#will the accuracy rate be different from the first test. When C=50,000, the accuracy is 0.8623853.
#In my third test. I tested C=100,000. The results came out the accuracy rate dropped from 0.8623853 to 0.6253823.
#In conclusion, I decided to keep the cost between 100 - 10,000 since the results shown the cost within this range has the highest accuracy.

#for loop for different c values
cvalues <- c(0.00001,0.0001,0.01,1,10,100,10000,1000000)
#setup linear equation for kernel we want to use in this question
k_kernel = 'vanilladot'
#create a dataframe to store the data from the for loop
results <- data.frame("C-Values" = integer(),"accuracy" = numeric())

for(cvalue in cvalues)
{
  #test the c values in model
  model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type='C-svc',kernel=k_kernel,C=cvalue,scaled=TRUE)
  
  #use ksvm to run formular in different c value.
  pred <- predict(model,data[,1:10])
  
  #result in fraction of the model’s predictions match the actual classification
  accuracy <- sum(pred == data[,11]) / nrow(data)
  
  #need function to contain value
  results[nrow(results) + 1,] <- c(cvalue,accuracy)
}
  # we find out the highest accuracy rate is 0.8639. Where the c value in between 0.01 - 100.
  #Therefore, the best c vaule of this model we can pick from 0.01-100 to get the highest accuracy rate.
  View(results)


#2.2.2 (Optional)
#try different methods other than vanilladot
diffkernels <- c("rbfdot","splinedot","laplacedot","polydot","vanilladot")
#for loop for different c values
cvalues <- c(0.00001,0.0001,0.01,1,10,100,10000,1000000)
#setup linear equation for kernel we want to use in this question
#create a dataframe to store the data from the for loop
results <- data.frame("C-Values" = integer(),"accuracy" = numeric(),"kernel_name" = character(),stringsAsFactors = FALSE)
n <- 0
for (ikernel in diffkernels) {

  n <- n+1  
  for(cvalue in cvalues)
  {
    #test the c values in model
    model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type='C-svc',kernel=ikernel,C=cvalue,scaled=TRUE)
    
    #use ksvm to run formular in different c value.
    pred <- predict(model,data[,1:10])
    
    #result in fraction of the model’s predictions match the actual classification
    accuracy <- sum(pred == data[,11]) / nrow(data)
    
    #need function to contain value
    results[nrow(results) + 1,] <- c(cvalue,accuracy,ikernel)
  }
}

# we find out the highest accuracy rate is 0.8639. Where the c value in between 0.01 - 100.
#Therefore, the best c vaule of this model we can pick from 0.01-100 to get the highest accuracy rate.
View(results)

#2.2.3
library(kknn)

knndata <- read.table("credit_card_data.txt", header = FALSE,stringsAsFactors = FALSE)

head(knndata)

#create function to run for the train and test value
accuracy_check = function(X){
  
  predicted <- rep(0,(nrow(knndata))) 
  
  for (i in 1:nrow(data))
  {
    
    knn_credit=kknn(V11~.,knndata[-i,],knndata[i,],k=X,distance = 2,scale = TRUE) 
    
    predicted[i] <- as.integer(fitted(knn_credit)+0.5) # round off to 0 or 1
  }
  
  # calculate % of correct predictions
  
  accuracy<- sum(predicted == knndata[,11]) / nrow(data)
  return(accuracy)
}

kknn_accuracy <- rep(0,20)

for (i in 1:20) {
  kknn_accuracy[i] <- accuracy_check(i)
}

kknn_result <- data.frame(kknn_accuracy)
View(kknn_result)

best_kvalue <- which.max(kknn_accuracy)
View(best_kvalue)

print(paste0("The highest kknn accuracy rate is ", max(kknn_accuracy), " and the best k value is ", best_kvalue))


#Plot the graph for accuracy vs. K-values
par(bg="white")
plot(kknn_accuracy,ylab="accuracy level",xlab="K values",type='b',col='blue', main='KKN accuracy')



#install.packages("tinytex")
#install.packages("rmarkdown")
