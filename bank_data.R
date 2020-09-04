library(readr)
library("MASS")
library(corrplot)
bank_full <- read_delim("C:/Users/rafik/Google Drive/Education/ds8002/ds8002project/bank/bank-full.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

#outcome vector indexing
outcome = vector()
for(i in 1:length(bank_full$y))
{
  if(bank_full$y[i] == "yes")
  {
    outcome[i] = 1
  }
  else
  {
    outcome[i] = 0
  }
}

#education vector indexing
education = vector()
for(i in 1:length(bank_full$education))
{
  if(bank_full$education[i] == "tertiary")
  {
    education[i] = 3
  }
  else if (bank_full$education[i] == "secondary")
  {
    education[i] = 2
  }
  else if (bank_full$education[i] == "primary")
  {
    education[i] = 1
  }
  else if (bank_full$education[i] == "unknown")
  {
    education[i] = 0
  }
}

#pick interesting features of bike data
bank_data <- data.frame(bank_full$age,
                        bank_full$balance,
                        education,
                        bank_full$previous,
                        outcome)

#plot histos
hist(bank_full$age)
hist(bank_full$balance)
hist(education)
hist(bank_full$previous)
hist(outcome)

#report stats
mean(bank_full$age)
median(bank_full$age)
var(bank_full$age)

mean(bank_full$balance)
median(bank_full$balance)
var(bank_full$balance)

mean(education)
median(education)
var(education)

mean(bank_full$previous)#meaningless metric here
median(bank_full$previous)
var(bank_full$previous) #meaningless metric here 

mean(outcome) #meaningless metric here
median(outcome)
var(outcome) #meaningless metric here 

#some notes here, the correlation is potentially meaningless as this is a class based problem unless we are looking to create a prediction value that gives a probability of yes/no
corr_matrix <- cor(bank_data)
corrplot(corr_matrix)


#data prep
data_size = length(bank_data$outcome)
train_val = .8*data_size
test_val = train_val + 1

#without response variable
bank_data_matrix = matrix(c(bank_data$bank_full.age,
                            bank_data$bank_full.balance,
                            bank_data$education,
                            bank_data$bank_full.previous),nrow = data_size, ncol = 4)
bank_data_matrix_train = bank_data_matrix[1:train_val,]
bank_data_matrix_test = bank_data_matrix[test_val:data_size,]

#with response variable for some calculations
bank_data_with_outcome = data.matrix(bank_data)
bank_data_train = bank_data_with_outcome[1:train_val,]
bank_data_test = bank_data_with_outcome[test_val:data_size,]

#create estimation functions
cov_custom <- function(x,means)
{
  row_size = length(x[,1])
  column_size = length(x[1,])
  output_matrix = matrix(0,column_size,column_size)
  for(i in 1:row_size)
  {
    output_matrix = output_matrix + (x[i,]-means)%*%t(x[i,]-means)
  }
  output_matrix = output_matrix/row_size
  return(output_matrix)
}

mu_custom <- function(x)
{
  row_size = length(x[,1])
  column_size = length(x[1,])
  mu = vector()
  for(i in 1:column_size)
  {
    mu = c(mu,mean(x[,i]))
  }
  return(mu)
}

priors_custom <- function(x,output_variable)
{
  classes = unique(output_variable)
  row_size = length(x[,1])
  
  priors_vector = vector()
  for(i in 1:length(classes))
  {
    class = subset(x,output_variable == classes[i])
    class_size = length(class[,1])
    prior = class_size/row_size
    priors_vector = c(priors_vector,prior)
  }
  return(priors_vector)
}

#determine # of classes in the data
classes = unique(outcome)
classes

#find the priors #find the mean
priors = priors_custom(bank_data_train,bank_data_train[,5])
priors 

#find the covariance matrix and the means
s_matrixes = list()
mu = matrix(0,2,4)
for(i in 1:length(classes))
{
   data_subset = subset(bank_data_train,bank_data_train[,5] == classes[i])
   mu[i,] = mu_custom(data_subset[,1:4])
   s_matrixes[[i]] = cov_custom(data_subset[,1:4],mu[i,])
}
s_matrixes
mu

#implement qda
discriminant_model <- function(x,mu,s,priors)
{
  discriminants = matrix(0,length(x[,1]),length(priors)+1)
  for(i in 1:length(x[,1]))
  {
    for(j in 1:length(priors))
    {
      wio = (-0.5) * (t(mu[j,])%*%ginv(s[[j]])%*%mu[j,]) - (0.5 * log(det(s[[j]]))) + log(priors[j])
      w = ginv(s[[j]]) %*% mu[j,]
      Wi = (-0.5) * ginv(s[[j]])
      discriminants[i,j] =  t(x[i,])%*%Wi%*%x[i,] + t(w)%*%x[i,] + wio
    }
    if(discriminants[i,1] > discriminants [i,2])
    {
      discriminants[i,3] = 0
    }
    else
    {
      discriminants[i,3] = 1
    }
  }

  return(discriminants)
}

result = discriminant_model(bank_data_matrix_test,mu,s_matricies,priors)

values_in_class_1= sum(bank_data_test[,5] == 1)
values_in_class_1_model = sum(result[,3] == 1)

#rda
for(alpha in seq(0,1,by=.1))
{
  s = matrix(0,4,4)
  for(i in 1:length(s_matrixes))
  {
    s = s + s_matrixes[[i]]
  }
  
  s_matricies = list()
  for(i in 1:length(s_matrixes))
  {
    s_matricies[[i]] = alpha * s_matrixes[[i]] + (1-alpha) * s  
  }
  
  output = discriminant_model(bank_data_matrix_test,mu,s_matricies,priors)
  values_in_class_1_model = sum(output[,3] == 1)
  print(values_in_class_1_model)
}

model <- qda(bank_data$outcome ~ bank_data$bank_full.age + bank_data$bank_full.balance + bank_data$education + bank_data$bank_full.previous, data= bank_data[1:train_val,])
predict(model,bank_data[test_val:length(bank_data[,1]),1:4])
