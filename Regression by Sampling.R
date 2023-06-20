
# Size of the data set
N = 10^8

# Number of rows sampled
Sample_Size = 100

# Number of times a sample is taken
Runs = 100

# Height of the red comparison line
height = 0.5

# Creating dummy variables
X1 = rnorm(N)
X2 = rnorm(N)
X3 = rnorm(N)

# Defining a dummy outcome variable
Y = 3*X1 + 2*X2 + X3 + 4 + rnorm(N)

# Setting up an empty matrix
Estimates <- matrix(0, nrow = Runs, ncol = 4)

# Fills the estimate matrix with coefficients calculated from the samples
for(i in 1:Runs){
Ind = sample(1:N, size = Sample_Size, replace = TRUE)
Estimates[i,] <- summary(lm(Y[Ind] ~ X1[Ind] + X2[Ind] + X3[Ind]))$coefficients[,1]
}

# A simple function that takes the absolute value difference of two inputs
Error <- function(x,y){return(round(abs(x-y),4))}

# Calculates the real estimates and prints the difference from the true values
Est <- apply(Estimates, MARGIN = 2, FUN = mean)
print(paste("Int Error =", Error(Est[1],4),"     X1 Error =", Error(Est[2],3),
            "     X2 Error =",Error(Est[3],2),"     X3 Error =",Error(Est[4],1)))

# Creates a barplot with the differences between estimates and parameters
barplot(Error(Est, c(4,3,2,1)), names.arg = c("Int","X1","X2","X3"), 
        main = "Resulting Error", xlab = "Parameters", ylab = "Absolute Difference",
        col = c("green","blue","orange","yellow"))
abline(h = height, col = "red")


Reg_Sampler <- function(Equation, Data, Runs = 100, Sample_Size = 100, Confidence = 1.96, Decimals = 3, Correct_Values = -99){
  
  # Samples the data and creates a data frame of estimated coefficients
  Estimates <- matrix(0, nrow = Runs, ncol = length(summary(lm(Equation, data = Data[1:ncol(Data),]))$coefficients[,1]))
  for(i in 1:Runs){
    Ind = sample(1:nrow(Data), size = Sample_Size, replace = TRUE)
    Estimates[i,] <- summary(lm(Equation, data = Data[Ind,]))$coefficients[,1]
  }
  
  # Takes the mean and standard deviation of coefficient estimates
  Est <- apply(Estimates, MARGIN = 2, FUN = mean)
  sd <- apply(Estimates, MARGIN = 2, FUN = sd)
  
  # Constructs confidence Intervals (that are validated as correct later)
  Intervals <- rep(0,length(Est))
  for(i in 1:length(Est)){
    Intervals[i] <- paste(round(Est[i]-Confidence*sd[i]*(1/sqrt(Runs)),Decimals),
                          "to",round(Est[i]+Confidence*sd[i]*(1/sqrt(Runs)),Decimals))
  }
  
  # This allows someone to specify the parameter values if known (for validation)
  if(Correct_Values[1] == -99){
    Correct_Values = rep(0,length(Est))
    print("No correct values supplied, which defaulted to 0!")
  }
  
  # This notes in the output if the confidence interval correctly captured the parameter
  Correct_Int = rep(0,length(Est))
  for(i in 1:length(Est)){
    if(round(Est[i]-Confidence*sd[i]*(1/sqrt(Runs)),Decimals) <= Correct_Values[i] & 
       round(Est[i]+Confidence*sd[i]*(1/sqrt(Runs)),Decimals) >= Correct_Values[i]){
      Correct_Int[i] = 1
    }
  }
  
  # The code below simply organizes and formats the output of the function
  Output <- data.frame(rbind(Est,Intervals,as.numeric(Correct_Int)))
  if(ncol(Output) > 1){names(Output) <- c("Intercept",paste("Coef",1:(ncol(Output)-1)))}
  if(ncol(Output) == 1){names(Output) <- "Intercept"}
  row.names(Output) <- c("Point Estimate", "Confidence Interval", "One if Correct")
  return(Output)
}

# Note that the intercept can be removed, but the column names will be made incorrect!
Reg_Sampler(Equation = Y ~ X1 + X2 + X3, Data = data.frame(X1,X2,X3,Y), Runs = 100, Sample_Size = 100)


# This function validates that the percentage of correct confidence intervals are what we expect.
# The default number of replications, N, is 100 because it can take a while to compute.
# With an N of 10,000 I obtained values of 0.9555, 0.9511, 0.9499, and 0.9488 showing that they are in fact 95% CIs.

Validation_Fun <- function(N = 100, Correct = 4:1, Conf = 1.96){
Test_Frame = matrix(0, nrow=N, ncol=4)
for(i in 1:N){
  Test_Frame[i,] = as.matrix(as.numeric(Reg_Sampler(Equation = Y ~ X1 + X2 + X3, Confidence = Conf,
                      Data = data.frame(X1,X2,X3,Y), Runs = 100, Sample_Size = 100, Correct_Values = Correct)[3,]))
}
return(apply(Test_Frame, MARGIN = 2, FUN = mean))
}

Validation_Fun()





