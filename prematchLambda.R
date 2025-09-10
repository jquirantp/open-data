source('soccer-utility.R')


data = read.csv('E0.csv')
plot(hist(data$HS,breaks=100))
plot(hist(data$AS,breaks=100))

data$vig = 1/data$PSH + 1/data$PSD + 1/data$PSA
data$Probability_H = data$PSH / data$vig
data$Probability_D = data$PSD / data$vig
data$Probability_A = data$PSA / data$vig

data$vig2 = 1/data$P.2.5 + 1/data$P.2.5.1
data$Probability_Under = data$P.2.5.1 / data$vig2
data$Probability_Over = data$P.2.5 / data$vig2
data$Line = 2.5
data = data[!is.na(data$Probability_Over),]
ret_par = list()
for(ii in 1:nrow(data)) {
  fit_par = soccerutility.fitWrapper(cs_gen = soccerutility.cs_poisson_df,
                                     had = c(data$Probability_H[ii], data$Probability_D[ii], data$Probability_A[ii]),
                                     hilo = c(data$Line[ii],data$Probability_Under[ii], data$Probability_Over[ii])
  )
  ret_par[[ii]] = fit_par;
}
ret_par_mat = as.data.frame(matrix(unlist(ret_par),ncol=3,byrow = T))
colnames(ret_par_mat) = c('lambdaH','lambdaA','lambdaO');
data = cbind(data,ret_par_mat)
data$expH = (data$lambdaH + data$lambdaA)/2
data$expA = (data$lambdaH - data$lambdaA)/2

