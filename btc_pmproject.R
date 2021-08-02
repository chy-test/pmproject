library(Boom)
library(zoo)
library(bsts)
library("writexl")
library(reshape2)

btc <- read.csv("data_btc.csv",
               header = TRUE)

btc_z <- read.zoo(btc)

# plotting the price
plot(btc_z$btc_usd, main='bitcoin price from 2017 to 2021',
     xlab='Date',
     ylab='Bitcoin price')
ss <- AddLocalLinearTrend(list(), btc_z$btc_usd)
#ss <- AddSeasonal(ss, btc_z$btc_usd, nseasons = 52)
model1 <- bsts(btc_z$btc_usd,
               state.specification = ss,
               niter = 1000)
plot(model1)
plot(model1, "components")
names(model1)
pred1 <- predict(model1, horizon = 156)
plot(pred1)

# Fit a bsts model with expected model size 1, the default.
model2 <- bsts(btc_usd ~ .,
               state.specification = ss,
               niter = 1000,
               data = btc_z)
plot(model2, "comp")
model3 <- bsts(btc_usd ~ .,
               state.specification = ss,
               niter = 10000,
               data = btc_z,
               expected.model.size = 5)  # Passed to SpikeSlabPrior.
plot(model2, "coef")
plot(model3, "coef")
CompareBstsModels(list("Model 1" = model1,
                       "Model 2" = model2,
                       "Model 3" = model3),
                  colors = c("black", "red", "blue"))
CompareBstsModels(list("Model 1" = model1,
                       "Model 2" = model2,
                       "Model 3" = model3),
                  colors = c("black", "red", "blue"))



### Scaled data ###
centered.x <- scale(btc_z)
head(centered.x)
plot(centered.x$btc_usd, main='bitcoin price from 2017 to 2021',
     xlab='Date',
     ylab='Bitcoin price')
ssc <- AddLocalLinearTrend(list(), centered.x$btc_usd)
model1s <- bsts(centered.x$btc_usd,
                state.specification = ssc,
                niter = 1000)
plot(model1s)
plot(model1s, "components")
pred1s <- predict(model1s, horizon = 12)
plot(pred1s)
# Fit a bsts model with expected model size 1, the default.
model2s <- bsts(btc_usd ~ .,
                state.specification = ssc,
                niter = 1000,
                data = centered.x)
plot(model2s, "comp")
plot(model2s, "coef")
#look at parameters to specify
# iter large enough, the first important regressors seem to be similar
model3s <- bsts(btc_usd ~ .,
                state.specification = ssc,
                niter = 1000,
                data = centered.x,
                expected.model.size = 20)  # Passed to SpikeSlabPrior.
plot(model3s, "coef")
model3s$coefficients
burn <- SuggestBurn(0.1, model3s)

#Calculate the inclusion probabilities. Maybe some other statistics we want to report more
inclusionprobs <- melt(colMeans(model3s$coefficients[-(1:burn),] != 0))

CompareBstsModels(list("Model 1" = model1s,
                       "Model 2" = model2s,
                       "Model 3" = model3s),
                  colors = c("black", "red", "blue"))
#US China bolivia nigeria south africa venezuela colombia 

#MAPE calculation for local level trend model----

ss <- AddLocalLinearTrend(list(), btc_z$btc_usd)

#ss <- AddSeasonal(ss, btc_z$btc_usd, nseasons = 52)
model1 <- bsts(btc_z$btc_usd,
               state.specification = ss,
               niter = 1000)
### Get a suggested number of burn-ins
burn <- SuggestBurn(0.1, model1)
### Predict
p <- predict.bsts(model1, horizon = 12, burn = burn, quantiles = c(.025, .975))

### Actual versus predicted
d2 <- data.frame(
  # fitted values and predictions
  c(as.numeric(-colMeans(model1$one.step.prediction.errors[-(1:burn),])+btc_z$btc_usd)),
  # actual data and dates 
  as.numeric(btc_z$btc_usd),
  as.Date(time(btc_z$btc_usd)))


names(d2) <- c("Fitted", "Actual", "Date")

### MAPE (mean absolute percentage error)
MAPE <- mean(abs(d2$Actual-d2$Fitted)/d2$Actual)
MSE <- mean((d2$Actual-d2$Fitted)^2)
SMAPE <- mean(2*abs(d2$Actual-d2$Fitted)/(d2$Actual+d2$Fitted))
 
#Local level model----                                          
#AddLocalLevel                                          
ss <- AddLocalLevel(list(), btc_z$btc_usd)

#ss <- AddSeasonal(ss, btc_z$btc_usd, nseasons = 52)
model_ll <- bsts(btc_z$btc_usd,
               state.specification = ss,
               niter = 1000)
### Get a suggested number of burn-ins
burn <- SuggestBurn(0.1, model_ll)
### Predict
p <- predict.bsts(model_ll, horizon = 12, burn = burn, quantiles = c(.025, .975))

### Actual versus predicted
d_ll <- data.frame(
  # fitted values and predictions
  c(as.numeric(-colMeans(model_ll$one.step.prediction.errors[-(1:burn),])+btc_z$btc_usd)),
  # actual data and dates 
  as.numeric(btc_z$btc_usd),
  as.Date(time(btc_z$btc_usd)))


names(d_ll) <- c("Fitted", "Actual", "Date")                                           

### MAPE (mean absolute percentage error)
MAPE_ll <- mean(abs(d_ll$Actual-d_ll$Fitted)/d_ll$Actual)   
MSE_ll <- mean((d_ll$Actual-d_ll$Fitted)^2)
SMAPE_ll <- mean(2*abs(d_ll$Actual-d_ll$Fitted)/(d_ll$Actual+d_ll$Fitted))
## Next task could be: experiment with predictions (not only fitted)
## check with other models: regression, shocks, etc
## Check with other errors
# to unscale: d$s.x * attr(d$s.x, 'scaled:scale') + attr(d$s.x, 'scaled:center')


# Running Spike and Slab with priors and calculate accuracy errors----
### Scaled data ###
centered.x <- scale(btc_z)
head(centered.x)
plot(centered.x$btc_usd, main='bitcoin price from 2017 to 2021',
     xlab='Date',
     ylab='Bitcoin price')
ssc <- AddLocalLinearTrend(list(), centered.x$btc_usd)

model3s <- bsts(btc_usd ~ .,
                state.specification = ssc,
                niter = 1000,
                data = centered.x,
                expected.model.size = 20)  # Passed to SpikeSlabPrior.
plot(model3s, "coef")
model3s$coefficients
burn <- SuggestBurn(0.1, model3s)

#Calculate the inclusion probabilities. Maybe some other statistics we want to report more
inclusionprobs <- melt(colMeans(model3s$coefficients[-(1:burn),] != 0))
#calculate the mean of coefficents and take them as the prior means
colMeans(model3s$coefficients)
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))

#Including Prior Expectations in the Model, need to include for intercept
prior.spikes <- c(0.05,0.05,0.05,0.05,0.05,0.05,1,0.05,1,1,0.05,1, 0.05, 1, 0.05, 0.05, 0.05, 1, 0.05, 0.05, 0.05)
prior.mean <- c(0,0,0,0,0,0,-0.0365,0,-0.0244,0.00766,0,-0.0797, 0, 0.0341, 0, 0, 0, 0.0127, 0, 0, 0)

### Set up the priors
#Q: if we change prior, should we change this prior.information.weight = 200? what is it?
prior <- SpikeSlabPrior(x=model.matrix(centered.x$btc_usd ~ ., data=centered.x), 
                        y=centered.x$btc_usd, 
                        #prior.information.weight = 200,
                        prior.inclusion.probabilities = prior.spikes,
                        optional.coefficient.estimate = prior.mean)

reg_priors <- bsts(centered.x$btc_usd ~ ., state.specification = ss, 
                        data = centered.x, 
                        niter = 500, 
                        prior=prior, 
                        ping=100, seed=2016)

### Get the average coefficients when variables were selected (non-zero slopes)
#coeff <- data.frame(melt(apply(reg_priors$coefficients[-(1:burn),], 2, PositiveMean)))
coeff <- apply(reg_priors$coefficients[-(1:burn),], 2, FUN=mean)
coeff
coeff$Variable <- as.character(row.names(coeff))
#Including Prior Expectations in the Model: Bernoulli
prior.spikes <- c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
prior.mean <- c(0.4,0.4,0.4,0,0,0,0,0,0,0.3,0)