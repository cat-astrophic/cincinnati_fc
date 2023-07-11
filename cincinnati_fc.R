# This script runs regressions for the Cincinnati FC project

# Loading libraries

library(AER)
library(stargazer)
library(sandwich)
library(jtools)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(modelsummary)

# Project directory info

direc <- 'D:/cincinnati_fc/'

# Reading in the data

data <- read.csv(paste(direc, 'data/real_house_prices.csv', sep = ''))

# Event dates

usl.announed <- '8/12/2015'
mls.announed <- '5/29/2018'
mls.match <- '3/2/2019'
tql.announed <- '12/18/2018' # groundbreaking date
tql.opened <- '5/16/2021'

# Adding indicators for period in which transactions occurred

# Two years before event

usl.a0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('8/12/2015', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('8/12/2013', '%m/%d/%Y')) # Two years before
mls.a0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('5/29/2018', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('5/29/2016', '%m/%d/%Y')) # Two years before
mls.m0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('3/2/2019', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('3/2/2017', '%m/%d/%Y')) # Two years before
tql.a0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('12/18/2018', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('12/18/2016', '%m/%d/%Y')) # Two years before
tql.o0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('5/16/2021', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('5/16/2019', '%m/%d/%Y')) # Two years before

# Two years after event

usl.a1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('8/12/2015', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('8/12/2017', '%m/%d/%Y')) # Two years after
mls.a1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('5/29/2018', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('5/29/2020', '%m/%d/%Y')) # Two years after
mls.m1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('3/2/2019', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('3/2/2021', '%m/%d/%Y')) # Two years before
tql.a1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('12/18/2018', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('12/18/2020', '%m/%d/%Y')) # Two years before
tql.o1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('5/16/2021', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('5/16/2023', '%m/%d/%Y')) # Two years before

# Add to data.frame

data$Pre.USL.Announced <- usl.a0
data$Post.USL.Announced <- usl.a1
data$Pre.MLS.Announced <- mls.a0
data$Post.MLS.Announced <- mls.a1
data$Pre.MLS.Match <- mls.m0
data$Post.MLS.Match <- mls.m1
data$Pre.TQL.Announced <- tql.a0
data$Post.TQL.Announced <- tql.a1
data$Pre.TQL.Opened <- tql.o0
data$Post.TQL.Opened <- tql.o1

# Create event markers

data$usl.a <- usl.a0 + usl.a1
data$mls.a <- mls.a0 + mls.a1
data$mls.m <- mls.m0 + mls.m1
data$tql.a <- tql.a0 + tql.a1
data$tql.o <- tql.o0 + tql.o1

# Creating a month-year variable

data$MY <- format(as.Date(data$Transfer.Date, format = '%m/%d/%Y'), format = '%m/%Y')

# Dropping foreclosures from the data set

data <- data[which(data$Foreclosure == 'No'),]

# Since no names are provided for sellers or buyers, this bit rules out outliers

# Initial histogram of real house prices as a reference

hist(data$Real.Price, breaks = 100)

# Running a simple hedonic to look at outliers via residuals

outlier.finder <- lm(log(Real.Price) ~ log(FinSqFt) + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2)
                     + Rooms*log(FinSqFt) + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt)
                     + Half.Baths*log(FinSqFt) + Acreage*factor(School.District) + factor(MY)
                     + factor(Deed.Type) + factor(Owner.Residence), data = data)

# Clustering at the school district level so that I can compare results here to main results later on

outlier.finder.x <- coeftest(outlier.finder, vcov = vcovCL, cluster = ~School.District)

# Viewing the results

stargazer(outlier.finder, outlier.finder.x, type = 'text')

# Get residuals

residuals <- outlier.finder$residuals

# View residuals

hist(residuals, breaks = 100)
abline(v =- sd(residuals)*2)
abline(v = sd(residuals)*2)

# Based on the histogram, let's keep anything within 3 standard deviations of the mean

keep <- which(abs(residuals) < sd(residuals)*2)

# Subset data based on residuals - 98.69% of the data set remains =>  1.31% was dropped

data <- data[keep,]

# Updating the residuals and real prices histograms

resid <- residuals[keep]
hist(resid, breaks = 100)
hist(data$Real.Price, breaks = 100)
hist(log(data$Real.Price), breaks = 100)

# Creating treatment variables

data$Treatment.Nippert.1k <- as.numeric(data$Nippert <= 1)
data$Treatment.Nippert.1m <- as.numeric(data$Nippert <= 1.61)
data$Treatment.TQL.1k <- as.numeric(data$TQL <= 1)
data$Treatment.TQL.1m <- as.numeric(data$TQL <= 1.61)

# Creating control variables

data$Control.Nippert.1k <- as.numeric(data$Nippert > 1) * as.numeric(data$Nippert <= 5)
data$Control.Nippert.1m <- as.numeric(data$Nippert > 1.61) * as.numeric(data$Nippert <= 3.22)
data$Control.TQL.1k <- as.numeric(data$TQL > 1) * as.numeric(data$TQL <= 5)
data$Control.TQL.1m <- as.numeric(data$TQL > 1.61) * as.numeric(data$TQL <= 3.22)

# Create event-specific data.frames

data$usl.a.x <- data$usl.a * (data$Treatment.Nippert.1k + data$Control.Nippert.1k)
data$mls.a.x <- data$mls.a * (data$Treatment.Nippert.1k + data$Control.Nippert.1k)
data$mls.m.x <- data$mls.m * (data$Treatment.Nippert.1k + data$Control.Nippert.1k)
data$tql.a.x <- data$tql.a * (data$Treatment.TQL.1k + data$Control.TQL.1k)
data$tql.o.x <- data$tql.o * (data$Treatment.TQL.1k + data$Control.TQL.1k)

data.usl.a <- data[which(data$usl.a.x == 1),]
data.mls.a <- data[which(data$mls.a.x == 1),]
data.mls.m <- data[which(data$mls.m.x == 1),]
data.tql.a <- data[which(data$tql.a.x == 1),]
data.tql.o <- data[which(data$tql.o.x == 1),]

# Run models for each scenario at 1 km and 1 mile

# USL Announcement

usl.1k <- lm(log(Real.Price) ~ Treatment.Nippert.1k*Post.USL.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
             + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
             + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
             + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
             + factor(Owner.Residence), data = data.usl.a)

usl.1m <- lm(log(Real.Price) ~ Treatment.Nippert.1m*Post.USL.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
             + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
             + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
             + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
             + factor(Owner.Residence), data = data.usl.a)

usl.1k.clustered <- coeftest(usl.1k, vcov = vcovCL, cluster = ~School.District)
usl.1m.clustered <- coeftest(usl.1m, vcov = vcovCL, cluster = ~School.District)

stargazer(usl.1k, usl.1k.clustered, usl.1m, usl.1m.clustered,
          type = 'text', omit = c('Deed.Type', 'School.District', 'MY'))

# MLS Announcement

mls.1k <- lm(log(Real.Price) ~ Treatment.Nippert.1k*Post.MLS.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
             + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
             + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
             + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
             + factor(Owner.Residence), data = data.mls.a)

mls.1m <- lm(log(Real.Price) ~ Treatment.Nippert.1m*Post.MLS.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
             + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
             + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
             + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
             + factor(Owner.Residence), data = data.mls.a)

mls.1k.clustered <- coeftest(mls.1k, vcov = vcovCL, cluster = ~School.District)
mls.1m.clustered <- coeftest(mls.1m, vcov = vcovCL, cluster = ~School.District)

stargazer(mls.1k, mls.1k.clustered, mls.1m, mls.1m.clustered,
          type = 'text', omit = c('Deed.Type', 'School.District', 'MY'))

# TQL Announcement

tqla.1k <- lm(log(Real.Price) ~ Treatment.TQL.1k*Post.TQL.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
              + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
              + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
              + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
              + factor(Owner.Residence), data = data.tql.a)

tqla.1m <- lm(log(Real.Price) ~ Treatment.TQL.1m*Post.TQL.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
              + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
              + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
              + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
              + factor(Owner.Residence), data = data.tql.a)

tqla.1k.clustered <- coeftest(tqla.1k, vcov = vcovCL, cluster = ~School.District)
tqla.1m.clustered <- coeftest(tqla.1m, vcov = vcovCL, cluster = ~School.District)

stargazer(tqla.1k, tqla.1k.clustered, tqla.1m, tqla.1m.clustered,
          type = 'text', omit = c('Deed.Type', 'School.District', 'MY'))

# TQL Opening

tqlo.1k <- lm(log(Real.Price) ~ Treatment.TQL.1k*Post.TQL.Opened + log(FinSqFt) + I(log(FinSqFt)^2)
              + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
              + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
              + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
              + factor(Owner.Residence), data = data.tql.o)

tqlo.1m <- lm(log(Real.Price) ~ Treatment.TQL.1m*Post.TQL.Opened + log(FinSqFt) + I(log(FinSqFt)^2)
              + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
              + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
              + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
              + factor(Owner.Residence), data = data.tql.o)

tqlo.1k.clustered <- coeftest(tqlo.1k, vcov = vcovCL, cluster = ~School.District)
tqlo.1m.clustered <- coeftest(tqlo.1m, vcov = vcovCL, cluster = ~School.District)

stargazer(tqlo.1k, tqlo.1k.clustered, tqlo.1m, tqlo.1m.clustered,
          type = 'text', omit = c('Deed.Type', 'School.District', 'MY'))

# Simulations incrementing over distance

usl.a.increments.co <- c()
mls.a.increments.co <- c()
tql.a.increments.co <- c()
tql.o.increments.co <- c()

usl.a.increments.se <- c()
mls.a.increments.se <- c()
tql.a.increments.se <- c()
tql.o.increments.se <- c()

for (i in 5:20) {
  
  print(i)
  
  data.usl.a$Treated <- as.numeric(data.usl.a$Nippert <= i/10)
  
  usl.in <- lm(log(Real.Price) ~ Treated*Post.USL.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
               + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
               + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
               + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
               + factor(Owner.Residence), data = data.usl.a)
  
  usl.in.cl <- coeftest(usl.in, vcov = vcovCL, cluster = ~School.District)

  xxx <- broom::tidy(usl.in.cl)
  id <- which(xxx$term == 'Treated:Post.USL.Announced')
  
  usl.a.increments.co <- c(usl.a.increments.co, xxx$estimate[id])
  usl.a.increments.se <- c(usl.a.increments.se, xxx$std.error[id])

}

for (i in 5:20) {
  
  print(i)
  
  data.mls.a$Treated <- as.numeric(data.mls.a$Nippert <= i/10)
  
  mls.in <- lm(log(Real.Price) ~ Treated*Post.MLS.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
               + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
               + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
               + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
               + factor(Owner.Residence), data = data.mls.a)
  
  mls.in.cl <- coeftest(mls.in, vcov = vcovCL, cluster = ~School.District)

  xxx <- broom::tidy(mls.in.cl)
  id <- which(xxx$term == 'Treated:Post.MLS.Announced')
  
  mls.a.increments.co <- c(mls.a.increments.co, xxx$estimate[id])
  mls.a.increments.se <- c(mls.a.increments.se, xxx$std.error[id])

}

for (i in 5:20) {
  
  print(i)
  
  data.tql.a$Treated <- as.numeric(data.tql.a$TQL <= i/10)
  
  tql.in <- lm(log(Real.Price) ~ Treated*Post.TQL.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
               + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
               + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
               + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
               + factor(Owner.Residence), data = data.tql.a)
  
  tql.in.cl <- coeftest(tql.in, vcov = vcovCL, cluster = ~School.District)
  
  xxx <- broom::tidy(tql.in.cl)
  id <- which(xxx$term == 'Treated:Post.TQL.Announced')
  
  tql.a.increments.co <- c(tql.a.increments.co, xxx$estimate[id])
  tql.a.increments.se <- c(tql.a.increments.se, xxx$std.error[id])

}

for (i in 5:20) {
  
  print(i)
  
  data.tql.o$Treated <- as.numeric(data.tql.o$TQL <= i/10)
  
  tql.in <- lm(log(Real.Price) ~ Treated*Post.TQL.Opened + log(FinSqFt) + I(log(FinSqFt)^2)
               + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
               + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
               + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
               + factor(Owner.Residence), data = data.tql.o)
  
  tql.in.cl <- coeftest(tql.in, vcov = vcovCL, cluster = ~School.District)

  xxx <- broom::tidy(tql.in.cl)
  id <- which(xxx$term == 'Treated:Post.TQL.Opened')
  
  tql.o.increments.co <- c(tql.o.increments.co, xxx$estimate[id])
  tql.o.increments.se <- c(tql.o.increments.se, xxx$std.error[id])

}

# Plotting the results with both sets of standard errors

Distance <- c(5:20) * 1000 / 10
Breaks <- c(1:10)/ 2 * 1000
Coefficient <- usl.a.increments.co
SE.low <- Coefficient - 1.96*usl.a.increments.se
SE.high <- Coefficient + 1.96*usl.a.increments.se
res.df <- as.data.frame(cbind(Distance, Coefficient, SE.low, SE.high))

png(paste(direc, 'figures/usl_plot.png', sep = ''))

ggplot(data = res.df, aes(x = Distance, y = Coefficient)) + 
  geom_line(size = 1, color = 'red4') + 
  geom_ribbon(aes(ymin = SE.low, ymax = SE.high), size = 1, alpha = 0.5, color = 'orange') +
  xlab('Distance in Meters') + 
  ylab('Hedonic Coefficient') + 
  labs(title = 'Estimated Effect of USL Announcement w.r.t. Distance') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = Breaks) + 
  geom_hline(yintercept = 0, size = 2)

dev.off()

Coefficient <- mls.a.increments.co
SE.low <- Coefficient - 1.96*mls.a.increments.se
SE.high <- Coefficient + 1.96*mls.a.increments.se
res.df <- as.data.frame(cbind(Distance, Coefficient, SE.low, SE.high))

png(paste(direc, 'figures/mls_plot.png', sep = ''))

ggplot(data = res.df, aes(x = Distance, y = Coefficient)) + 
  geom_line(size = 1, color = 'red4') + 
  geom_ribbon(aes(ymin = SE.low, ymax = SE.high), size = 1, alpha = 0.5, color = 'orange') +
  xlab('Distance in Meters') + 
  ylab('Hedonic Coefficient') + 
  labs(title = 'Estimated Effect of MLS Announcement w.r.t. Distance') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = Breaks) + 
  geom_hline(yintercept = 0, size = 2)

dev.off()

Coefficient <- tql.a.increments.co
SE.low <- Coefficient - 1.96*tql.a.increments.se
SE.high <- Coefficient + 1.96*tql.a.increments.se
res.df <- as.data.frame(cbind(Distance, Coefficient, SE.low, SE.high))

png(paste(direc, 'figures/tqla_plot.png', sep = ''))

ggplot(data = res.df, aes(x = Distance, y = Coefficient)) + 
  geom_line(size = 1, color = 'red4') + 
  geom_ribbon(aes(ymin = SE.low, ymax = SE.high), size = 1, alpha = 0.5, color = 'orange') +
  xlab('Distance in Meters') + 
  ylab('Hedonic Coefficient') + 
  labs(title = 'Estimated Effect of TQL Announcement w.r.t. Distance') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = Breaks) + 
  geom_hline(yintercept = 0, size = 2)

dev.off()

Coefficient <- tql.o.increments.co
SE.low <- Coefficient - 1.96*tql.o.increments.se
SE.high <- Coefficient + 1.96*tql.o.increments.se
res.df <- as.data.frame(cbind(Distance, Coefficient, SE.low, SE.high))

png(paste(direc, 'figures/tqlo_plot.png', sep = ''))

ggplot(data = res.df, aes(x = Distance, y = Coefficient)) + 
  geom_line(size = 1, color = 'red4') + 
  geom_ribbon(aes(ymin = SE.low, ymax = SE.high), size = 1, alpha = 0.5, color = 'orange') +
  xlab('Distance in Meters') + 
  ylab('Hedonic Coefficient') + 
  labs(title = 'Estimated Effect of TQL Opening w.r.t. Distance') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = Breaks) + 
  geom_hline(yintercept = 0, size = 2)

dev.off()

# Compile info on relevant hedonic coefficients

reg.stars <- function (t) {
  
  if (abs(t) > 2.576) {
    
    ret <- '***'
    
  } else if (abs(t) > 1.96) {
    
    ret <- '**'
    
  } else if (abs(t) > 1.645) {
    
    ret <- '*'
    
  } else {
    
    ret <- ''
    
  }
  
  return(ret)
  
}

reg.coefs <- round(cbind(usl.a.increments.co, mls.a.increments.co, tql.a.increments.co, tql.o.increments.co),3)
reg.serrs <- round(cbind(usl.a.increments.se, mls.a.increments.se, tql.a.increments.se, tql.o.increments.se),3)

reg.t.stats <- round(reg.coefs / reg.serrs, 3)

all.signs <- c()

for (i in 1:length(reg.t.stats)) {
  
  all.signs <- c(all.signs, reg.stars(reg.t.stats[i]))
  
}

reg.signs <- cbind(all.signs[1:16], all.signs[17:32], all.signs[33:48], all.signs[49:64])

# Saving results as a cute little data.frame

reg.coefs <- as.data.frame(reg.coefs)
reg.serrs <- as.data.frame(reg.serrs)
reg.signs <- as.data.frame(reg.signs)

usl.vec <- c()
mls.vec <- c()
tqla.vec <- c()
tqlo.vec <- c()

for (i in 1:16) {
  
  ifelse (reg.signs$V1[i] == '', usl.vec <- c(usl.vec, reg.coefs$usl.a.increments.co[i]), usl.vec <- c(usl.vec, paste(reg.coefs$usl.a.increments.co[i], '$^{', reg.signs$V1[i], '}$', sep = '')))
  ifelse (reg.signs$V2[i] == '', mls.vec <- c(mls.vec, reg.coefs$mls.a.increments.co[i]), mls.vec <- c(mls.vec, paste(reg.coefs$mls.a.increments.co[i], '$^{', reg.signs$V2[i], '}$', sep = '')))
  ifelse (reg.signs$V3[i] == '', tqla.vec <- c(tqla.vec, reg.coefs$tql.a.increments.co[i]), tqla.vec <- c(tqla.vec, paste(reg.coefs$tql.a.increments.co[i], '$^{', reg.signs$V3[i], '}$', sep = '')))
  ifelse (reg.signs$V4[i] == '', tqlo.vec <- c(tqlo.vec, reg.coefs$tql.o.increments.co[i]), tqlo.vec <- c(tqlo.vec, paste(reg.coefs$tql.o.increments.co[i], '$^{', reg.signs$V4[i], '}$', sep = '')))
  
  usl.vec <- c(usl.vec, paste('(', reg.serrs$usl.a.increments.se[i], ')', sep = ''))
  mls.vec <- c(mls.vec, paste('(', reg.serrs$mls.a.increments.se[i], ')', sep = ''))
  tqla.vec <- c(tqla.vec, paste('(', reg.serrs$tql.a.increments.se[i], ')', sep = ''))
  tqlo.vec <- c(tqlo.vec, paste('(', reg.serrs$tql.o.increments.se[i], ')', sep = ''))
  
}

results.df <- as.data.frame(cbind(usl.vec, mls.vec, tqla.vec))
results.df.4 <- as.data.frame(cbind(usl.vec, mls.vec, tqla.vec, tqlo.vec))

# Creating tex tables

textable <- c()
textable4 <- c()

d <- 400

for (i in 1:32) {
  
  x <- ' & '
  
  if (i%%2 == 1) {
    
    d <- d + 100
    string <- paste(d, 'm', x, results.df$usl.vec[i], x, results.df$mls.vec[i], x, results.df$tqla.vec[i], '\\\\', sep = '')
    string4 <- paste(d, 'm', x, results.df.4$usl.vec[i], x, results.df.4$mls.vec[i], x, results.df.4$tqla.vec[i], x, results.df.4$tqlo.vec[i], '\\\\', sep = '')
    
  } else {
    
    string <- paste(x, results.df$usl.vec[i], x, results.df$mls.vec[i], x, results.df$tqla.vec[i], '\\\\', sep = '')
    string4 <- paste(x, results.df.4$usl.vec[i], x, results.df.4$mls.vec[i], x, results.df.4$tqla.vec[i], x, results.df.4$tqlo.vec[i], '\\\\', sep = '')
    
  }
  
  textable <- c(textable, string)
  textable4 <- c(textable4, string4)
  
}

write.csv(textable, paste(direc, 'results/main.txt', sep = ''), row.names = FALSE)
write.csv(textable4, paste(direc, 'results/all.txt', sep = ''), row.names = FALSE)

# Create a repeat sales data set based on the sales that remain

# Determine which parcels were sold more than once

u.counts <- c()

for (parcel in unique(data$Parcel.Number)) {
  
  u.counts <- c(u.counts, dim(data[which(data$Parcel.Number == parcel),])[1])
  
}

counts <- c()

for (i in 1:dim(data)[1]) {
  
  parcel <- which(unique(data$Parcel.Number) == data$Parcel.Number[i])
  counts <- c(counts, u.counts[parcel])
  
}

keep.again <- which(counts > 1)
repeat.data <- data[keep.again,]

# Determining which parcels were sold in pre and post event periods for each event

repeat.parcels <- unique(repeat.data$Parcel.Number)

final.parcels.usl <- c()
final.parcels.mls <- c()
final.parcels.tql.a <- c()
final.parcels.tql.o <- c()

for (parcel in repeat.parcels) {
  
  tmp <- repeat.data[which(repeat.data$Parcel.Number == parcel),]
  
  if (dim(tmp)[1] == 2 & tmp$Nippert[1] < 3 & sum(tmp$Pre.USL.Announced) == 1 & sum(tmp$Post.USL.Announced) == 1) {
    
    final.parcels.usl <- c(final.parcels.usl, parcel)
    
  } else if (dim(tmp)[1] == 2 & tmp$Nippert[1] < 3 & sum(tmp$Pre.MLS.Announced) == 1 & sum(tmp$Post.MLS.Announced) == 1) {
    
    final.parcels.mls <- c(final.parcels.mls, parcel)
    
  } else if (dim(tmp)[1] == 2 & tmp$TQL[1] < 3 & sum(tmp$Pre.TQL.Announced) == 1 & sum(tmp$Post.TQL.Announced) == 1) {
    
    final.parcels.tql.a <- c(final.parcels.tql.a, parcel)
    
  } else if (dim(tmp)[1] == 2 & tmp$TQL[1] < 3 & sum(tmp$Pre.TQL.Opened) == 1 & sum(tmp$Post.TQL.Opened) == 1) {
    
    final.parcels.tql.o <- c(final.parcels.tql.o, parcel)
    
  }
  
}

repeat.data.usl <- repeat.data[which(repeat.data$Parcel.Number %in% final.parcels.usl),]
repeat.data.mls <- repeat.data[which(repeat.data$Parcel.Number %in% final.parcels.mls),]
repeat.data.tql.a <- repeat.data[which(repeat.data$Parcel.Number %in% final.parcels.tql.a),]
repeat.data.tql.o <- repeat.data[which(repeat.data$Parcel.Number %in% final.parcels.tql.o),]

# Create the Joshi et al. figure for Cincinnati

nippert.data <- data[which(data$Nippert < 3),]
tql.data <- data[which(data$TQL < 3),]

bins <- 1:50 * 100

usl.pre <- c()
usl.post <- c()
mls.pre <- c()
mls.post <- c()
tql.a.pre <- c()
tql.a.post <- c()
tql.o.pre <- c()
tql.o.post <- c()

usl.pre.sd.high <- c()
usl.post.sd.high <- c()
mls.pre.sd.high <- c()
mls.post.sd.high <- c()
tql.a.pre.sd.high <- c()
tql.a.post.sd.high <- c()
tql.o.pre.sd.high <- c()
tql.o.post.sd.high <- c()

usl.pre.sd.low <- c()
usl.post.sd.low <- c()
mls.pre.sd.low <- c()
mls.post.sd.low <- c()
tql.a.pre.sd.low <- c()
tql.a.post.sd.low <- c()
tql.o.pre.sd.low <- c()
tql.o.post.sd.low <- c()

for (i in 1:50) {
  
  print(i)
  
  d0 <- .1*(i-1)
  d1 <- .1*i
  
  #tmp.usl <- nippert.data[which(nippert.data$Nippert > d0 & nippert.data$Nippert <= d1),]
  #tmp.mls <- nippert.data[which(nippert.data$Nippert > d0 & nippert.data$Nippert <= d1),]
  tmp.usl <- nippert.data[which(nippert.data$Nippert <= d1),]
  tmp.mls <- nippert.data[which(nippert.data$Nippert <= d1),]
  tmp.tql <- tql.data[which(tql.data$TQL > d0 & tql.data$TQL <= d1),]

  usl.pre <- c(usl.pre, mean(tmp.usl[which(tmp.usl$Pre.USL.Announced == 1),]$Real.Price))
  usl.post <- c(usl.post, mean(tmp.usl[which(tmp.usl$Post.USL.Announced == 1),]$Real.Price))
  
  usl.pre.sd.high <- c(usl.pre.sd.high, mean(tmp.usl[which(tmp.usl$Pre.USL.Announced == 1),]$Real.Price) + 1.96*sd(tmp.usl[which(tmp.usl$Pre.USL.Announced == 1),]$Real.Price))
  usl.post.sd.high <- c(usl.post.sd.high, mean(tmp.usl[which(tmp.usl$Post.USL.Announced == 1),]$Real.Price) + 1.96*sd(tmp.usl[which(tmp.usl$Post.USL.Announced == 1),]$Real.Price))
  
  usl.pre.sd.low <- c(usl.pre.sd.low, mean(tmp.usl[which(tmp.usl$Pre.USL.Announced == 1),]$Real.Price) - 1.96*sd(tmp.usl[which(tmp.usl$Pre.USL.Announced == 1),]$Real.Price))
  usl.post.sd.low <- c(usl.post.sd.low, mean(tmp.usl[which(tmp.usl$Post.USL.Announced == 1),]$Real.Price) - 1.96*sd(tmp.usl[which(tmp.usl$Post.USL.Announced == 1),]$Real.Price))
  
  mls.pre <- c(mls.pre, mean(tmp.mls[which(tmp.mls$Pre.MLS.Announced == 1),]$Real.Price))
  mls.post <- c(mls.post, mean(tmp.mls[which(tmp.mls$Post.MLS.Announced == 1),]$Real.Price))
  
  mls.pre.sd.high <- c(mls.pre.sd.high, mean(tmp.mls[which(tmp.mls$Pre.MLS.Announced == 1),]$Real.Price) + 1.96*sd(tmp.mls[which(tmp.mls$Pre.MLS.Announced == 1),]$Real.Price))
  mls.post.sd.high <- c(mls.post.sd.high, mean(tmp.mls[which(tmp.mls$Post.MLS.Announced == 1),]$Real.Price) + 1.96*sd(tmp.mls[which(tmp.mls$Post.MLS.Announced == 1),]$Real.Price))
  
  mls.pre.sd.low <- c(mls.pre.sd.low, mean(tmp.mls[which(tmp.mls$Pre.MLS.Announced == 1),]$Real.Price) - 1.96*sd(tmp.mls[which(tmp.mls$Pre.MLS.Announced == 1),]$Real.Price))
  mls.post.sd.low <- c(mls.post.sd.low, mean(tmp.mls[which(tmp.mls$Post.MLS.Announced == 1),]$Real.Price) - 1.96*sd(tmp.mls[which(tmp.mls$Post.MLS.Announced == 1),]$Real.Price))
  
  tql.a.pre <- c(tql.a.pre, mean(tmp.tql[which(tmp.tql$Pre.TQL.Announced == 1),]$Real.Price))
  tql.a.post <- c(tql.a.post, mean(tmp.tql[which(tmp.tql$Post.TQL.Announced == 1),]$Real.Price))
  
  tql.a.pre.sd.high <- c(tql.a.pre.sd.high, mean(tmp.tql[which(tmp.tql$Pre.TQL.Announced == 1),]$Real.Price) + 1.96*sd(tmp.tql[which(tmp.tql$Pre.TQL.Announced == 1),]$Real.Price))
  tql.a.post.sd.high <- c(tql.a.post.sd.high, mean(tmp.tql[which(tmp.tql$Post.TQL.Announced == 1),]$Real.Price) + 1.96*sd(tmp.tql[which(tmp.tql$Post.TQL.Announced == 1),]$Real.Price))
  
  tql.a.pre.sd.low <- c(tql.a.pre.sd.low, mean(tmp.tql[which(tmp.tql$Pre.TQL.Announced == 1),]$Real.Price) - 1.96*sd(tmp.tql[which(tmp.tql$Pre.TQL.Announced == 1),]$Real.Price))
  tql.a.post.sd.low <- c(tql.a.post.sd.low, mean(tmp.tql[which(tmp.tql$Post.TQL.Announced == 1),]$Real.Price) - 1.96*sd(tmp.tql[which(tmp.tql$Post.TQL.Announced == 1),]$Real.Price))
  
  tql.o.pre <- c(tql.o.pre, mean(tmp.tql[which(tmp.tql$Pre.TQL.Opened == 1),]$Real.Price))
  tql.o.post <- c(tql.o.post, mean(tmp.tql[which(tmp.tql$Post.TQL.Opened == 1),]$Real.Price))
  
  tql.o.pre.sd.high <- c(tql.o.pre.sd.high, mean(tmp.tql[which(tmp.tql$Pre.TQL.Opened == 1),]$Real.Price) + 1.96*sd(tmp.tql[which(tmp.tql$Pre.TQL.Opened == 1),]$Real.Price))
  tql.o.post.sd.high <- c(tql.o.post.sd.high, mean(tmp.tql[which(tmp.tql$Post.TQL.Opened == 1),]$Real.Price) + 1.96*sd(tmp.tql[which(tmp.tql$Post.TQL.Opened == 1),]$Real.Price))
  
  tql.o.pre.sd.low <- c(tql.o.pre.sd.low, mean(tmp.tql[which(tmp.tql$Pre.TQL.Opened == 1),]$Real.Price) - 1.96*sd(tmp.tql[which(tmp.tql$Pre.TQL.Opened == 1),]$Real.Price))
  tql.o.post.sd.low <- c(tql.o.post.sd.low, mean(tmp.tql[which(tmp.tql$Post.TQL.Opened == 1),]$Real.Price) - 1.96*sd(tmp.tql[which(tmp.tql$Post.TQL.Opened == 1),]$Real.Price))
  
}

# USL Figure

usl.fig.df <- as.data.frame(cbind(usl.pre, usl.post, usl.pre.sd.high, usl.pre.sd.low, usl.post.sd.high, usl.post.sd.low, bins))

png(paste(direc, 'figures/usl_repfig.png', sep = ''))

ggplot(data = usl.fig.df[which(complete.cases(usl.fig.df) == TRUE),], aes(x = bins, y = usl.pre)) + 
  geom_ribbon(aes(ymin = usl.post.sd.low / 1000, ymax = usl.post.sd.high / 1000), size = 1, alpha = 0.5, color = 'red4') + 
  geom_ribbon(aes(ymin = usl.pre.sd.low / 1000, ymax = usl.pre.sd.high / 1000), size = 1, alpha = 0.5, color = 'orange') + 
  xlab('Distance in Meters') + 
  ylab('Sale Price (1,000 USD)') + 
  labs(title = 'Price Gradient - USL Announcement') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = Breaks)

dev.off()

# MLS Figure

mls.fig.df <- as.data.frame(cbind(mls.pre, mls.post, mls.pre.sd.high, mls.pre.sd.low, mls.post.sd.high, mls.post.sd.low, bins))

png(paste(direc, 'figures/mls_repfig.png', sep = ''))

ggplot(data = mls.fig.df[which(complete.cases(mls.fig.df) == TRUE),], aes(x = bins, y = mls.pre)) + 
  geom_ribbon(aes(ymin = mls.post.sd.low / 1000, ymax = mls.post.sd.high / 1000), size = 1, alpha = 0.5, color = 'red4') + 
  geom_ribbon(aes(ymin = mls.pre.sd.low / 1000, ymax = mls.pre.sd.high / 1000), size = 1, alpha = 0.5, color = 'orange') + 
  xlab('Distance in Meters') + 
  ylab('Sale Price (1,000 USD)') + 
  labs(title = 'Price Gradient - mls Announcement') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = Breaks)

dev.off()

# TQL Figure

tql.a.fig.df <- as.data.frame(cbind(tql.a.pre, tql.a.post, tql.a.pre.sd.high, tql.a.pre.sd.low, tql.a.post.sd.high, tql.a.post.sd.low, bins))

png(paste(direc, 'figures/tqla_repfig.png', sep = ''))

ggplot(data = tql.a.fig.df[which(complete.cases(tql.a.fig.df) == TRUE),], aes(x = bins, y = tql.a.pre)) + 
  geom_ribbon(aes(ymin = tql.a.post.sd.low / 1000, ymax = tql.a.post.sd.high / 1000), size = 1, alpha = 0.5, color = 'red4') + 
  geom_ribbon(aes(ymin = tql.a.pre.sd.low / 1000, ymax = tql.a.pre.sd.high / 1000), size = 1, alpha = 0.5, color = 'orange') + 
  xlab('Distance in Meters') + 
  ylab('Sale Price (1,000 USD)') + 
  labs(title = 'Price Gradient - tql.a Announcement') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = Breaks)

dev.off()

