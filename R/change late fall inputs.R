
# Month_return_proportions for Battle and Clear creeks Nov, Dec, Jan, Feb: 10,40,40,10
# For upper Sacramento Oct- Feb distribution, 10,20, 40, 20, 10
params$month_return_proportions<-matrix(c(0.1,0.2,0.4,0.2,0.1,
                                          0,0.1,0.4,0.4,0.1), byrow=T,nrow=2)
colnames(params$month_return_proportions)<-c("Oct","Nov", "Dec", "Jan", "Feb")


### change proportion hatchery based on CWT ests
params$proportion_hatchery[params$proportion_hatchery>0]<-0
params$proportion_hatchery[1]<-0.075
params$proportion_hatchery[3]<-0.027

# change removal rate using CWT ests
params$natural_adult_removal_rate[params$natural_adult_removal_rate>0]<-0
params$natural_adult_removal_rate[1]<-0.014

# change harvest rate using CWT ests
params$adult_harvest_rate[params$adult_harvest_rate>0]<-0
params$adult_harvest_rate[c(1,3,7)]<-0.0676

# change hatchery allocation using CWT ests
params$hatchery_allocation[params$hatchery_allocation>0]<-0
params$hatchery_allocation[1]<-0.083
params$hatchery_allocation[3]<-0.015
