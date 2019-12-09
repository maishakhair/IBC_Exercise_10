#Dynamic modeling

#Fixed values
N0=1
M0=0
rn=0.1
rm=0.1
K=1000000
timesteps=200
IntM=0

#Time of chemotherapy
chemo=100

#Vector to store N's
Ns=numeric(length=timesteps)
Ns[1]=N0
Ms=numeric(length=timesteps)
Ms[1]=M0

for(t in 1:200){
  Ns[t+1] <- Ns[t] + rn*Ns[t]*(1-(Ns[t]+Ms[t])/K)
  Ms[t+1] <- Ms[t] + rm*Ms[t]*(1-(Ns[t]+Ms[t])/K)
  #Appearance of resistant cells when cancer cells equals 100
  if(Ns[t+1]>99 & IntM==0) {
    Ms[t+1] <- 1
    IntM = IntM + 1
  }
  #Time of chemotherapy
  if(t>chemo){
    rn=-0.1
    rm=0.05
  }
}

library(ggplot2)
simEvents<-data.frame(time=1:length(Ns),N=Ns,M=Ms)
ggplot(data=simEvents)+
  geom_line(aes(x=time,y=N),col='blue')+
  geom_line(aes(x=time,y=M),col='red')+
  theme_classic()+
  xlab("Time (Days)")+ylab("Number of Cancer Cells")