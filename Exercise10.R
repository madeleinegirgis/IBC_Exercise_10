#Exercise 10 

#Ns[t+1] <- Ns[t] + rn*Ns[t]*(1-(Ns[t]+Ms[t])/K)
#Ms[t+1] <- Ms[t] + rm*Ms[t]*(1-(Ns[t]+Ms[t])/K)

rn<-0.1
rm<-0.1
K<-1000000
rnDrug<- -0.1
rmDrug<-0.05
N0<-1
M0<-100
timesteps=300

#At time 100, M=1 and N=100... need to reevaluate what is happening. 
#Simulate growth of the two sub-populations in the tumor to equilibrium followed by drug treatment. 
#Plot in a line graph. 

Ns <- numeric(length=timesteps)
Ns[1]=N0
Ms <- numeric(length=timesteps)
Ms[1]=M0
for(t in 1:(timesteps-1)){
  if (Ns<100){
    Ns[t+1] <- Ns[t] + rn*Ns[t]*(1-(Ns[t]+Ms[t])/K)
  }else(Ns>100){
    Ns[t+1] <- Ns[t] + rnDrug*Ns[t]*(1-(Ns[t]+Ms[t])/K)
    Ms[t+1] <- Ms[t] + rmDrug*Ms[t]*(1-(Ns[t]+Ms[t])/K)
  }
}

cells<-data.frame(time=1:300)
ggplot(data=cells, aes(x=time,y=Ns))+ 
  geom_line()+
  theme_classic()

  




