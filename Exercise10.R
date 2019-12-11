#Exercise 10 

#Ns[t+1] <- Ns[t] + rn*Ns[t]*(1-(Ns[t]+Ms[t])/K)
#Ms[t+1] <- Ms[t] + rm*Ms[t]*(1-(Ns[t]+Ms[t])/K)

#Set parameters
rn<-0.1
rm<-0.1
K<-1000000
rnDrug<- -0.1
rmDrug<-0.05
N0<-99
M0<-1
timesteps=600

#At time 100, M=1 and N=100... need to reevaluate what is happening. 
#Simulate growth of the two sub-populations in the tumor to equilibrium followed by drug treatment. 
#Plot in a line graph. 

Ns <- numeric(length=timesteps)
Ns[1]=N0
Ms <- numeric(length=timesteps)
Ms[1]=M0
for(t in 1:(timesteps-1)){
  if(t<(timesteps/2)){
    Ns[t+1] <- Ns[t] + rn*Ns[t]*(1-(Ns[t]+Ms[t])/K)
    Ms[t+1] <- Ms[t] + rm*Ms[t]*(1-(Ns[t]+Ms[t])/K) 
  }else{
    Ns[t+1] <- Ns[t] + rnDrug*Ns[t]*(1-(Ns[t]+Ms[t])/K)
    Ms[t+1] <- Ms[t] + rmDrug*Ms[t]*(1-(Ns[t]+Ms[t])/K)
  }
}
plot(Ns)
plot(Ms)

library(ggplot2)
cells<-data.frame(time=1:600, Ns=Ns, Ms=Ms)
cellplot<-ggplot(data=cells,aes(x=time,y=Ns))+ 
  geom_line(aes(x=time,y=Ns,col="blue"))+
  geom_line(aes(x=time,y=Ms,col="red"))+
  theme_classic()+
  xlab("Time")+ylab("Population")

#fix legend so that it's titled Cell Type and has Ns and Ms populations
cellplot+scale_color_discrete(name="Cell Type", 
  breaks=c("blue","red"), 
  labels=c("Ns","Ms"))



#alternative:

plot(cells$time,cells$Ns,type="l",col="blue",xlab="Time",ylab="Population")
lines(cells$time,cells$Ms,col="red")


  




