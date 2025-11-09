library(ggplot2)
n<-10
t<-60
s<-c(2.5,2.5,2.5,2.5,2.5,2,2,2,2,1)
p<-matrix(0,t+1,n)
for(i in 1:t){
v<-rnorm(n,s,0.3)
v<-pmax(v,0.1)
a<-numeric(n)
a[1]<-v[1]
for(j in 2:n){
x<-p[i,j]+v[j]/60
y<-p[i,j-1]
if(x>y){a[j]<-(y-p[i,j])*60}else{a[j]<-v[j]}
}
p[i+1,]<-p[i,]+a/60
}
d<-data.frame()
for(k in 1:n){
d<-rbind(d,data.frame(t=0:t,s=k,p=p[,k],h=k==n))
}
ggplot(d,aes(t,p,group=s,color=h))+geom_line()+scale_color_manual(values=c("gray","red"))
f<-d[d$t==t,]
ggplot(f,aes(s,p,fill=h))+geom_col()+scale_fill_manual(values=c("blue","red"))
