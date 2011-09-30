ord.max<-5
dados.real<-ret.log.real
dados.total<-ret.log.total

print(round(sum(abs(dados.real))/500,digits=4))
v.ar<-rep(0,ord.max)

for(ciclo in 1:500)
{
	for(i in 1:ord.max)
	{
		x<-ar(dados.total[1:(length(dados.total)-500+ciclo-1)],aic=FALSE,order.max=i)
		pred<-predict(x,n.ahead=1)$pred
		v.ar[i]<-v.ar[i]+(abs(dados.real[ciclo]-pred))
	}
}
ema<-round(v.ar/500,digits=4)
print(ema)