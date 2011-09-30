ord.max<-5
dados.real<-dif.real
dados.total<-dif.total

print(round(sum(abs(dados.real))/500,digits=4))
v.ma<-rep(0,ord.max)
v.arma<-rep(0,ord.max)

for(ciclo in 1:500)
{
	for(i in 1:ord.max)
	{
		x<-arima(dados.total[1:(length(dados.total)-500+ciclo-1)],order = c(0,0,i))
		pred<-predict(x,n.ahead=1)$pred
		v.ma[i]<-v.ma[i]+(abs(dados.real[ciclo]-pred))
	}
}
ema<-round(v.ma/500,digits=4)
print("MA  1   2   3   4   5")
print(ema)
for(ciclo in 1:500)
{
	for(i in 1:ord.max)
	{
		x<-arima(dados.total[1:(length(dados.total)-500+ciclo-1)],order = c(i,0,i))
		pred<-predict(x,n.ahead=1)$pred
		v.arma[i]<-v.arma[i]+(abs(dados.real[ciclo]-pred))
	}
}

print("ARMA  1   2   3   4   5")
print(round(v.arma/500,digits=4))