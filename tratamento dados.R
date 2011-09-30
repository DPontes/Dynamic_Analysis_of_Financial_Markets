#Rotina de tratamento de dados

dados<-read.csv('ftse.csv')		#vai tirar os valores do .csv para dados
N<-length(dados[,5])				#passa para N o tamanho da coluna 5
fecho<-dados[,5]					#passa para um vector fecho os valores da 5a coluna


for(i in 2:15)
{
	if(N>500*i && N<500*(i+1))
	{
		fechor<-rep(0,500*i+1)
		return.total<-rep(0,500*i)
		ret.log.total<-rep(0,500*i)
		dif.total<-rep(0,500*i)
		
		for(j in 1:(500*i+1))
		{
			fechor[j]<-fecho[N-j+1]
		}
		
		for(l in 1:(500*i))
		{
			return.total[l]<-round(((fechor[l+1]-fechor[l])/fechor[l])*100,digits=2)
		}
		
		for(m in 1:(500*i))
		{
			ret.log.total[m]<-round(log(fechor[m+1]/fechor[m])*100,digits=2)
		}
		
		for(j in 1:(500*i))
		{
			dif.total[j]<-round((fechor[j+1]-fechor[j]),digits=4)
		}
		dif.real<-c(dif.total[(500*(i-1)+1):(500*i)])
	}
}

return.real<-return.total[(length(return.total)-499):length(return.total)]
ret.log.real<-ret.log.total[(length(ret.log.total)-499):length(ret.log.total)]