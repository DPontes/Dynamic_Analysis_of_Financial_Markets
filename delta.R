# --- Teste do método de análise nao-linear dos indices -------
# --- Utilizacao de um intervalo de teste (delta) e testado ---
# para de x a y dos ultimos valores a serem comparados --------

E<-0.5

matrix<-rep(0,6*500)
dim(matrix)<-c(500,6)

for (ciclo in 1:500)
{
	return<-rep(0,(length(return.total)-501+ciclo))
	for (t in 1:(length(return.total)-501+ciclo))
	{
		return[t]<-return.total[t]
	}
	Lr<-length(return)
	
	for(int in 5:10)
	{
		v.temp<-0
		vt<-rep(0,int)							#vector com os valores a serem comparados
		v.valor.obt<-0
		for (t in 1:int)
		{
			vt[t]<-return[Lr+t-int]
		}
		
		for(i in 1:(Lr-2*int+1))
		{
			soma<-0
			
			for(t in 1:int)
			{
				soma<-soma+(return[i+t-1]-vt[t])^2
			}
			delta<-sqrt(soma)
					
			if(delta<=E)
			{
				v.temp<-c(v.temp,return.total[i+int])
			}
		}
		if(length(v.temp)>1)
		{	
			matrix[ciclo,(int-4)]<-sum(v.temp[2]:v.temp[length(v.temp)])/(length(v.temp)-1)
		}
	}
	print(round(ciclo*100/500,digits=1))
}


for(m in 1:6)
{
	print(round(sum(abs(matrix[,m]-return.real))/500,digits=4))
}