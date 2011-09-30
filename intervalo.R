# --- Teste do método de análise nao-linear dos indices -------
# --- Utilizacao de um intervalo de teste (delta) e testado ---
# para de x a y dos ultimos valores a serem comparados --------

matrix<-0

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
		vt<-rep(0,int)							#vector com os valores a serem comparados
		v.valor.obt<-0
		for (t in 1:int)
		{
			vt[t]<-return[Lr+t-int]
		}
		
		for(i in 1:(Lr-2*int+1))
		{
			contador<-0
			soma<-0
			for(t in 1:int)
			{
				soma<-soma+(return[i+t-1]-vt[t])^2
			}
			delta<-sqrt(soma)
			
			for(r in 1:int)
			{
				if(return[i+r-1]>=(vt[r]-(delta/2)) && return[i+r-1]<=(vt[r]+(delta/2)))
				{
					contador<-contador+1
				}
			}
			if(contador==int)
			{
				v.valor.obt<-c(v.valor.obt,return[i+int])
			}
		}
		
		ifelse(length(v.valor.obt)>1,matrix<-c(matrix,round(sum(v.valor.obt)/(length(v.valor.obt)-1),digits=2)),matrix<-c(matrix,0))

	}
}

matrix2<-rep(0,(length(matrix))-1)
for(j in 1:(length(matrix)-1))
{
	matrix2[j]<-matrix[j+1]
}

dim(matrix2)<-c(6,500)
for(m in 1:6)
{
	print(round(sum(abs(matrix2[m,]-return.real))/500,digits=4))
}