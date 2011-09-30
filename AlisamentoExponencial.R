days<-1
matrix<-rep(0,500*days)
dim(matrix)<-c(500,days)

for (ciclo in 1:500)
{
	return<-rep(0,(length(return.total)-501+ciclo))
	for (t in 1:(length(return.total)-501+ciclo))
	{
		return[t]<-return.total[t]
	}
	
	
	x<-HoltWinters(return, gamma = 0, beta = 0)
	pred<-predict(x,n.ahead=days)
	for(d in 1:days)
	{
		matrix[ciclo,d]<-pred[d]
	}
	
}

erro.exp<-rep(0,days)
for(i in 1:days)
{
	erro.exp[i]<-round(sum(abs(matrix[,i]-return.real))/500,digits=4)
}
print(erro.exp)