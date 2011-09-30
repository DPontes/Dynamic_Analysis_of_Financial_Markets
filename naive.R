v.teste<-rep(0,500)

for(i in 1:500)
{
	v.teste[i]<-round(abs(dif.total[length(dif.total)-500+i]-0),digits=4)
}

print(erro.total<-round(sum(v.teste)/500,digits=4))