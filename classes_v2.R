vtclass<-c(10,25,50,75,100)

mclass.return.total<-rep(0,(length(return.total)*length(vtclass)))
dim(mclass.return.total)<-c(length(return.total),length(vtclass))
delta<-rep(0,length(vtclass))

for(class in 1:length(vtclass))
{
	nclass<-vtclass[class]
	delta[class]<-abs(max(return.total)-min(return.total))/nclass
	for(i in 1:length(return.total))
	{	
		if(return.total[i]==max(return.total))	 #
		{										 #
			mclass.return.total[i,class]<-nclass # Devido a erro no programa relativamente aos
		}							 		 	 #valores extremos do vector return,
		if(return.total[i]==min(return.total))	 #é necessário realizar estas duas condições,
		{										 #uma para o valor máximo, outra para o valor
			mclass.return.total[i,class]<-1		 #mínimo
		}										 #
				
		for(t in 0:(nclass-1))
		{
			if(min(return.total)+delta[class]*t<=return.total[i] && min(return.total)+delta[class]*(t+1)>return.total[i])
			{
				mclass.return.total[i,class]<-t
			}
		}	
	}
}

m.prev.max<-rep(0,500*length(vtclass))
dim(m.prev.max)<-c(500,length(vtclass))

m.prev.all<-rep(0,500*length(vtclass))
dim(m.prev.all)<-c(500,length(vtclass))

for(class in 1:length(vtclass))
{
	for(ciclo in 1:500)
	{
		mtemp<-0
		vtemp<-0
		vteste<-rep(0,3)
		for(vt in 0:2)
		{
			vteste[vt+1]<-mclass.return.total[length(return.total)-501+ciclo-vt,class]
		}
		for(i in 1:(length(return.total)-506+ciclo))
		{
			count<-0
			for(tc in 0:2)
			{
				if(mclass.return.total[i+tc,class]==vteste[3-tc])
				{
					count<-count+1
				}
			}
			if(count==3)
			{
				vtemp<-c(vtemp,mclass.return.total[i+3,class])
			}
		}
		
		if(length(vtemp)>1)
		{
			mc<-0
			for(c in 1:vtclass[class])
			{
				teste<-0
				for(j in 2:length(vtemp))
				{
					if(vtemp[j]==c)
					{
						if(teste==0)
						{
							mc<-mc+1
							mtemp[(mc-1)*4+2]<-0
						}
						teste<-teste+1
						mtemp[(mc-1)*4+1]<-c
						mtemp[(mc-1)*4+2]<-mtemp[(mc-1)*4+2]+1
					}
				}
			}
			for(m in 1:mc)
			{
				mtemp[m*4-1]<-0
				mtemp[m*4]<-0
			}
			dim(mtemp)<-c(4,mc)
			for(m in 1:mc)
			{
				mtemp[3,m]<-round(mtemp[2,m]*100/sum(mtemp[2,]),digits=2)
				mtemp[4,m]<-round(min(return.total)+delta[class]*(mtemp[1,m]-0.5),digits=2)
			}
			
			soma<-0
			for(l in 1:mc)
			{
				soma<-soma+(mtemp[3,l]*mtemp[4,l]/100)
			}
			m.prev.all[ciclo,class]<-round(soma/mc,digits=2)
			
			soma<-0
			w<-0
			for(l in 1:mc)
			{
				if(mtemp[3,l]==max(mtemp[3,]))
				{
					w<-w+1
					soma<-soma+mtemp[4,l]
				}
			}
			m.prev.max[ciclo,class]<-round(soma/w,digits=2)
		}
	}
}

print(erro.prev.all.10<-round(sum(abs(m.prev.all[,1]-return.real))/500,digits=4))
print(erro.prev.all.25<-round(sum(abs(m.prev.all[,2]-return.real))/500,digits=4))
print(erro.prev.all.50<-round(sum(abs(m.prev.all[,3]-return.real))/500,digits=4))
print(erro.prev.all.75<-round(sum(abs(m.prev.all[,4]-return.real))/500,digits=4))
print(erro.prev.all.100<-round(sum(abs(m.prev.all[,5]-return.real))/500,digits=4))
print(erro.prev.all.125<-round(sum(abs(m.prev.all[,6]-return.real))/500,digits=4))
print(erro.prev.all.150<-round(sum(abs(m.prev.all[,7]-return.real))/500,digits=4))
print(erro.prev.all.200<-round(sum(abs(m.prev.all[,8]-return.real))/500,digits=4))

print(erro.prev.max.10<-round(sum(abs(m.prev.max[,1]-return.real))/500,digits=4))
print(erro.prev.max.25<-round(sum(abs(m.prev.max[,2]-return.real))/500,digits=4))
print(erro.prev.max.50<-round(sum(abs(m.prev.max[,3]-return.real))/500,digits=4))
print(erro.prev.max.75<-round(sum(abs(m.prev.max[,4]-return.real))/500,digits=4))
print(erro.prev.max.100<-round(sum(abs(m.prev.max[,5]-return.real))/500,digits=4))
print(erro.prev.max.125<-round(sum(abs(m.prev.max[,6]-return.real))/500,digits=4))
print(erro.prev.max.150<-round(sum(abs(m.prev.max[,7]-return.real))/500,digits=4))
print(erro.prev.max.200<-round(sum(abs(m.prev.max[,8]-return.real))/500,digits=4))