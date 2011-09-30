vtclass<-c(10,15,20,25,30,35,40,45,50)
diag<-500
min.seq<-3
max.seq<-5

mclass<-rep(0,length(return.total)*length(vtclass))
dim(mclass)<-c(length(return.total),length(vtclass))
delta<-rep(0,length(vtclass))

m.prev<-rep(0,length(vtclass)*(max.seq-min.seq+1))
dim(m.prev)<-c((max.seq-min.seq+1),length(vtclass))

m.inf<-rep(0,length(vtclass)*(max.seq-min.seq+1))
dim(m.inf)<-c((max.seq-min.seq+1),length(vtclass))

for(d in 1:length(vtclass))
{
	delta[d]<-round(abs(max(return.total)-min(return.total))/vtclass[d],digits=2)
}

for(ciclo in 1:length(return.total))
{
	for(class in 1:length(vtclass))
	{
		mclass[ciclo,class]<-round((return.total[ciclo]-min(return.total))/delta[class],digits=0)
		
#		if(mclass[ciclo,class]==0)
#		{
#			mclass[ciclo,class]<-1
#		}
#		if(mclass[ciclo,class]==(vtclass[class]+1))
#		{
#			mclass[ciclo,class]<-(vtclass[class]-1)
#		}
	}
}

for(ciclo in 1:diag)
{
	for(class in 1:length(vtclass))
	{
		vet<-0
		for(busca in 1:(length(mclass[,1])-diag+ciclo-1))
		{
			if(mclass[busca,class]==mclass[(length(mclass[,class])-diag+ciclo),class])
			{
				vet<-c(vet,busca)
			}
		}
		vet<-vet[2:length(vet)]
		mequal<-rep(0,max.seq*length(vet))
		dim(mequal)<-c(max.seq,length(vet))
		
		for(k in 1:length(vet))
		{
			mequal[,k]<-mclass[((vet[k]+max.seq-1):vet[k]),class]
		}
		
		for(seq in min.seq:max.seq)
		{
			v.oco<-0
			for(k in 1:length(mequal[1,]))
			{
				if(sum(mclass[((length(mclass[,class])-diag+ciclo-seq+1):(length(mclass[,class])-diag+ciclo)),class]==mequal[(1:seq),k])==seq)
				{
					v.oco<-c(v.oco,mclass[vet[k]-1])
				}
			}
			if(length(v.oco)==1)
			{
				m.prev[seq-min.seq+1,class]<-m.prev[seq-min.seq+1,class]+abs(return.real[ciclo])
			}
			if(length(v.oco)>1)
			{
				m.oco<-rep(0,2*vtclass[class])
				dim(m.oco)<-c(2,vtclass[class])
				m.oco[1,]<-c(1:vtclass[class])
				v.oco<-v.oco[2:length(v.oco)]	#elimina o zero que se encontra na 1a casa
				for(l in 1:length(v.oco))
				{
					m.oco[2,v.oco[l]]<-m.oco[2,v.oco[l]]+1
				}
				prev<-abs(sum((m.oco[1,]*delta[class]+min(return.total))*(m.oco[2,]/sum(m.oco[2,])))-return.real[ciclo])
				m.prev[seq-min.seq+1,class]<-m.prev[seq-min.seq+1,class]+prev
				m.inf[seq-min.seq+1,class]<-m.inf[seq-min.seq+1,class]+1
			}
			
		}
	}
	print(round(ciclo*100/500,digits=1))
}

print(m.prev<-round(m.prev/diag,digits=4))
print(m.inf<-round((diag-m.inf)*100/diag,digits=1))