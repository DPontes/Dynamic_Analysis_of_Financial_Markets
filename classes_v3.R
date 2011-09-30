vtclass<-c(10,15,20,25,30,35,40,45,50)
diag<-500
min.seq<-3
max.seq<-5

delta<-rep(0,length(vtclass))

dados.total<-dif.total
dados.real<-dif.real

mclass<-rep(0,length(dados.total)*length(vtclass))
dim(mclass)<-c(length(dados.total),length(vtclass))

m.prev<-rep(0,length(vtclass)*(max.seq-min.seq+1))
dim(m.prev)<-c((max.seq-min.seq+1),length(vtclass))

m.inf<-rep(0,length(vtclass)*(max.seq-min.seq+1))
dim(m.inf)<-c((max.seq-min.seq+1),length(vtclass))

for(d in 1:length(vtclass))
{
	delta[d]<-round(abs(max(dados.total)-min(dados.total))/vtclass[d],digits=2)
}

for(ciclo in 1:length(dados.total))
{
	for(class in 1:length(vtclass))
	{
		mclass[ciclo,class]<-round((dados.total[ciclo]-min(dados.total))/delta[class],digits=0)
	}
}

for(ciclo in 1:diag)
{
	for(class in 1:length(vtclass))
	{
		for(seq in min.seq:max.seq)
		{
			v.oco<-0
			vteste<-rep(0,seq)
			for(vt in 1:seq)
			{
				vteste[vt]<-mclass[length(mclass[,class])-diag+ciclo-vt,class]
			}
			for(teste in 1:(diag-seq+1))
			{
				vseq<-rep(0,seq)
				for(vs in 1:seq)
				{
					vseq[vs]<-mclass[length(mclass[,class])-diag+ciclo-vs-teste,class]
				}
				count<-0
				for(equiv in 1:seq)
				{
					if(vteste[equiv]==vseq[equiv])
					{
						count<-count+1
					}
				}
				if(count==seq)
				{
					v.oco<-c(v.oco,mclass[length(mclass[,class])-diag+ciclo-teste,class])
				}
			}
			if(length(v.oco)==1)
			{
				m.prev[seq-min.seq+1,class]<-m.prev[seq-min.seq+1,class]+abs(dados.real[ciclo])
			}
			if(length(v.oco)>1)
			{
				m.oco<-rep(0,2*vtclass[class])
				dim(m.oco)<-c(2,vtclass[class])
				m.oco[1,]<-c(1:vtclass[class])
				v.oco<-v.oco[2:length(v.oco)]
				for(l in 1:length(v.oco))
				{
					m.oco[2,v.oco[l]]<-m.oco[2,v.oco[l]]+1
				}
				prev<-abs(sum((m.oco[1,]*delta[class]+min(dados.total))*(m.oco[2,]/sum(m.oco[2,])))-dados.real[ciclo])
				m.prev[seq-min.seq+1,class]<-m.prev[seq-min.seq+1,class]+prev
				m.inf[seq-min.seq+1,class]<-m.inf[seq-min.seq+1,class]+1
			}
			
		}
	}
	print(round(ciclo*100/500,digits=1))
}

print(m.prev<-round(m.prev/diag,digits=4))
print(m.inf<-round((diag-m.inf)*100/diag,digits=1))
