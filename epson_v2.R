epson<-c(0,1,2,3,4,5,6,7,8,9,10)
alpha<-0
min.seq<-3
max.seq<-7
diag<-500
d.ahead<-1
count.per<-0
dados<-dif.total
dados.real<-dif.real
matrix.loss<-rep(0,(max.seq-min.seq+1)*length(epson))
dim(matrix.loss)<-c((max.seq-min.seq+1),length(epson))

matrix.epson<-rep(0,(max.seq-min.seq+1)*length(epson))
dim(matrix.epson)<-c((max.seq-min.seq+1),length(epson))

for(e in 1:length(epson))
{
	matrix<-rep(0,(max.seq-min.seq+1)*diag)
	dim(matrix)<-c(diag,(max.seq-min.seq+1))
	
	for(ciclo in 1:diag)
	{
		vect.corr<-0
		v.ad<-rep(0,(max.seq-min.seq+1))
		v.co<-rep(0,(max.seq-min.seq+1))
	
		for(proc in max.seq:(length(dados)-diag+ciclo-min.seq))
		{
			if(abs(dados[proc+min.seq-1]-(dados[length(dados)-diag-1+ciclo]))<=(epson[e]))
			{
				vect.corr<-c(vect.corr,proc+1)
			}
		}
		
		if(length(vect.corr>1))
		{
			vect.corr<-vect.corr[2:length(vect.corr)]
		}
		
	
		for(i in 1:length(vect.corr))
		{
			for(seq in min.seq:max.seq)
			{
				count<-0
				for(v in 1:(seq-1))
				{
					if(abs(dados[vect.corr[i]-v]-dados[length(dados)-diag+1+ciclo-v])<=(epson[e]) && length(vect.corr)>1)
					{
						count<-count+1
					}
				}
				if(count==seq-1)
				{
					v.ad[seq-(min.seq-1)]<-v.ad[seq-(min.seq-1)]+dados[vect.corr[i]+1]
					v.co[seq-(min.seq-1)]<-v.co[seq-(min.seq-1)]+1
				}
			}
		}
		for(r in 1:(max.seq-min.seq+1))
		{
			if(v.co[r]>0)
			{
				matrix[ciclo,r]<-round(sum(v.ad[r])/v.co[r],digits=4)
			}
		}
	count.per<-count.per+1
#	print(count.per*100/(diag*length(epson)))
	}
	
	for(f in 1:(max.seq-min.seq+1))
	{
		count.loss<-0
		for(c.loss in 1:diag)
		{
			if(matrix[c.loss,f]==0)
			{
				count.loss<-count.loss+1
			}
		}
		matrix.loss[f,e]<-round(count.loss*100/diag,digits=1)
	}

	for(d in 1:(max.seq-min.seq+1))
	{
		matrix.epson[d,e]<-round(sum(abs(matrix[,d]-dados.real[1:diag]))/diag,digits=4)
	}
}
print(matrix.epson)

print(matrix.loss)