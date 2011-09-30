epson<-c(0.1,0.20,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)					
min.seq<-2
max.seq<-6

matrix<-rep(0,(max.seq-min.seq+1)*500)
dim(matrix)<-c(500,(max.seq-min.seq+1))

matrix.epson<-rep(0,(max.seq-min.seq+1)*length(epson))
dim(matrix.epson)<-c((max.seq-min.seq+1),length(epson))

for(e in 1:length(epson))
{
	
	for(ciclo in 1:500)
	{
		vect.corr<-0
		v.ad<-rep(0,(max.seq-min.seq+1))
		v.co<-rep(0,(max.seq-min.seq+1))
	
		for(proc in 1:(length(return.total)-ciclo-5+1))
		{
			if(return.total[proc+max.seq-1]>=(return.total[length(return.total)-501+ciclo]-(epson[e]/2)) && return.total[proc+max.seq-1]<(return.total[length(return.total)-501+ciclo]+(epson[e]/2)))
			{
				vect.corr<-c(vect.corr,proc+seq-1)
			}
		}
	
		for(i in 2:length(vect.corr))
		{
			for(seq in min.seq:max.seq)
			{
				count<-0
				for(v in 1:(seq-1))
				{
					if(return.total[vect.corr[i]-v]>=return.total[length(return.total)-501+ciclo-v]-(epson[e]/2*(1+seq/10)) && return.total[vect.corr[i]-v]<return.total[length(return.total)-501+ciclo-v]+(epson[e]/2*(1+seq/10)) && length(vect.corr)>1)
					{
						count<-count+1
					}
				}
				if(count==seq-1)
				{
					v.ad[seq-1]<-v.ad[seq-1]+return.total[vect.corr[i]+1]
					v.co[seq-1]<-v.co[seq-1]+1
				}
			}
		}
		for(r in 1:(max.seq-1))
		{
			if(v.co[r]>0)
			{
				matrix[ciclo,r]<-round(sum(v.ad[r])/v.co[r],digits=2)
			}
		}
		print(round(ciclo*e*100/(500*length(epson)),digits=4))
	}

	for(d in 1:5)
	{
		matrix.epson[d,e]<-round(sum(abs(matrix[,d]-return.real))/500,digits=4)
	}
}

print(matrix.epson)