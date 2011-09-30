#Rotina para criar vector de classes

vect.all.10<-rep(0,500)
vect.all.25<-rep(0,500)
vect.all.50<-rep(0,500)
vect.all.75<-rep(0,500)
vect.all.100<-rep(0,500)
vect.all.125<-rep(0,500)
vect.all.150<-rep(0,500)

vect.max.10<-rep(0,500)
vect.max.25<-rep(0,500)
vect.max.50<-rep(0,500)
vect.max.75<-rep(0,500)
vect.max.100<-rep(0,500)
vect.max.125<-rep(0,500)
vect.max.150<-rep(0,500)


for(ciclo in 1:500)
{	
	return<-rep(0,length(return.total)-501+ciclo)
	for(j in 1:(length(return.total)-501+ciclo))
	{
		return[j]<-return.total[j]
	}
	Lr<-length(return)

	vtclass<-c(10,25,50,75,100,125,150)

	for(c in 1:length(vtclass))
	{
		nclass<-vtclass[c]
		delta<-abs(max(return)-min(return))/nclass
		vclass<-c(0)

#---Ciclo que verifica a que classe pertence cada um dos valores do vector return-------
		for(i in 1:Lr)
		{	
			if(return[i]==max(return))	#
			{							#
				vclass[i]<-nclass		# Devido a erro no programa relativamente aos
			}							#valores extremos do vector return,
			if(return[i]==min(return))	#é necessário realizar estas duas condições,
			{							#uma para o valor máximo, outra para o valor
				vclass[i]<-1			#mínimo
			}							#
			
			for(t in 0:(nclass-1))
			{
				if(min(return)+delta*t<=return[i] && min(return)+delta*(t+1)>return[i])
				{
					vclass[i]<-t
				}
			}	
		}
#---------------------------------------------------------------------------------------

#---Ciclo que encontra sequências iguais no vector de classes---------------------------

		vtemp<-c(0)

		for(i in 1:(Lr-5))
		{
			n<-0
			for(t in 0:2)
			{
				if(vclass[Lr-t]==vclass[Lr-t-2-i])
				{	
					n<-n+1
				}
			}
			if(n==3)
			{	
				valor<-vclass[Lr-1-i]
				vtemp<-c(vtemp,valor)
			}
		}
	
#---------------------------------------------------------------------------------------
		if(nclass==10 && length(vtemp)>1)
		{
			vclass10.rep<-c(0)
			for(i in 1:length(vtemp))
			{
				vclass10.rep[i]<-vtemp[length(vtemp)+1-i]
			}
			length(vclass10.rep)<-length(vclass10.rep)-1
			vd<-rep(0,nclass)
			for(j in 1:length(vclass10.rep))
			{
				for(t in 1:nclass)
				{
					if(vclass10.rep[j]==t)
					{
						vd[t]<-vd[t]+1
					}
				}
			}
			
			mtemp<-0
			nd<-0
			for(l in 1:length(vd))
			{
				if(vd[l]!=0)
				{
					nd<-nd+1
					percent<-round((vd[l]/length(vclass10.rep))*100,digits=2)
					valor<-min(return)+delta*(l-0.5)
					mtemp<-c(mtemp,l,vd[l],percent,round(valor,digits=2))
				}
			}
			matrix10<-rep(0,length(mtemp)-1)
			for(l in 1:length(mtemp)-1)
			{
				matrix10[l]<-mtemp[l+1]
			}
			dim(matrix10)<-c(4,nd)
		
			soma<-0
			w<-0

			for(i in 1:length(matrix10[3,]))
			{
				if(matrix10[3,i]==max(matrix10[3,]))
				{
					w<-w+1
					soma<-soma+matrix10[4,i]
				}
			}
			valormax.prev10<-round(soma/w,digits=2)
		
			soma<-0
			w<-length(matrix10[3,])

			for(i in 1:length(matrix10[3,]))
			{
				soma<-soma+(matrix10[4,i]*(matrix10[3,i])/100)
			}
			valorall.prev10<-round(soma/w,digits=2)
		}

	
		if(nclass==25 && length(vtemp)>1)
		{
			vclass25.rep<-c(0)
			for(i in 1:length(vtemp))
			{
				vclass25.rep[i]<-vtemp[length(vtemp)+1-i]
			}
			length(vclass25.rep)<-length(vclass25.rep)-1
			vd<-rep(0,nclass)
			for(j in 1:length(vclass25.rep))
			{
				for(t in 1:nclass)
				{
					if(vclass25.rep[j]==t)
					{
						vd[t]<-vd[t]+1
					}
				}
			}			
			mtemp<-0
			nd<-0
			for(l in 1:length(vd))
			{
				if(vd[l]!=0)
				{
					nd<-nd+1
					percent<-round((vd[l]/length(vclass25.rep))*100,digits=2)
					valor<-min(return)+delta*(l-0.5)
					mtemp<-c(mtemp,l,vd[l],percent,round(valor,digits=2))
				}
			}
			matrix25<-rep(0,length(mtemp)-1)
			for(l in 1:length(mtemp)-1)
			{
				matrix25[l]<-mtemp[l+1]
			}
			dim(matrix25)<-c(4,nd)
			soma<-0
			w<-0

			for(i in 1:length(matrix25[3,]))
			{
				if(matrix25[3,i]==max(matrix25[3,]))
				{
					w<-w+1
					soma<-soma+matrix25[4,i]
				}
			}
			valormax.prev25<-round(soma/w,digits=2)
			soma<-0
			w<-length(matrix25[3,])

			for(i in 1:length(matrix25[3,]))
			{
				soma<-soma+(matrix25[4,i]*(matrix25[3,i])/100)
			}
			valorall.prev25<-round(soma/w,digits=2)
		}

		if(nclass==50 && length(vtemp)>1)
		{
			vclass50.rep<-c(0)
			for(i in 1:length(vtemp))
			{
				vclass50.rep[i]<-vtemp[length(vtemp)+1-i]
			}
			length(vclass50.rep)<-length(vclass50.rep)-1
			vd<-rep(0,nclass)
			for(j in 1:length(vclass50.rep))
			{
				for(t in 1:nclass)
				{
					if(vclass50.rep[j]==t)
					{
						vd[t]<-vd[t]+1
					}
				}
			}		
			mtemp<-0
			nd<-0
			for(l in 1:length(vd))
			{
				if(vd[l]!=0)
				{
					nd<-nd+1
					percent<-round((vd[l]/length(vclass50.rep))*100,digits=2)
					valor<-min(return)+delta*(l-0.5)
					mtemp<-c(mtemp,l,vd[l],percent,round(valor,digits=2))
				}
			}
			matrix50<-rep(0,length(mtemp)-1)
			for(l in 1:length(mtemp)-1)
			{
				matrix50[l]<-mtemp[l+1]
			}
			dim(matrix50)<-c(4,nd)
			soma<-0
			w<-0

			for(i in 1:length(matrix50[3,]))
			{
				if(matrix50[3,i]==max(matrix50[3,]))
				{
					w<-w+1
					soma<-soma+matrix50[4,i]
				}
			}
			valormax.prev50<-round(soma/w,digits=2)
		
			soma<-0
			w<-length(matrix50[3,])

			for(i in 1:length(matrix50[3,]))
			{
				soma<-soma+(matrix50[4,i]*(matrix50[3,i])/100)
			}
			valorall.prev50<-round(soma/w,digits=2)
		}
	
		if(nclass==75 && length(vtemp)>1)
		{
			vclass75.rep<-c(0)
			for(i in 1:length(vtemp))
			{
				vclass75.rep[i]<-vtemp[length(vtemp)+1-i]
			}
			length(vclass75.rep)<-length(vclass75.rep)-1
			vd<-rep(0,nclass)
			for(j in 1:length(vclass75.rep))
			{
				for(t in 1:nclass)
				{
					if(vclass75.rep[j]==t)
					{
						vd[t]<-vd[t]+1
					}
				}
			}			
			mtemp<-0
			nd<-0
			for(l in 1:length(vd))
			{
				if(vd[l]!=0)
				{
					nd<-nd+1
					percent<-round((vd[l]/length(vclass75.rep))*100,digits=2)
					valor<-min(return)+delta*(l-0.5)
					mtemp<-c(mtemp,l,vd[l],percent,round(valor,digits=2))
				}
			}
			matrix75<-rep(0,length(mtemp)-1)
			for(l in 1:length(mtemp)-1)
			{
				matrix75[l]<-mtemp[l+1]
			}
			dim(matrix75)<-c(4,nd)
			soma<-0
			w<-0

			for(i in 1:length(matrix75[3,]))
			{
				if(matrix75[3,i]==max(matrix75[3,]))
				{
					w<-w+1
					soma<-soma+matrix75[4,i]
				}
			}
			valormax.prev75<-round(soma/w,digits=2)
			soma<-0
			w<-length(matrix75[3,])

			for(i in 1:length(matrix75[3,]))
			{
				soma<-soma+(matrix75[4,i]*(matrix75[3,i])/100)
			}
			valorall.prev75<-round(soma/w,digits=2)
		}

		if(nclass==100 && length(vtemp)>1)
		{
			vclass100.rep<-c(0)
			for(i in 1:length(vtemp))
			{
				vclass100.rep[i]<-vtemp[length(vtemp)+1-i]
			}
			length(vclass100.rep)<-length(vclass100.rep)-1
			vd<-rep(0,nclass)
			for(j in 1:length(vclass100.rep))
			{
				for(t in 1:nclass)
				{
					if(vclass100.rep[j]==t)
					{
						vd[t]<-vd[t]+1
					}
				}
			}		
			mtemp<-0
			nd<-0
			for(l in 1:length(vd))
			{
				if(vd[l]!=0)
				{
					nd<-nd+1
					percent<-round((vd[l]/length(vclass100.rep))*100,digits=2)
					valor<-min(return)+delta*(l-0.5)
					mtemp<-c(mtemp,l,vd[l],percent,round(valor,digits=2))
				}
			}
			matrix100<-rep(0,length(mtemp)-1)
			for(l in 1:length(mtemp)-1)
			{
				matrix100[l]<-mtemp[l+1]
			}
			dim(matrix100)<-c(4,nd)
			soma<-0
			w<-0

			for(i in 1:length(matrix100[3,]))
			{
				if(matrix100[3,i]==max(matrix100[3,]))
				{
					w<-w+1
					soma<-soma+matrix100[4,i]
				}
			}
			valormax.prev100<-round(soma/w,digits=2)
			soma<-0
			w<-length(matrix100[3,])

			for(i in 1:length(matrix100[3,]))
			{
				soma<-soma+(matrix100[4,i]*(matrix100[3,i])/100)
			}
			valorall.prev100<-round(soma/w,digits=2)
		}

		if(nclass==125 && length(vtemp)>1)
		{
			vclass125.rep<-c(0)
			for(i in 1:length(vtemp))
			{
				vclass125.rep[i]<-vtemp[length(vtemp)+1-i]
			}
			length(vclass125.rep)<-length(vclass125.rep)-1
			vd<-rep(0,nclass)
			for(j in 1:length(vclass125.rep))
			{
				for(t in 1:nclass)
				{
					if(vclass125.rep[j]==t)
					{
						vd[t]<-vd[t]+1
					}
				}
			}			
			mtemp<-0
			nd<-0
			for(l in 1:length(vd))
			{
				if(vd[l]!=0)
				{
					nd<-nd+1
					percent<-round((vd[l]/length(vclass125.rep))*100,digits=2)
					valor<-min(return)+delta*(l-0.5)
					mtemp<-c(mtemp,l,vd[l],percent,round(valor,digits=2))
				}
			}
			matrix125<-rep(0,length(mtemp)-1)
			for(l in 1:length(mtemp)-1)
			{
				matrix125[l]<-mtemp[l+1]
			}
			dim(matrix125)<-c(4,nd)
			soma<-0
			w<-0

			for(i in 1:length(matrix125[3,]))
			{
				if(matrix125[3,i]==max(matrix125[3,]))
				{
					w<-w+1
					soma<-soma+matrix125[4,i]
				}
			}
			valormax.prev125<-round(soma/w,digits=2)
			soma<-0
			w<-length(matrix125[3,])

			for(i in 1:length(matrix125[3,]))
			{
				soma<-soma+(matrix125[4,i]*(matrix125[3,i])/100)
			}
			valorall.prev125<-round(soma/w,digits=2)
		}

		if(nclass==150 && length(vtemp)>1)
		{
			vclass150.rep<-c(0)
			for(i in 1:length(vtemp))
			{
				vclass150.rep[i]<-vtemp[length(vtemp)+1-i]
			}
			length(vclass150.rep)<-length(vclass150.rep)-1
			vd<-rep(0,nclass)
			for(j in 1:length(vclass150.rep))
			{
				for(t in 1:nclass)
				{
					if(vclass150.rep[j]==t)
					{
						vd[t]<-vd[t]+1
					}
				}
			}		
			mtemp<-0
			nd<-0
			for(l in 1:length(vd))
			{
				if(vd[l]!=0)
				{
					nd<-nd+1
					percent<-round((vd[l]/length(vclass150.rep))*100,digits=2)
					valor<-min(return)+delta*(l-0.5)
					mtemp<-c(mtemp,l,vd[l],percent,round(valor,digits=2))
				}
			}
			matrix150<-rep(0,length(mtemp)-1)
			for(l in 1:length(mtemp)-1)
			{
				matrix150[l]<-mtemp[l+1]
			}
			dim(matrix150)<-c(4,nd)
			soma<-0
			w<-0

			for(i in 1:length(matrix150[3,]))
			{
				if(matrix150[3,i]==max(matrix150[3,]))
				{
					w<-w+1
					soma<-soma+matrix150[4,i]
				}
			}
			valormax.prev150<-round(soma/w,digits=2)
			soma<-0
			w<-length(matrix150[3,])

			for(i in 1:length(matrix150[3,]))
			{
				soma<-soma+(matrix150[4,i]*(matrix150[3,i])/100)
			}
			valorall.prev150<-round(soma/w,digits=2)
		}
	}
	vect.all.10[ciclo]<-valorall.prev10
	vect.all.25[ciclo]<-valorall.prev25
	vect.all.50[ciclo]<-valorall.prev50
	vect.all.75[ciclo]<-valorall.prev75
	vect.all.100[ciclo]<-valorall.prev100
	vect.all.125[ciclo]<-valorall.prev125
	vect.all.150[ciclo]<-valorall.prev150
	
	vect.max.10[ciclo]<-valormax.prev10
	vect.max.25[ciclo]<-valormax.prev25
	vect.max.50[ciclo]<-valormax.prev50
	vect.max.75[ciclo]<-valormax.prev75
	vect.max.100[ciclo]<-valormax.prev100
	vect.max.125[ciclo]<-valormax.prev125
	vect.max.150[ciclo]<-valormax.prev150
}

vect.erro.all.10<-100*abs(return.real-vect.all.10)/return.real
vect.erro.all.25<-100*abs(return.real-vect.all.25)/return.real
vect.erro.all.50<-100*abs(return.real-vect.all.50)/return.real
vect.erro.all.100<-100*abs(return.real-vect.all.100)/return.real
vect.erro.all.125<-100*abs(return.real-vect.all.125)/return.real
vect.erro.all.150<-100*abs(return.real-vect.all.150)/return.real
vect.erro.all.75<-100*abs(return.real-vect.all.75)/return.real

vect.erro.max.10<-100*abs(return.real-vect.max.10)/return.real
vect.erro.max.25<-100*abs(return.real-vect.max.25)/return.real
vect.erro.max.50<-100*abs(return.real-vect.max.50)/return.real
vect.erro.max.75<-100*abs(return.real-vect.max.75)/return.real
vect.erro.max.100<-100*abs(return.real-vect.max.100)/return.real
vect.erro.max.125<-100*abs(return.real-vect.max.125)/return.real
vect.erro.max.150<-100*abs(return.real-vect.max.150)/return.real

erro.medio.all.10<-round(sum(vect.erro.all.10)/500,digits=2)
erro.medio.all.10<-round(sum(vect.erro.all.10)/500,digits=2)
erro.medio.all.10<-round(sum(vect.erro.all.10)/500,digits=2)
erro.medio.all.10<-round(sum(vect.erro.all.10)/500,digits=2)
erro.medio.all.10<-round(sum(vect.erro.all.10)/500,digits=2)
erro.medio.all.10<-round(sum(vect.erro.all.10)/500,digits=2)
erro.medio.all.10<-round(sum(vect.erro.all.10)/500,digits=2)

erro.medio.max.10<-round(sum(vect.erro.max.10)/500,digits=2)
erro.medio.max.25<-round(sum(vect.erro.max.25)/500,digits=2)
erro.medio.max.50<-round(sum(vect.erro.max.50)/500,digits=2)
erro.medio.max.75<-round(sum(vect.erro.max.75)/500,digits=2)
erro.medio.max.100<-round(sum(vect.erro.max.100)/500,digits=2)
erro.medio.max.125<-round(sum(vect.erro.max.125)/500,digits=2)
erro.medio.max.150<-round(sum(vect.erro.max.150)/500,digits=2)
