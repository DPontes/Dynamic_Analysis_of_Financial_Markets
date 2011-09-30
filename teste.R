mydata<-read.csv('data.csv')		#vai tirar os valores do .csv para mydata
N<-length(mydata[,5])				#passa para N o tamanho da coluna 5
fecho<-mydata[,5]					#passa para um vector close os valores da 5a coluna

dif<-rep(0,N-1)						#cria um vector em dif de N 0's
return<-rep(0,N-1)					#cria um vector em return de N 0's


if(N%%2==0)									#
	{										#
		N<-N-1								#Se N for par, é ignorado o ultimo valor,
		length(fecho)<-(length(fecho)-1) 	#que é o valor mais antigo
		dif<-rep(0,N-1)						#
		return<-rep(0,N-1)					#
	}										#

fechor<-rep(0,N)



for(i in 1:N)					#
{								# Inverte o vector fecho, para os valores mais antigos estarem
	fechor[i]<-fecho[N+1-i]		#	em primeiro
}								#

#fazer com os retornos e diferenças

for(i in 1:N-1)									#
	{											#Coloca em dif as diferenças dos valores
		dif[i]<-fechor[i+1]-fechor[i]			#
	}											#
	
	
	
for(i in 1:N-1)									#
	{											#Coloca em return o retorno dos valores em percentagem
		return[i]<-(dif[i]/fechor[i])*100		#
		return[i]<-round(return[i],digits=2)	#Arredonda o vector return para valores com 2 casas decimais
	}											#


maxr<-round(max(return),digits=2)
minr<-round(min(return),digits=1)-0.05
T<-round(abs(maxr-minr)/0.05,digits=0)

hist(return,T)

vectorh<-rep(0,T)
Lr<-length(return)
#
#for(j in 1:Lr)									               #
#	{											               # Funcao que coloca em vectorh o numero de
#	for(t in 0:T)								               # vezes que um certo valor de retorno se repete
#		{										               # em intervalos de 0.05
#		if(return[j]>minr+t*0.05 && return[j]<minr+(t+1)*0.05) #
#			{									               #
#			vectorh[t+1]<-vectorh[t+1]+1		               #
#			}									               #
#		}										               #
#	}											               #
#	
#M<-L/2
#fazer uma AR(1) com intervalos incrementais

vect.ar1<-rep(0,M)

for(f in 0:M)
	{
		newvect<-rep(0,(M+f))
		for(j in 1:(M+f))
		{	
			newvect[j]<-dif[j]					#passa metade+f do vector dif para o vector newvect
		}
	newvect<-ts(newvect)
	ar1<-ar(newvect,aic=FALSE,order.max=1)
	prev.ar1<-predict(ar1,n.ahead=1)$pred
	vect.ar1[f+1]<-prev.ar1[1]
	}
	
	
#Criar vector de classes
vtclass<-c(5,10,25,50,75,100,250,500,1000,1500,2000)

for(c in 1:length(vtclass))
{
nclass<-vtclass[c]
delta<-abs(max(return)-min(return))/nclass
vclass<-c(0)

for(i in 1:Lr)
	{	
		if(return[i]==max(return))
			{
				vclass[i]<-nclass
			}
		if(return[i]==min(return))
			{
				vclass[i]<-1
			}
		for(t in 1:100)
			{
				if(min(return)+delta*t<=return[i] && min(return)+delta*(t+1)>=return[i])
					{
						vclass[i]<-t
					}
			}
	}
	
#Encontrar sequências iguais no vector de classes

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
	
vcrep<-c(0)
	for(i in 1:length(vtemp))
		{
			vcrep[i]<-vtemp[length(vtemp)+1-i]
		}
length(vcrep)<-length(vcrep)-1


}