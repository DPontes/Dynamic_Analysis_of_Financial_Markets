# Rotina de volatilidade em periodos de 5 amostras
#
# Variáveis exteriores utilizadas:
#									Lr
#									return
#
# Significado das variáveis:
#
# return5 <- vector de tamanho arredondado a multiplo de 5
# Lr5 <- Tamanho do vector return5
# Lv5 <- Tamanho do vector de volatilidade pretendido
#
#-------------------------------------------------

if(Lr%%n!=0)						#
{									#
	Lr5<-(round(Lr/10,digits=0))*10	#
									# Arredondamento
	if(Lr5>Lr)						#para um vector de tamanho
	{								#multiplo de 5
		Lr5<-Lr5-5					#
	}								#
}									#

if(Lr%%5==0)
{
	Lr5=Lr
}

return5<-c(1:Lr5)

for(m in 0:Lr5-1)					#
{									# Passagem dos valores do
	return5[Lr5-m]<-return[Lr-m]	#vector return para
}									#o vector return5
length(return5)<-length(return5)-1

Lv5<-Lr5/5
volatilidade<-rep(0,Lv5)

for(i in 1:Lv5)
{
	soma<-0
	
	for(t in (i*5-4):(i*5))
	{
		soma<-soma+return5[t]
	}
	
	media<-soma/5
	sumatorio<-0
	
	for(j in (i*5-4):(i*5))
	{
		sumatorio<-sumatorio+sqrt(abs(return[j]-media))
	}

	volatilidade[i]<-(sumatorio/4)
}