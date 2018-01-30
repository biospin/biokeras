a=c(2,1)
b=c(0.5,0.5)
sum(a*b)
par(mfrow=c(2,2))
#------------------
# AND gate
#------------------
  and=function(input=c(),weight=c(0.5,0.5),theta=-0.7){
    a=(input);w=(weight)
    tmp=sum(a*w)+theta
    if(tmp<=0){k=0}else{k=1}
    return(k)
  }
  and(input=c(0,0))
  and(input=c(1,0))
  and(input=c(0,1))
  and(input=c(1,1))
  x=seq(0,1,length.out = 50)
  plot(NULL,xlim=range(x),ylim=range(x),main='AND')
  for(i in 1:1000){
    x1=sample(x,1);x2=sample(x,1)
    k=and(input=c(x1,x2))
    if(k==0){col='blue';pch='X'}else{col='red';pch=19}
    points(x1,x2,col=col,pch=pch)
  }
#------------------
# NAND gate
#------------------
  nand=function(input=c(),weight=c(-0.5,-0.5),theta=0.7){
    a=(input);w=(weight)
    tmp=sum(a*w)+theta
    if(tmp<=0){k=0}else{k=1}
    return(k)
  }
  nand(input=c(0,0))
  nand(input=c(1,0))
  nand(input=c(0,1))
  nand(input=c(1,1))
  plot(NULL,xlim=range(x),ylim=range(x),main='NAND')
  for(i in 1:1000){
    x1=sample(x,1);x2=sample(x,1)
    k=nand(input=c(x1,x2))
    if(k==0){col='blue';pch='X'}else{col='red';pch=19}
    points(x1,x2,col=col,pch=pch)
  }

#------------------
# OR gate
#------------------
  or=function(input=c(),weight=c(0.5,0.5),theta=-0.2){
    a=(input);w=(weight)
    tmp=sum(a*w)+theta
    if(tmp<=0){k=0}else{k=1}
    return(k)
  }
  plot(NULL,xlim=range(x),ylim=range(x),main='OR')
  for(i in 1:1000){
    x1=sample(x,1);x2=sample(x,1)
    k=or(input=c(x1,x2))
    if(k==0){col='blue';pch='X'}else{col='red';pch=19}
    points(x1,x2,col=col,pch=pch)
  }

#------------------
# XOR gate
#------------------
  xor=function(input=c()){
    i1=nand(input=input)
    i2=or(input=input)
    o=and(input=c(i1,i2))
    return(o)
  }
  xor(c(0,0))
  xor(c(0,1))
  xor(c(1,0))
  xor(c(1,1))
  x=seq(0,1,length.out = 100)
  plot(NULL,xlim=range(x),ylim=range(x),main='XOR')
  for(i in 1:1000){
    x1=sample(x,1);x2=sample(x,1)
    k=xor(input=c(x1,x2))
    if(k==0){col='blue';pch='X'}else{col='red';pch=19}
    points(x1,x2,col=col,pch=pch)
  }
  dev.off()

#----------------------
# Activation function
#----------------------
  par(mfrow=c(2,2))
  x1=seq(-1,1,length.out = 100)
  and_res=unlist(lapply(x1,function(x){and(input=c(x,1))}))
  nand_res=unlist(lapply(x1,function(x){nand(input=c(x,1))}))
  or_res=unlist(lapply(x1,function(x){or(input=c(x,1))}))
  xor_res=unlist(lapply(x1,function(x){xor(input=c(x,1))}))
  plot(x1,and_res,pch=19,'l',main='and_res')
  plot(x1,nand_res,pch=19,'l',main='nand_res')
  plot(x1,or_res,pch=19,'l',main='or_res')
  plot(x1,xor_res,pch=19,'l',main='xor_res')
dev.off()
