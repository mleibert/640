#Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#helper function to print functions
print.fun=function(x){header=deparse(args(x))[1]; b=body(x);  print(gsub('function',x,header));  print(b);}

# one-hidden layer logistic net
# n inputs, nh hidden nodes (one output)
# only changes to regression code
# - change cost function
# - add activation to output node (in fwd.prop)

#source("nn_fns.R")
#source("../lib/data_sources.R")
library(zeallot)   # defines unpacking assignment %<-%


# see: https://www.aaai.org/Papers/AAAI/1997/AAAI97-084.pdf
# lawrence, giles: Lessons in Neural Network Training
data.lawrence.giles=function(seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  # break
  X1 = seq(0,pi,length.out = 7)[c(-1,-7)]
  X2 = seq(pi,2*pi,length.out=15)
  Y1 = -cos(X1)+runif(length(X1),-.25,.25)
  Y2 = cos(3*(X2-pi))+runif(length(X2),-.25,.25)
  X = c(X1,X2)
  dim(X)=c(1,length(X))
  Y = c(Y1,Y2)
  dim(Y)=c(1,length(Y))
  X.grid=seq(0,2*pi,length.out = 99)
  Y.grid=c(-cos(seq(0,pi,length.out=50))[-1],cos(3*(seq(pi,2*pi,length.out = 50)-pi)))
  X=X/(2*pi)
  X.grid=X.grid/(2*pi)
  list(X=X,Y=Y,X.grid=X.grid,Y.grid=Y.grid)
}
plot.lawrence.giles=function(lg.data){
  plot(lg.data$X.grid,lg.data$Y.grid)
  points(lg.data$X,lg.data$Y,col=3,lwd=3)
}



#install.packages('downloader')
download_mnist=function(){
  library(downloader)
  
  if(!file.exists("train-images-idx3-ubyte")) {
    if(!file.exists("train-labels-idx1-ubyte.gz")) download(url="http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz",destfile="train-images-idx3-ubyte.gz")
    system("gunzip train-images-idx3-ubyte.gz") 
  }
  if(!file.exists("train-labels-idx1-ubyte")) {
    if(!file.exists("train-images-idx3-ubyte.gz")) download(url="http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz",destfile="train-labels-idx1-ubyte.gz")
    system("gunzip train-labels-idx1-ubyte.gz")
  }
  if(!file.exists("t10k-images-idx3-ubyte")) {
    if(!file.exists("t10k-images-idx3-ubyte.gz")) download(url="http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz",destfile="t10k-images-idx3-ubyte.gz")
    system("gunzip t10k-images-idx3-ubyte.gz")
  }
  if(!file.exists("t10k-labels-idx1-ubyte")) {
    if(!file.exists("t10k-labels-idx1-ubyte.gz")) download(url="http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz",destfile="t10k-labels-idx1-ubyte.gz")
    system("gunzip t10k-labels-idx1-ubyte.gz")
  }
  print("MNIST data load complete")
}


#https://stackoverflow.com/questions/48928652/load-the-mnist-digit-recognition-dataset-with-r-and-see-any-results
load_image_file <- function(filename) {
  ret = list()
  f = file(filename,'rb')
  readBin(f,'integer',n=1,size=4,endian='big')
  ret$n = readBin(f,'integer',n=1,size=4,endian='big')
  nrow = readBin(f,'integer',n=1,size=4,endian='big')
  ncol = readBin(f,'integer',n=1,size=4,endian='big')
  x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
  ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
  close(f)
  ret
}

load_label_file <- function(filename) {
  f = file(filename,'rb')
  readBin(f,'integer',n=1,size=4,endian='big')
  n = readBin(f,'integer',n=1,size=4,endian='big')
  y = readBin(f,'integer',n=n,size=1,signed=F)
  close(f)
  y
}


# binary classication data 
library(mlbench)


softmax=function(a){
  ea=exp(a)
  t(t(ea)/colSums(ea))
}
stable.softmax=function(a){
  a=a-max(a)
  ea=exp(a)
  t(t(ea)/colSums(ea))
}




chunker.cl=function(M,chunk.size){
  chunk=0
  defect= M %% chunk.size
  num.chunks=floor((M+chunk.size-1)/chunk.size)
  # this is very wasteful if chunk.size evenly divides M
  chunks=rep(chunk.size,num.chunks)
  if(sum(chunks)>M) chunks[num.chunks]=M-sum(chunks[-num.chunks])
  sum.chunks=0 # placeholder
  sequence=0   # placeholder
  
  function(){
    if(chunk==0){
      chunks<<-chunks[sample(1:num.chunks,num.chunks,replace=FALSE)]  # reorder chunks
      sum.chunks<<-cumsum(chunks)
      sequence<<-sample(1:M,M,replace=FALSE)
    }
    chunk<<-chunk+1
    if(chunk > num.chunks) {
      chunk<<-0; return(NULL)
    }else{
      start=ifelse(chunk==1,1,sum.chunks[chunk-1]+1)
      sequence[start:sum.chunks[chunk]]
    }
  }
}

if(FALSE){ # example, how to use chunker.cl
  m=32   # number of samples
  chunker=chunker.cl(m,12)  # 12=mini-batch size
  test.chunker=c()
  while(TRUE){
    samples=chunker()
    if(is.null(samples)) break;
    test.chunker=c(test.chunker,samples)
    print(samples)
  }
  all(sort(test.chunker)==(1:m))
}

# alternative to chunker - nicer sematics

seq.generator.cl=function(M,seq.size){
  num.chunks=floor((M+seq.size-1)/seq.size)
  sequence=0     # placeholder
  chunk.start=0  # placeholder
  defect.size= M %% seq.size
  defect.start=0 # placeholder
  seq.not.done=function(){
    response = (chunk.start <= M)
    if(response==FALSE){
      chunk.start <<- 0
    }
    response
  }
  next.seq=function(){
    if(chunk.start==0){ # start new sequence
      chunk.start <<- 1
      sequence <<- sample(1:M,M,replace=FALSE)
      if(defect.size != 0) {
        defect.start <<- (sample(num.chunks,1)-1)*seq.size+1
      }
    }
    if(defect.size==0){
      seq.chunk=sequence[chunk.start:(chunk.start+seq.size-1)]
      chunk.start <<- chunk.start+seq.size
    }else{
      size=ifelse(chunk.start==defect.start,defect.size,seq.size)
      seq.chunk=sequence[chunk.start:(chunk.start+size-1)]
      chunk.start <<- chunk.start+size
    }
    seq.chunk
  }
  list(seq.not.done=seq.not.done,next.seq=next.seq)
}
if(FALSE){
  require(zeallot)
  M=12
  batch.size=7
  c(seq.not.done,next.seq) %<-% seq.generator.cl(M,batch.size)
  for(epochs in 1:3){
    sequence=c()
    while(seq.not.done()){
      sequence=c(sequence,next.seq())
      print(sequence)
    }
    print(all(1:M == sort(sequence)))
  }
}


init.wgts=function(n.in,n.hid,n.out){
  b1 = runif(n.hid,-.1,.1)
  w1 = matrix(rnorm(n.in*n.hid,0,.1),nrow=n.hid,ncol=n.in)
  b2 = runif(n.out,-.1,.1)
  w2 = matrix(rnorm(n.out*n.hid,0,.1),nrow=n.out,ncol=n.hid)
  list(b1=b1,w1=w1,b2=b2,w2=w2)
}
