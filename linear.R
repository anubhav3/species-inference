#Linear model y = a*x + b  ~Food Web Model
linear <- function(real_a,real_b,num){
  yy <- real_a*x+real_b
  return(yy)
}


num<- 100
x <- runif(num,1,100) # ~Real Body Sizes
# ~Real Parameters
real_a <-3
real_b <-4

# ~Real food web
y <- linear(real_a,real_b,num) 
# ~Stochasticity in the system's biology
noise <- rnorm(num,mean=0,sd=1)
y <- y + noise

x_y <- data.frame(x=x,y=y)
# ~Interactions observed by a human
prob <- 0.3
n_sample <- as.integer(prob*num)
obs <- x_y[sample(nrow(x_y), n_sample), ]

#Paremeter Estimation (ABC)
numm <- 10000
a <- runif(numm,0,5)
b <- runif(numm,0,5)0
Q = rep(NA,numm)


for(i in 1:numm)
{
  Q[i] <-  sum(abs(obs[,2]-a[i]*obs[,1]-b[i])) 
}
datta <- data.frame(a=a,b=b)
p_datta <- datta[Q<100,]

meann = colMeans(p_datta)

# ~Estimated Parameters
obs_a <- meann[1]
obs_b <- meann[2]
plot(x,y)
lines(x,obs_a*x+obs_b,type='l')
