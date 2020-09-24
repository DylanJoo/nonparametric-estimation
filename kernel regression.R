

### Excercise 1 ###
# 0): x ~ uni(-1, 1) + N(0, 0.01) / y = sin(20*x)
# 1): Using dnorm as the kernel function

m=function(x){return(sin(20*x))}
m.hat=function(xi, h, x, y, k=dnorm){
  # estimate by kernel
  m.est = sum(y*k((xi - x)/h)) / sum(k((xi - x)/h))
  return(m.est)
}

# global x and y data
N=1000
set.seed(2020)
x=runif(N, min=-1, max=1) + rnorm(N, 0, 0.01)
set.seed(2020)
y=m(x)

# Test for different h @ estimated value
m.hat(N=1000, xi=0.1, h=0.01, k=dnorm, x=x, y=y)
m.hat(N=1000, xi=0.1, h=0.005, k=dnorm, x=x, y=y)
m.hat(N=1000, xi=0.1, h=0.001, k=dnorm, x=x, y=y)
m.hat(N=1000, xi=0.1, h=0.0005, k=dnorm, x=x, y=y)

### Excercise 2 ###
# 1): calculate the mhat for S times (10000) 
# 2): each mhat should be approximate by 1000 samples
m.expect.err=function(S=10000, xi=0.1, h, k=dnorm, m.star){
  m.expect=c()
  for(i in c(1:S)){
    set.seed(i)
    x=runif(N, min=-1, max=1) + rnorm(N, 0, 0.01)
    set.seed(i)
    y=m.star(x)
    m.expect[i] = m.hat(1000, xi=xi, h=h, k=dnorm, 
                        x=x, y=y)
  }
  out<-list(bias=m.star(xi) - mean(m.expect), 
            variance=var(m.expect))
}

# Test for different h @ bias = m*(x0) - m.exp(x)
h1=m.expect.err(S=10000, xi=0.1, h=0.01, k=dnorm, m.star=m)
h2=m.expect.err(S=10000, xi=0.1, h=0.005, k=dnorm, m.star=m)
h3=m.expect.err(S=10000, xi=0.1, h=0.001, k=dnorm, m.star=m)
h4=m.expect.err(S=10000, xi=0.1, h=0.0005, k=dnorm, m.star=m)
cat(h1$bias,"\t", h2$bias,"\t",h3$bias,"\t",h4$bias)
cat(h1$variance,"\t", h2$variance,"\t",h3$variance,"\t",h4$variance)

### Excercise 3 ###
# 1): Leave one out for the h selections(given candidates)

h.loocv=function(h.seq, k=dnorm, m.star, x, y){
  h.select=c()
  rss=c()
  
  for(h.idx in 1:length(h.seq)){
    for(i in 1:length(x)){
      rss[i] = (y[i] - m.hat(x[i], h.seq[h.idx], x[-i], y[-i])) ** 2
    }
    RSS <<- rss
    h.select[h.idx] = sum(rss)
  }
  return(h.select)
}

x.new = runif(1000, min=-1, max=1) + rnorm(1000, 0, 0.01)
y.new = m(x.new)

h.result=h.loocv(h.seq=c(0.0005), 
                 k=dnorm, m.star=m,
                 x=x.error,y=y.error)
cat(h.result)


