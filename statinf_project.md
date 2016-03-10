# Statistical Inference Course Project
  
# Part One
In this report we will investigate the exponential distribution. The exponential distribution can be simulated in R with **rexp(n, lambda)** where *lambda* is the rate parameter. The mean of a (e.g., $n=10$) exponential distribution is $\frac{1}{lambda}$ with the standard deviation is $\frac{^1/_{lambda}}{\sqrt{n}}$. We will use $lambda = 0.2$, and explore the distribution of means derived from $n=40$ sampled exponentials.

### The Knowns Variables

```r
#lets set the known variables:
n=40
lambda = 0.2

#the population or true statistics
theoretical.mean <- 1/lambda 
theoretical.stdv <- 1/lambda/sqrt(n)
theoretical.var = theoretical.stdv^2

#reproducibility
set.seed(8384)
```
The theoretical mean ($\mu$): **5** and variance ($\sigma^2$): **0.625**. 

### Simulations
*Below is a function to simulate the exponential distribution.* 

```r
#a function to simulate rexp with n simulation and nx exponentials
sim.rexp.means <- function(n, nx = 40) {
    rexp.means <- numeric()
    for (i in 1:n) { 
        rexp.means <- c(rexp.means, mean(rexp(nx, lambda)))
    }
rexp.means
}
```

NOTE, given the Law of Large Numbers (LLN), we should be able to estimate the population or true mean and variance (mentioned previously) from an *independent and identically distributed* (*idd*) sample. Lets simulate 1000 sample means, each a mean of $n=40$ sampled exponential distribution using the R function $rexp(n=40, rate=0.2)$. 

```r
m <- sim.rexp.means(n = 1000)
sample.mean <- mean(m) # sample mean
sample.sd <- sd(m) # sample standard deviation
```
We can see that the sample mean **4.9745652** is a good estimate of the theoretical mean (again, **5**), likewise, the sample variance **0.6242533** is a good estimation of the theoretical variance **0.625**.  


When analysing the shape of the simulated distribution we can see that it is bell shaped, similar to the normal distribution.

```r
hist(m, main = "Distribution of means 1000 x rexp(40, 0.2)", xlab = "Sample means")
```

![](statinf_project_files/figure-html/unnamed-chunk-4-1.png) 



### Central Limit Theorem
Roughly, the central limit theorem (CLT) states that the distribution of the average (or sum) of a large number of idd variables will be approximately normal, regardless of the underlying distribution. (http://www.math.uah.edu/stat/sample/CLT.html). Lets investigate the simulated distribution versus CLT.  

Note, the red bell-curve is the true normal and the black curve is the simulated data.

```r
#a function to simulate CLT using    rexp with n simulation and nx exponentials
sim.clt.rexp.means <- function(n, nx = 40) {
    rexp.means <- numeric()
    for (i in 1:n) { 
        rexp.means <- c(rexp.means, (mean(rexp(nx, lambda)) - theoretical.mean) / theoretical.stdv/sqrt(nx))
    }
rexp.means
}

#the standard deviation of averages of 40 standard normals must be 1/sqrt(40). (p.38)
#1/sqrt(n)
m1<- sim.clt.rexp.means(1000)
plot(density(m1), main = "Distribution of means 1000 x rexp(40, 0.2)", xlab = "Sample means")
x = seq(-1,1,.001)
lines(x=x,y=dnorm(x,mean=0 ,sd=1/sqrt(n)), xlab = "Sample means", col="red", type="l", lty=2)
```

![](statinf_project_files/figure-html/unnamed-chunk-6-1.png) 

# Part two

In this part we will analyze the *ToothGrowth* dataset in the R *datasets* package. 


```r
library(datasets)

#summary statistics of different variables
summary(ToothGrowth)
```

```
##       len        supp         dose      
##  Min.   : 4.20   OJ:30   Min.   :0.500  
##  1st Qu.:13.07   VC:30   1st Qu.:0.500  
##  Median :19.25           Median :1.000  
##  Mean   :18.81           Mean   :1.167  
##  3rd Qu.:25.27           3rd Qu.:2.000  
##  Max.   :33.90           Max.   :2.000
```


### TootGrowth data description
Recreated from TootGrowth package documentation:
The response is the length (*len*) of odontoblasts (teeth) in each of 10 guinea pigs at each of three *dose* levels (0.5, 1, and 2 mg) of Vitamin C with each of two delivery methods (*supp*) (orange juice (*OJ*) or ascorbic acid (*VC*)).

#### Format
A data frame with 60 observations on 3 variables.

[,1]	 len	 numeric	 Tooth length  
[,2]	 supp	 factor	 Supplement type (VC or OJ)  
[,3]	 dose	 numeric	 Dose in milligrams  
  

```r
#lets visualise the data
#coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth, xlab = "ToothGrowth data: length vs dose, given type of supplement")

boxplot(len~supp*dose, data = ToothGrowth, col=(c("red1","green1")), main="ToothGrowth data", xlab="Supplement.Dose")
```

![](statinf_project_files/figure-html/unnamed-chunk-8-1.png) 

Given the above box plot we could formulate three main hypothesis.

1.  
$H_0:$ there is no difference in teeth growth in the sampled guine pigs between OJ vs VC supplements with 0.5mg dose.  
$H_a:$  

```r
#1. null H: there is no difference OJ vs VC dose .5
ToothGrowth.0.5 <- ToothGrowth[(ToothGrowth$supp=="OJ" | ToothGrowth$supp=="VC") & ToothGrowth$dose==.5,]
t1 <- t.test(len ~ supp, paired=FALSE, var.equal=FALSE, data = ToothGrowth.0.5, conf.level = .95)

t1$conf.int
```

```
## [1] 1.719057 8.780943
## attr(,"conf.level")
## [1] 0.95
```

```r
t1$p.value
```

```
## [1] 0.006358607
```

```r
#reject null H
```

2.   
$H_0:$ there is no difference in teeth growth in the sampled guine pigs between OJ vs VC supplements with 1mg dose.  
$H_a:$   

```r
#2. null H: there is no difference OJ vs VC dose 1
ToothGrowth.1 <- ToothGrowth[(ToothGrowth$supp=="OJ" | ToothGrowth$supp=="VC") & ToothGrowth$dose==1,]
t2 <- t.test(len ~ supp, paired=FALSE, var.equal=FALSE, data = ToothGrowth.1, conf.level = .95)

t2$conf.int
```

```
## [1] 2.802148 9.057852
## attr(,"conf.level")
## [1] 0.95
```

```r
t2$p.value
```

```
## [1] 0.001038376
```

```r
#reject null
```

3.  
$H_0:$ there is no difference in teeth growth in the sampled guine pigs between OJ vs VC supplements with 2mg dose.  
$H_a:$ 

```r
#3. null H: there is no difference OJ vs VC dose 2
ToothGrowth.2 <- ToothGrowth[(ToothGrowth$supp=="OJ" | ToothGrowth$supp=="VC") & ToothGrowth$dose==2,]
t3 <- t.test(len ~ supp, paired=FALSE, var.equal=FALSE, data = ToothGrowth.2, conf.level = .95) 
#we fail to reject the null-H.

t3$conf.int
```

```
## [1] -3.79807  3.63807
## attr(,"conf.level")
## [1] 0.95
```

```r
t3$p.value
```

```
## [1] 0.9638516
```
