
if("boot" %in% rownames(installed.packages()) == FALSE) {install.packages("boot")} else{library("boot")}

#   ____________________________________________________________________________
#   Unbiased RW                                                             ####


tot_walks <- 3000
length_walk <- 2500 # 2500 steps

stop <- c() #store the number of steps before hitting an absorbing barrier

for(walk in 1:tot_walks){
    
    x <- 30 + cumsum(sample(c(-1, 1), length_walk, TRUE)) # simulate a unbiased random walk
                                                          # of 2500 steps starting from 30
    
    counter <- 1
    
    while(x[counter]>0 & x[counter]<50 & counter <=2500){
    
      counter <- counter +1 # update the number of steps until we reach 0 or 50
      
    }
    

    stop <- c(stop, counter) # store the number of steps for each walk
  
  
}

boot(stop, statistic = function(x, inds) mean(x[inds]), R = 100) # run a bootstrap on the mean

'''
In our example k = 30 and N=0 so Lk=k(N-k)=600 agrees with the results from our simulation
'''


#   ____________________________________________________________________________
#   Biased RW                                                           ####

tot_walks <- 3000
length_walk <- 2500 # 2500 steps

stop <- c() #store the number of steps before hitting an absorbing barrier

for(walk in 1:tot_walks){
  
  x <- 30 + cumsum(sample(c(-1, 1), length_walk, TRUE, prob = c(0.45,0.55))) # simulate a biased random walk
                                                                             # of 2500 steps starting from 30
                                                                             # with p(xn=+1) = 0.55

  counter <- 1
  
  while(x[counter]>0 & x[counter]<50 & counter <=2500){
    
    counter <- counter +1 # update the number of steps until we reach 0 or 50
    
  }
  
  stop <- c(stop, counter) # store the number of steps for each walk
  
  
}

boot(stop, statistic = function(x, inds) mean(x[inds]), R = 100)  # run a bootstrap on the mean

'''
In our example k = 30 and N=0 so Lk=200 agrees with the results from our simulation
'''
