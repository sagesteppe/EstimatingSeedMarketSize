#create datasets
x <- c(3, 3, 4, 4, 5, 6, 7, 7, 7, 8, 12, 13, 14, 17, 19, 19)

# use default settings to generate the kernel density plot 
# not too many n are needed, because our data going in are relatively coarse. 
kd <- density(x, n = 1000)
plot(kd)

# now we want to transform these values into sampling probabilities so that we
# can use them for drawing values from the distribution. 
probs <- scales::rescale(kd$y,  to = c(0, 1))
sample_wts <- probs * (1/sum(probs))

# now we can compare the drawn values as a histogram to the more accurate and realistic
# distribution from the KDE. 
samples <- sample(1:1000, prob = sample_wts, replace = TRUE)

hist(samples)
plot(kd) 

rm(probs, kd, sample_wts)



##########################

# Another useful tool is ensuring that realistic combinations of annual fire size
# and number of fires are returned from simulations. 