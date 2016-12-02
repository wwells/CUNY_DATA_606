n_draw <- 100000
n_fish <- sample(20:250, n_draw, replace=TRUE)
head(n_fish)

hist(n_fish, main="Prior Dist")

#plug in each draw to gen model which generates "fake" data
pick_fish <- function(n_fish) {
    fish <- rep(0:1, c(n_fish - 20, 20))
    sum(sample(fish, 20))
}

#how many marked fish on 2nd pass
n_marked <- rep(NA, n_draw)
for (i in 1:n_draw) {
    n_marked[i] <- pick_fish(n_fish[i])
}

head(n_marked, n=10)

#model
post_fish <- n_fish[n_marked==5]
hist(post_fish, main="posterior distribution")
abline(v=median(post_fish), col='red')
abline(v=quantile(post_fish, probs=c(.25, .75)), col="green")

# what if we have better priors?
n_fish <- rnbinom(n_draw, mu=200-20, size=4) + 20
hist(n_fish, main='Prior Dist')

#how many marked fish on 2nd pass
n_marked <- rep(NA, n_draw)
for (i in 1:n_draw) {
    n_marked[i] <- pick_fish(n_fish[i])
}

head(n_marked, n=10)

#plot again
post_fish <- n_fish[n_marked==5]
hist(post_fish, main="posterior distribution")
abline(v=median(post_fish), col='red')
abline(v=quantile(post_fish, probs=c(.25, .75)), col="green")