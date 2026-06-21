rm(list = ls())

library(ggplot2)

estimate_PBgA <- function(nRep,
                          sample_space=c(rep("atajada", 20), rep("penal", 10), rep("gol", 34)),
                          sample_size=5) {
    countAB <- 0
    countA <- 0
    for(i in 1:nRep) {
        sample <- sample(sample_space, sample_size)
        A <- sum(sample == "atajada") > sum(sample == "penal") 
        B <- sum(sample == "gol") == 0
        if(A) {
            countA <- countA + 1
            if(B) countAB <- countAB + 1
        }
    }
    pBgA <- countAB / countA
    return(pBgA)
}

derivePBgA <- function() {
    pAAAAA <- (20*19*18*17*16)/(64*63*62*61*60)
    pAAAAP <- (20*19*18*17*10)/(64*63*62*61*60)
    pAAAPP <- (20*19*18*10*9 )/(64*63*62*61*60)
    pAAAAG <- (20*19*18*17*34)/(64*63*62*61*60)
    pAAAPG <- (20*19*18*10*34)/(64*63*62*61*60)
    pAAAGG <- (20*19*18*34*33)/(64*63*62*61*60)
    pAAPGG <- (20*19*10*34*33)/(64*63*62*61*60)
    pAAGGG <- (20*19*34*33*32)/(64*63*62*61*60)
    pAGGGG <- (20*34*33*32*31)/(64*63*62*61*60)

    p5a     <- pAAAAA
    p4a1p   <- pAAAAP*factorial(5)/factorial(4)
    p3a2p   <- pAAAPP*factorial(5)/(factorial(3)*factorial(2))
    p4a1g   <- pAAAAG*factorial(5)/factorial(4) 
    p3a1p1g <- pAAAPG*factorial(5)/factorial(3)
    p3a2g   <- pAAAGG*factorial(5)/(factorial(3)*factorial(2)) 
    p2a1p2g <- pAAPGG*factorial(5)/(factorial(2)*factorial(2))
    p2a3g   <- pAAGGG*factorial(5)/(factorial(3)*factorial(2))
    p1a4g   <- pAGGGG*factorial(5)/factorial(4) 

    pA0g <- p5a + p4a1p + p3a2p
    pA1g <- p4a1g + p3a1p1g
    pA2g <- p3a2g + p2a1p2g
    pA3g <- p2a3g
    pA4g <- p1a4g

    pAB <- pA0g
    pA <- pA0g + pA1g + pA2g + pA3g + pA4g
    pBgA <- pAB / pA

    return(pBgA)
}

nReps <- seq(from=1000, to=130000, by=5000) # nRep we will use
# nReps <- seq(from=1000, to=15000, by=5000) # nRep we will use
nIter <- 50 # for each nRep, repeat the estimation nIter times to estimate the mean and CI of the estimator
confidence <- .99 # for error bars

# set the multiplier of the standard error based on the desired confidence for
# the confidence intervals
if(confidence==.95) {
    seMult = 1.96
} else {
    if(confidence==.99) {
        seMult = 2.576
    } else {
        error(sprintf("Invalid confidence %.4f, it must be .95 or .99", confidence))
    }
}

estimates <- matrix(nrow=nIter, ncol=length(nReps))
for(j in 1:length(nReps)) {
    print(sprintf("Processing number of repetitions: %d", nReps[j]))
    for(i in 1:nIter) {
        estimates[i, j] <- estimate_PBgA(nRep=nReps[j])
    }
}

pBgAa <- derivePBgA()

print("Plotting")
groups <- sprintf("%d", nReps)
means <- colMeans(estimates)
stds <- apply(estimates, 2, sd)
se <- stds/sqrt(nIter)
lower <- means - seMult * se
upper <- means + seMult * se

# Create a data frame
df <- data.frame(groups, means, lower, upper)
df$groups <- factor(df$groups, levels = df$groups)

# Plot
ggplot(df, aes(x = groups, y = means)) +
    geom_point(size = 3, color = "blue") +  # Plot the means
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.2, # Width of the whisker caps
                  color = "black", linewidth = 0.8) +
    labs(title = sprintf("Means with %d%% Confidence Intervals", round(100*confidence)),
         x = "Number of Repetitions",
         y = "P(B|A)") +
    geom_hline(yintercept = pBgAa, color = "blue", linetype = "dotted", linewidth = 1) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
fig_filename <- "../../figures/ICD1a.png"
ggsave(fig_filename)
print(sprintf("Saved figure to %s", fig_filename))

