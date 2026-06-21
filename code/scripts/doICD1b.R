rm(list = ls())

library(ggplot2)

estimate_PBs <- function(nReps,
                         sample_space=c(rep("atajada", 20), rep("penal", 10), rep("gol", 34)),
                         sample_size=5) {
    pBs <- c()
    for(nRep in nReps) {
        countB <- 0
		print(sprintf("Processing number of repetitions: %d", nRep))
        for(i in 1:nRep) {
            sample <- sample(sample_space, sample_size)
            B <- sum(sample == "gol") == 0
            if(B) {
                countB <- countB + 1
            }
        }
        pBs <- c(pBs, countB / nRep)
    }
    return(pBs)
}

nReps <- seq(from=1000, to=80000, by=1000) # nRep we will use
# nReps <- seq(from=1000, to=15000, by=5000) # nRep we will use
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

# Estimated PBs
pBs <- estimate_PBs(nReps=nReps)

# Analytical PBs
p <- 30 * 29 * 28 * 27 * 26 / (64 * 63 * 62 * 61 * 60)
meanA <- p * rep(1, times=length(nReps))
seA <- sqrt(p * (1-p) / nReps)
lowerA <- meanA - seMult * seA
upperA <- meanA + seMult * seA

plot_data <- data.frame(
  nReps = nReps,
  estimated = pBs,
  analyticalM = meanA,
  analyticalLower = lowerA,
  analyticalUpper = upperA
)

# --- 1. Build the ggplot2 Visualization ---
analyticalCIlabel <- sprintf("%d%% Sampling Interval", round(confidence*100))
color_values <- c("Estimated Probability" = "blue")
color_values[analyticalCIlabel] <- "red"

ggplot(plot_data, aes(x = nReps)) +
  # 1. Analytical Series: Mean line
  # geom_point(aes(y = analyticalM, color = analyticalCIlabel), 
  #           size = 2.5) +
  
  # 2. Analytical Series: Error bars for discrete nReps steps
  # 'width' controls the horizontal spread of the whisker caps
  geom_errorbar(aes(ymin = analyticalLower, ymax = analyticalUpper, color = analyticalCIlabel), 
                width = 150, linewidth = 0.8) +
  
  # 3. Estimated Series: Connected points
  geom_line(aes(y = estimated, color = "Estimated Probability"), 
            linewidth = 1) +
  geom_point(aes(y = estimated, color = "Estimated Probability"), 
             size = 2.5) +
  geom_hline(aes(yintercept = p, color = analyticalCIlabel), linetype = "dotted", linewidth = 1) +

  
  # Customizing Colors and Legend Names
  scale_color_manual(name = "Series", values = color_values) +

  # Labels and Styling
  labs(
    title = sprintf("Estimated Probability and %d%% Sampling Interval for Event B", round(100*confidence)),
    x = "Number of Repetitions",
    y = "Probability of B"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

fig_filename <- "../../figures/ICD1b.png"
ggsave(fig_filename)
print(sprintf("Saved figure to %s", fig_filename))
