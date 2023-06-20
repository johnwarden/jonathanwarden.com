# include necessary libraries
library(ggplot2)
library(svglite)
library(gridExtra)


priors = data.frame(scenario=c("1. Alive+Duke", "2. Dead+Duke", "3. Dead+Count"), probability=c(0.5, 0.25, 0.25))
posteriors = data.frame(scenario=c("1. Alive+Duke", "2. Dead+Duke", "3. Dead+Count"), probability=c(0, 0.5, 0.5))


p1 = ggplot(priors, aes(x=scenario, y=probability)) +
  geom_bar(stat="identity", width=0.2, fill="lightblue") +
  scale_y_continuous(labels=scales::percent)  +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Prior") +
  geom_text(aes(x=scenario,y=probability,label=percent(probability)), position = position_stack(vjust = .5), size = 3)
print(p1)


p2 = ggplot(posteriors, aes(x=scenario, y=probability)) +
  geom_bar(stat="identity", width=0.2, fill="lightblue") +
  scale_y_continuous(labels=scales::percent)  +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Posterior") +
  geom_text(aes(x=scenario,y=probability,label=percent(probability)), position = position_stack(vjust = .5), size = 3)
print(p2)

marginalPriors = data.frame(culprit=c("Duke", "Count"), probability=c(.75, .25))
marginalPosteriors = data.frame(culprit=c("Duke", "Count"), probability=c(.50, .50))


p3 = ggplot(marginalPriors, aes(x=culprit, y=probability)) +
  geom_bar(stat="identity", width=0.2, fill="lightblue") +
  scale_y_continuous(labels=scales::percent)  +
  scale_x_discrete(limits = c("Duke","Count"))  +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # labs(title="Marginal Prior") +
  geom_text(aes(x=culprit,y=probability,label=percent(probability)), position = position_stack(vjust = .5), size = 3)
print(p3)

p4 = ggplot(marginalPosteriors, aes(x=culprit, y=probability)) +
  geom_bar(stat="identity", width=0.2, fill="lightblue") +
  scale_y_continuous(labels=scales::percent)  +
  scale_x_discrete(limits = c("Duke","Count"))  +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # labs(title="Marginal Prior") +
  geom_text(aes(x=culprit,y=probability,label=percent(probability)), position = position_stack(vjust = .5), size = 3)
print(p4)


# lot two charts side by side using a grid
g <- grid.arrange(p1, p2, p3, p4, ncol=2)

# now export the plot as a transparent svg
ggsave(file="~/Downloads/reallocation-of-probabilities-3.svg", g, width=8, height=8, units="in", device="svg", dpi=75, bg="transparent")

