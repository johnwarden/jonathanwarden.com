
png(file="../assets/images/bayesian-argumentation/relevance-delta.png", width=6, height=6, units="in", res=300)



high = 0.8
low = 0.4
rd = high - low
pb <- 0.68
pa <- low + rd * pb
df <- data.frame(b = c(0,1), a = c(low,high))




par(mar = c(5.1, 4.1, 4.1, 5.5) )

plot(a ~ b, df, type='l', col=9, xaxs='i', yaxs='i', ylim=c(0,1),xlim=c(0,1), xlab=expression(bold("P'(B)")), ylab=expression(bold("P'(A)")), family="serif", main="Chart 1", sub="Relevance as Slope")

# Dotted line with label "P(A)"
segments(pb, pa, 1, pa, lty=3, col=4)
mtext(expression(bold("P(A)")), 4, at=pa, col=4, line=0.3, family="serif")

# Dotted line with label "P(B)"
segments(pb, pa, pb, 0, lty=3, col=4)
mtext(expression(bold("P(B)")), 1, at=pb, col=4, line=0.3, family="serif")

# Dotted line with label "P(A|B)"
segments(0, high, 1, high, lty=3, col="darkgreen")
mtext(expression(bold("P(A|B)")), 4, at=high, col="darkgreen", line=0.3, family="serif")


# Dotted line with label "P(A|B)"
segments(0, low, 1, low, lty=3, col="darkred")

mtext(expression(paste(bold("P(A|"),bar(bold("B")),")"))
, 4, at=low, col="darkred", line=0.3, family="serif")


segments(1.06, high, 1.08, high, xpd=TRUE)
segments(1.06, low, 1.08, low, xpd=TRUE)
segments(1.07, low, 1.07, high, lty=3, xpd=TRUE)

mtext(
  expression(paste(
    bolditalic("R(A, B)")
  )), 
4, at=(low + rd/2), line=2, family="serif", las=1)

dev.off()
