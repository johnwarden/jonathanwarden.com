
png(file="../assets/images/bayesian-argumentation/necessity-and-sufficiency.png", width=7, height=6, units="in", res=300)



high = 0.8
low = 0.4
rd = high - low
pb <- 0.68
pa <- low + rd * pb
df <- data.frame(b = c(0,1), a = c(low,high))



par(mar = c(5.1, 4.1, 4.1, 11) )

plot(a ~ b, df, type='l', col=9, xaxs='i', yaxs='i', ylim=c(0,1),xlim=c(0,1), xlab=expression(bold("P'(B)")), ylab=expression(bold("P'(A)")), family="serif", main="Chart 2", sub="Relationship Between Relevance (R), Necessity (N), and Sufficiency (S)")

# Dotted line with label "P(A)"
segments(pb, pa, 1, pa, lty=3, col=4)
mtext(expression(bold("P(A)")), 4, at=pa, col=4, line=0.3, family="serif")

# Dotted line with label "P(B)"
segments(pb, pa, pb, 0, lty=3, col=4)
mtext(expression(bold("P(B)")), 1, at=pb, col=4, line=0.3, family="serif")

# Dotted line with label "P(A|B)"
segments(0, high, 1, high, lty=3, col="green")
mtext(expression(bold("P(A|B)")), 4, at=high, col="darkgreen", line=0.3, family="serif")


# Dotted line with label "P(A|B)"
segments(0, low, 1, low, lty=3, col="darkred")

mtext(expression(paste(bold("P(A|"),bar(bold("B")),")"))
, 4, at=low, col="darkred", line=0.3, family="serif")



offsetX = 1.08


# Show Necessity delta in margin
segments(offsetX+.01, low, offsetX+.01, pa, lty=3, xpd=TRUE)
segments(offsetX, pa, offsetX+.02, pa, xpd=TRUE)
segments(offsetX, low, offsetX+.02, low, xpd=TRUE)

nd = pa - low
mtext(
  expression(paste(
    bolditalic("N(A, B)")
  )), 
4, at=(low + nd/2), line=2.0, family="serif", las=1)

# Show Sufficiency delta in margin
segments(offsetX+.01, pa, offsetX+.01, high, lty=3, xpd=TRUE)
segments(offsetX, high, offsetX+.02, high, xpd=TRUE)

sd = high - pa
mtext(
  expression(paste(
    bolditalic("S(A, B)")
  )), 
4, at=(pa + sd/2), line=2.0, family="serif", las=1)


offsetX = 1.29

# Show Relevance delta in margin
segments(offsetX+.01, low, offsetX+.01, high, lty=3, xpd=TRUE)
segments(offsetX, high, offsetX+.02, high, xpd=TRUE)
segments(offsetX, low, offsetX+.02, low, xpd=TRUE)

mtext(
  expression(paste(
    bolditalic("R(A, B)")
  )), 
4, at=(low + rd/2), line=6.30, family="serif", las=1)



## Show formulas in margin


# Sufficiency Delta Formula
mtext(
  expression(paste(
    bolditalic("S(A, B) = P(A"),"|",bolditalic("B) - P(A)")
  )), 
4, at=0.25, line=0.5, family="serif", las=1)


# Necessity Delta Formula
mtext(
  expression(paste(
    bolditalic("N(A, B) = P(A) - P(A"),"|",bolditalic(bar("B")),")"
  )), 
4, at=0.2, line=0.5, family="serif", las=1)


# Relevance Delta Formula
mtext(
  expression(paste(
    bolditalic("R(A, B) = P(A"),"|",bolditalic("B) - P(A"),"|",bolditalic(bar("B")),")"
  )), 
4, at=0.15, line=0.5, family="serif", las=1)

# Relevance = Sufficiency + Necessity
mtext(
  expression(paste(
    bolditalic("             = S(A, B) + N(A, B)")
  )), 
4, at=0.1, line=0.5, family="serif", las=1)




dev.off()