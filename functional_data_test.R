set.seed(1)
c = 3
D = 21
Sigma1 = Sigma2 = (1:D)^-1
Sigma2[1] = c*Sigma1[1]
basis.f = create.fourier.basis(c(0,1), nbasis=D)
fdata1 = fun_IID(n=100, nbasis=D, Sigma=Sigma1, basis=basis.f)
fdata2 = fun_IID(n=100, nbasis=D, Sigma=Sigma2, basis=basis.f)
fundata = fd(cbind(fdata1$coefs, fdata2$coefs), basis.f)
cc <- fundata$coefs

circles <- fundata[31:158]
circles <- circles$coefs[1:21,]
dd_2 <- function(x, y) {
  sqrt(sum((x - y)^2))
}

ts <- circles
dists_3 <- matrix(rep(0, 21 * 128), nrow = 21)
for(i in 1:21) {
  coord <- sample(128, 2)
  A <- ts[,coord[1]]
  B <- ts[,coord[2]]
  den <- dd_2(A, B)
  for(j in 1:128) {
    dists_3[i,j] <- (dd_2(A, ts[,j])^2 - dd_2(B, ts[,j])^2)/den
  }
}
dists <- t(dists_3)
bootJLDetectChangePoint(dists, rotate_xaxis = TRUE)

plot(ts[,10])
plot(ts[,100])
plot(ts[,50])