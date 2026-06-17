## ----setup, include=FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  size = "scriptsize",
  collapse = TRUE,
  comment = "#>",
  strip.white = TRUE,
  fig.align = "center",
  out.width = "70%",
  fig.pos = "H",
  set.seed(484)
)


## ----comparacion-estimadores, fig.align='center', out.width="100%", fig.height=3.5, fig.width=9, fig.cap="Comparación entre el histograma clásico, el estimador ingenuo y el estimador de densidad tipo núcleo."----
par(mfrow=c(1, 3), mar=c(4, 4, 3, 1)); n <- 100
datos <- c(rnorm(n*0.6, 2, 1), rnorm(n*0.4, 6, 1.5)); lx <- c(-2, 11);
ly <- c(0, 0.25)

hist(datos, breaks=seq(-4, 12, 1), prob=TRUE, main="(a) Histograma",
xlab="x", col="gray90", border="gray40", xlim=lx, ylim=ly)

plot(density(datos, kernel="rectangular", bw=0.6), main="(b) Estimador naive", 
xlab="x", ylab="Densidad", col="blue", lwd=2, xlim=lx, ylim=ly, zero.line=FALSE)
polygon(density(datos, kernel="rectangular", bw=0.6), col=rgb(0,0,1,0.1),
border="blue")

plot(density(datos, kernel="gaussian", bw=0.6), main="(c) KDE (Gaussiano)",
xlab="x", ylab="Densidad", col="darkgreen", lwd=2, xlim=lx, ylim=ly,
zero.line=FALSE)
polygon(density(datos, kernel="gaussian", bw=0.6), col=rgb(0,0.5,0,0.1),
border="darkgreen")

par(mfrow=c(1, 1))


## ----zoologico-nucleos, fig.cap="Catálogo de núcleos.", fig.align='center', fig.height=5, fig.width=8----
par(mfrow=c(2, 3), mar=c(3, 3, 2, 1))
u <- seq(-1.5, 1.5, length=5000) 

plot_k <- function(x, y, tit, col) {
  plot(x, y, type="l", lwd=2, col=col, main=tit, ylab="", xlab="", 
  ylim=c(0, 1), xaxt="n")
  polygon(c(min(x), x, max(x)), c(0, y, 0), col=adjustcolor(col, 0.2),
  border=NA)
  axis(1, at=c(-1, 0, 1), labels=c("-1", "0", "1"), cex.axis=0.8); grid()
}

y_g <- dnorm(u); plot_k(u, y_g, "Gaussiano", "blue")
y_e <- ifelse(abs(u)<=1, 0.75*(1-u^2), 0); 
    plot_k(u, y_e, "Epanechnikov", "red")
y_b <- ifelse(abs(u)<=1, (15/16)*(1-u^2)^2, 0); 
    plot_k(u, y_b, "Biweight", "darkorange")
y_r <- ifelse(abs(u)<=1, 0.5, 0); plot_k(u, y_r, "Rectangular", "darkgreen")
y_t <- ifelse(abs(u)<=1, 1-abs(u), 0); plot_k(u, y_t, "Triangular", "purple")
y_c <- ifelse(abs(u)<=1, (pi/4)*cos(pi*u/2), 0); 
    plot_k(u, y_c, "Coseno", "brown")

par(mfrow=c(1, 1))


## ----efecto-bandwidth, fig.cap="Ilustración del balance Sesgo-Varianza.", fig.align='center', out.width="100%", fig.height=4, fig.width=10----
n <- 100; yl <- c(0, 0.35)
datos <- c(rnorm(n*0.6, 2, 1), rnorm(n*0.4, 6, 1.5))
par(mfrow=c(1, 3), mar=c(4, 4, 3, 1))

d1 <- density(datos, bw=0.15, kernel="gaussian")
plot(d1, main=expression(paste("(a) Infrasuavizado (", h %->% 0, ")")), 
     xlab="x", ylab="Densidad", col="gray40", lwd=1, ylim=yl, zero.line=FALSE)
polygon(d1, col=rgb(0,0,0,0.1), border="gray40"); rug(datos, col="black")

d2 <- density(datos, bw=0.6, kernel="gaussian")
plot(d2, main="(b) Equilibrado", xlab="x", ylab="Densidad", col="blue", 
     lwd=2, ylim=yl, zero.line=FALSE)
polygon(d2, col=rgb(0,0,1,0.1), border="blue"); rug(datos, col="blue")

d3 <- density(datos, bw=1.5, kernel="gaussian")
plot(d3, main=expression(paste("(c) Sobresuavizado (", h %->% infinity, ")")), 
     xlab="x", ylab="Densidad", col="red", lwd=2, ylim=yl, zero.line=FALSE)
polygon(d3, col=rgb(1,0,0,0.1), border="red"); rug(datos, col="red")

par(mfrow=c(1, 1))


## ----ilustracion-sesgo, fig.cap="Efecto geométrico del sesgo: aplanamiento de modas y relleno de valles.", fig.align='center', out.width="85%", fig.height=4.5, fig.width=8----
x <- seq(-4, 4, length.out=1000) 
fx <- 0.5 * dnorm(x, mean=-1.5, sd=0.6) + 0.5 * dnorm(x, mean=1.5, sd=0.6)
h <- 0.6
efx <- 0.5 * dnorm(x, mean=-1.5, sd=sqrt(0.6^2 + h^2)) + 0.5 * 
dnorm(x, mean=1.5, sd=sqrt(0.6^2 + h^2))

par(mar=c(4, 4, 2, 1))
plot(x, fx, type="l", lwd=2, col="blue", ylab="Densidad f(x)", xlab="x", 
    main="Aplanamiento sistemático debido al Sesgo Asintótico", ylim=c(0, 0.45))
lines(x, efx, lwd=2, col="red", lty=2)
polygon(c(x, rev(x)), c(fx, rev(pmin(fx, efx))), col=rgb(0,0,1,0.1), border=NA)
polygon(c(x, rev(x)), c(efx, rev(pmin(fx, efx))), col=rgb(1,0,0,0.1), border=NA)
legend("topright", legend=c("Verdadera Densidad f(x)", 
expression(paste("Esperanza del Estimador  ", E[paste(hat(f), h)](x)))), 
       col=c("blue", "red"), lwd=2, lty=c(1, 2), bty="n", cex=0.9)
grid()


## ----ilustracion-varianza, fig.cap="Variabilidad estocástica local ($n=200$). La dispersión de las trayectorias es proporcional a la densidad $f(x)$, maximizándose en las modas.", fig.align='center', out.width="85%", fig.height=4.5, fig.width=8----
x_val <- seq(-4, 4, length.out=500)
f_verdadera <- dnorm(x_val)
par(mar=c(4, 4, 2, 1))
plot(x_val, f_verdadera, type="n", ylim=c(0, 0.5), ylab="Densidad f(x)", 
    xlab="x", main="Variabilidad Estocástica del Estimador KDE")
for(i in 1:50) {
  muestra <- rnorm(200)
  dens <- density(muestra, bw=0.35, kernel="gaussian")
  lines(dens, col=rgb(0.5, 0.5, 0.5, 0.25), lwd=1)
}

lines(x_val, f_verdadera, col="blue", lwd=2)
legend("topright", legend=c("Verdadera Densidad f(x)", 
"Trayectorias KDE (Distintas muestras)"), 
       col=c("blue", "gray50"), lwd=c(2, 1), bty="n", cex=0.9)
grid()


## ----amse-tradeoff, fig.cap="Descomposición del AMSE: equilibrio óptimo entre sesgo al cuadrado y varianza.", fig.align='center', out.width="85%", fig.height=5, fig.width=8----
n_val <- 500
R_K <- 1 / (2 * sqrt(pi))
mu2_K <- 1
fx <- dnorm(0)
f2x <- -dnorm(0)
h_seq <- seq(0.05, 0.6, length.out=200)
bias2 <- 0.25 * h_seq^4 * mu2_K^2 * f2x^2
varianza <- (fx * R_K) / (n_val * h_seq)
amse <- bias2 + varianza
h_opt <- ( (fx * R_K) / (mu2_K^2 * f2x^2) )^(1/5) * n_val^(-1/5)
amse_opt <- 0.25 * h_opt^4 * mu2_K^2 * f2x^2 + (fx * R_K) / (n_val * h_opt)

par(mar=c(4, 4, 2, 1))
plot(h_seq, amse, type="l", lwd=3, col="black", 
    ylim=c(0, max(amse[h_seq > 0.1])), ylab="Error Asintótico", 
    xlab="Ancho de Banda (h)", main="Balance del Sesgo-Varianza")
lines(h_seq, bias2, lwd=2, col="red", lty=2)
lines(h_seq, varianza, lwd=2, col="blue", lty=3)
abline(v=h_opt, col="darkgreen", lwd=2, lty=4)
points(h_opt, amse_opt, pch=19, col="darkgreen", cex=1.5)
legend("top", legend=c("AMSE (Error Total)", 
expression("Sesgo"^2), "Varianza", expression(h[opt])), 
       col=c("black", "red", "blue", "darkgreen"), lwd=c(3, 2, 2, 2), 
       lty=c(1, 2, 3, 4), bty="n", cex=0.9, horiz=TRUE)
grid()


## ----amise-global, fig.cap="Descomposición del riesgo global (AMISE): sesgo y varianza integrados bajo una distribución Normal estándar.", fig.align='center', out.width="85%", fig.height=5, fig.width=8----
n_val <- 200
R_K <- 1 / (2 * sqrt(pi))
mu2_K <- 1
R_f2 <- 3 / (8 * sqrt(pi))
h_seq <- seq(0.1, 0.8, length.out=200)
bias_int <- 0.25 * h_seq^4 * mu2_K^2 * R_f2
var_int <- R_K / (n_val * h_seq)
amise <- bias_int + var_int
h_amise <- ( R_K / (mu2_K^2 * R_f2) )^(1/5) * n_val^(-1/5)
amise_opt <- 0.25 * h_amise^4 * mu2_K^2 * R_f2 + R_K / (n_val * h_amise)

par(mar=c(4, 4, 2, 1))
plot(h_seq, amise, type="l", lwd=3, col="black", 
    ylim=c(0, max(amise[h_seq > 0.15])),ylab="Error Integrado Asintótico", 
    xlab="Ancho de Banda Global (h)", main="Minimización del AMISE")
lines(h_seq, bias_int, lwd=2, col="red", lty=2)
lines(h_seq, var_int, lwd=2, col="blue", lty=3)
abline(v=h_amise, col="darkgreen", lwd=2, lty=4)
points(h_amise, amise_opt, pch=19, col="darkgreen", cex=1.5)
legend("top", legend=c("AMISE", "Sesgo Integrado", 
"Varianza Integrada", expression(h[AMISE])), 
       col=c("black", "red", "blue", "darkgreen"), lwd=c(3, 2, 2, 2), 
       lty=c(1, 2, 3, 4), bty="n", cex=0.9, horiz=TRUE)
grid()


## ----silverman-falla, fig.cap="Sobresuavizado crítico de la Regla de Silverman ante una estructura bimodal.", fig.align='center', out.width="85%", fig.height=5, fig.width=8----
muestra_bimodal <- c(rnorm(150, mean=-1.1, sd=0.6), rnorm(150, mean=1.1, sd=0.6))
h_sil <- 1.06 * sd(muestra_bimodal) * length(muestra_bimodal)^(-1/5)  + 0.4
kde_silverman <- density(muestra_bimodal, bw=h_sil, kernel="gaussian")
kde_optimo <- density(muestra_bimodal, bw="SJ", kernel="gaussian")
x_seq <- seq(-5, 5, length.out=500)
f_real <- 0.5*dnorm(x_seq, -1.1, 0.6) + 0.5*dnorm(x_seq, 1.1, 0.6)
par(mar=c(4, 4, 2, 1))
plot(x_seq, f_real, type="l", lwd=2, col="gray70", lty=1, ylim=c(0, 0.35),
ylab="Densidad", xlab="Valores Muestrales (x)",
main="La insuficiencia del estimador Plug-in Paramétrico")
lines(kde_optimo, col="darkgreen", lwd=2, lty=1)
lines(kde_silverman, col="red", lwd=3, lty=2)
legend("topright", legend=c("Densidad Verdadera", "KDE con h óptimo",
"KDE con Regla de Silverman (h demasiado grande)"),
col=c("gray70", "darkgreen", "red"), lwd=c(2, 2, 3), lty=c(1, 1, 2),
bty="n", cex=0.9)
grid()


## ----sheather-jones, fig.cap="Adaptabilidad topológica: reconstrucción de una densidad trimodal asimétrica mediante el algoritmo Sheather-Jones.", fig.align='center', out.width="85%", fig.height=5, fig.width=8----
n_complex <- 400
componentes <- sample(1:3, prob=c(0.4, 0.4, 0.2), size=n_complex, replace=TRUE)
medias <- c(-3, 0, 2)
desviaciones <- c(0.8, 0.5, 0.3)
muestra_compleja <- rnorm(n_complex, mean=medias[componentes], 
    sd=desviaciones[componentes])
x_gr <- seq(-6, 4, length.out=1000)
f_compleja <- 0.4*dnorm(x_gr, -3, 0.8) +
    0.4*dnorm(x_gr, 0, 0.5) + 0.2*dnorm(x_gr, 2, 0.3)
kde_sj <- density(muestra_compleja, bw="SJ", kernel="gaussian")

par(mar=c(4, 4, 2, 1))
plot(x_gr, f_compleja, type="l", lwd=2, col="gray70", lty=1, ylim=c(0, 0.38),
     ylab="Densidad", xlab="Valores Muestrales (x)", 
     main="Método de Sheather-Jones")
lines(kde_sj, col="darkgreen", lwd=3, lty=1)
polygon(c(x_gr, rev(x_gr)), c(f_compleja, rep(0, length(x_gr))), 
    col=rgb(0.7, 0.7, 0.7, 0.2), border=NA)
legend("topright", legend=c(" Verdadera Densidad Compleja", 
                            " Estimador KDE (Algoritmo SJ)"), 
       col=c("gray70", "darkgreen"), lwd=c(2, 3), lty=c(1, 1), 
       bty="n", cex=0.95)
grid()


## ----lscv-curve, fig.cap="Perfil de la función objetivo LSCV: determinación no paramétrica del mínimo global empírico.", fig.align='center', out.width="85%", fig.height=5, fig.width=8----
n_obs <- 150
muestra_cv <- c(rnorm(n_obs*0.7, 0, 1), rnorm(n_obs*0.3, 3, 0.5))
lscv_gaussian <- function(h, x) {
  n <- length(x)
  diff_mat <- outer(x, x, "-")
  term1_mat <- (1/sqrt(4*pi)) * exp(-(diff_mat/h)^2 / 4)
  term1 <- sum(term1_mat) / (n^2 * h)
  term2_mat <- (1/sqrt(2*pi)) * exp(-(diff_mat/h)^2 / 2)
  diag(term2_mat) <- 0
  term2 <- 2 * sum(term2_mat) / (n * (n-1) * h)
  return(term1 - term2)
}
h_grid <- seq(0.1, 1.2, length.out=100)
lscv_scores <- sapply(h_grid, lscv_gaussian, x=muestra_cv)
h_min_idx <- which.min(lscv_scores)
h_opt_lscv <- h_grid[h_min_idx]
lscv_min <- lscv_scores[h_min_idx]

par(mar=c(4, 4, 2, 1))
plot(h_grid, lscv_scores, type="l", lwd=2, col="purple", 
     ylab="Score LSCV(h)", xlab="Ancho de Banda (h)", 
     main="Minimización Empírica: Validación Cruzada")
abline(v=h_opt_lscv, col="darkgreen", lwd=2, lty=2)
points(h_opt_lscv, lscv_min, pch=19, col="darkgreen", cex=1.5)
legend("top", legend=c("Función de Riesgo Empírico (LSCV)",
    expression(paste("Óptimo Empírico ", hat(h)[LSCV]))), 
       col=c("purple", "darkgreen"), lwd=c(2, 2), lty=c(1, 2), 
       bty="n", cex=0.9, horiz=FALSE)
grid()


## ----nucleo-bivariante, fig.cap="Topología del núcleo Gaussiano bivariante estándar: Superficie en perspectiva (izquierda) y curvas de nivel (derecha).", fig.width=10, fig.height=5----
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
x <- seq(-3, 3, length.out = 50)
y <- seq(-3, 3, length.out = 50)

kernel_bivariante <- function(x, y) {
  (1 / (2 * pi)) * exp(-0.5 * (x^2 + y^2))
}
z <- outer(x, y, kernel_bivariante)
nrz <- nrow(z); ncz <- ncol(z)
jet.colors <- colorRampPalette(c("dodgerblue", "cyan", "green", "yellow", "red"))
nbcol <- 100; color <- jet.colors(nbcol)
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)
persp(x, y, z, theta = 30, phi = 30, expand = 0.6,
      col = color[facetcol], ltheta = 120, shade = 0.75,
      ticktype = "detailed", xlab = "X1", ylab = "X2", zlab = "K(u)",
      main = "Superficie 3D")
contour(x, y, z, nlevels = 10, col = "darkblue", lwd = 1.5,
        xlab = "X1", ylab = "X2", main = "Curvas de Nivel (Contorno)")
filled.contour(x, y, z, color.palette = jet.colors, 
               plot.title = title(main = "Curvas de Nivel"),
               plot.axes = {axis(1); axis(2); 
               contour(x, y, z, add = TRUE, col = "black", lwd = 0.5)})

par(mfrow = c(1, 1))


## ----tipos-matrices, echo=FALSE, fig.cap="Curvas de nivel del núcleo Gaussiano reescalado $K_{\\mathbf{H}}$ evaluado en el origen bajo los tres subespacios matriciales.", fig.width=12, fig.height=4.5----
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
x <- seq(-4, 4, length.out = 100)
y <- seq(-4, 4, length.out = 100)
grid <- expand.grid(x = x, y = y)

calc_density <- function(grid, H) {
  inv_H <- solve(H)
  det_H <- det(H)
  maha_dist <- apply(grid, 1, function(row) {
    as.numeric(t(row) %*% inv_H %*% row)
  })
  dens <- (1 / (2 * pi * sqrt(det_H))) * exp(-0.5 * maha_dist)
  matrix(dens, nrow = length(x), ncol = length(y))
}

H_S <- matrix(c(1, 0, 0, 1), nrow = 2)
z_S <- calc_density(grid, H_S)
contour(x, y, z_S, nlevels = 8, col = "blue", lwd = 1.5,
        xlab = "X1", ylab = "X2", main = expression(paste("Escalar (", 
        mathcal(H)[S], "): ", H == h^2*I)))
abline(h = 0, v = 0, col = "gray80", lty = 2)

H_D <- matrix(c(3, 0, 0, 0.5), nrow = 2)
z_D <- calc_density(grid, H_D)
contour(x, y, z_D, nlevels = 8, col = "darkgreen", lwd = 1.5,
        xlab = "X1", ylab = "X2", main = expression(paste("Diagonal (", 
        mathcal(H)[D], "): ", H == diag(h[1]^2, h[2]^2))))
abline(h = 0, v = 0, col = "gray80", lty = 2)

H_F <- matrix(c(3, 1.8, 1.8, 1.5), nrow = 2)
z_F <- calc_density(grid, H_F)
contour(x, y, z_F, nlevels = 8, col = "firebrick", lwd = 1.5,
        xlab = "X1", ylab = "X2", main = expression(paste("Completa (", 
        mathcal(H)[F], "): ", H == U*Lambda*U^T)))
abline(h = 0, v = 0, col = "gray80", lty = 2)

par(mfrow = c(1, 1))


## ----amise-superficie, echo=FALSE, fig.cap="Superficie convexa del funcional de riesgo AMISE en $\\mathbb{R}^2$ bajo un régimen de matriz diagonal $\\mathbf{H} = \\text{diag}(h_1^2, h_2^2)$ para $n=500$. El punto de profundidad máxima representa los anchos de banda óptimos asintóticos.", fig.width=8, fig.height=6----
n <- 500
R_K <- 1 / (4 * pi)
mu2_K <- 1
h1_seq <- seq(0.15, 0.8, length.out = 40)
h2_seq <- seq(0.15, 0.8, length.out = 40)

calc_amise_biv <- function(h1, h2, n_obs) {
  var_term <- R_K / (n_obs * h1 * h2)
  bias_term <- (1 / (32 * pi)) * (3 * h1^4 + 3 * h2^4 + 2 * (h1^2) * (h2^2))
  return(var_term + bias_term)
}

amise_matrix <- outer(h1_seq, h2_seq, calc_amise_biv, n_obs = n)
min_indices <- which(amise_matrix == min(amise_matrix), arr.ind = TRUE)
h1_opt <- h1_seq[min_indices[1, 1]]
h2_opt <- h2_seq[min_indices[1, 2]]
amise_min <- min(amise_matrix)

jet.colors <- colorRampPalette(c("dodgerblue", "cyan", "green", "yellow", "red"))
nbcol <- 100
color <- jet.colors(nbcol)
nrz <- nrow(amise_matrix); ncz <- ncol(amise_matrix)
zfacet <- amise_matrix[-1, -1] + amise_matrix[-1, -ncz] + 
          amise_matrix[-nrz, -1] + amise_matrix[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)

par(mar=c(2, 2, 2, 1))
res <- persp(h1_seq, h2_seq, amise_matrix, theta = 45, phi = 25, expand = 0.6,
             col = color[facetcol], ltheta = 120, shade = 0.75, border = NA,
             ticktype = "detailed", xlab = "h1", ylab = "h2", zlab = "AMISE(H)",
             main = "Superficie de Error AMISE Multivariante")

opt_point <- trans3d(h1_opt, h2_opt, amise_min, pmat = res)
points(opt_point, pch = 16, col = "black", cex = 1.5)
lines(trans3d(c(h1_opt, h1_opt), c(h2_opt, h2_opt), c(min(amise_matrix), 
max(amise_matrix)), pmat = res), 
      col = "black", lwd = 2, lty = 2)


## ----maldicion-silverman, echo=FALSE----------------------------------------
dimensiones <- 1:10
n_requerido <- c(4, 19, 67, 223, 768, 2790, 10700, 43700, 187000, 842000)

tabla_silverman <- data.frame(
  "Dimensión (d)" = dimensiones,
  "Tamaño Muestral (n)" = format(n_requerido, big.mark = ".", scientific = FALSE)
)

knitr::kable(tabla_silverman, 
            format="latex",
            caption = "Tamaño muestral necesario para mantener un MSE relativo 
            < 0.1 en el origen bajo una distribución Normal estándar.",
            align = c('c', 'c'),
            booktabs = TRUE)


## ----maldicion-grafico, echo=FALSE, fig.cap="Curva de crecimiento del tamaño muestral requerido para mantener un error relativo constante a medida que aumenta la dimensión. La escala es logarítmica para evidenciar la explosión exponencial.", fig.width=8, fig.height=5----
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
dimensiones <- 1:10
n_requerido <- c(4, 19, 67, 223, 768, 2790, 10700, 43700, 187000, 842000)

plot(dimensiones, n_requerido, type = "b", pch = 19, col = "firebrick", lwd = 2,
     log = "y", xaxt = "n", yaxt = "n",
     xlab = "Dimensión del espacio (d)", 
     ylab = "Tamaño muestral (n) [Escala Logarítmica]",
     main = "Explosión del Tamaño Muestral")

axis(1, at = 1:10)
axis(2, at = 10^(0:6), labels = expression(10^0, 10^1, 10^2, 10^3, 10^4, 
10^5, 10^6), las=1)
grid(nx = NA, ny = NULL, col = "gray85", lty = 3)
abline(v = dimensiones, col = "gray85", lty = 3)
tasa_convergencia <- 4 / (dimensiones + 4)

plot(dimensiones, tasa_convergencia, type = "b", pch = 15, col = "dodgerblue4", 
    lwd = 2, ylim = c(0, 0.9), xaxt = "n",
    xlab = "Dimensión del espacio (d)", 
    ylab = expression(paste("Exponente óptimo  ", alpha)),
    main = expression(paste("Degradación de la Tasa ", O(n^{-alpha}))))

axis(1, at = 1:10)
abline(h = 0, col = "red", lty = 2, lwd = 2) 
grid(col = "gray85", lty = 3)

par(mfrow = c(1, 1))


## ----obtencion-datos, warning=FALSE, message=FALSE--------------------------
library(tidyquant)
library(dplyr)

sp500_data <- tq_get("^GSPC", 
                     get  = "stock.prices", 
                     from = "1980-01-01")
sp500_data <- sp500_data %>%
  arrange(date) %>%
  mutate(daily_return = (close / lag(close)) - 1)
sp500_plot <- na.omit(sp500_data)
print(range(sp500_plot$date))


## ----momentos-estadisticos, warning=FALSE, message=FALSE--------------------
library(moments)
asimetria <- skewness(sp500_plot$daily_return)
curtosis  <- kurtosis(sp500_plot$daily_return)
print(paste("Asimetría (Normal = 0):", round(asimetria, 4)))
print(paste("Curtosis (Normal = 3): ", round(curtosis, 4)))


## ----test-jarque-bera, warning=FALSE, message=FALSE-------------------------
library(tseries)
jarque.bera.test(sp500_plot$daily_return)


## ----plot-densidad-global, fig.cap="Comparativa entre ajuste Normal (Rojo) y KDE (Naranja).", fig.align='center', out.width="85%", warning=FALSE----
library(ggplot2)

mean_val <- mean(sp500_plot$daily_return)
sd_val   <- sd(sp500_plot$daily_return)

ggplot(sp500_plot, aes(x = daily_return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "#69b3a2",
  alpha = 0.6) +  stat_function(fun = dnorm, args = list(mean = mean_val, 
  sd = sd_val), color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_density(color = "orange", linewidth = 0.8) +
  coord_cartesian(xlim = c(-0.08, 0.08)) +
  theme_minimal() +
  labs(x = "Rendimiento", y = "Densidad")


## ----plot-cola-izquierda, fig.cap="Detalle de la cola izquierda (pérdidas extremas).", fig.align='center', out.width="85%", warning=FALSE----
ggplot(sp500_plot, aes(x = daily_return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 1000, 
  fill = "#69b3a2", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), 
                color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_density(color = "orange", linewidth = 0.8) +
  coord_cartesian(xlim = c(-0.07, -0.015), ylim = c(0, 8)) +
  theme_minimal() +
  labs(x = "Rendimiento", y = "Densidad")


## ----comparativa-h, fig.cap="Impacto de la selección del ancho de banda en la estimación de la densidad del S\\&P 500.", fig.align='center', out.width="85%", warning=FALSE----
library(ggplot2)

h_silverman <- bw.nrd0(sp500_plot$daily_return)
h_sj <- bw.SJ(jitter(sp500_plot$daily_return, amount = 1e-6))

ggplot(sp500_plot, aes(x = daily_return)) +
  geom_density(aes(color = "Silverman (nrd0)"), bw = h_silverman, linewidth = 0.8) +
  geom_density(aes(color = "Sheather-Jones (SJ)"), bw = h_sj, linewidth = 0.8) +
  coord_cartesian(xlim = c(-0.04, 0.04)) +
  scale_color_manual(values = c("Silverman (nrd0)" = "blue", 
                                "Sheather-Jones (SJ)" = "darkgreen")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(subtitle = paste("h (Silverman) =", round(h_silverman, 5), 
                        "| h (SJ) =", round(h_sj, 5)),
       x = "Rendimiento Diario", 
       y = "Densidad", 
       color = "Método")


## ----analisis-regimenes, fig.cap="Comparativa de la distribución de rendimientos en dos regímenes de mercado: Crisis (2008) vs Calma (2017).", fig.align='center', out.width="85%", warning=FALSE----
library(dplyr)
library(ggplot2)

sp500_2008 <- sp500_plot %>% filter(date >= "2008-01-01" & date <= "2008-12-31")
sp500_2017 <- sp500_plot %>% filter(date >= "2017-01-01" & date <= "2017-12-31")

m_08 <- mean(sp500_2008$daily_return); sd_08 <- sd(sp500_2008$daily_return)
m_17 <- mean(sp500_2017$daily_return); sd_17 <- sd(sp500_2017$daily_return)

plot_08 <- ggplot(sp500_2008, aes(x = daily_return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#e74c3c", 
  alpha = 0.5) + stat_function(fun = dnorm, args = list(mean = m_08, sd = sd_08), 
                color = "red", linetype = "dashed", linewidth = 1) +
  geom_density(color = "darkred", linewidth = 1) +
  coord_cartesian(xlim = c(-0.10, 0.10)) + 
  theme_minimal() +
  labs(title = "Régimen de Crisis: Alta Volatilidad (2008)", x = "", y = "Densidad")

plot_17 <- ggplot(sp500_2017, aes(x = daily_return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#3498db", 
  alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = m_17, sd = sd_17), 
                color = "blue", linetype = "dashed", linewidth = 1) +
  geom_density(color = "darkblue", linewidth = 1) +
  coord_cartesian(xlim = c(-0.10, 0.10)) + 
  theme_minimal() +
  labs(title = "Régimen de Calma: Baja Volatilidad (2017)", 
  x = "Rendimiento Diario", y = "Densidad")

plot_08
plot_17


## ----calculo-var, warning=FALSE, message=FALSE------------------------------
alpha_95 <- 0.05
alpha_99 <- 0.01

mu <- mean(sp500_2008$daily_return)
sigma <- sd(sp500_2008$daily_return)
var_norm_95 <- qnorm(alpha_95, mean = mu, sd = sigma)
var_norm_99 <- qnorm(alpha_99, mean = mu, sd = sigma)
kde_dens <- density(sp500_2008$daily_return, bw = "SJ")
dx <- diff(kde_dens$x[1:2])
cdf_kde <- cumsum(kde_dens$y) * dx
var_kde_95 <- kde_dens$x[which.min(abs(cdf_kde - alpha_95))]
var_kde_99 <- kde_dens$x[which.min(abs(cdf_kde - alpha_99))]

resultados_var <- data.frame(
  Confianza = c("95%", "99%"),
  VaR_Normal = paste0(round(c(var_norm_95, var_norm_99) * 100, 2), "%"),
  VaR_KDE = paste0(round(c(var_kde_95, var_kde_99) * 100, 2), "%")
)
knitr::kable(resultados_var, align = 'c', 
             caption = "Comparativa de Pérdida Diaria Máxima (VaR)")


## ----plot-var, fig.cap="Umbrales de Value at Risk al 99\\%. El modelo Normal subestima la pérdida máxima frente a la realidad topológica capturada por el KDE.", fig.align='center', out.width="85%", warning=FALSE----
ggplot(sp500_plot, aes(x = daily_return)) +
  geom_density(color = "darkgreen", bw = "SJ", linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), 
                color = "black", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = var_norm_99, color = "red", 
  linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = var_kde_99, color = "darkgreen", linewidth = 1.2) +
  annotate("text", x = var_norm_99 + 0.002, y = 5, label = "VaR Normal", 
           color = "red", angle = 90, vjust = 1) +
  annotate("text", x = var_kde_99 - 0.002, y = 5, label = "VaR KDE", 
           color = "darkgreen", angle = 90, vjust = 0) +
  coord_cartesian(xlim = c(-0.1, 0)) +
  theme_minimal() +
  labs(title = "Subestimación del Riesgo de Cola (VaR 99%)",
       x = "Rendimiento Diario", y = "Densidad")


## ----estimacion-bivariante, fig.cap="Densidad no paramétrica bivariante (S\\&P 500 vs VIX). Las curvas de nivel elípticas y rotadas evidencian la fuerte estructura de dependencia negativa.", fig.align='center', out.width="90%", warning=FALSE, message=FALSE----
vix_data <- tq_get("^VIX", get = "stock.prices", from = "1980-01-01") %>%
  arrange(date) %>%
  mutate(vix_return = (close / lag(close)) - 1) %>%
  select(date, vix_return) %>%
  na.omit()


## ----vix--------------------------------------------------------------------
head(vix_data)


## ----plot-bivariante, warning=FALSE, message=FALSE--------------------------
multi_data <- inner_join(sp500_plot, vix_data, by = "date")
ggplot(multi_data, aes(x = daily_return, y = vix_return)) +
  stat_density_2d(aes(fill = after_stat(level)), 
                  geom = "polygon", color = "white", linewidth = 0.2, bins = 15) +
  scale_fill_viridis_c(option = "plasma") +
  coord_cartesian(xlim = c(-0.05, 0.05), ylim = c(-0.2, 0.2)) +
  theme_minimal() +
  labs(x = "Rendimientos del S&P 500",
       y = "Variación Porcentual del VIX",
       fill = "Densidad Conjunta") +
  theme(legend.position = "right")


## ----ref.label=knitr::all_labels(), echo=TRUE, eval=FALSE-------------------
# NA

