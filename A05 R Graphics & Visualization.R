#A05 R Graphics & Visualization

myplot <- function(n){
    for (i in 1:n)
    {
        plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
        text(1, 1, labels = paste(i), cex = 3)
    }
}

orgi.par <- par(mai = c(0.1, 0.1, 0.1, 0.1), mfrow = c(3, 2))
myplot(6)
par(orgi.par)
par(mai = c(0.1, 0.1, 0.1, 0.1), mfcol = c(2, 3))

orig.par <- par(mai = c(0.1, 0.1, 0.1, 0.1))
mat1 <- matrix(c(1, 2, 1, 3), 2, 2)

layout(mat1)
myplot(3)
par(orig.par)

plot(1:10, rep(1:10), pch = 20, col = 1:10, cex = 3, xlab = "", ylab = "")
text(1:10, rep(1:10), labels = 1:10)

par(mfrow = c(1, 1))
plot(iris[, 3], iris[, 4], pch = 16, 
     col = ifelse(iris[, 1] > median(iris[, 1]), "red", "blue"))

#Color Sets(p.42)
par(mfrow = c(2, 3))
n <- 24
pie(rep(1, n), col = rainbow(n))
pie(rep(1, n), col = heat.colors(n))
pie(rep(1, n), col = terrain.colors(n))
pie(rep(1, n), col = topo.colors(n))
pie(rep(1, n), col = cm.colors(n))
pie(rep(1, n), col = grey.colors(n))

#Legend(p.48, 49)
par(mfrow = c(1, 1))
plot(iris[, 3], iris[, 4], type = "n")
my.label <- c(rep("a", 50), rep("b", 50), rep("c", 50))
text(iris[, 3], iris[, 4], my.label, cex = 0.7, 
     col = ifelse(iris[, 1] > median(iris[, 1]), "red", "blue"))

plot(iris[, 1], iris[, 2], xlim = c(0, 10), ylim = c(0, 10))
text(2, 8, "This is a test")

plot(1:7, rnorm(7), xaxt = "n", frame = FALSE)
axis(1, 1:7, LETTERS[1:7], col = "green")
axis(3, 1:7, paste("test", LETTERS[1:7]), col.axis = "blue", las = 2)
axis(4, lty = 2, lwd = 2, las = 2)

#Axis(p.51)
plot(1:8, xaxt = "n", xlab = "")
axis(1, labels = FALSE)
my.labels <- paste("Label", 1:8, sep = "-")
text(1:8, par("usr")[3] - 0.25, srt = 45, adj = 1, 
     labels = my.labels, xpd = TRUE)
mtext(1, text = "X Axis Label", line = 3)

#Draw Symbols(p.52)
plot(0, xlim = c(0, 14), ylim = c(0, 14), type = "n", xlab = "", ylab = "", main = "Rectangle")
rect(1, 2, 3, 6)
n <- 0:3
rect(5 + n, 5 + n, 6 + 2 * n, 6 + 2 * n, col = rainbow(4), border = n + 1, lwd = 4)
symbols(x = c(2, 6), y = c(2, 6), circles = c(1, 4), 
        xlim = c(0, 10), ylim = c(0, 10), bg = c("red", "gray"), xlab = "", ylab = "")

#Draw One or More Raster Images(p.53)
plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "")
i1 <- as.raster(matrix(0:1, ncol = 6, nrow = 7))
rasterImage(i1, 180, 380, 220, 430, interpolate = FALSE)

i2 <- as.raster(matrix(colors()[1:100], ncol = 5))
rasterImage(i2, 100, 300, 150, 400, interpolate = FALSE)




