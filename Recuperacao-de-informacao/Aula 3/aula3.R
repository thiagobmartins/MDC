##################################################################
# Mineracao de Dados Complexos -- MDC 2021 
# Recuperacao de Informacao
# Codigos da Aula 3 - Recuperacao de Imagem
##################################################################


##################################################################
# Preparando ambiente - workspace
# # Linux
# setwd("~/user/diretorio/INF-0611/aula3/")
# # Windows
# setwd("C:\\user\\diretorio\\INF-0611\\aula3\\")

# # Instalando os pacotes
# install.packages(wvtool)
# install.packages(imager)
# install.packages(ecodist)
# install.packages(grid)
# install.packages(gridExtra)
# install.packages(utils)
# install.packages(OpenImageR)
# install.packages(raster)
# install.packages(IM)
# install.packages(EBImage)
# install.packages(grDevices)


##################################################################
# Descritores de Cor
##################################################################
##################################################################
# Histograma de Cor 
##################################################################
# incluindo os pacotes
library(wvtool)
library(imager)
library(grid)
# incluindo implementacoes extras
source("./cbir_aula3.R")


# caminho da pasta com as imagens
path <- "./futebol"
# listando os caminhos das imagens
path_imgs <- list.files(path, full.names = TRUE)
# listando apenas os nomes das imagens
name_imgs <- list.files(path, full.names = FALSE)
names(path_imgs) <- name_imgs; name_imgs

# construindo uma matriz com os vetores de características de 
# todas as imagens da base (cada linha representa uma imagem)
M <- t(sapply(path_imgs, extract_color_hist))
# visualizando dimensoes dos descritores
dim(M)
# visualizando parte dos vetores de caracteristicas 
M[1:3, c(1:10)]

# imagem de consulta
consulta = 'liverpool_01.jpg'
# calculando as distâncias entre a consulta e as imagens da base
distancias <- lapply(name_imgs, 
                     function(x) dist(M[c(consulta, x),], 
                                      method = "euclidean"))
distancias
distancias <- unlist(distancias)
names(distancias) <- name_imgs; distancias

# obtendo imagens mais próximas 
ranking <- names(sort(distancias)); ranking

# plotando as imagens retornadas
par(mfrow = c(3,3), mar = rep(2, 4))
for(img in ranking[1:9]){
  plot(load.image(path_imgs[img]), axes = FALSE, main = img)
}

##################################################################
# Momentos de Cor 
##################################################################
# incluindo implementações extras
source("cbir_aula3.R")

# construindo uma matriz com os vetores de caracteristicas de 
# todas as imagens da base  
M <- t(sapply(path_imgs, extract_color_moments))
# visualizando dimensoes dos descritores
dim(M)
# visualizando parte dos vetores de caracteristicas 
M[1:3,]

# imagem de consulta
consulta = 'liverpool_01.jpg'

# definindo pesos uniformes para os canais e momentos
w <- rep(1, 12)

# gerando o ranking
ranking <- get_ranking_by_moment(M, consulta, w)

# plotando as imagens retornadas
par(mfrow = c(3,3), mar = rep(2, 4))
for(img in ranking[1:9]){
  plot(load.image(path_imgs[img]), axes = FALSE, main = img)
}

# definindo pesos com preferências para o primeiro momento
w <- rep(4:1, each = 3)
# gerando o ranking
ranking <- get_ranking_by_moment(M, consulta, w)
# plotando as imagens retornadas
par(mfrow = c(3,3), mar = rep(2, 4))
for(img in ranking[1:9]){
  plot(load.image(path_imgs[img]), axes = FALSE, main = img)
}

##################################################################
# Vetor de Coerencia de Cor 
##################################################################
# incluindo implementações extras
source("cbir_aula3.R")

# imagem de consulta
consulta = "liverpool_01.jpg"
# gerando o ranking
ranking_ccv <- get_ranking_CCV(path_imgs, name_imgs, consulta)

# plotando as imagens retornadas
par(mfrow = c(3,3), mar = rep(2, 4))
for(img in ranking_ccv[1:9]){
  plot(load.image(path_imgs[img]), axes = FALSE,
       main = img)
}

##################################################################
# Border/Interior Classification
##################################################################
# incluindo implementações extras
source("cbir_aula3.R")

# imagem de consulta
consulta = 'liverpool_01.jpg'
# gerando o ranking
ranking_bic <- get_ranking_BIC(path_imgs, name_imgs, consulta)

# plotando as imagens retornadas
par(mfrow = c(3,3), mar = rep(2, 4))
for(img in ranking_bic[1:9]){
  plot(load.image(path_imgs[img]), axes = FALSE, main = img)
}




##################################################################
# Descritores de Textura
##################################################################
##################################################################
# Matriz de Co-ocorrencia

# carregando o pacote tiff usado para ler imagens no formato tiff
library(tiff)
# carregando o pacote que contem as implementacoes da matriz
# de co-ocorrencia
library(wvtool)
# incluindo implementações extras
source("cbir_aula3.R")

# caminho da pasta com as imagens
path <- "./Brodatz-tiff"
# listando os caminhos das imagens
path_imgs <- list.files(path, full.names = TRUE)
# listando apenas os nomes das imagens
name_imgs <- list.files(path, full.names = FALSE)
names(path_imgs) <- name_imgs

# definindo a imagem de consulta
consulta <- 'D10_01.tif'

# visualizando a imagem de consulta
library(gridExtra)
library(grid)
grid.arrange(grobTree(grid::rasterGrob(
  tiff::readTIFF(path_imgs[consulta]))), ncol=1)


img <- readTIFF(path_imgs[consulta], as.is = TRUE, info = TRUE)
matriz_consulta <- glcm(img)


# gerando o ranking
ranking_mcc <- get_ranking_cooccurrence(path_imgs, name_imgs, 
                                        consulta)

# plotando as imagens retornadas
gs <- lapply(path_imgs[ranking_mcc[1:9]], function(ii) 
  grobTree(grid::rasterGrob(tiff::readTIFF(ii))))
grid.arrange(grobs=gs, ncol=3)


##################################################################
# Matriz de Co-ocorrencia - Descritores de Haralick

# incluindo implementações extras
source("cbir_aula3.R")

# imagem de consulta
consulta = "D10_01.tif"
# visualizando a imagem de consulta
grid.arrange(grobTree(grid::rasterGrob(
  tiff::readTIFF(path_imgs[consulta]))), ncol=1)

# gerando o ranking
ranking_ha <- get_ranking_haralick(path_imgs, name_imgs, consulta)

# plotando as imagens retornadas
gs <- lapply(path_imgs[ranking_ha[1:9]], function(ii) 
  grobTree(grid::rasterGrob(tiff::readTIFF(ii))))
grid.arrange(grobs=gs, ncol=3)

##################################################################
# Local Binary Pattern - LBP

# incluindo pacotes
library(utils)
library(OpenImageR)
library(imager)
library(wvtool)

# carregaando imagens
path_brodatz <- "./Brodatz-test"
path_imgs <- list.files(path_brodatz, full.names = TRUE)
name_imgs <- list.files(path_brodatz, full.names = FALSE)
names(path_imgs) <- name_imgs
img <-  load.image(path_imgs[1])

dim(img)
# calcula lbp para imagem
r1 <- lbp(img[,,1,1],1)

# plota imagem original
par(mfrow = c(2,3), cex.main=1.75)
image(rot90c(img[,,1,1]), col = gray((0:58)/58), 
      main = "Original", useRaster = TRUE, asp = 1, 
      axes= FALSE)
# plota lbp original
image(rot90c(r1$lbp.ori),col = gray((0:255)/255), 
      main = "lbp.ori (r=1, 8 points)", 
      useRaster=TRUE, asp = 1, axes = FALSE)
# plota histograma do lbp original
hist(r1$lbp.ori, breaks = 256, 
     main = "Histograma do lbp.ori")

# plota imagem original
image(rot90c(img[,,1,1]), col = gray((0:58)/58), 
      main = "Original", useRaster = TRUE, asp = 1, 
      axes =  FALSE)

# plota lbp uniforme
image(rot90c(r1$lbp.u2), col = gray((0:58)/58), 
      main = "lbp.u2 (r=1, 8 points)", 
      useRaster = TRUE, asp = 1, axes = FALSE)

# plota histograma do lbp uniforme
hist(r1$lbp.u2, breaks = 59, 
     main = "Histograma do lbp.u2")

# imagem de consulta
consulta = 'D101_01.jpg'

# construindo uma matriz com os vetores de 
# características de todas as imagens da base  
M_lbp <- t(as.data.frame(lapply(path_imgs, extract_lbp_image)))

# calculando as distâncias entre a consulta e as imagens da base
distancias <- lapply(name_imgs, 
                     function(x) dist(M_lbp[c(consulta, x),], 
                                      method = "euclidean"))
distancias
distancias <- unlist(distancias)
names(distancias) <- name_imgs; distancias

# obtendo imagens mais próximas 
ranking <- names(sort(distancias)); ranking
# plotando as imagens retornadas
par(mfrow = c(3,3), mar = rep(2, 4))
for(img in ranking[1:9]){
  plot(load.image(path_imgs[img]), axes = FALSE, main = img)
}

##################################################################
# Descritores de Forma
##################################################################
# Area, Perimetro e Compacidade
# 
library(imager)

perimetro <- function(fig) {
  temp <- as.cimg(fig)
  bordas <- contours(temp, level = 0.1)
  perimetro <- 0
  for (borda in bordas) {
    perimetro <- perimetro + length(borda$x) - 1 ; perimetro
  }
  return(perimetro)
}

area <- function(fig) {
  sum(fig)
}

compacidade <- function(fig) {
  perimetro(fig)^2 / area(fig)
}

# Exemplo 1
quadrado <- rbind(
  c(0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0))

# Exemplo 2
triangulo <- rbind(
  c(0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 0),
  c(0, 1, 1, 0, 0, 0, 0, 0),
  c(0, 1, 1, 1, 0, 0, 0, 0),
  c(0, 1, 1, 1, 1, 0, 0, 0),
  c(0, 1, 1, 1, 1, 1, 0, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0))

displayImg(quadrado)
area(quadrado)
perimetro(quadrado)
compacidade(quadrado)

displayImg(triangulo)
area(triangulo)
perimetro(triangulo)
compacidade(triangulo)


# Exemplo 3
circulo <- rbind(
  c(0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 1, 0, 0, 0),
  c(0, 0, 1, 1, 1, 1, 0, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 0, 1, 1, 1, 1, 0, 0),
  c(0, 0, 0, 1, 1, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0))

displayImg(circulo)
area(circulo)
perimetro(circulo)
compacidade(circulo)

# Exemplo 4
quad_triang <- rbind(
  c(0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 1, 0, 0, 0, 0, 0),
  c(0, 1, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 0, 1, 1, 0),
  c(0, 0, 0, 0, 1, 1, 1, 0),
  c(0, 0, 0, 1, 1, 1, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0))

displayImg(quad_triang)
area(quad_triang)
perimetro(quad_triang)
compacidade(quad_triang)

# Exemplo 5
fig <- rbind(
  c(0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 0, 0, 0, 0, 0, 1, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 1, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0))

displayImg(fig)

area(fig)

perimetro(fig)

compacidade(fig)

# Gerando um retângulo
px <- 10
py <- 10
dx <- 50
dy <- 80
sx <- 100
sy <- 100
fig <- matrix(nrow = sy, ncol = sx)

for (x in 1:sx) {
  for (y in 1:sy) {
    fig[x, y] <- ((px < x) & (x <= px + dx)) &
      ((py < y) & (y <= py + dy))
  }
}

displayImg(fig)

area(fig)

perimetro(fig)

compacidade(fig)

# Gerando um círculo
cx <- 50
cy <- 50
r <- 40
sx <- 100
sy <- 100
fig <- matrix(nrow = sy, ncol = sx)

for (x in 1:sx) {
  for (y in 1:sy) {
    fig[x, y] <- sqrt((x - cx)^2 + (y - cy)^2) <= r
  }
}

displayImg(fig)

area(fig)

perimetro(fig)

compacidade(fig)

##################################################################
# Forma -- Retangulo Envolvente
library(imager)
library(raster)
library(EBImage)
# 

# gerando as imagens
s <- 50
x <- 100
y <- 100
xx <- matrix(1:256, nrow = 256, ncol = 256)
yy <- t(xx)

# gerando o disco
disc <- sqrt((xx - x)^2 + (yy - y)^2)
im_disc <- (disc < s) 
displayImg(im_disc)

# obtendo somente a região delimitada pelo retangulo envolvente
im_disc <- as.cimg(im_disc)
px_disc <- im_disc > 0.5
bounding_box_disc <- crop.bbox(im_disc, px_disc) %>% plot

# comprimento
comp <- dim(bounding_box_disc)[1]
# largura
larg <- dim(bounding_box_disc)[2]

alongamento <- larg/comp; alongamento


##################################################################
# Envoltoria convexa
library(grDevices)
x <- c()
y <- c()
fig <- rbind(
  c(0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 1, 1, 1, 1, 0),
  c(0, 0, 0, 0, 0, 0, 1, 0),
  c(0, 1, 1, 1, 1, 1, 1, 0),
  c(0, 1, 0, 0, 1, 0, 0, 0),
  c(0, 1, 0, 0, 1, 0, 0, 0),
  c(0, 1, 1, 1, 1, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0))
displayImg(fig)
dims <- dim(fig)

# convertendo imagem binaria para nuvem de pontos
for (i in 1:dims[1]) {
  for (j in 1:dims[2]) {
    if(fig[i,j]){
      x <- c(x,i)
      y <- c(y,j)
} } }
X <- cbind(x,y)

# calcula a envoltória convexa
hpts <- chull(X)
# plota os ponto da envoltória convexa
plot(X[hpts, c(2,1)], cex = 0.5, xlim = c(1,8), ylim = c(8,1))
# inclui ponto inicial ao final da lista para fechar o polígono
hpts <- c(hpts, hpts[1])
# plota as linhas ligando a envoltória convexa
lines(X[hpts, c(2,1)])

##################################################################
# Convexidade

# pontos
cnvx_pts <- X[hpts,]

perimetro_chull <- 0
for(i in 1:(dim(cnvx_pts)[1]-1)) {
 # distância entre dois pontos consecutivos
 perimetro_chull <- perimetro_chull +
   pointDistance(c(cnvx_pts[i,1], cnvx_pts[i,2]),
           c(cnvx_pts[i+1,1], cnvx_pts[i+1,2]),
           lonlat = FALSE)
}
# recuperando o formato original da imagem
im_disc <- (disc < s) 
# computa caracteristicas de forma
fts_disc <- computeFeatures.shape(im_disc); fts_disc

# computa convexidade
convexidade <- perimetro_chull/fts_disc[2]; convexidade

##################################################################
# Momentos
# carregando pacotes
#  

# carregando base de dados
# as imagens são carregadas em img
# os rotulos em labels

load(file="bacteria.rda")
# visualizacao da imagem 1
displayImg(img[[1]])

momento <- function(M, p, q) {
  r <- 0
  for (i in 1:nrow(M))
    for (j in 1:ncol(M))
      r <- r + i^p * j^q * M[i,j]  
  return(r)
}
# calculando a area
area <- function(M) {
  momento(M, 0, 0)
}

m00 <- area(img[[1]]);m00
# calculando momento m10 da imagem 1
m10 <-momento(img[[1]], 1, 0);m10

# calculando centroide
centroide <- function(M) {
  c(momento(M, 1, 0) / momento(M, 0, 0),
    momento(M, 0, 1) / momento(M, 0, 0))
}

centroide_coord <- centroide(img[[1]])
centroide_coord


# calculando momentos centrais
momento <- function(M, p, q, central = FALSE) {
  r <- 0
  if (central) {
    c <- centroide(M)
    x <- c[1]
    y <- c[2]
  } else {
    x <- 0
    y <- 0
  }
  for (i in 1:nrow(M))
    for (j in 1:ncol(M))
      r <- r + (i - x)^p * (j - y)^q * M[i,j]  
  return(r)
}

# momento M00
M00 <- momento(img[[1]], 0,0,
              central = TRUE)
# momento M10
M10 <- momento(img[[1]], 1,0,
               central = TRUE)
M00; M10

# calculando terceiro momento central (assimetria)
# momento M33
assimetria <- function(M) {
  momento(M, 3, 3, central = TRUE)
}

M33 <- assimetria(img[[1]]);M33

# calculando quarto momento central (curtose)
# momento M44
curtose <- function(M) {
  momento(M, 4, 4, central = TRUE)
}

M44 <- curtose(img[[1]]); M44



##################################################################
# Criando um ranking com descritores de forma


# incluindo implementações extras
source("cbir_aula3.R")

# caminho da pasta com as imagens
path <- "./transito"
# listando os caminhos das imagens
path_imgs <- list.files(path, full.names = TRUE)
# listando apenas os nomes das imagens
name_imgs <- list.files(path, full.names = FALSE)
names(path_imgs) <- name_imgs



# imagem de consulta
consulta = './transito/2.jpg'
# gerando o ranking
ranking_aula3 <- get_ranking_by_shape_moment(path_imgs, 
                                             name_imgs, 
                                             consulta)

# plotando as imagens retornadas
par(mfrow = c(3,3), mar = rep(2, 4))
for(img in ranking_aula3[1:9]){
  plot(load.image(path_imgs[img]), axes = FALSE,
       main = img)
}




