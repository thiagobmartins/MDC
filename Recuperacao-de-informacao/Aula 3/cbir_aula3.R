#########
# Color Histograms
#########
# Extraindo o vetor de carecterísticas, juntando os
# histogramas de cada banda (255 dimensoes por cada banda)
extract_color_hist <- function(path_img){
  img <- load.image(path_img)
  r <- hist(img[,,1]*255, plot=FALSE, breaks=0:255)$counts
  g <- hist(img[,,2]*255, plot=FALSE, breaks=0:255)$counts
  b <- hist(img[,,3]*255, plot=FALSE, breaks=0:255)$counts
  return(c(r, g, b))
}


#########
# Color Moments
#########
extract_color_moments <- function(path_img){
  # carregando a imagem
  img <- load.image(path_img)
  # atualizando os valores dos canais 
  img[,,1:3] <- img[,,1:3]*255
  # computando o número de pixels
  n <- dim(img)[1] * dim(img)[2]
  # computando o momento 1 para os 3 canais
  m1 <- c(sum(img[,,1]), sum(img[,,2]), sum(img[,,3]))/n

  # cálculo comum ao demais momentos de cor
  I_E <- list(img[,,1] - m1[1], img[,,2] - m1[2], img[,,3] - m1[3])
  # computando o momento 2 para os 3 canais
  m2 <- c(sum(I_E[[1]]^2), sum(I_E[[2]]^2), sum(I_E[[3]]^2))/n
  m2 <- m2^(1/2)
  
  # computando o momento 3 para os 3 canais
  cuberoot <- function(x)sign(x)*abs(x)^(1/3)
  m3 <- c(sum(I_E[[1]]^3), sum(I_E[[2]]^3), sum(I_E[[3]]^3))/n
  m3 <- cuberoot(m3)

  # computando o momento 4 para os 3 canais
  m4 <- c(sum(I_E[[1]]^4), sum(I_E[[2]]^4), sum(I_E[[3]]^4))/n
  m4 <- m4^(1/4)  
  
  return(c(m1, m2, m3, m4))
}

get_ranking_by_moment <- function(M, query, w){
  moment_distance <- function(img_a, img_b, w) {
    return (sum(abs(img_a-img_b) * w))
  }
  d <- lapply(rownames(M), function(x) moment_distance(M[query,], M[x,], w))
  d <- unlist(d)
  names(d) <- rownames(M)
  return(names(sort(d)))
}

#########
# Color Coherence Vector
#########
get_connected_componnents <- function(img) {
  n <- dim(img)[1]
  m <- dim(img)[2]
  C <- matrix(0, nrow = n, ncol = m)
  C_colors <- list()
  K <- 1
  for (a in 1:n) {
    for (b in 1:m) {
      if (C[a,b] == 0){
        C[a,b] <- K
        C_colors[length(C_colors) + 1] <- list(img[a,b,])
        Q <- list(c(a,b))
        q <- 1
        while(q <= length(Q)){
          i <- Q[[q]][1]
          j <- Q[[q]][2]
          for (x in -1:1) {
            for (y in -1:1) {
              i_ <- i + x
              j_ <- j + y
              if (i_ > 0 && i_ <= n && j_ > 0 && j_ <= m) {
                if(identical(img[i,j,], img[i_,j_,]) && C[i_, j_] == 0){
                  Q[[length(Q) + 1]] <- c(i_, j_)
                  C[i_,j_] <- K
                }
              }
            }
          }
          q <- q + 1
        }
        K <- K + 1
      }
    }
  }
  return(list(C, C_colors))
};


extractCCV <- function(path_img, tau = -1) {
  img <- load.image(path_img)
  n <- dim(img)[1][1]
  m <- dim(img)[2][1]
  if (tau == -1) {
    tau <- round(n*m*0.01)
  }

  # discretizando as cores
  img[,,1] <- (img[,,1]*255) %/%64  
  img[,,2] <- (img[,,2]*255) %/%64  
  img[,,3] <- (img[,,3]*255) %/%64  
  
  # matriz com as components conexas 
  result <- get_connected_componnents(img[,,])
  C <- result[[1]]
  C_colors <- result[[2]]

  # conta o tamanho das componentes
  size_comps <- tapply(unlist(C),factor(unlist(C)),length)

  # criação dos descritores
  alpha <- rep(0, 64)
  beta <- rep(0, 64)
  dim(alpha) <- c(4,4,4)
  dim(beta) <- c(4,4,4)

  # atualização dos descritores
  for (k in 1:length(size_comps)){
    pos_i <- C_colors[[k]][1] + 1
    pos_j <- C_colors[[k]][2] + 1
    pos_k <- C_colors[[k]][3] + 1
    if (size_comps[k] >= tau){
      alpha[pos_i, pos_j, pos_k] <- alpha[pos_i, pos_j, pos_k] + size_comps[[k]]
    }else{
      beta[pos_i, pos_j, pos_k] <- beta[pos_i, pos_j, pos_k] + size_comps[[k]]
    }
  }
  return(list(alpha, beta))
}

distanceCCV <- function(descA, descB){
  
  alphaA <- descA[[1]]
  alphaB <- descB[[1]]
  betaA <- descA[[2]]
  betaB <- descB[[2]]
  
  # Comparação feita usando distância L1 
  return (sum(abs(alphaA - alphaB) + abs(betaA - betaB)))
}


get_ranking_CCV <- function(path_img, name_imgs, query){
  d <- c()
  descQuery <- extractCCV(path_img[query])
  for(i in 1:length(path_img)){
    cat("processing", path_img[i], "...\n")
    descImg <- extractCCV(path_img[i])
    d[i] <- distanceCCV(descImg, descQuery)
  }
  names(d) <- name_imgs
  d <- sort(d)
  return(names(d))
}


#########
# Co-ocurrence Matrix
#########
get_ranking_cooccurrence <- function(path_imgs, name_imgs, query){

  # carregando a imagem de consulta
  query <- readTIFF(path_imgs[query], as.is = TRUE, info = TRUE)
  # obtendo o descritor para a imagem de consulta
  descQuery <- c(glcm(query)$glcm$ave)
  
  # carrega uma imagem, extrai o descritor e computa a distancia 
  # para a imagem de consulta
  get_distance <- function(img) {
    cat("processing ", img, "...\n")
    img <- readTIFF(img, as.is = TRUE, info = TRUE)
    descImg <- c(glcm(img)$glcm$ave)
    return(dist(t(data.frame(descImg, descQuery)), 
                method = "manhattan"))
  }
  # computa as distancias da imagem de consulta para todas as imagens
  distances <- unlist(lapply(path_imgs, get_distance))
  # recuperando as referencias das imagens
  names(distances) <- name_imgs
  # retornando o ranking (referencias das imagens ordenadas)
  return(names(sort(distances)))
}


#########
# Co-ocurrence Matrix - Haralick
#########
get_ranking_haralick <- function(path_imgs, name_imgs, query){
  d <- c()
  query <- glcm(readTIFF(path_imgs[query], as.is = T, info = T))
  descQuery <- haralick(query)[,"ave"]
  for(i in 1:length(path_imgs)){
    cat("processing", path_imgs[i], "...\n")
    img_glcm <- glcm(readTIFF(path_imgs[i], as.is = T, info = T))
    descImg <- haralick(img_glcm)[, "ave"]
    d[i] <- dist(t(data.frame(descImg, descQuery)), method = "manhattan")
  }
  names(d) <- name_imgs
  d <- sort(d)
  return(names(d))
}

#########
# Border/Interior Classification
#########
border_intern <- function(img){
  n <- dim(img)[1]
  m <- dim(img)[2]
  
  viz4 <- list(c(-1,0), c(0,-1), c(0,1), c(1,0))  
  C <- matrix(F, nrow = n, ncol = m)
  
  for (i in 1:n){
    for (j in 1:m){
      interno <- T
      for (viz in viz4){
        i_ <- i + viz[1]
        j_ <- j + viz[2]
        if (i_ > 0 && i_ <= n && j_ > 0 && j_ <= m) {
          if(!identical(img[i,j], img[i_,j_])){
            interno = F
            break
          }
        }
      } 
      C[i,j] <- interno
    }
  }
  
  return(C)
}

BIC <- function(path_img){
  img <- load.image(path_img)
  n <- dim(img)[1][1]
  m <- dim(img)[2][1]
  
  # discretizando as cores
  img[,,1] <- (img[,,1]*255) %/% 64  
  img[,,2] <- (img[,,2]*255) %/% 64  
  img[,,3] <- (img[,,3]*255) %/% 64  
  img <- img[,,,]
  int_img <- matrix(0, nrow=n, ncol=m)
  for (i in 1:n){
    for (j in 1:m){
      int_img[i,j] <- img[i,j,3] + img[i,j,2]*4 + img[i,j,1]*16 + 1
    }
  }
  
  C <- border_intern(int_img)  
  
  intern_hist <- hist(int_img[C], plot=FALSE, breaks=0:64)$counts
  border_hist <- hist(int_img[!C], plot=FALSE, breaks=0:64)$counts
    
  return (c(intern_hist, border_hist))
}

get_ranking_BIC <- function(path_img, name_imgs, query){
  descQuery <- BIC(path_img[query])
  get_distance <- function(img) {
    cat("processing ", img, "...\n")
    # extraindo o vetor de caracteristica com o método BIC
    descImg <- BIC(img)
    return(dist(t(data.frame(descImg, descQuery)), 
                method = "manhattan"))
  }
  # computa as distancias da imagem de consulta para todas as imagens
  distances <- unlist(lapply(path_img, get_distance))
  # recuperando as referencias das imagens
  names(distances) <- name_imgs
  # retornando o ranking (referencias das imagens ordenadas)
  return(names(sort(distances)))
}

#########
# Momentos: Forma
#########

# calculando centroide
centroide <- function(M) {
  c(momento(M, 1, 0) / momento(M, 0, 0),
    momento(M, 0, 1) / momento(M, 0, 0))
}

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

Momentos <-function(path_img){
  img <- load.image(path_img)
  img <- grayscale(img)[,,1,1]
  features <-NULL
  for(i in 0:2){
    for(j in 0:2){
      #adiciona um novo momento como caracteristica no vetor de caracteristicas da imagem
      features <- cbind(features,momento(img, i,j, central=TRUE))
    }
  }
return(features)
}

get_ranking_by_shape_moment <- function(path_img, name_imgs, query){
  distL1_vect <- function(x, y){
    return (sum(abs(x-y)))
  }

  d <- c()
  descQuery <- Momentos(query)
  for(i in 1:length(path_img)){
    descImg <- Momentos(path_img[i])
    print(i)
    d[i] <- distL1_vect(descImg, descQuery)
  }
  names(d) <- name_imgs
  d <- sort(d)
  return(names(d))
}


#########
# LBP UNIFORME
#########

extract_lbp_image <- function(path_img){
  img <- load.image(path_img)
  cat("processing ", path_img, "...\n")
  r1 <- lbp(img[,,1,1],1)
  lbp_uniforme <- hist(r1$lbp.u2, plot=FALSE, breaks=59)$counts
  return(c(lbp_uniforme))
}

###########
# AUX Functions IM package
###########
histeq <- function(I) {
  I = (I-min(I))/(max(I)-min(I))
  I = round(I*255);
  
  G =256;	  
  H =array(0:255,256);		
  T =array(0,256);
  
  H = apply(H,1, function(z){ sum(I==z) });
  
  for (i in 2:length(H)){
    H[i]= H[i-1]+H[i]	
  }
  
  T = H*(G-1)/length(I);
  
  for (i in 1:length(I)){
    I[i]=T[I[i]+1]
  }
  
  return(I)
}

rotate270 <- function(img) {
  im <- as.data.frame(t(img));
  im <- rev(im);
  im <- as.matrix(im);
  return(im)
}

displayImg <- function(img) {
  #if image is not grayscale, convert to grayscale
  if(length(dim(img))>2) {
    img = rowSums(img, dims=2)/3
  }
  if(length(dim(img))==2) {
    levels = seq(0,1,.0000001);
    g = gray(levels);
    #rotate image so that it appears aligned
    img = rotate270(img);
    #perform histogram equalization on displayed image
    img <- histeq(img);
    par(mfrow = c(1,1))
    image(img,col=g,axes=FALSE);
  } else {
    return("problem with image format")
  }
}


