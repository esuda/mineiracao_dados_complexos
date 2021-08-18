
#------------------------------------------------#
# Funcao para exibir uma imagem                  #
#------------------------------------------------#
show_image <- function(img, classe){
  displayImg(img)
  title(classe)
}

#------------------------------------------------#
# Carrega e retorna todas as imagens             #
#------------------------------------------------#
read_images <- function(path){
  name_imgs <- list.files(path, full.names = TRUE)
  all_im <- lapply(name_imgs, load.image)
  
  return(all_im)
}

#------------------------------------------------#
# Retorna ground_truth escolhida classe relevante#
#------------------------------------------------#
get_ground_truth<- function(classes, classe_relevante){
  ground_truth <- integer(length(classes))
  ground_truth[which(classes %in% classe_relevante)] <-1
  return(ground_truth)
}

#------------------------------------------------#
#                   COMBMIN                      #
#------------------------------------------------#

combmin <- function(...) {
  order(mapply(min, ...))
}

#------------------------------------------------#
#                   COMBMAX                      #
#------------------------------------------------#
combmax <- function(...) {
  order(mapply(max, ...))
}


#------------------------------------------------#
#                   COMBSUM                      #
#------------------------------------------------#

combsum <- function(...) {
  order(mapply(sum, ...))
}

#------------------------------------------------#
#                     BORDA                      #
#------------------------------------------------#

bordacount <- function(...) {
  # obtem os rankings
  rankings <- mapply(rank, list(...), SIMPLIFY = FALSE)
  # calcula a ordem baseada na soma das posições dos rankings
  return(do.call(combsum, rankings))
}

#------------------------------------------------#
#              MOMENTOS DE FORMA                 #
#------------------------------------------------#

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
