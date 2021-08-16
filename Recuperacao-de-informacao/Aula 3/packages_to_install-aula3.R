##################################################################
# Mineração de Dados Complexos -- MDC 2021 
# Recuperação de Informação
# Pacotes extras para a Aula 3 - Recuperação de Imagens
# 
# 
# 
# Abra este arquivo com o Rstudio e execute cada 
# linha separadamente. Caso encontre algum erro entre 
# em contato com os monitores. Lembre-se de indicar
# aos monitores o seu sistemaa operacional e a versão 
# do R instalada. 
##################################################################



# Instalando o pacote 
install.packages("imager")
# Carregando o pacote
library(imager)
# Mensagem esperada:
# Loading required package: magrittr

# Attaching package: ‘imager’

# The following object is masked from ‘package:magrittr’:

#     add

# The following objects are masked from ‘package:stats’:

#     convolve, spectrum

# The following object is masked from ‘package:graphics’:

#     frame

# The following object is masked from ‘package:base’:

#     save.image

# Dependências no Ubuntu
# sudo apt-get install libfftw3-dev libtiff5-dev
# sudo apt-get install libx11-dev  

# Dependência no MacOS
# Aplicativo xQuartz disponível em https://www.xquartz.org/


# Instalando o pacote 
install.packages("OpenImageR")
# Carregando o pacote
library(OpenImageR)
# Mensagem esperada:
# (Sem mensagem)

# Instalando o pacote
install.packages("IM_1.0.tar.gz", repos = NULL)
# Carregando o pacote
library(IM)
# Mensagem esperada:
# Loading required package: png
# Loading required package: jpeg
# Loading required package: bmp


# Instalando o pacote 
install.packages("gridExtra")
# Carregando o pacote
library(gridExtra)
# Mensagem esperada:
# (Sem mensagem)


# Instalando o pacote 
install.packages("wvtool")
# Carregando o pacote
library(wvtool)
# Mensagem esperada:
# (Sem mensagem)


# Instalando o pacote 
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("EBImage")
# Carregando o pacote
library(EBImage)
# Mensagem esperada:
# (Sem mensagem)


# Instalando o pacote 
install.packages("raster")
# Carregando o pacote
library(raster)
# Mensagem esperada:
# Loading required package: sp
#
# Attaching package: ‘sp’
#
# The following object is masked from ‘package:imager’:
#
#     bbox
#
# Attaching package: ‘raster’
#
# The following object is masked from ‘package:IM’:
#
#     flip
#
# The following object is masked from ‘package:magrittr’:
#
#     extract
#
# The following object is masked from ‘package:wvtool’:
#
#     crop

# Instalando o pacote 
install.packages("grDevices")
# Carregando o pacote
library(grDevices)
# Mensagem esperada:
# (Sem mensagem)

