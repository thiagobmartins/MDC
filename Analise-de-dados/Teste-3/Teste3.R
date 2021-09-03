########################################
# Teste 3 - Trabalho Final         
# Nome(s): Daniele Monetenegro
#          Rodrigo Dantas da Silva 
#          Thiago Bruschi Martins
########################################

library(dplyr)
library(ggplot2)

########### Input #########################
con <- url('https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv')
names <- c("horario", "temp", "vento", "umid", "sensa")
types <- c('datet')
df_raw <- read.csv(con, header = FALSE , sep = ";", col.names = names, skip=42082, nrows=311922, fill=TRUE,colClasses=)
head(df_raw)
tail(df_raw)

############ Pré-processamento ################
cepagri <- df_raw

######## Feature Eng ########
sapply(cepagri, class)

# Dados faltantes
cepagri <- cepagri |> mutate(dados_faltantes = (is.na(cepagri$sensa) | is.na(cepagri$vento) | is.na(cepagri$umid)))

# Mês e Ano
cepagri[ , 1] <- as.POSIXct(cepagri[ , 1], format = '%d/%m/%Y-%H:%M', tz = "America/Sao_Paulo") # converte a primeira coluna para o tipo data
cepagri$horario <- as.POSIXlt(cepagri$horario) # ajusta o tipo da coluna horario
cepagri$ano <- unclass(cepagri$horario)$year + 1900 # cria nova feature
cepagri$mes <- unclass(cepagri$horario)$mon + 1 # cria nova feature

# Nome do mês
meses <- c("Jan","Fev","Mar", "Abr","Mai","Jun", "Jul","Ago","Set", "Out","Nov","Dez")
cepagri[, 'nome_mes'] <- meses[cepagri$mes]
cepagri[, 'nome_mes'] <- factor(cepagri$nome_mes, 
                                levels=meses, 
                                ordered = TRUE)

# Estacao do ano
cepagri[, 'estacao'] <- factor(case_when(
                                  cepagri$mes >= 1 & cepagri$mes<= 3 ~ 'Verao',
                                  cepagri$mes >= 4 & cepagri$mes<= 6 ~ 'Outono',
                                  cepagri$mes >= 7 & cepagri$mes<= 9 ~ 'Inverno',
                                  TRUE ~ 'Primavera'), levels=c('Verao', 'Outono', 'Inverno', 'Primavera'), ordered = TRUE)

summary(cepagri)
# Através do Summary podemos verificar que há muitos dados faltantes, outliers e 
# algum problema com o tipo da coluna temperatura

# Contagem de dados faltantes por mês
tapply(cepagri$dados_faltantes, list(cepagri$ano, cepagri$mes), sum)

# Podemos ver na tabela gerada que há muitos dados faltantes em 2020.
# Vamos explorá-los graficamente
cepagri_2020 <- cepagri |> filter(ano==2020)

ggplot(cepagri_2020, aes(x=nome_mes, fill=dados_faltantes)) +
  geom_bar() +
  ggtitle('Leituras em 2020') +
  ylab('Nº de leituras') +
  xlab('Mês de 2020')

# Através deste gráfico podemos observar que houve muitos problemas de leitura durante o período de  
# quarentena mais restrita em 2020, meses de maio a outubro. A partir de Novembro, quando o acesso a Unicamp
# começou a ser possível, os erros foram corrigidos e o número de leituras válidas começou a aumentar


## Data Cleaning
# Agora vamos tratar os dados faltantes e outliers. Como em alguns meses há muitas leituras faltantes,
# não há muito o que fazer além de deletar esses dados

# Através do Summary podemos verificar que a sensação térmica possui outliers de valor 99.9
summary(cepagri)

(antes <- nrow(cepagri))
cepagri <- filter(cepagri, !dados_faltantes)
(depois <- nrow(cepagri))

perc_removidas <- round((antes-depois)/(antes+depois)*100.0, 2)
print(paste("Total de linhas lidas:", antes))
print(paste("Portentagem de linhas nulas removidas:", perc_removidas,'%'))

# Busca por outliers
cepagri$temp <- as.numeric(cepagri$temp) # converte temperatura para numérico, agora que as mensagens de erro foram retiradas
cepagri <- filter(cepagri, sensa < 99.9)  # remove outliers de temperatura que estão com o valor 99.9
cepagri <- filter(cepagri, umid > 0)  # Remove outliers de umidade do ar com valor 0

# Agora podemos verificar que o range de todas as variáveis está correto
summary(cepagri)

# Função que retorna os valores repetidos k vezes consecutivamente
# Obs: Função retirada dos slides da aula 4
consecutive <- function(vector, k = 2) {
  n <- length(vector)
  result <- logical(n)
  for (i in k:n)
    if (all(vector[(i-k+1):i] == vector[i]))
      result[(i-k+1):i] <- TRUE
  
  return(result)
}

# Remoção de observações repetidas nos 4 campos de entrada, o que muito provavelmente indica um erro
entrada = cepagri$temp
r <- logical(length(entrada))
repetidos <- (consecutive(cepagri$temp) & consecutive(cepagri$vento) & consecutive(cepagri$umid) & consecutive(cepagri$sensa) )
sum(repetidos)
nrow(cepagri)

# Dias em que houve pelo menos um erro de leitura (indicado pela observação repetida)
dias_com_erro <- unique(as.Date(cepagri[repetidos , 1])) # Armazenando este vetor para uma análise posterior

#cepagri_old <- cepagri
cepagri <- cepagri[!repetidos,] # apenas os dados validos

#### Análise dos dados

# Análises: pelo menos quatro análises distintas, sendo pelo menos uma delas 
# comparando os dados de pelo menos 4 anos diferentes.

# Analise 1) Ano de 2019
cepagri_2019 <- filter(cepagri, ano==2019)

ggplot(cepagri_2019, aes(x=vento, y=umid)) + 
  geom_point(aes(colour=temp), alpha=.3) +
  scale_color_continuous(low = "blue", high = "red") +
  labs(colour='Temp. (ºC)', title = 'Umidade, Temperatura e Vento em 2019') +
  facet_wrap( ~ nome_mes) +
  ylab('Umidade Relativa do Ar (%)') +
  xlab('Velocidade do Vento (km/h') +
  theme_gray()

# Tabela resumo de 2019
cepagri_2019 |> group_by(estacao) |>
  summarize(mean(umid), mean(temp), mean(vento))

# Analise 2) Estações
cepagri_inverno <- filter(cepagri, estacao=='Inverno', ano<=2018)

tapply(cepagri_inverno$umid, cepagri_inverno$ano, mean)
tapply(cepagri_inverno$temp, cepagri_inverno$ano, mean)
tapply(cepagri_inverno$vento, cepagri_inverno$ano, mean)

ggplot(cepagri_inverno, aes(x=vento, y=temp)) + 
  geom_point(aes(colour=umid), alpha=.3) +
  scale_color_continuous(low = "pink", high = "Blue") +
  labs(colour='Umidade (%)', title = 'Invernos de 2015 a 2018') +
  facet_wrap( ~ ano) +
  ylab('Temperatura (ºC)') +
  xlab('Velocidade do Vento (km/h') +
  theme_gray()


# Análise 3) Densidades das estações de 2015 a 2019

  cepagri_2019 <- filter(cepagri, ano <=2019)
  ggplot(cepagri, aes(x=temp)) +
    geom_density(alpha=0.3) +
    facet_wrap( ~ estacao) +
    ylab(' Densidade') +
    xlab('Temperatura (ºC)') +
    ggtitle('Densidade das estações de 2015 a 2019')
  theme_light()
  
  cepagri_2019 |> group_by(estacao) |>
    summarize(mean(temp), sd(temp))

# Análise 4) Diferenca entre temperatura e sensacao termica
  cepagri[, 'diferenca'] <- cepagri$temp - cepagri$sensa
  
  cepagri_4 <- filter(cepagri, ano == 2018)
  ggplot(cepagri_4 , aes(x = nome_mes, y = diferenca, fill=estacao)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title='Diferença entre Temperatura e Sensação em 2018') +
  ylab('(Temperatura - Sensação térmica)') + 
  xlab('Mês')
      
  cepagri_2016 <- filter(cepagri, ano ==2016)
  ggplot(cepagri_2016, aes(x=vento, y=diferenca)) +
    scale_color_continuous(low = "pink", high = "blue") +
    geom_point(aes(colour=umid)) +
    facet_wrap( ~ nome_mes) +
    labs(colour='Umidade Rel. do Ar (%)', title = 'Temperatura - Sensacao Termica em 2016') +
    ylab(' Diferença') +
    xlab('Velocidade do Vento (km/h') +
    theme_gray()
  
  