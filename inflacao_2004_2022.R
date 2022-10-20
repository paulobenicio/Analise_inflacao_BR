##### CONFIGURACOES INICIAS #####

#install.packages("tidyverse")
#install.packages("sidrar")

library(tidyverse)
library(sidrar)

# Tabela 1737 do SIDRA - acumulado dos ultimos 12 meses

ipca_raw <- sidrar::get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202")
glimpse(ipca_raw) #versão transposta da versão print

##### TRATAMENTO DOS DADOS #####

data_inicial <- lubridate::as_date("2004-01-01")

ipca <- ipca_raw %>% dplyr::select("data" = "Mês (Código)", "ipca" = "Valor") %>% 
  dplyr::mutate(data = lubridate::ym(data)) %>%  
    # Aqui, utilizaremos a função ym 
    #do pacote lubridate para alterar os dados da coluna data para o tipo date 
    #está em formato character). %>% 
  dplyr::filter(data >= data_inicial) %>% 
  dplyr::as_tibble()
    #Por fim, utilizamos a função as_tibble para alterar a estrutura de dados 
    #(a classe) do objeto para tibble, que é uma classe similar ao data frame usual,
    #entretanto com mais informações.

##### GRÁFICOS #####

summary(ipca) #Minimo, 1º Quadrante, Mediana, Media, 3º Quadrante e Máximo

#Gráfico de evolução e tendência
ipca %>% 
  ggplot2::ggplot(aes(x = data, y = ipca)) +
  theme_minimal() + #Mudar Tema
  ggplot2::geom_line(color = "cadetblue4") +
  ggplot2::geom_smooth(color = "deepskyblue4") + #Linha de tendência
  labs(x = "Ano", y = "Variação percentual IPCA", 
       title = "Evolução do IPCA no Brasil", subtitle = "De Jan/2004 a Set/22") +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) #Posição do title e subt

#Histograma do IPCA
ipca %>% 
  ggplot2:: ggplot(aes(x = ipca)) +
  ggplot2::geom_histogram(color = "white", fill = "deepskyblue4") +
  theme_minimal() +
  labs(x = "Ano", title = "Distribuição da variação do IPCA", 
       subtitle = "De Jan/2004 a Set/22") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Boxplot IPCA
ipca %>% 
  ggplot2::ggplot(aes(y = ipca)) +
  geom_boxplot() +
  labs(y = "Variação percentual IPCA", title = "Quartis da variação do IPCA", 
       subtitle = "De Jan/2004 a Set/22") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

##### MODELAGEM #####

desocupacao_raw <- sidrar::get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")

desocupacao <- desocupacao_raw %>% 
  dplyr::select("data" = "Trimestre Móvel (Código)", "desocupacao" = "Valor") %>% 
  dplyr::mutate(data = lubridate::ym(data)) %>% 
  dplyr::as_tibble()

summary(desocupacao)

df_dados <- ipca %>% 
  inner_join(desocupacao, by = "data")

df_dados %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = data, y = ipca,
                                  color = "IPCA - 12 meses"),
                     size = 1) +
  ggplot2::geom_line(ggplot2::aes(x = data, y = desocupacao,
                                  color = "Taxa de Desocupação"),
                     size = 1)+
  scale_color_manual(values = c("#282f6b", "#b22200")) +
  labs(y = "Variação Percentual",x = "Ano", title = "Curva de Phillips", 
       subtitle = "Mar/2012 a Ago/22") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Regressão Simples
modelo_phillips <- lm(ipca ~ desocupacao, data = df_dados)
summary(modelo_phillips)

#Gráfico de dispersão
ggplot2::ggplot(df_dados, ggplot2::aes(x = desocupacao, y = ipca)) +
  ggplot2::geom_point()+
  geom_smooth(method = 'lm')

