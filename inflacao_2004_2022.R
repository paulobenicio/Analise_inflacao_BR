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
