
### Economic complexity and health - data prep
### Economic complexity and health - data prep
### Economic complexity and health - data prep
### Economic complexity and health - data prep



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(data.table)
library(economiccomplexity)
library(EconGeo)
library(RCy3)

options(scipen = 999L)

rm(list = ls(all = TRUE))

# Função para product space
source("2_scripts/function_create_product_space.R")



municipios <- read.csv("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/municipio.csv") %>% 
    select(id_municipio_6, nome) %>% 
    mutate(id_municipio_6 = as.character(id_municipio_6))


# Data --------------------------------------------------------------------

dados <- read_rds("1_data/base_20230504.RDS") %>% 
    mutate(CNES = as.character(CNES),
           PROC_REA = as.character(PROC_REA))

#unique(dados$ANO_CMPT)
#length(unique(dados$MUNIC_MOV))
#length(unique(dados$CNES))
#length(unique(dados$PROC_REA))
#length(unique(dados$n))
anos <- unique(dados$ANO_CMPT)

cytoscapePing()


for(k in anos){
    
    data <- dados %>% 
        filter(ANO_CMPT == k) %>% 
        group_by(MUNIC_MOV, PROC_REA) %>% 
        summarise(n = sum(n, na.rm = TRUE)) %>% 
        ungroup() %>% 
        rename(country = MUNIC_MOV,
               product = PROC_REA,
               value = n) %>% 
        pivot_wider(names_from = product,
                    values_from = value,
                    values_fill = 0) %>% 
        pivot_longer(cols = 2:last_col(),
                     values_to = "value",
                     names_to = "product")
    
    procs_unicos <- data %>% 
        group_by(country) %>% 
        mutate(proc = ifelse(value > 0,
                             yes = 1,
                             no = 0)) %>% 
        summarise(n.proc = sum(proc, na.rm = TRUE)) %>% 
        ungroup() %>% 
        filter(n.proc < 2) %>% 
        pull(country)
    
    muns_zero <- data %>% 
        group_by(country) %>% 
        summarise(total = sum(value, na.rm = TRUE)) %>% 
        ungroup() %>% 
        filter(total < 100) %>% 
        pull(country)
    
    products_zero <- data %>% 
        group_by(product) %>% 
        summarise(total = sum(value, na.rm = TRUE)) %>% 
        ungroup() %>% 
        filter(total == 0) %>% 
        pull(product)
    
    
    data <- data %>% 
        filter(!country %in% muns_zero & !product %in% products_zero & !country %in% procs_unicos)
    
    data <- data %>% 
        pivot_wider(names_from = product,
                    values_from = value,
                    values_fill = 0) %>% 
        {.->> data_mat} %>% 
        pivot_longer(cols = 2:last_col(),
                     values_to = "value",
                     names_to = "product")
    
    
    
    data_mat <- data_mat %>% 
        column_to_rownames(var = "country")
    
    nomes <- list(names1 = rownames(data_mat), 
                  names2 = colnames(data_mat))
    
    data_mat <- as.matrix(x = data_mat)
    dimnames(data_mat) <- nomes
    
    
    # Diversity and ubiquity --------------------------------------------------
    
    mat.rca <- location.quotient(mat = data_mat, binary = TRUE)
    
    mat.rca_long <- mat.rca %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "country") %>% 
        pivot_longer(cols = 2:last_col(),
                     values_to = "value",
                     names_to = "product")
        
    
    coocorrencias <- co.occurrence(mat = t(mat.rca), diagonal = TRUE) #c?lculo da coocorr?ncias - exige a matriz transposta
    proximidades <- relatedness(mat = coocorrencias, method = "cosine")
    
    
    
    diversificacao <- data.frame(diversificacao = EconGeo::diversity(mat = mat.rca, RCA = FALSE)) %>% 
        rownames_to_column(var = "country") %>%
        left_join(municipios, by = c("country" = "id_municipio_6")) %>% 
        select(country, nome, diversificacao)
    
    
    ubiquidade <- data.frame(ubiquidade = EconGeo::ubiquity(mat = mat.rca, RCA = FALSE)) %>% 
        rownames_to_column(var = "product")
    
    
    
    # Economic complexity -----------------------------------------------------
    
    # RCA
    mcp <- economiccomplexity::balassa_index(data = data,
                                             discrete = TRUE,
                                             cutoff = 1,
                                             country = "country",
                                             product = "product",
                                             value = "value")
    
    
    
    
    # Definindo número de interacoes ------------------------------------------
    
    lista_var_eci <- list()
    lista_var_pci <- list()
    
    for (j in seq(2, 120, 2)){
      
      print(j)
      
      complexity <- complexity_measures(mcp, iterations = j, method = "reflections")
      
      product_complexity <- data.frame(pci = complexity[[2]]) %>%
        mutate(pci = -pci) %>% 
        rownames_to_column(var = "product") 
      
      
      regions_complexity <- data.frame(eci = complexity[[1]]) %>%
        mutate(eci = -eci) %>% 
        rownames_to_column(var = "country")
      
      if (j > 2){
        
        ranking_i <- regions_complexity %>% 
          arrange(desc(eci)) %>% 
          mutate(eci_ranking_i = as.integer(frank(eci))) %>% 
          select(country, eci_ranking_i)
        
        ranking_i_pci <- product_complexity %>% 
          arrange(desc(pci)) %>% 
          mutate(pci_ranking_i = as.integer(frank(pci))) %>% 
          select(product, pci_ranking_i)
        
        
        complexity <- complexity_measures(mcp, iterations = j-2, method = "reflections")
        
        lista_var_eci[[j]] <- data.frame(eci = complexity[[1]]) %>%
          mutate(eci = -eci) %>% 
          rownames_to_column(var = "country") %>%
          arrange(desc(eci)) %>% 
          mutate(eci_ranking_im2 = as.integer(frank(eci))) %>% 
          select(country, eci_ranking_im2) %>% 
          left_join(ranking_i) %>% 
          mutate(var = eci_ranking_i-eci_ranking_im2) %>% 
          summarise(var_sum = sum(abs(var)),
                    var_mean = mean(abs(var))) %>% 
          mutate(inter = j)
        
        lista_var_pci[[j]] <- data.frame(pci = complexity[[2]]) %>%
          mutate(pci = -pci) %>% 
          rownames_to_column(var = "product") %>% 
          arrange(desc(pci)) %>% 
          mutate(pci_ranking_im2 = as.integer(frank(pci))) %>% 
          select(product, pci_ranking_im2) %>% 
          left_join(ranking_i_pci) %>% 
          mutate(var = pci_ranking_i-pci_ranking_im2) %>% 
          summarise(var_sum = sum(abs(var)),
                    var_mean = mean(abs(var))) %>% 
          mutate(inter = j)
        
        if(lista_var_eci[[j]]$var_sum[1] == 0){
            break
        }
        
      }
      
    
      
     }
    
    
    avaliacao_ranking_eci <- do.call(rbind, lista_var_eci) %>% 
      write_excel_csv2(str_c("3_results/avaliacao_ranking_ECI_", k, ".csv"))
    
    avaliacao_ranking_pci <- do.call(rbind, lista_var_pci) %>% 
      write_excel_csv2(str_c("3_results/avaliacao_ranking_PCI_", k, ".csv"))
    
    
    n.inter <- j
    
    
    
    # Complexity indexes
    complexity <- complexity_measures(mcp, iterations = n.inter, method = "reflections")
    
    
    product_complexity <- data.frame(pci = complexity[[2]]) %>%
        mutate(pci = pci) %>% 
        rownames_to_column(var = "product") %>% 
        left_join(ubiquidade) 
    
    
    
    regions_complexity <- data.frame(eci = complexity[[1]]) %>%
        mutate(eci = eci) %>% 
        rownames_to_column(var = "country") %>%
        left_join(diversificacao) %>% 
        filter(!is.na(nome))
    
    write_excel_csv2(regions_complexity, str_c("3_results/ECI_", k, ".csv"))
    write_excel_csv2(product_complexity, str_c("3_results/PCI_", k, ".csv"))
    
    
    
    a <- ggplot()+
        geom_point(data = regions_complexity, aes(y = eci, x = diversificacao)) +
        theme_gray(base_size = 12)
    
        ggsave(str_c("3_results/ECI_diversificacao_", k, ".png"), plot = a,
               height = 20, width = 20, units = "cm")
    
    
    a <- ggplot()+
        geom_point(data = product_complexity, aes(y = pci, x = ubiquidade)) +
        theme_gray(base_size = 12)
    
    ggsave(str_c("3_results/PCI_ubiquidade_", k, ".png"), plot = a,
           height = 20, width = 20, units = "cm")
    
   
    
    # Proximities
    proximities <- economiccomplexity::proximity(balassa_index = mcp,
                                                 compute = "both")
    
    
    
    
    product_network <- complexity_graph(proximity_mat = proximities[[2]], threshold = 0.5)
    
    
    edgelist_product <- as_edgelist(product_network)
    edgelist_product <- data.frame(source=V(product_network)[edgelist_product[,1]]$name,
                               target=V(product_network)[edgelist_product[,2]]$name,
                               weight=E(product_network)$weight,
                               mst=E(product_network)$mst)
    
    plot(stats::density(edgelist_product$weight))
    
    
    nodes_list_product <- product_complexity %>% 
        rename(id = product)
    
    
    write.csv(edgelist_product, str_c("3_results/PS_edgelist_", k, ".csv"), row.names = FALSE)
    write.csv(nodes_list_product, str_c("3_results/PS_nodelist_", k, ".csv"), row.names = FALSE)
    
    

    
    createNetworkFromIgraph(product_network, title = str_c("Procedures Space ", k), collection = "Procedures Space")
    
    
    gc()
}





















