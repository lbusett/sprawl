in_shape <- "/home/lb/tmp/mappa_rischio_robi/Com2016_WGS84_g.shp" %>%
  read_vect() %>%
  dplyr::filter(COD_REG %in% c(1,3,4,5))

plot_vect(in_shape,fill_var = "COD_REG")


in_txt <- "/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_EI_R3_Risk_Biotic/2017/Txt_ensurance/ERMES_indici_rischi_biotici_20170807_054427.txt"
in_risk <- data.table::fread(in_txt) %>%
  dplyr::rename(PRO_COM = Comune, Rischio = Value)

data <- in_shape %>%
  dplyr::left_join(in_risk) %>%
  dplyr::filter(!is.na(Prodotto))

p <- plot_vect(data, fill_var = "Rischio",
          palette = "RdYlGn",
          borders_layer = get_boundaries("ITA", 2),
          borders_color = "darkred",
          borders_size = 0.5,
          title = "Rischio di Infezione da Brusone - 07/08/2017",
          direction = -1, leg_position = "bottom",
          zlims = c(0,1), outliers_style = "to_minmax")
ggsave(p, filename = "/home/lb/tmp/mappa_rischio_robi/figures/Rischio_continuo_07_Agosto.png")

data$`Classe di Rischio` <- cut(data$Rischio, breaks = c(-1, 0.33, 0.66, 1 ), labels = c("Basso",
                                                                         "Medio",
                                                                         "Alto"))
p <- plot_vect(data, fill_var = "`Classe di Rischio`",
          palette = "RdYlGn",
          borders_layer = get_boundaries("ITA", 2),
          borders_color = "darkred",
          borders_size = 0.5,
          title = "Rischio di Infezione da Brusone - 07/08/2017",
          direction = -1,
          leg_position = "bottom") + scale_fill_manual(values = c("#34B52B", "orange", "darkred"),
                                                       drop = FALSE) +
          guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
ggsave(p, filename = "/home/lb/tmp/mappa_rischio_robi/figures/Rischio_categorie_07_Agosto.png")



