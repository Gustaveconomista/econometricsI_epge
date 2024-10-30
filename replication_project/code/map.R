# Carregar o arquivo de dados brdb.dta
data = read.dta(here("replication_project/data", "brdb.dta"))

# Filtrar dados, excluindo "Litigated Zone"
data = data %>% filter(ADMIN_NAME != "Litigated Zone")

# Criar a variável inst_c com valores baseados em ADMIN_NAME
data = data %>% 
  mutate(inst_c = ifelse(ADMIN_NAME == "Acre", 0.4481768,
                         ifelse(ADMIN_NAME == "Alagoas", 0.0718791,
                                ifelse(ADMIN_NAME == "Amazonas", 0.5444938,
                                       ifelse(ADMIN_NAME == "Amapa", 0.5981804,
                                              ifelse(ADMIN_NAME == "Bahia", 0.3632319,
                                                     ifelse(ADMIN_NAME == "Ceara", 0.4165474,
                                                            ifelse(ADMIN_NAME == "Distrito Federal", 1,
                                                                   ifelse(ADMIN_NAME == "Espirito Santo", 0.5625693,
                                                                          ifelse(ADMIN_NAME == "Goias", 0.3886635,
                                                                                 ifelse(ADMIN_NAME == "Maranhao", 0.2972295,
                                                                                        ifelse(ADMIN_NAME == "Minas Gerais", 0.4609004,
                                                                                               ifelse(ADMIN_NAME == "Mato Grosso do Sul", 0.4614621,
                                                                                                      ifelse(ADMIN_NAME == "Mato Grosso", 0.3511314,
                                                                                                             ifelse(ADMIN_NAME == "Para", 0.4281587,
                                                                                                                    ifelse(ADMIN_NAME == "Paraiba", 0.2953183,
                                                                                                                           ifelse(ADMIN_NAME == "Pernambuco", 0.4842924,
                                                                                                                                  ifelse(ADMIN_NAME == "Piaui", 0.2258026,
                                                                                                                                         ifelse(ADMIN_NAME == "Parana", 0.4492917,
                                                                                                                                                ifelse(ADMIN_NAME == "Rio de Janeiro", 0.8993561,
                                                                                                                                                       ifelse(ADMIN_NAME == "Rio Grande do Norte", 0.3123622,
                                                                                                                                                              ifelse(ADMIN_NAME == "Rondonia", 0.3648933,
                                                                                                                                                                     ifelse(ADMIN_NAME == "Roraima", 0.6315905,
                                                                                                                                                                            ifelse(ADMIN_NAME == "Rio Grande do Sul", 0.5240926,
                                                                                                                                                                                   ifelse(ADMIN_NAME == "Santa Catarina", 0.3744836,
                                                                                                                                                                                          ifelse(ADMIN_NAME == "Sergipe", 0.3345228,
                                                                                                                                                                                                 ifelse(ADMIN_NAME == "Sao Paulo", 0.7631133,
                                                                                                                                                                                                        ifelse(ADMIN_NAME == "Tocantins", 0.1472582, NA))))))))))))))))))))))))))))

# Ajuste para valores de inst_c para estados específicos
data = data %>%
  mutate(inst_c = ifelse(ADMIN_NAME %in% c("Alagoas", "Rio de Janeiro", "Amapa", "Roraima"), 
                         1,
                         inst_c))

state_df = read_state(year = 2000) %>% 
  mutate(name_state = stri_trans_general(name_state, "Latin-ASCII"),
         name_state = ifelse(str_detect(name_state, "Do"),
                             str_replace_all(name_state, "Do", "do"),
                             ifelse(str_detect(name_state, "De"),
                                    str_replace(name_state, "De", "de"),
                                    name_state)))
state_coord = left_join(state_df, data, by = c("name_state" = "ADMIN_NAME"))
br_coord = left_join(br_coord, data, by = c("state_id" = "id"))

# Criar o mapa com ggplot2
no_axis = theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

ggplot() +
  geom_sf(data=state_coord, aes(fill=inst_c), color= "black", size=.15) +
  labs(subtitle="Share of electorate using electronic voting: 1998 election", size=8) +
  scale_fill_distiller(name="", limits = c(0.1, 1), direction = 1, na.value = "white") +
  theme_minimal() +
  no_axis
