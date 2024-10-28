# Defina o seu projeto no Google Cloud
set_billing_id("projetoapp-340617")
#bq_auth(email = "gustaveconomiafurg@gmail.com", scopes = "https://www.googleapis.com/auth/bigquery")
# Para carregar o dado direto no R
query <- "
SELECT
    dados.ano as ano,
    dados.id_eleicao as id_eleicao,
    dados.tipo_eleicao as tipo_eleicao,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.id_municipio_tse AS id_municipio_tse,
    diretorio_id_municipio_tse.nome AS id_municipio_tse_nome,
    dados.cargo as cargo,
    dados.aptos as aptos,
    dados.aptos_totalizadas as aptos_totalizadas,
    dados.comparecimento as comparecimento,
    dados.abstencoes as abstencoes,
    dados.votos_validos as votos_validos,
    dados.votos_brancos as votos_brancos,
    dados.votos_nulos as votos_nulos,
    dados.votos_nominais as votos_nominais,
    dados.votos_legenda as votos_legenda,
    dados.proporcao_comparecimento as proporcao_comparecimento,
    dados.proporcao_votos_validos as proporcao_votos_validos,
    dados.proporcao_votos_brancos as proporcao_votos_brancos,
    dados.proporcao_votos_nulos as proporcao_votos_nulos
FROM `basedosdados.br_tse_eleicoes.detalhes_votacao_municipio` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
LEFT JOIN (SELECT DISTINCT id_municipio_tse,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio_tse
    ON dados.id_municipio_tse = diretorio_id_municipio_tse.id_municipio_tse
"

df = read_sql(query, billing_project_id = get_billing_id()) %>% 
  filter(ano %in% c(1994, 1998, 2002) & cargo == "governador")

