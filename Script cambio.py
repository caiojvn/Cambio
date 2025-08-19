import pandas as pd
import requests
from prophet import Prophet

# ========================
# 1. Parâmetros da consulta
# ========================
url_base = "https://olinda.bcb.gov.br/olinda/servico/PTAX/versao/v1/odata/"
endpoint = "CotacaoDolarPeriodo(dataInicial=@dataInicial,dataFinalCotacao=@dataFinalCotacao)"
params = {
    "@dataInicial": "'01-01-2024'",
    "@dataFinalCotacao": "'01-01-2026'",
    "$top": "1000",
    "$format": "json"
}

# ========================
# 2. Baixar dados da API Olinda
# ========================
url = url_base + endpoint
resp = requests.get(url, params=params)
dados_json = resp.json()["value"]

# Criar DataFrame
cambio_compra = pd.DataFrame(dados_json)

# ========================
# 3. Ajustar colunas e tipos
# ========================
cambio_compra["Data"] = pd.to_datetime(cambio_compra["dataHoraCotacao"]).dt.date
cambio_compra["Compra"] = pd.to_numeric(cambio_compra["cotacaoCompra"], errors="coerce")
cambio_compra = cambio_compra[["Data", "Compra"]].sort_values("Data").reset_index(drop=True)

# ========================
# 4. Calcular média móvel
# ========================
ms_range = 14
cambio_compra["MMS_Compra"] = cambio_compra["Compra"].rolling(ms_range).mean()

# ========================
# 5. Preparar dados para Prophet
# ========================
dados_prophet = cambio_compra[["Data", "MMS_Compra"]].dropna()
dados_prophet.columns = ["ds", "y"]

# ========================
# 6. Criar e treinar modelo Prophet
# ========================
periodo = 60
m = Prophet(
    growth="linear",
    changepoint_prior_scale=0.8,
    yearly_seasonality=True,
    weekly_seasonality=False,
    daily_seasonality=False,
    interval_width=0.95
)
m.fit(dados_prophet)

# ========================
# 7. Criar datas futuras e prever
# ========================
future = m.make_future_dataframe(periods=periodo)
forecast = m.predict(future)

# ========================
# 8. Tabela final de previsões
# ========================
previsao = forecast(periodo)[["ds", "yhat", "yhat_lower", "yhat_upper"]]
previsao.columns = ["Data", "Previsao", "Minimo", "Maximo"]
previsao = previsao.round(4).reset_index(drop=True)

# ========================
# 9. Salvar como CSV
# ========================
previsao.to_csv("previsao_dolar.csv", index=False)
