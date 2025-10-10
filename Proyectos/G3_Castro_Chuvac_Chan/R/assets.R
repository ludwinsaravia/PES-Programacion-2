# R/assets.R
ALL_TICKERS <- c(
    # Acciones/ETFs principales
    "SPY", "QQQ", "IWM", "VTI", "VOO",
    
    # Internacional
    "EFA", "EEM", "VEU", "IXUS",
    
    # Bonos
    "IEF", "TLT", "AGG", "BND", "LQD",
    
    # Materias primas
    "GLD", "SLV", "DBC", "USO",
    
    # Sectores
    "XLK", "XLF", "XLV", "XLE", "XLU",
    
    # Acciones individuales (líquidas)
    "AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA"
)

# Nombres amigables (opcional)
TICKER_LABELS <- setNames(ALL_TICKERS, ALL_TICKERS)