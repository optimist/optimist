#' @title get_VectorAnalitico
#' @description Creates a data.frame from a csv file
#' containing VectorAnalitico.
#' @param file_VectorAnalitico file name of csv containing
#' VectorAnalitico.
#' @param file_Monedas file name of csv containing Monedas.
#' @details Assume file_VectorAnalitico for a single date.
#' @return xx
#' @examples
#' get_VectorAnalitico("VectorAnalitico20161130MD.csv")
#' @export
get_VectorAnalitico <- function(
  file_VectorAnalitico,
  file_Monedas = NULL
){
  header_VectorAnalitico <- c(
    "Fecha",
    "TipoValor",
    "Emisora",
    "Serie",
    "PrecioSucio",
    "PrecioLimpio",
    "InteresesAcumulados",
    "CuponActual",
    "Sobretasa",
    "NombreCompleto",
    "Sector",
    "MontoEmitido",
    "MontoCirculacion",
    "FechaEmision",
    "PlazoEmision",
    "FechaVto",
    "ValorNominal",
    "MonedaEmision",
    "Subyacente",
    "RendColocacion",
    "StColocacion",
    "FrecCpn",
    "TasaCupon",
    "DiasTranscCpn",
    "ReglaCupon",
    "CuponesEmision",
    "CuponesXCobrar",
    "HechoMkt",
    "FechaUH",
    "PrecioTeorico",
    "PostCpra",
    "PostVta",
    "YieldCpra",
    "YieldsVta",
    "SpreadCpra",
    "SpreadVta",
    "Mdys",
    "SnP",
    "Bursatilidad",
    "Liquidez",
    "CambioDiario",
    "CambioSemanal",
    "PrecioMax12m",
    "PrecioMin12m",
    "Suspension",
    "Volatilidad1",
    "Volatilidad2",
    "Duracion",
    "DuracionMonet",
    "Convexidad",
    "VaR",
    "DesviacionStnd",
    "ValorNominalActualizado",
    "CalificacionFitch",
    "FechaPrecioMaximo",
    "FechaPrecioMinimo",
    "Sensibilidad",
    "DuracionMacaulay",
    "TasaDeRendimiento",
    "HrRatings",
    "DuracionEfectiva"
  )

  VectorAnalitico <- read.csv(
    file_VectorAnalitico,
    stringsAsFactors = FALSE,
    skip = 1,
    header = FALSE,
    col.names = header_VectorAnalitico
  )

  VectorAnalitico$AssetId <- get_AssetId(
    VectorAnalitico$TipoValor,
    VectorAnalitico$Emisora,
    VectorAnalitico$Serie
  )

  VectorAnalitico$Fecha <- as.Date(
    as.character(VectorAnalitico$Fecha),
    format = "%Y%m%d"
  )

  VectorAnalitico$FechaEmision <- as.Date(
    VectorAnalitico$FechaEmision
  )

  VectorAnalitico$FechaVto <- as.Date(
    VectorAnalitico$FechaVto
  )

  VectorAnalitico$FechaUH <- as.Date(
    VectorAnalitico$FechaUH
  )

  VectorAnalitico$FechaPrecioMaximo <- as.Date(
    VectorAnalitico$FechaPrecioMaximo
  )

  VectorAnalitico$FechaPrecioMinimo <- as.Date(
    VectorAnalitico$FechaPrecioMinimo
  )

  if(!is.null(file_Monedas)) {
    Monedas <- read.csv(file_Monedas, stringsAsFactors = FALSE)
    VectorAnalitico_ <- VectorAnalitico %>%
      rename(MonedaAssetId = AssetId) %>%
      rename(MonedaPrecio = PrecioSucio) %>%
      select(MonedaAssetId,
             MonedaPrecio)
    Monedas <- left_join(Monedas, VectorAnalitico_, by = 'MonedaAssetId')

    VectorAnalitico <- left_join(VectorAnalitico, Monedas, by = 'MonedaEmision')
    idx <- which(VectorAnalitico$MonedaEmision == "[MPS] Peso Mexicano (MXN)")
    VectorAnalitico$MonedaPrecio[idx] <- 1
  }

  idx1 <- grep("Cada ", VectorAnalitico$FrecCpn)
  idx2 <- grep(" dia\\(s\\)", VectorAnalitico$FrecCpn)
  idx <- intersect(idx1, idx2)

  VectorAnalitico$PlazoCupon <- NA
  VectorAnalitico$PlazoCupon[idx] <- gsub("Cada ", "", VectorAnalitico$FrecCpn[idx])
  VectorAnalitico$PlazoCupon[idx] <- gsub(" dia\\(s\\)", "", VectorAnalitico$PlazoCupon[idx])
  VectorAnalitico$PlazoCupon <- as.numeric(VectorAnalitico$PlazoCupon)

  VectorAnalitico$DiasTranscCpn <- as.numeric(VectorAnalitico$DiasTranscCpn)

  VectorAnalitico$CuponesXCobrar <- as.numeric(VectorAnalitico$CuponesXCobrar)

  VectorAnalitico
}
