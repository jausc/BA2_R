# Vergleich von Modellen zur impliziten Volatilitätsbestimmung des Austrian Traded Index (ATX) während verschiedener Phasen der COVID-19-Krise im Zeitraum 01.07.2019 bis 31.06.2020

Dieses Repository enthält den R-Quellcode für meine Bachelorarbeit II über den Vergleich von verschiedenen statistischen Modellen zur Prognose der Volatilität des ATX.

---

## Voraussetzungen

Die folgenden Voraussetzungen konnten nicht in das Repository aufgenommen werden und müssen ggf. separat heruntergeladen werden:

- R (https://www.r-project.org/)

Weiters werden folgende R-Pakete benötigt: 
- FinTS (https://cran.r-project.org/web/packages/FinTS/index.html)
- forecast (https://cran.r-project.org/web/packages/forecast/index.html)
- ICglm (https://cran.r-project.org/web/packages/ICglm/index.html)
- MASS (https://cran.r-project.org/web/packages/MASS/index.html)
- Metrics (https://cran.r-project.org/web/packages/Metrics/index.html)
- MuMIn (https://cran.r-project.org/web/packages/MuMIn/index.html)
- pwr (https://cran.r-project.org/web/packages/pwr/index.html)
- rugarch (https://cran.r-project.org/web/packages/rugarch/index.html)
- smooth (https://cran.r-project.org/web/packages/smooth/index.html)
- tidyr (https://cran.r-project.org/web/packages/tidyr/index.html)
- tseries (https://cran.r-project.org/web/packages/tseries/index.html)
- TTR (https://cran.r-project.org/web/packages/TTR/index.html)
- vars (https://cran.r-project.org/web/packages/vars/index.html)
- xtable (https://cran.r-project.org/web/packages/xtable/index.html)
- xts (https://cran.r-project.org/web/packages/xts/index.html)
- zoo (https://cran.r-project.org/web/packages/zoo/index.html)
- ggplot2 (https://cran.r-project.org/web/packages/ggplot2/index.html)
- ggforce (https://cran.r-project.org/web/packages/ggforce/index.html)
- ggpubr (https://cran.r-project.org/web/packages/ggpubr/index.html)
- ggthemes (https://cran.r-project.org/web/packages/ggthemes/index.html)
- here (https://cran.r-project.org/web/packages/here/index.html)
- latex2exp (https://cran.r-project.org/web/packages/latex2exp/index.html)
- scales (https://cran.r-project.org/web/packages/scales/index.html)

bzw. direkt in R:

```R
install.packages("FinTS")
install.packages("forecast")
install.packages("ICglm")
install.packages("MASS")
install.packages("Metrics")
install.packages("MuMIn")
install.packages("pwr")
install.packages("rugarch")
install.packages("smooth")
install.packages("tidyr")
install.packages("tseries")
install.packages("TTR")
install.packages("vars")
install.packages("xtable")
install.packages("xts")
install.packages("zoo")

install.packages("ggplot2")
install.packages("ggforce")
install.packages("ggpubr")
install.packages("ggthemes")

install.packages("here")
install.packages("latex2exp")
install.packages("scales")
```

---

## Daten

Die historischen Kurse des ATX können hier abgerufen werden: https://www.wienerborse.at/

Die historischen Kurse des MSCI World können hier abgerufen werden: https://www.msci.com/

Kopien der historischen Kurse im .CSV-Format finden sich im Repository unter:

- [ATX](../BA2_R/data/processed/historicalData_ATX.csv)
- [MSCI World (unformatiert)](../BA2_R/data/raw/historyIndex_MSCI_raw.xls)
- [MSCI World (formatiert)](../BA2_R/data/processed/historyIndex_MSCI.csv)

---

## Start

Um den R-Quellcode ausführen zu können, kann das Repository als [.ZIP-Datei](https://github.com/jausc/BA2_R/archive/refs/heads/main.zip) heruntergeladen und als lokales Arbeitsverzeichnis verwendet oder mit Git geklont werden.

Der R-Quellcode ist in mehrere Dateien aufgeteilt, die Haupt- bzw. Arbeitsdatei stellt `main.r` da, über welche die verschiedenen Nebendateien (z.B. Funktionen) aufgerufen werden.
