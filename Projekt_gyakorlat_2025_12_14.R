# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                                                               #
#                    PROJEKT GYAKORLAT - 2025/26/1 félév                        #
#          Kerékpár-számláló mérőműszerek működésének validálása                #
#                                                                               #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####                       1. LIBRARY-K BEHÍVÁSA                             ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Szükséges csomagok betöltése
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)
library(ggplot2)
library(fBasics)
library(tibble)
library(MASS)
library(purrr)
library(patchwork)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####                       2. ADATOK IMPORTÁLÁSA                             ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

rm(list = ls())

getwd()

# A zárójelen belül írd át a saját, adatokat tartalmazó mappádra az elérési utat
setwd("C:/TB/ELTE/III. félév/Projekt gyakorlat")

data <- "MK kerékpárszámlálók nyers.xlsx"

# Excel lapok nevének összegyűjtése egy listába - ezek az állomások azonosítói
sheet_names <- excel_sheets(data)

# Végigmegyünk minden munkalapon, és kiolvassuk az A1:B20 tartományt
metadata_function <- map(sheet_names, ~ {
  # Beolvassuk a metaadatokat (kulcs-érték párok)
  meta_raw <- read_excel(
    path = data,
    sheet = .x,          # .x hivatkozik az aktuális munkalap nevére
    range = "A1:B20",
    col_names = c("Név", "Érték") # Neveket adunk az oszlopoknak
  )
  # Átalakítjuk a név-érték párokat egyetlen sorrá
  meta_wide <- tidyr::pivot_wider(
    meta_raw,
    names_from = Név,
    values_from = Érték
  )
  return(meta_wide)
})
# Összefűzzük az összes munkalap metaadatait egyetlen adatbázisba
metadata <- bind_rows(metadata_function)

# Nevezzük át a fontos oszlopokat rövidebb, kezelhetőbb nevekre
metadata <- metadata %>%
  rename(azonosito = `Mérési pont azonosító`,
         nev = `Mérési pont megnevezése`,
         szelesseg = `Szélesség`,
         hosszusag = `Hosszúság`,
         telepules = `Település`,
         regio = `Régió`,
         megye = `Vármegye`)

# Végigmegyünk minden munkalapon, és kiolvassuk a tényleges adatokat
all_data_function <- map(sheet_names, ~ {
  # Beolvassuk az adatokat, kihagyva az első 20 (metaadat) + 1 (üres) sort
  # A 22. sort automatikusan fejlécként fogja értelmezni
  data_sheet <- read_excel(
    path = data,
    sheet = .x,
    skip = 21 # Kihagyja az első 21 sort, a 22. sort olvassa be fejlécként
  )
  
  # Hozzáadjuk az állomáskódot (ami a munkalap neve) új oszlopként, az elejére
  data_sheet_with_code <- data_sheet %>%
    mutate(azonosito = .x, .before = 1)
  
  return(data_sheet_with_code)
})
# Összefűzzük az összes munkalap adatait egyetlen "long" adatbázisba
df <- bind_rows(all_data_function)

# A 8. oszloptól kezdve igazából nincs szükségünk az oszlopokra,
# mivel ezek az információk a metaadatokban megtalálhatóak
df <- df[,c(1,2,3,4,5,6,7)]
# Itt is nevezzük át az oszlopokat rövidebb, kezelhetőbb nevekre
df <- df %>%
  rename(irany = `Irány`,
         kerekp_forg = `Kerékpáros forgalom mennyisége`,
         gyalog_forg = `Gyalogos forgalom mennyisége`,
         egyeb_forg = `Egyéb forgalom mennyisége`,
         meres_kezd = `Mérés kezdő időpontja`,
         meres_vege = `Mérés vége`)

rm(all_data_function, metadata_function, data, sheet_names)

# Hozzunk létre több, kezelhetőbb dátum és idő oszlopot
df <- df %>%
  mutate(
    # Év kinyerése (pl. 2018)
    ev = year(meres_kezd),
    # Hónap kinyerése (számként: 1-12)
    honap = month(meres_kezd),
    # Hét kinyerése (számként)
    het = week(meres_kezd),
    # Nap kinyerése (a hónap napja, pl. 27)
    nap = day(meres_kezd),
    # "Milyen nap" (a hét napja, pl. "péntek")
    # A weekdays() funkció általában a rendszer nyelvét használja.
    het_napja = weekdays(meres_kezd),
    # Óra kinyerése (0-23)
    ora = hour(meres_kezd),
    # Évszak létrehozása (a 'honap' változó alapján)
    evszak = case_when(
      honap %in% c(12, 1, 2)  ~ "tél",
      honap %in% c(3, 4, 5)   ~ "tavasz",
      honap %in% c(6, 7, 8)   ~ "nyár",
      honap %in% c(9, 10, 11) ~ "ősz"))

saveRDS(df, "long_full_df.RDS")


##           HA MEGVAN A "long_full_df.RDS" elég innentől futtatni             ##
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####                   3. ADATTISZTÍTÁS ÉS -FELDOLGOZÁS                      ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

df <- readRDS("long_full_df.RDS")

# Vizsgáljuk meg az adatok minőségét, és végezzük el a szükséges adattisztítást
reszletes_osszefoglalo <- function(x) {
  
  # Speciális értékek számolása
  osszes_elem <- length(x)
  na_darab <- sum(is.na(x) & !is.nan(x))       # Csak a "tiszta" NA-k
  nan_darab <- sum(is.nan(x))                  # NaN (Not a Number)
  inf_darab <- sum(is.infinite(x) & x > 0)     # Pozitív végtelen
  minf_darab <- sum(is.infinite(x) & x < 0)    # Negatív végtelen
  
  # "Tiszta" adatok kinyerése: csak a valós, véges számok
  tiszta_adatok <- x[!is.na(x) & !is.infinite(x)]
  n <- length(tiszta_adatok)
  
  # Statisztikák számolása (csak ha van tiszta adat)
  if (n == 0) {
    # Ha nincs érvényes adat, NA-t adunk vissza
    stats_list <- list(
      min = NA,
      p_0.001_pct = NA,
      p_25 = NA,
      median = NA,
      p_75 = NA,
      p_99.999_pct = NA,
      max = NA,
      atlag = NA
    )
  } else {
    # Statisztikák számolása a tiszta adatokon
    atlag <- mean(tiszta_adatok)
    
    # Kvantilisek
    # A 0.001 percentilis = 0.00001 kvantilis
    # A 99.999 percentilis = 0.99999 kvantilis
    q_vals <- quantile(tiszta_adatok, 
                       probs = c(0.0, 0.00001, 0.25, 0.5, 0.75, 0.99999, 1.0), 
                       names = FALSE)
    
    stats_list <- list(
      min = q_vals[1],
      p_0.001_pct = q_vals[2],
      p_25 = q_vals[3],
      median = q_vals[4],
      p_75 = q_vals[5],
      p_99.999_pct = q_vals[6],
      max = q_vals[7],
      atlag = atlag
    )
  }
  
  # Eredmény lista numerikus adatokra
  return(c(
    list(
      tipus = class(x),
      osszes_elem = osszes_elem,
      nem_numerikus_elem = 0,
      na_darab = na_darab,
      nan_string_darab = 0,
      inf_string_darab = 0,
      minf_string_darab = 0,
      nan_numeric_darab = nan_darab,
      inf_numeric_darab = inf_darab,
      minf_numeric_darab = minf_darab,
      tiszta_elem_n = n
    ),
    stats_list # Hozzáfűzzük a statisztikákat
  ))
}
# Készítsük el az objektumot, ami tartalmazza a vizsgálat eredményét
eredmeny_lista <- lapply(df[,c(3,4,5)], reszletes_osszefoglalo)

## ~~~~~~~~~~~~~~~~~~~~~~~~    Kerékpáros forgalom    ~~~~~~~~~~~~~~~~~~~~~~~~ ##
eredmeny_lista$kerekp_forg
# Nincs semmilyen NA, NaN vagy nem-numerikus érték
# Legkisebb érték: -3552, ez elég gyanús, hogy adathiba
# Legnagyobb érték: 5334, szintén elég nagynak tűnik, percenként ~89 biciklis

## ~~~~~~~~~~~~~~~~~~~~~~~~     Gyalogos forgalom     ~~~~~~~~~~~~~~~~~~~~~~~~ ##
eredmeny_lista$gyalog_forg
# Itt sincs semmilyen NA, NaN vagy nem-numerikus érték
# Legkisebb érték: 0, ez korrektnek tűnik
# Legnagyobb érték: 156, ez percenként 2.6 gyalogos, jó eséllyel valid adat

## ~~~~~~~~~~~~~~~~~~~~~~~~       Egyéb forgalom      ~~~~~~~~~~~~~~~~~~~~~~~~ ##
eredmeny_lista$egyeb_forg
# Itt sincs semmilyen NA, NaN vagy nem-numerikus érték
# Legkisebb érték: 0, ez szintén korrektnek tűnik
# Legnagyobb érték: 263, ez percenként 4.38-as forgalmat jelent, ez is valid lehet
# DE IGAZÁBÓL NEM IS TUDJUK, HOGY MI AZ AZ "EGYÉB FORGALOM"!

rm(eredmeny_lista, reszletes_osszefoglalo)

# Cseréljük ki a negatív megfigyeléseket 0-ra, ezek biztosan adathibák
df <- df %>%
  mutate(kerekp_forg = if_else(kerekp_forg < 0, 0, kerekp_forg))

# Távolítsuk el a teljesen duplikált sorokat is, ezekre nem lesz szükségünk
df <- df %>%
  distinct(
    azonosito, irany, kerekp_forg, gyalog_forg, egyeb_forg, ev, honap, nap, ora,
    .keep_all = TRUE)
# 74 978 sort távolítottunk el így

# Vannak vajon olyanok, ahol a megfigyelés időpontja megegyezik, de eltér az érték?
# Tehát ahol valamiért több mérési eredményünk is van, és azok konfliktusban állnak
konfliktusos_meresek <- df %>%
  group_by(azonosito, irany, ev, honap, nap, ora) %>%
  # Megszámoljuk, hány sor tartozik minden egyes kulcs-kombinációhoz
  summarise(
    megfigyelesek_szama = n(), 
    .groups = "drop") %>%
  # Csak azokat tartjuk meg, ahol 1-nél több sor (azaz eltérő érték) volt
  filter(megfigyelesek_szama > 1) %>%
  # Rendezés (opcionális): a legtöbb konfliktussal rendelkező elöl
  arrange(desc(megfigyelesek_szama))
print(konfliktusos_meresek)
# Van további 8 965 olyan mérési időpont, amihez több megfigyelt érték is tartozik
konfliktusos_meresek_reszl <- df %>%
  semi_join(konfliktusos_meresek, by = c("azonosito", "irany", "ev", "honap", "nap", "ora")) %>%
  # Rendezés, hogy az összetartozó sorok egymás alatt legyenek
  arrange(azonosito, irany, ev, honap, nap, ora)
# Itt meg is tudjuk őket nézni:
konfliktusos_meresek_reszl

## Úgy döntöttünk, hogy ezeket a megfigyeléseket úgy ahogy van, eltávolítjuk!
# df előtte: 2 765 302 megfigyelés

df <- anti_join(
  df, 
  konfliktusos_meresek, 
  by = c("azonosito", "irany", "ev", "honap", "nap", "ora")
)

# df utána: 2 747 350 megfigyelés

# Ellenőrzés újra:
konfliktusos_meresek_2 <- df %>%
  group_by(azonosito, irany, ev, honap, nap, ora) %>%
  summarise(
    megfigyelesek_szama = n(), 
    .groups = "drop") %>%
  filter(megfigyelesek_szama > 1) %>%
  arrange(desc(megfigyelesek_szama))
print(konfliktusos_meresek_2)
# Üres tábla, tehát jól működött az eltávolítás

# Vizsgáljuk meg a kiugróan nagy megfigyeléseket, ahol óránként több, mint
# 500 biciklis haladt át az állomás szerint
magas_forgalom_df <- df %>%
  # Szűrés a feltétel alapján
  filter(kerekp_forg >= 500) %>%
  # Új 'napszak' oszlop létrehozása
  mutate(
    napszak = case_when(
      ora %in% 0:5  ~ "1. Éjszaka (00-05)",
      ora %in% 6:9  ~ "2. Reggeli csúcs (06-09)",
      ora %in% 10:15 ~ "3. Napközben (10-15)",
      ora %in% 16:18 ~ "4. Délutáni csúcs (16-18)",
      ora %in% 19:23 ~ "5. Este (19-23)"))
ossz_allomas <- magas_forgalom_df %>%
  count(azonosito, sort = TRUE)
ossz_ev <- magas_forgalom_df %>%
  count(ev, sort = TRUE)
ossz_honap <- magas_forgalom_df %>%
  count(honap, sort = TRUE)
ossz_het_napja <- magas_forgalom_df %>%
  count(het_napja, sort = TRUE)
ossz_napszak <- magas_forgalom_df %>%
  count(napszak, sort = TRUE)

## Nézzük mely állomásokon fordult elő ilyen kiugróan nagy forgalom
ossz_allomas
# Négy olyan állomás van, amely többször is ilyen kiugróan nagy adatokat
# produkált, ezek a 119004, 107004, 116002, 116004 azonosítójúak, amelyek a
# Zánka, Pákozd (Budai út), Karcag (Madarasi út), Tiszafüred állomások

# Ezek egymástól meglehetősen messze találhatóak, így a kiugró értékek
# egymástól valószínűleg függetlenek, kivéve, ha pl. a Pákozd-Zánka
# távolsághoz képest reális időn belül történtek

ossz_ev
# A legtöbb kiugró megfigyelés 2020-ban és 2024-ben volt, illetve még
# 2021-ben volt számottevő ilyen adatpont.

ossz_honap
# A legtöbb kiugró megfigyelés szeptemberben és októberben történt, illetve
# májusban volt még számottevő ilyen adatpont.

ossz_het_napja

ossz_napszak
# A legtöbb kiugró megfigyelés napközben, 10 és 15 óra közt történt, illetve
# néhány még este (19-23), illetve a délutáni csúcs (16-18) során.

rm(ossz_ev, ossz_allomas, ossz_het_napja, ossz_honap, ossz_napszak)
rm(konfliktusos_meresek, konfliktusos_meresek_2, konfliktusos_meresek_reszl)
rm(magas_forgalom_df)

## Úgy döntöttünk, hogy az outliereknél 1000-nél húzzuk meg a határt, az 1000
## feletti értékeket eltávolítjuk.

df <- df %>%
  filter(kerekp_forg <= 1000)
# Ez a szűrés 13 megfigyelés eltávolítását jelentette.

saveRDS(df, "long_szurt_df.RDS")


##           HA MEGVAN A "long_szurt_df.RDS" elég innentől futtatni             ##
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####                          4. LEÍRÓ STATISZTIKÁK                          ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
df <- readRDS("long_szurt_df.RDS")

## Óránkénti frekvencia ábrája
df %>%
  ggplot(aes(x = kerekp_forg)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7) +
  labs(title = "Óránkénti kerékpáros forgalom eloszlása",
       x = "Óránkénti kerékpárosok száma", y = "Megfigyelések száma") +
  coord_cartesian(xlim = c(NA, 100), ylim = c(NA, 500000))

## Napi frekvencia ábrája
df_napi <- df %>%
  group_by(azonosito, irany, ev, honap, nap, evszak) %>%
  summarise(
    kerekp_forg = sum(kerekp_forg, na.rm = TRUE),
    .groups = "drop" # Elengedjük a csoportosítást
  )
df_napi %>%
  ggplot(aes(x = kerekp_forg)) +
  geom_histogram(binwidth = 50, fill = "blue", alpha = 0.7) +
  labs(title = "Napi kerékpáros forgalom eloszlása",
       x = "Napi kerékpárosok száma", 
       y = "Megfigyelések száma") +
  coord_cartesian(xlim = c(0, 1500))

## Heti frekvencia ábrája
df_heti <- df %>%
  group_by(azonosito, irany, ev, het) %>%
  summarise(
    kerekp_forg = sum(kerekp_forg, na.rm = TRUE),
    .groups = "drop" # Elengedjük a csoportosítást
  )
df_heti %>%
  ggplot(aes(x = kerekp_forg)) +
  geom_histogram(binwidth = 100, fill = "blue", alpha = 0.7) +
  labs(title = "Heti kerékpáros forgalom eloszlása",
       x = "Heti kerékpárosok száma", 
       y = "Megfigyelések száma") +
  coord_cartesian(xlim = c(0, 8000))

## Havi frekvencia ábrája
df_havi <- df %>%
  group_by(azonosito, irany, ev, honap) %>%
  summarise(
    kerekp_forg = sum(kerekp_forg, na.rm = TRUE),
    .groups = "drop" # Elengedjük a csoportosítást
  )
df_havi %>%
  ggplot(aes(x = kerekp_forg)) +
  geom_histogram(binwidth = 250, fill = "blue", alpha = 0.7) +
  labs(title = "Havi kerékpáros forgalom eloszlása",
       x = "Havi kerékpárosok száma", 
       y = "Megfigyelések száma") +
  coord_cartesian(xlim = c(0, 15000))

### Leíró statisztikák kiszámolása

calculate_stats <- function(x) {
  # A biztonság kedvéért kiszűrjük az NA értékeket
  x_clean <- x[!is.na(x)]
  
  # A kért kvantilisek kiszámítása egy lépésben
  quantiles <- quantile(x_clean, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  
  # Eredmény visszaadása egy "tibble"-ben (modern data frame)
  tibble(
    Min = min(x_clean),
    P10 = quantiles[1],
    P25_Q1 = quantiles[2],
    P50_Median = quantiles[3],
    P75_Q3 = quantiles[4],
    P90 = quantiles[5],
    Atlag = mean(x_clean),
    Max = max(x_clean),
    Szoras = sd(x_clean)
  )
}

# Számolás az órás adatokra (a tiszta 'kerekp_forg' oszlopon)
stats_hourly <- calculate_stats(df$kerekp_forg) %>%
  mutate(Szint = "1. Órás", .before = 1)

# Számolás a napi adatokra (az új 'forgalom' oszlopon)
stats_daily <- calculate_stats(df_napi$kerekp_forg) %>%
  mutate(Szint = "2. Napi", .before = 1)

# Számolás a heti adatokra
stats_weekly <- calculate_stats(df_heti$kerekp_forg) %>%
  mutate(Szint = "3. Heti", .before = 1)

# Számolás a havi adatokra
stats_monthly <- calculate_stats(df_havi$kerekp_forg) %>%
  mutate(Szint = "4. Havi", .before = 1)

# Az összes eredmény összefűzése egyetlen táblázatba
final_stats_table <- bind_rows(
  stats_hourly,
  stats_daily,
  stats_weekly,
  stats_monthly
)

# Állítsuk be, hogy a tibble minden oszlopot kiírjon
options(tibble.width = Inf)

print(final_stats_table)

rm(stats_daily, stats_hourly, stats_monthly, stats_weekly)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####                    5. PRÓBA-ELEMZÉSEK VÉGREHAJTÁSA                      ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Aggregálja egy adott állomás adatait a megadott évekre és idő-egységre.
# data_df    - A felhasználandó adatbázis (pl. 'df').
# allomas_id - A vizsgálni kívánt állomás 'azonosito'-ja
# evek       - Egy numerikus vektor a vizsgálni kívánt évekről (pl. c(2021, 2022)).
# time_group - Az időbeli aggregáció szintje: "honap", "het", evszak", "het_napja".

aggregalt_allomas_adat <- function(data_df, allomas_id, evek, time_group = "honap") {
  valid_time <- c("honap", "evszak", "het_napja", "het")
  if (!time_group %in% valid_time) {
    stop(paste("Ismeretlen 'time_group' érték:", time_group, 
               ". Lehetőségek: 'honap', 'evszak', 'het', 'het_napja'."))}
  # Dinamikusan meghatározzuk a csoportosítási változókat
  # Mindig csoportosítunk 'ev' szerint, ÉS a kért 'time_group' szerint
  grouping_vars <- c("ev", time_group)
  aggregalt_adat <- data_df %>%
    filter(
      azonosito == allomas_id,
      ev %in% evek) %>%
    # Csoportosítás (pl. ev, honap)
    group_by(across(all_of(grouping_vars))) %>%
    # Összesítés
    summarise(
      osszes_kerekpar_forg = sum(kerekp_forg, na.rm = TRUE),
      megfigyelesek_szama = n(),
      .groups = "drop") %>%
    # Rendezés a jobb átláthatóságért
    arrange(across(all_of(grouping_vars)))
  return(aggregalt_adat)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Az aggregált adatokból elkészíti a ggplot ábrá(ka)t is.
# data_df     - A felhasználandó adatbázis (pl. 'df').
# metadata_df - A meta-adatokat tartalmazó adatbázis
# allomas_id  - A vizsgálni kívánt állomás 'azonosito'-ja
# evek        - Egy numerikus vektor a vizsgálni kívánt évekről (pl. c(2021, 2022)).
# time_group  - Az időbeli aggregáció szintje: "honap", "het", evszak", "het_napja".

plot_allomas_trend <- function(data_df, metadata_df, allomas_id, evek, time_group) {
  
  # --- 1. Adatok aggregálása ---
  aggregalt_data <- aggregalt_allomas_adat(
    data_df = data_df,
    allomas_id = allomas_id,
    evek = evek,
    time_group = time_group
  )
  
  # --- 2. Dinamikus változók és adat-előkészítés ---
  # Állomás nevének kinyerése a címhez
  allomas_nev_check <- metadata_df %>%
    filter(azonosito == allomas_id) %>%
    pull(nev)
  
  # Ha nem találja, az ID-t használja névként
  allomas_nev <- if (length(allomas_nev_check) > 0) allomas_nev_check[1] else allomas_id
  
  if (time_group == "honap") {
    # Adat-előkészítés: Hónapok átalakítása rendezett faktorrá
    aggregalt_data <- aggregalt_data %>%
      mutate(honap = factor(honap, levels = 1:12, 
                            labels = c("Jan", "Feb", "Már", "Ápr", "Máj", "Jún", 
                                       "Júl", "Aug", "Szep", "Okt", "Nov", "Dec"), 
                            ordered = TRUE))
    # Ábra-specifikus beállítások
    x_label <- "Hónap"
    title_part <- "Havi"
  } else if (time_group == "het") {
    # Nincs szükség adat-előkészítésre, a 'het' numerikus
    # Ábra-specifikus beállítások
    x_label <- "Hét"
    title_part <- "Heti"
  } else {
    stop("Ez a függvény csak 'honap' vagy 'het' ábrázolásra van felkészítve.")
  }
  
  # --- 3. Ábra elkészítése ---
  # .data[[time_group]] használata lehetővé teszi, hogy az 'aes()'
  # dinamikusan használja a 'time_group' változóban tárolt oszlopnevet (pl. "honap" or "het")
  plot_base <- ggplot(aggregalt_data, aes(x = .data[[time_group]], 
                                          y = osszes_kerekpar_forg,
                                          color = factor(ev), 
                                          group = factor(ev))) +
    geom_line(size = 1) +
    geom_point(size = 2, shape = 21, fill = "white") +
    labs(
      title = paste0(title_part, " kerékpárforgalom alakulása\n", allomas_nev, " (", allomas_id, ")"),
      x = x_label,
      y = "Összes kerékpárforgalom",
      color = "Év"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # --- 4. Opcionális rétegek hozzáadása ---
  # Ha heti ábrát készítünk, állítsuk be az X tengelyt
  if (time_group == "het") {
    plot_final <- plot_base +
      scale_x_continuous(breaks = seq(from = 0, to = 52, by = 5))
  } else {
    plot_final <- plot_base
  }
  # Visszaadjuk a kész ggplot objektumot
  return(plot_final)
}

# ~~~~~~~~~~~~~~              Függvények használata              ~~~~~~~~~~~~~~ #

evek_listaja <- c(2019, 2020, 2021, 2022, 2023, 2024)

plot_allomas_trend(
  data_df = df,
  metadata_df = metadata,
  allomas_id = "107006",
  evek = evek_listaja,
  time_group = "honap")

plot_allomas_trend(
  data_df = df,
  metadata_df = metadata,
  allomas_id = "107006",
  evek = evek_listaja,
  time_group = "het")

plot_allomas_trend(
  data_df = df,
  metadata_df = metadata,
  allomas_id = "111004",
  evek = evek_listaja,
  time_group = "honap")

plot_allomas_trend(
  data_df = df,
  metadata_df = metadata,
  allomas_id = "111004",
  evek = evek_listaja,
  time_group = "het")

plot_allomas_trend(
  data_df = df,
  metadata_df = metadata,
  allomas_id = "109003",
  evek = evek_listaja,
  time_group = "honap")

plot_allomas_trend(
  data_df = df,
  metadata_df = metadata,
  allomas_id = "109003",
  evek = evek_listaja,
  time_group = "het")

# ...


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####      6. NAPI ÖSSZESÍTETT FORGALMAKAT TARTALMAZÓ TÁBLA LÉTREHOZÁSA       ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Egységesítjük az oszlopnevet a ploton való szép megjelenítéshez
names(df_napi)[names(df_napi) == "kerekp_forg"] <- "Nyers"

# Napi adatok
napi_adatok <- df_napi %>% 
  left_join(metadata %>% dplyr::select(azonosito, nev), by = "azonosito")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####                7. BENCHMARK-ÁLLOMÁSOK MEGHATÁROZÁSA                     ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Azokat az állomásokat tekintettük referenciáinknak, ahol a vizsgált 
# időszakban/évszakban a három évre vonatkozó rendelkezésre állási adat 
# átlagos értéke legalább 80% volt.

benchmark_allomasok <- bind_rows(
  tibble(evszak = "tavasz", nev = c("Balatonederics, 71. főút bevezető szakasz", 
                                    "Budapest-Dunakeszi EV6 kerékpárút", 
                                    "Tiszafüred", "Zánka", "Baja")),
  tibble(evszak = "nyár",   nev = c("Balatonederics, 71. főút bevezető szakasz",
                                    "Tiszafüred", "Örvényes")),
  tibble(evszak = "ősz",    nev = c("Balatonederics, 71. főút bevezető szakasz",
                                    "Gyöngyös", "Tiszafüred")),
  tibble(evszak = "tél",    nev = c("Balatonederics, 71. főút bevezető szakasz",
                                    "Fertőszéplak, Soproni u.", 
                                    "BuBa-Füle (EV14)", "Örvényes", 
                                    "Pákozd, Budai út", "BuBa-Szabadbattyán (EV14)",
                                    "Tarcal", "Törökbálint BuBa")))

evek <- c(2022, 2023, 2024)

# Csak a listában szereplő állomásokat tartjuk meg és csak a hozzájuk rendelt 
# évszakban!
napi_adatok_benchmark <- napi_adatok %>%
  filter(ev %in% evek) %>% 
  semi_join(benchmark_allomasok, by = c("evszak", "nev"))


## Benchmark állomások átnevezése rövidebb nevekre:
napi_adatok_benchmark <- napi_adatok_benchmark %>%
  mutate(nev = case_when(
    nev == "Balatonederics, 71. főút bevezető szakasz" ~ "Balatonederics",
    nev == "Budapest-Dunakeszi EV6 kerékpárút" ~ "Dunakeszi",   
    nev == "Fertőszéplak, Soproni u." ~ "Fertőszéplak",  
    nev == "BuBa-Füle (EV14)" ~ "Füle",  
    nev == "Pákozd, Budai út" ~ "Pákozd",  
    nev == "BuBa-Szabadbattyán (EV14)" ~ "Szabadbattyán",  
    nev == "Törökbálint BuBa" ~ "Törökbálint",  
    TRUE ~ nev
  ))

napi_adatok_benchmark_norm <- napi_adatok_benchmark %>%
  group_by(azonosito, nev, evszak) %>%
  mutate(
    szezon_atlag = mean(Nyers, na.rm = TRUE),
    Relatív = Nyers / szezon_atlag
  ) %>%
  ungroup()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####         8. BENCHMARK-ÁLLOMÁSOK ELOSZLÁSÁNAK MEGHATÁROZÁSA               ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~ Eloszlás és sűrűség-ábrázoló függvény egy adott évszakra és állomásra ~~~ #


# Eloszlás és sűrűség-ábrázoló függvény egy adott évszakra és állomásra
plot_forgalom_eloszlas <- function(napi_adatok,
                                   valtozo,
                                   allomas_id,
                                   target_evszak,
                                   tipus = c("hist", "ecdf")) {
  tipus <- match.arg(tipus)
  # Szűrés az állomásra és évszakra
  plot_data <- napi_adatok %>%
    filter(
      azonosito == allomas_id,
      evszak == target_evszak
    )
  # Ha nincs adat, dobjunk érthető hibát
  if (nrow(plot_data) == 0) {
    stop("Nincs adat a megadott évszakra és állomásra (ellenőrizd az azonosítót és az évszakot).")
  }
  
  # Állomás név (feltételezve, hogy egységes az adott azonosítóra)
  aktualis_nev <- plot_data$nev[1]
  
  if (tipus == "hist") {
    # Hisztogram + sűrűségfüggvény
    gg <- ggplot(plot_data, aes(x = .data[[valtozo]])) +
      geom_histogram(aes(y = ..density..),
                     bins = 30,
                     alpha = 0.6,
                     fill = "steelblue") +
      geom_density(color = "red", linewidth = 1.1) +
      labs(
        title = paste0(target_evszak," ", valtozo, " – napi forgalom eloszlása"),
        subtitle = paste0(aktualis_nev, " (", allomas_id, ")"),
        x = paste0(valtozo, " napi kerékpáros forgalom"),
        y = "Sűrűség"
      ) +
      theme_minimal()
    
  } else if (tipus == "ecdf") {
    # Empirikus eloszlásfüggvény (ECDF)
    gg <- ggplot(plot_data, aes(x = .data[[valtozo]])) +
      stat_ecdf(geom = "step", linewidth = 1) +
      labs(
        title = paste0(target_evszak," ", valtozo," forgalom – empirikus eloszlásfüggvénye (ECDF)"),
        subtitle = paste0(aktualis_nev, " (", allomas_id, ")"),
        x = paste0(valtozo, " napi kerékpáros forgalom"),
        y = "F(x)"
      ) +
      theme_minimal()
  }
  
  return(gg)
}

# ~~~~~~~~~~~~~~~~~~~~                Próba               ~~~~~~~~~~~~~~~~~~~~ #

# Be lehet állítani, hogy "Nyers" vagy "Relatív" forgalmi adatok legyenek az ábrán

# Példa 1a: Balatonederics, tavasz histogram
plot_forgalom_eloszlas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  allomas_id = "119003",
  target_evszak = "tavasz",
  tipus = "hist"
)

# Példa 1b: Balatonederics, tavasz ecdf
plot_forgalom_eloszlas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  allomas_id = "119003",
  target_evszak = "tavasz",
  tipus = "ecdf"
)

# Példa 2a: Füle, tél ecdf
plot_forgalom_eloszlas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  allomas_id = "107006",
  target_evszak = "tél",
  tipus = "hist"
)

# Példa 2_2a: Örvényes, tél ecdf
plot_forgalom_eloszlas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  allomas_id = "119006",
  target_evszak = "tél",
  tipus = "ecdf"
)

# Példa 2b: Füle, tél ecdf
plot_forgalom_eloszlas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  allomas_id = "107006",
  target_evszak = "tél",
  tipus = "ecdf"
)

# ~~~~~~~~~~~~~~~~~~~~    Eloszlás-ábrázoló függvény egy adott évszakra      ~~~~~~~~~~~~~~~~~~~~ #

# Eloszlás-ábrázoló függvény egy adott évszakra
plot_ecdf_minden_allomas <- function(napi_adatok,
                                     target_evszak,
                                     valtozo,
                                     facet = FALSE) {
  # Szűrés a megadott évszakra
  adat_evszak <- napi_adatok %>%
    filter(evszak == target_evszak)
  
  if (nrow(adat_evszak) == 0) {
    stop("Nincs adat a megadott évszakra (ellenőrizd a 'target_evszak' értékét).")
  }
  
  if (!facet) {
    # MINDEN állomás egy ábrán, külön színnel
    gg <- ggplot(adat_evszak,
                 aes(x = .data[[valtozo]],
                     color = nev,
                     group = nev)) +
      stat_ecdf(geom = "step", linewidth = 1) +
      labs(
        title = paste0(target_evszak," ", valtozo," forgalom – empirikus eloszlásfüggvények"),
        subtitle = "Minden állomás",
        x = paste0(valtozo, " napi kerékpáros forgalom (db)"),
        y = "F(x)",
        color = "Állomás"
      ) +
      theme_minimal()
  } else {
    # Facet: külön panel állomásonként
    gg <- ggplot(adat_evszak, aes(x = .data[[valtozo]])) +
      stat_ecdf(geom = "step", linewidth = 1) +
      facet_wrap(~ nev, scales = "free_x") +
      labs(
        title = paste0(target_evszak, " ", valtozo, " forgalom – empirikus eloszlásfüggvények állomásonként"),
        x = paste0(valtozo, " napi kerékpáros forgalom (db)"),
        y = "F(x)"
      ) +
      theme_minimal()
  }
  
  return(gg)
}


# ~~~~~~~~~~~~~~~~~~~~                Próba               ~~~~~~~~~~~~~~~~~~~~ #

# Be lehet állítani, hogy "Nyers" vagy "Relatív" forgalmi adatok legyenek az ábrán

# 1) Minden állomás egy közös ECDF-ábrán
plot_ecdf_minden_allomas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "nyár"
)

# 2) Külön facet-panel állomásonként
plot_ecdf_minden_allomas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "nyár",
  facet = TRUE
)


# ~~~~~~~~~~~~~~~~~~~~    Sűrűség-ábrázoló függvény egy adott évszakra      ~~~~~~~~~~~~~~~~~~~~ #

plot_density_minden_allomas <- function(napi_adatok,
                                        target_evszak,
                                        valtozo,
                                        facet = FALSE) {
  # Szűrés a megadott évszakra
  adat_evszak <- napi_adatok %>%
    filter(evszak == target_evszak)
  
  if (nrow(adat_evszak) == 0) {
    stop("Nincs adat a megadott évszakra (ellenőrizd a 'target_evszak' értékét).")
  }
  
  if (!facet) {
    # MINDEN állomás egy ábrán, külön színnel
    gg <- ggplot(
      adat_evszak,
      aes(x = .data[[valtozo]], color = nev, fill = nev)
    ) +
      geom_density(alpha = 0.25) +
      labs(
        title = paste0(target_evszak, " ", valtozo," – napi forgalom sűrűségfüggvényei"),
        subtitle = "Minden állomás",
        x = paste0(valtozo, " napi kerékpáros forgalom (db)"),
        y = "Sűrűség",
        color = "Állomás",
        fill  = "Állomás"
      ) +
      theme_minimal()
    
  } else {
    # Facet: külön panel állomásonként
    gg <- ggplot(adat_evszak, aes(x = .data[[valtozo]])) +
      geom_density(fill = "steelblue", alpha = 0.4) +
      facet_wrap(~ nev, scales = "free_x") +
      labs(
        title = paste0(target_evszak," ", valtozo, " – napi forgalom sűrűségfüggvényei állomásonként"),
        x = paste0(valtozo, " napi kerékpáros forgalom (db)"),
        y = "Sűrűség"
      ) +
      theme_minimal()
  }
  
  return(gg)
}

# ~~~~~~~~~~~~~~~~~~~~                Próba               ~~~~~~~~~~~~~~~~~~~~ #

# Be lehet állítani, hogy "Nyers" vagy "Relatív" forgalmi adatok legyenek az ábrán

# 1) Minden állomás egy közös sűrűség-ábrán
plot_density_minden_allomas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "nyár"
)

# 2) Külön facet-panel állomásonként
plot_density_minden_allomas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "nyár",
  facet = TRUE
)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####  9. BENCHMARK-ÁLLOMÁSOK NEM AGGREGÁLT ELOSZLÁSÁNAK ÖSSZEHASONLÍTÁSA     ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

ks_allomasok_evszak <- function(napi_adatok, target_evszak, valtozo) {
  
  # Szűrés az évszakra
  adat <- napi_adatok %>%
    filter(evszak == target_evszak)
  
  # Ha nincs adat → hiba
  if (nrow(adat) == 0) {
    stop("Nincs adat a megadott évszakra.")
  }
  
  # ID → név leképezés (feltételezve: azonosito - nev páros egyértelmű)
  id_to_name <- adat %>%
    distinct(azonosito, nev) %>%
    deframe()           # named vector: names(.) = azonosito, value = nev
  
  # Mely állomásokat fogjuk összehasonlítani?
  allomasok_id <- unique(adat$azonosito)
  
  # Páronkénti kombinációk
  kombinaciok <- t(combn(allomasok_id, 2))
  colnames(kombinaciok) <- c("A", "B")
  
  # KS-tesztek futtatása
  eredmenyek <- apply(kombinaciok, 1, function(par) {
    A_id <- par["A"]
    B_id <- par["B"]
    
    mintA <- adat %>% filter(azonosito == A_id) %>% pull(.data[[valtozo]])
    mintB <- adat %>% filter(azonosito == B_id) %>% pull(.data[[valtozo]])
    
    ks <- ks.test(mintA, mintB)
    
    tibble(
      allomas_A       = A_id,
      allomas_B       = B_id,
      allomas_A_nev   = id_to_name[[A_id]],
      allomas_B_nev   = id_to_name[[B_id]],
      allomas_A_elem  = length(mintA),
      allomas_B_elem  = length(mintB),
      evszak          = target_evszak,
      statistic       = ks$statistic,
      p_value         = ks$p.value
    )
  }) %>% bind_rows()
  
  # Benjamini–Hochberg korrekció + szignifikancia jelölése
  eredmenyek <- eredmenyek %>%
    mutate(
      p_adjusted_BH = p.adjust(p_value, method = "BH"),
      `Szignifikáns-e az eltérés?` = if_else(
        p_adjusted_BH <= 0.05,
        "igen",
        "nem"
      )
    )
  
  return(eredmenyek)
}

ecdf_allomasok_evszak <- function(napi_adatok, target_evszak, valtozo) {
  
  # Szűrés az évszakra
  adat <- napi_adatok %>%
    filter(evszak == target_evszak)
  
  # Ha nincs adat → hiba
  if (nrow(adat) == 0) {
    stop("Nincs adat a megadott évszakra.")
  }
  
  # ID → név leképezés
  id_to_name <- adat %>%
    distinct(azonosito, nev) %>%
    deframe()
  
  # Összehasonlítandó állomások
  allomasok_id <- unique(adat$azonosito)
  
  # Állomáspárok
  kombinaciok <- t(combn(allomasok_id, 2))
  colnames(kombinaciok) <- c("A", "B")
  
  # ECDF-alapú átlagos eltérés függvény (helyben definiálva)
  ecdf_diff_mean <- function(x, y) {
    Fx <- ecdf(x)
    Fy <- ecdf(y)
    pts <- sort(unique(c(x, y)))
    mean(abs(Fx(pts) - Fy(pts)))
  }
  
  # Páronkénti számítás
  eredmenyek <- apply(kombinaciok, 1, function(par) {
    A_id <- par["A"]
    B_id <- par["B"]
    
    mintA <- adat %>% filter(azonosito == A_id) %>% pull(.data[[valtozo]])
    mintB <- adat %>% filter(azonosito == B_id) %>% pull(.data[[valtozo]])
    
    avg_diff <- ecdf_diff_mean(mintA, mintB)
    
    tibble(
      allomas_A       = A_id,
      allomas_B       = B_id,
      allomas_A_nev   = id_to_name[[A_id]],
      allomas_B_nev   = id_to_name[[B_id]],
      allomas_A_elem  = length(mintA),
      allomas_B_elem  = length(mintB),
      evszak          = target_evszak,
      avg_diff        = avg_diff
    )
  }) %>% bind_rows()
  
  return(eredmenyek)
}


# ~~~~~~~~~~        Kolmogorov-Smirnov a relatív forgalmi adatokra      ~~~~~~~~~~~~~~ #

ks_tavasz_rel <- ks_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "tavasz"
)

ks_nyar_rel <- ks_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "nyár"
)

ks_osz_rel <- ks_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "ősz"
)

ks_tel_rel <- ks_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "tél"
)

ks_eredmenyek_rel <- rbind(ks_tavasz_rel, ks_nyar_rel, ks_osz_rel, ks_tel_rel)


# ~~~~~~~~~~        Kolmogorov-Smirnov a Nyers napi forgalmi adatokra      ~~~~~~~~~~~~~~ #

ks_tavasz_nyers <- ks_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Nyers",
  target_evszak = "tavasz"
)

ks_nyar_nyers <- ks_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Nyers",
  target_evszak = "nyár"
)

ks_osz_nyers <- ks_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Nyers",
  target_evszak = "ősz"
)

ks_tel_nyers <- ks_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Nyers",
  target_evszak = "tél"
)

ks_eredmenyek_nyers <- rbind(ks_tavasz_nyers, ks_nyar_nyers, ks_osz_nyers, ks_tel_nyers)


# ~~~~~~    Eloszlás-ábrázoló függvény egy adott évszakra adott két állomásra      ~~~~~~~~~~ #

plot_ecdf_par <- function(napi_adatok, target_evszak, allomas_A, allomas_B, valtozo) {
  
  # ID -> név leképezés
  id_to_name <- napi_adatok %>%
    distinct(azonosito, nev) %>%
    deframe()  # named vector: names(.) = azonosito, value = nev
  
  # Az adott állomások nevei
  A_nev <- id_to_name[[allomas_A]]
  B_nev <- id_to_name[[allomas_B]]
  
  adat_plot <- napi_adatok %>%
    filter(
      evszak == target_evszak,
      azonosito %in% c(allomas_A, allomas_B)
    )
  
  ggplot(adat_plot, aes(x = .data[[valtozo]], color = nev)) +
    stat_ecdf(geom = "step", linewidth = 1) +
    labs(
      title    = paste0(target_evszak, " ", valtozo, " – ECDF: ", A_nev, " vs ", B_nev),
      subtitle = paste0(
        "ID-k: ", allomas_A, "  vs  ", allomas_B
      ),
      x = paste0(valtozo, " napi kerékpáros forgalom (db)"), #JAVÍTÁS
      y = "F(x)",
      color = "Állomás"
    ) +
    theme_minimal()
}

# ~~~    Ábra a KS alapján nem eltérő eloszlásokra a relatív forgalom alapján   ~~~~~~ #

# Tiszafüred vs Balatonederics tavasz
plot_ecdf_par(napi_adatok_benchmark_norm, "tavasz", "116004", "119003", valtozo = "Relatív")

# Tiszafüred vs Balatonederics ősz
plot_ecdf_par(napi_adatok_benchmark_norm, "ősz", "116004", "119003", valtozo = "Relatív")

# Pákozd vs Balatonederics tél
plot_ecdf_par(napi_adatok_benchmark_norm, "tél", "107004", "119003", valtozo = "Relatív")

# Füle vs Balatonederics tél
plot_ecdf_par(napi_adatok_benchmark_norm, "tél", "107006", "119003", valtozo = "Relatív")

# Füle vs Örvényes tél
plot_ecdf_par(napi_adatok_benchmark_norm, "tél", "107006", "119006", valtozo = "Relatív")


# ~~~~    Ábra KS alapján a nem eltérő eloszlásokra a napi forgalom alapján      ~~~~~ #

# Tiszafüred vs Balatonederics ősz
plot_ecdf_par(napi_adatok_benchmark_norm, "ősz", "116004", "119003", valtozo = "Nyers")

# Füle vs Örvényes tél
plot_ecdf_par(napi_adatok_benchmark_norm, "tél", "107006", "119006", valtozo = "Nyers")


# ~~~~~~~~     ECDF-alapú átlagos eltérése a relatív benchmark forgalmi adatokra      ~~~~~~~~~~ #

ecdf_tavasz_rel <- ecdf_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo     = "Relatív",
  target_evszak = "tavasz"
)

ecdf_nyar_rel <- ecdf_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo     = "Relatív",
  target_evszak = "nyár"
)

ecdf_osz_rel <- ecdf_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo     = "Relatív",
  target_evszak = "ősz"
)

ecdf_tel_rel <- ecdf_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo     = "Relatív",
  target_evszak = "tél"
)

ecdf_eredmenyek_rel <- rbind(ecdf_tavasz_rel, ecdf_nyar_rel, ecdf_osz_rel, ecdf_tel_rel)

# Balatonederics vs Örvényes tél
plot_ecdf_par(napi_adatok_benchmark_norm, "tél", "119003", "119006", valtozo = "Relatív")

# Tiszafüred vs Balatonederics ősz
plot_ecdf_par(napi_adatok_benchmark_norm, "ősz", "116004", "119003", valtozo = "Relatív")

# Füle vs Balatonederics tél
plot_ecdf_par(napi_adatok_benchmark_norm, "tél", "107006", "119003", valtozo = "Relatív")

# Füle vs Örvényes tél
plot_ecdf_par(napi_adatok_benchmark_norm, "tél", "107006", "119006", valtozo = "Relatív")

# Tiszafüred vs Balatonederics tavasz
plot_ecdf_par(napi_adatok_benchmark_norm, "tavasz", "116004", "119003", valtozo = "Relatív")



# ~~~~~~~~     ECDF-alapú átlagos eltérése a nyers forgalmi adatokra      ~~~~~~~~~~ #


ecdf_tavasz_nyers <- ecdf_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo     = "Nyers",
  target_evszak = "tavasz"
)

ecdf_nyar_nyers <- ecdf_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo     = "Nyers",
  target_evszak = "nyár"
)

ecdf_osz_nyers <- ecdf_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo     = "Nyers",
  target_evszak = "ősz"
)

ecdf_tel_nyers <- ecdf_allomasok_evszak(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo     = "Nyers",
  target_evszak = "tél"
)

ecdf_eredmenyek_nyers <- rbind(ecdf_tavasz_nyers, ecdf_nyar_nyers, ecdf_osz_nyers, ecdf_tel_nyers)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####           11. ÁLLOMÁSOK RENDELKEZÉSRE ÁLLÁSÁNAK IMPORTÁLÁSA             ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Ha esetelg a napi_adatok df formázásakor valami gond lép fel, futtasd újra ezt

# napi_adatok <- df_napi %>%
  # left_join(metadata %>% dplyr::select(azonosito, nev), by = "azonosito")


# Rendelkezésre állási adatok behívása és szűrése
df_rend_allas <- read_excel("Rendelkezésre állási adatok.xlsx", sheet = "Összefűzött adatok_teljes")
df_rend_allas <- df_rend_allas[,c(8,11,12,13)]
colnames(df_rend_allas) <- c("rend_allas", "ev", "evszak", "azonosito")
df_rend_allas$azonosito <- as.character(df_rend_allas$azonosito)

# 2020 nyártól vannak rendelekezésre állási adataink
# Mivel a covidot kivettük a vizsgálatból, ezért továbbra is csak a 2022, 2023, 2024
# évek maradnak a vizsgálatban
napi_adatok <- napi_adatok %>%
  filter(ev %in% evek)

# Számoljuk ki minden állomásra a relatív forgalmakat, mint korábban a benchmarkokra
napi_adatok_norm <- napi_adatok %>%
  group_by(azonosito, nev, evszak) %>%
  mutate(
    szezon_atlag = mean(Nyers, na.rm = TRUE),
    Relatív = Nyers / szezon_atlag
  ) %>%
  ungroup()

# Rendezzük az oszlopokat a szebb view-hoz
napi_adatok <- napi_adatok[, c(
  "azonosito", "nev", "irany", "ev", "evszak", "honap", "nap", "Nyers")]

napi_adatok_norm <- napi_adatok_norm[, c(
  "azonosito", "nev", "irany", "ev", "evszak", "honap", "nap",
  "Nyers", "Relatív", "szezon_atlag")]



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### 12. KS TESZTEK MINDEN (FÜGGETLENÜL BENCHMARKTÓL) ÁLLOMÁSRA MAJD HEATMAP ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Mivel a teszt, csak elegendő megfigyelt forgalmi érték mellett fut le,
# (hibát dob a nem elegendő y-ra)
# szükséges szűrni a rendelkezésre állási %-ra
# A szűrő ezen formájában azt ragadja meg, hogy
# egy adott évszakon belül a három évben összegezve az állomás elérte a 
# legalább 25%-ot rendelkezésre állásban


# Ahhoz, hogy ábrázolni tudjuk heatmapen az eredményeket, szükséges az adott év 
# adott időszakában megfigyelt rendelkezésre állási százalékokat valahogyan aggregálni
# hiszen a teszt 3 év egyes évszakaira futott, és jó esetben van 3 megfigyelt 
# rendelkezésre állási % érték a df-ben
# Létrehozzuk az aggregált rendelkezésre állásokat
df_rend_allas_agg <- df_rend_allas %>%
  filter(ev %in% evek) %>% 
  group_by(azonosito, evszak) %>%
  summarise(
    sum_rend_allas = sum(rend_allas, na.rm = T),
    .groups = "drop"
  )

# Aggregált rendelkezésre állásokra szűrűnk sum(rend_allas) >=0.25
df_rend_allas_agg_szurt <- df_rend_allas_agg %>% 
  filter(sum_rend_allas >= 0.25)

# Szűrünk a kiválasztott állomásokra
napi_adatok_norm_szurt <- napi_adatok_norm %>%
  semi_join(df_rend_allas_agg_szurt, by = c("azonosito", "evszak"))


# ~~~~~~~~     KS-alapú eltérések a relatív forgalmi adatokra      ~~~~~~~~~~ #

ks_tavasz_teljes_rel <- ks_allomasok_evszak(
  napi_adatok = napi_adatok_norm_szurt,
  valtozo = "Relatív",
  target_evszak = "tavasz"
)

ks_nyár_teljes_rel <- ks_allomasok_evszak(
  napi_adatok = napi_adatok_norm_szurt,
  valtozo = "Relatív",
  target_evszak = "nyár"
)

ks_ősz_teljes_rel <- ks_allomasok_evszak(
  napi_adatok = napi_adatok_norm_szurt,
  valtozo = "Relatív",
  target_evszak = "ősz"
)

ks_tél_teljes_rel <- ks_allomasok_evszak(
  napi_adatok = napi_adatok_norm_szurt,
  valtozo = "Relatív",
  target_evszak = "tél"
)

ks_eredmenyek_teljes_rel <- rbind(ks_tavasz_teljes_rel, ks_nyár_teljes_rel,
                                  ks_ősz_teljes_rel, ks_tél_teljes_rel)


# Tegyük az eredmények adattáblába az allomas_A és allomas_B mellé a megfelelő
# rendelkezésre állási sum értékeket
ks_eredmenyek_teljes_rel <- ks_eredmenyek_teljes_rel %>%
  # join allomas_A-ra
  left_join(
    df_rend_allas_agg %>% 
      rename(allomas_A = azonosito,
             sum_rend_allas_A = sum_rend_allas),
    by = c("allomas_A", "evszak")
  ) %>%
  # join allomas_B-re
  left_join(
    df_rend_allas_agg %>% 
      rename(allomas_B = azonosito,
             sum_rend_allas_B = sum_rend_allas),
    by = c("allomas_B", "evszak")
  )

### ÁRBRA ELKÉSZÍTÉSE (HEATMAP) ###
ks_plot <- ks_eredmenyek_teljes_rel %>%
  mutate(
    A_cat = cut(sum_rend_allas_A, breaks = 15),
    B_cat = cut(sum_rend_allas_B, breaks = 15)
  )

ggplot(ks_plot, aes(x = A_cat, y = B_cat, fill = statistic)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(
    x = "Rendelkezésre állás összege (A_állomás) (kategória)",
    y = "Rendelkezésre állás összege (B_állomás) (kategória)",
    fill = "statistic",
    title = "Heatmap az eloszlások KS-alapú eltéréseire"
  )

# ~~~~~~~~     EDCF átlagolás-alapú eltérések a relatív forgalmi adatokra      ~~~~~~~~~~ #

ecdf_tavasz_teljes_rel <- ecdf_allomasok_evszak(
  napi_adatok = napi_adatok_norm_szurt,
  valtozo     = "Relatív",
  target_evszak = "tavasz"
)

ecdf_nyar_teljes_rel <- ecdf_allomasok_evszak(
  napi_adatok = napi_adatok_norm_szurt,
  valtozo     = "Relatív",
  target_evszak = "nyár"
)

ecdf_osz_teljes_rel <- ecdf_allomasok_evszak(
  napi_adatok = napi_adatok_norm_szurt,
  valtozo     = "Relatív",
  target_evszak = "ősz"
)

ecdf_tel_teljes_rel <- ecdf_allomasok_evszak(
  napi_adatok = napi_adatok_norm_szurt,
  valtozo     = "Relatív",
  target_evszak = "tél"
)

ecdf_eredmenyek_teljes_rel <- rbind(ecdf_tavasz_teljes_rel, ecdf_nyar_teljes_rel,
                                    ecdf_osz_teljes_rel, ecdf_tel_teljes_rel)

# Tegyük az eredmények adattáblába az allomas_A és allomas_B mellé a megfelelő
# rendelkezésre állási sum értékeket
ecdf_eredmenyek_teljes_rel <- ecdf_eredmenyek_teljes_rel %>%
  # join allomas_A-ra
  left_join(
    df_rend_allas_agg %>% 
      rename(allomas_A = azonosito,
             sum_rend_allas_A = sum_rend_allas),
    by = c("allomas_A", "evszak")
  ) %>%
  # join allomas_B-re
  left_join(
    df_rend_allas_agg %>% 
      rename(allomas_B = azonosito,
             sum_rend_allas_B = sum_rend_allas),
    by = c("allomas_B", "evszak")
  )


### ÁRBRA ELKÉSZÍTÉSE (HEATMAP) ###

ecdf_plot <- ecdf_eredmenyek_teljes_rel %>%
  mutate(
    A_cat = cut(sum_rend_allas_A, breaks = 15),
    B_cat = cut(sum_rend_allas_B, breaks = 15)
  )

ggplot(ecdf_plot, aes(x = A_cat, y = B_cat, fill = avg_diff)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(
    x = "Rendelkezésre állás összege (A_állomás) (kategória)",
    y = "Rendelkezésre állás összege (B_állomás) (kategória)",
    fill = "avg_diff",
    title = "Heatmap az eloszlások ECDF átlagolás-alapú eltéréseire"
  )




################ EDDIG CSINÁLTAM ÉN - LEVI ############
# Innentől az idősoros ábrázolások vannak, de mivel belenyúltam a korábbi kódokba is
# lehet hogy nem fog lefutni, ha akarjuk használni, akkor ezeket a kódokat újra át kell nézni

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
###################### 13.LOGNORMÁLIS SŰRŰSŰG ELLENŐRZÉSE #######################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

lognorm_evszak <- function(
    df,
    target_evszak,
    valtozo = c("Relatív", "Nyers"),
    alpha = 0.05,
    zero_offset = 1.001
) {
  valtozo <- match.arg(valtozo)
  
  df %>%
    filter(evszak == target_evszak) %>%
    group_by(azonosito, nev, evszak) %>%
    group_modify(~ {
      
      x <- .x[[valtozo]]
      x <- x[!is.na(x)]
      
      if (length(x) < 3) {
        return(tibble(
          log_ertekek = list(NA),
          shapiro_p   = NA_real_,
          normalis    = NA,
          atlag       = NA_real_,
          szoras      = NA_real_
        ))
      }
      
      if (any(x == 0)) {
        x <- x + zero_offset
      }
      
      log_x <- log(x)
      
      sw <- shapiro.test(log_x)
      normalis <- sw$p.value > alpha
      
      fit <- fitdistr(log_x, "normal")
      
      tibble(
        log_ertekek = list(log_x),
        shapiro_p   = sw$p.value,
        normalis    = normalis,
        atlag       = fit$estimate["mean"],
        szoras      = fit$estimate["sd"]
      )
    }) %>%
    ungroup() %>%
    arrange(desc(normalis))
}


#tavaszra - relatív

lognorm_tavasz_rel <- lognorm_evszak(
  df = napi_adatok_norm_szurt,
  target_evszak = "tavasz",
  valtozo = "Relatív")

#tavaszra - nyers

lognorm_tavasz_nyers <- lognorm_evszak(
  df = napi_adatok_norm_szurt,
  target_evszak = "tavasz",
  valtozo = "Nyers")

#nyárra- relatív --> Sátoraljaújhely, Füle, Zalaszentgyörgy
lognorm_nyar_rel <- lognorm_evszak(
  df = napi_adatok_norm_szurt,
  target_evszak = "nyár",
  valtozo = "Relatív")

#nyárra - nyers --> Füle

lognorm_nyar_nyers <- lognorm_evszak(
  df = napi_adatok_norm_szurt,
  target_evszak = "nyár",
  valtozo = "Nyers")

#őszre - relatív --> Mélykút

lognorm_osz_rel <- lognorm_evszak(
  df = napi_adatok_norm_szurt,
  target_evszak = "ősz",
  valtozo = "Relatív")

#őszre - nyers --> Pécs, Ranga László út

lognorm_osz_nyers <- lognorm_evszak(
  df = napi_adatok_norm_szurt,
  target_evszak = "ősz",
  valtozo = "Nyers")

#télre - relatív --> Visegrád
lognorm_tel_rel <- lognorm_evszak(
  df = napi_adatok_norm_szurt,
  target_evszak = "tél",
  valtozo = "Relatív")

#télre - nyers
lognorm_tel_nyers <- lognorm_evszak(
  df = napi_adatok_norm_szurt,
  target_evszak = "tél",
  valtozo = "Nyers")

#nézzük meg a Fülétkülön, majd ezután egy függvénnyal, hogy egyszerűbb legyen
fule_suruseg <- napi_adatok_norm_szurt %>%
  filter(azonosito == "107006") %>%
  mutate(logrel = log(Relatív + 0.001))

ggplot(fule_suruseg, aes(x = logrel)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Logaritmizált Relatív változó sűrűsége",
    x = "log(Relativ)",
    y = "Sűrűség"
  )

qqnorm(fule_suruseg$logrel)
qqline(fule_suruseg$logrel, col = "red")

#függvény az ábrázoláshoz:

lognorm_suruseg <- function(df = napi_adatok_norm_szurt,
                                target_evszak,
                                azonositok,
                                valtozo = "Relatív", # "Relatív" vagy "Nyers"
                                facet = TRUE) {
  
  if (!valtozo %in% c("Relatív", "Nyers")) {
    stop("A 'valtozo' argumentumnak 'Relatív' vagy 'Nyers' értéket kell adni.")
  }

  adat <- df %>%
    filter(evszak == target_evszak,
           azonosito %in% azonositok)
  
  adat <- adat %>%
    mutate(
      érték = .data[[valtozo]],
      
      # Ha 0 vagy kisebb → állítsuk át 1.001-re
      érték = ifelse(is.na(érték) | érték <= 0, 1.001, érték),
      
      logrel = log(érték),
      nev = as.factor(nev)
    )
  
  p <- ggplot(adat, aes(x = logrel, fill = nev)) +
    geom_density(alpha = 0.4) +
    labs(
      title = paste0("log(", valtozo, ") sűrűségfüggvénye – Évszak: ", target_evszak),
      x = paste0("log(", valtozo, ")"),
      fill = "Név"
    ) +
    theme_minimal(base_size = 14)
  
  if (facet && length(azonositok) > 1) {
    p <- p + facet_wrap(~ nev, scales = "free")
  }
  
  return(p)
}

#tavazsra ugye nincsen normális eo

#nyárra - relatív
#Sátoraljaújhely 105002, Füle 107006, és Zalaszentgyörgy 120002

lognorm_suruseg(napi_adatok_norm_szurt, "nyár", azonositok = c("105002", "107006",
                                                               "120002"), "Relatív")

#nyár - nyers
#csak Füle 107006

lognorm_suruseg(napi_adatok_norm_szurt, "nyár", "107006", "Nyers")

#ősz- relatív
#Mélykút 103010

lognorm_suruseg(napi_adatok_norm_szurt, "ősz", "103010", "Relatív")

#ősz - nyers
#Pécs 102001

lognorm_suruseg(napi_adatok_norm_szurt, "ősz", "102001", "Nyers") #ez érdekes...

#tél - relatív
#Visegrád 113003

lognorm_suruseg(napi_adatok_norm_szurt, "tél", "113003", "Relatív")

#téli nyers adatoknál ugyancsak nincsen normálisnak eo-nak tekintett állomás

#Mivel alig-alig van olyan eo-nk, amire a teszt alapján rá lehetne mondani, hogy
#valamilyen pm-ű lognorm eo-t követ, illetve ezek közül egyik sem BM állomás, ez
#a rész itt véget ér.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
######## 14.1. TESZTSTATISZTIKA ÉS RENDÁLLÁSOK KAPCSOLATÁNAK VIZSGÁLATA #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

#itt az egész vizsgált időszakban megfigyelt és Levi által kiszámolt rend. adatok
#kerültek az állomások adott évszakához

napi_szurt_rendall<- napi_adatok_norm_szurt %>%
  left_join(
    df_rend_allas_agg %>%
      dplyr::select(azonosito, evszak, sum_rend_allas),
    by = c("azonosito", "evszak")
  )

#ha kell, itt van az éves bontás szerinti rendállás is
napi_szurt_rendall <- napi_szurt_rendall %>%
  left_join(
    df_rend_allas %>%
      dplyr::select(azonosito, ev, evszak, rend_allas),
    by = c("azonosito", "ev", "evszak")
  )

kulonbozo_sorok_rendall <- napi_szurt_rendall %>%
  filter(sum_rend_allas != rend_allas | is.na(sum_rend_allas) != is.na(rend_allas)) %>%
  nrow()
kulonbozo_sorok_rendall

egyezo_sorok_szama <- napi_szurt_rendall %>%
  filter(sum_rend_allas == rend_allas) %>%
  nrow()
egyezo_sorok_szama
#itt vannak bőven olyan sorok, amik megegyeznek.

ismetlodo_sorok_rendellosdf <- napi_szurt_rendall %>% 
  count(azonosito, honap, nap, irany) %>% 
  filter(n > 1)

max(ismetlodo_sorok$n)

#tehát itt vannak olyan hónap+nap+irány kombinációk, amik akár 3x is szerepelnek
#ebben a df-ben. Megnézem, hogy ez áll-e az eredeti df-re is

ismetlodos_sorok_nans <- napi_adatok_norm_szurt %>%
  count(azonosito, honap, nap, irany) %>% 
  filter(n > 1)

#megnézem, hogy a nyers értékek esetében is ismétlődnek-e ezek a sorok

ismetlodos_sorok_nans_nyers <- napi_adatok_norm_szurt %>%
  count(azonosito, honap, nap, irany, Nyers) %>% 
  filter(n > 1)

#Mindenesetre ha a Levi a summázottakkal számolt, ezt viszem tovább én is
#De mindez amúgy felesleges volt, mert sokkal hasznosabb lett volna a ks-es
#eredményeket tartalmazó df-ekhez hozzácsapni

#Első körben megnézem a tavaszi benchmarkok közül Tiszafüredet (ennek a kiválasztása
#most önkényes volt, ránézésre ez tűnt a legszebbnek, de ugyanúgy választhattam volna
#Edericset is)

tiszafured_osszehasonlitas <- ks_tavasz_teljes_rel %>%
  filter(allomas_A == "116004" | allomas_B == "116004")

#a másik állomáshoz tartozó rendelkezésre állással kiegészítve:

#először rendezem az adatokat, hogy könnyebb legyen joinelni

tiszafured_ossz_rendezett <- tiszafured_osszehasonlitas %>%
  mutate(
    csere = allomas_B_nev == "Tiszafüred",
    uj_allomas_A      = if_else(csere, allomas_B, allomas_A),
    uj_allomas_A_nev  = if_else(csere, allomas_B_nev, allomas_A_nev),
    uj_allomas_A_elem = if_else(csere, allomas_B_elem, allomas_A_elem),
    uj_allomas_B      = if_else(csere, allomas_A, allomas_B),
    uj_allomas_B_nev  = if_else(csere, allomas_A_nev, allomas_B_nev),
    uj_allomas_B_elem = if_else(csere, allomas_A_elem, allomas_B_elem)
  ) %>%
  transmute(
    allomas_A      = uj_allomas_A,
    allomas_B      = uj_allomas_B,
    allomas_A_nev  = uj_allomas_A_nev,
    allomas_B_nev  = uj_allomas_B_nev,
    allomas_A_elem = uj_allomas_A_elem,
    allomas_B_elem = uj_allomas_B_elem,
    across(c(evszak, statistic, p_value, p_adjusted_BH, `Szignifikáns-e az eltérés?`))
  )

#hozzácsapjuk a másikak állomáshozhoz a rendállásokat

tiszafured_ossz_rendezett <- tiszafured_ossz_rendezett %>%
  left_join(
    df_rend_allas_agg %>%
      dplyr::select(azonosito, evszak, sum_rend_allas), 
    by = c("allomas_B" = "azonosito", "evszak"    = "evszak")
  )

#jöhet a heatmap

# Felosztjuk a statistic és a sum_rend_allas értékeket
tiszafured_heatmap_df <- tiszafured_ossz_rendezett %>%
  filter(!is.na(sum_rend_allas)) %>%
  mutate(
    
    #a breaks arggal lehet a kategóriák intervalljai állítani, minél nagyobb az
    #érték, annál több kategóriát hoz léte
    
    #binning a d stathoz
    statistic_bin = cut(statistic, breaks = 10, include.lowest = TRUE,
                        ordered_result = TRUE),
    #binning a rendálláshoz
    allas_bin = cut(sum_rend_allas, breaks = 10, include.lowest = TRUE,
                    ordered_result = TRUE)
  ) %>%
  # Megszámoljuk az egyes cellákban lévő pontok számát
  group_by(statistic_bin, allas_bin) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  )

tiszafured_heatmap <- ggplot(tiszafured_heatmap_df, aes(x = allas_bin, y = statistic_bin, fill = count)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Megfigyelések száma") +
  labs(
    title = "A rendelkezésre állás és a teszt statisztika kapcsolata",
    x = "Rendelkezésre állás",
    y = "D-statisztika értéke"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

print(tiszafured_heatmap)

#nem vagyok benne biztos, hogy ez az, amit akartunk...
#ezen a heatmapen ugye azt látjuk, hogy az egyes cellákba (ahol kategorizálva
#van a D-stat és a rendállás is) hány megfigyelés tartozik. 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
######## 14.2. STAT. ÉRTÉKEK ÉS REND.ÁLLÁSOK EGYÜTTJÁRÁSA #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

#előbb a Tiszafüredi példát folytatva

tiszafured_korr_df <- tiszafured_ossz_rendezett %>%
  dplyr::select(statistic, sum_rend_allas) %>%
  na.omit()

tiszafured_d_rendall <- ggplot(tiszafured_korr_df,
                               aes(x = sum_rend_allas, y = statistic)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Rendelkezésre állás",
    y = "D-statisztika értéke",
    title = "Tiszafüred (tavasz): D-statisztika és rendelkezésre állás kapcsolata"
  ) +
  theme_minimal()

tiszafured_d_rendall

#érdemes megnézni úgy is, ha lm helyett loess illesztést használunk

tiszafured_d_rendall_loess <- ggplot(tiszafured_korr_df,
                                     aes(x = sum_rend_allas, y = statistic)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    x = "Rendelkezésre állás",
    y = "D-statisztika értéke",
    title = "Tiszafüred (tavasz): D-statisztika és rendelkezésre állás kapcsolata"
  ) +
  theme_minimal()

tiszafured_d_rendall_loess

#Az ábra alapján azt mondhatjuk, hogy minél nagyobb volt az adott állomás rendállása,
#annál kisebb D-statsiztikát kaptunk a kétmintás KS teszttel az állomást Tisza-
#füredhez hasonlítva, vagyis a két állomás tapasztalati eloszlásafüggvénye konvergál
#egymáshoz, ahogy a másik állomás rendelekezésre állása nő. Az én konklúzióm ebből
#az, hogy az állomások tapasztalati eloszlásfüggvényei (függetelnül azok földrajzi
#elhelyezkedéstől és minden egyéb tényezőtől) valójában eléggé hasonlóak.
#Ez persze csak egy plot alapján eléggé elhamarkodott kijelentés.
#Ha nem hozzuk létre a "szupereloszlást" (amit később a 14.2 pontban próbáltam
#létrehozni), akkor azt csináljuk, hogy a benchmarkok közül kiválasztunk egy etalon
#állomást, és ehhez nézzük meg a D-statisztika és a rendelkezés állás kapcsolatát,
#ahogy tettem az imént Tiszafüreddel. Első körben az állomás kiválasztása történik
#(következő szakasz), majd az ábrák létrehozása (kiválasztott állomások vs többi állomás
#szakasz)

############ referencia állomás kiválasztása a benchmarkok közül ###########

#kiválasztom előbb a rendelkezésre állás, majd a "szemteszt" alapján legjobbnak
#ítélt eloszlást a benchmarkok közül

#minden évszak legmagasabb rendállású állomása:

#tavasz

ref_rendall_tavasz <- napi_szurt_rendall %>%
  filter(evszak == "tavasz") %>%
  filter(sum_rend_allas == max(sum_rend_allas, na.rm = TRUE)) %>%
  dplyr::select(azonosito, nev) %>%
  slice_head(n = 1)
ref_rendall_tavasz #Zánka

#nyár

ref_rendall_nyar <- napi_szurt_rendall %>%
  filter(evszak == "nyár") %>%
  filter(sum_rend_allas == max(sum_rend_allas, na.rm = TRUE)) %>%
  dplyr::select(azonosito, nev) %>%
  slice_head(n = 1)
ref_rendall_nyar #Örvényes

#ősz

ref_rendall_osz <- napi_szurt_rendall %>%
  filter(evszak == "ősz") %>%
  filter(sum_rend_allas == max(sum_rend_allas, na.rm = TRUE)) %>%
  dplyr::select(azonosito, nev) %>%
  slice_head(n = 1)
ref_rendall_osz #Ederics

#tél

ref_rendall_tel <- napi_szurt_rendall %>%
  filter(evszak == "tél") %>%
  filter(sum_rend_allas == max(sum_rend_allas, na.rm = TRUE)) %>%
  dplyr::select(azonosito, nev) %>%
  slice_head(n = 1)
ref_rendall_tel #Tarcal

#egyben:
ref_rendall_tavasz$szezon <- "tavasz"
ref_rendall_nyar$szezon <- "nyár"
ref_rendall_osz$szezon <- "ősz"
ref_rendall_tel$szezon <- "tél"

#egyben:
ref_rendall <- rbind(tavasz_ref_rendall, nyar_ref_rendall, 
                     osz_ref_rendall, tel_ref_rendall)

#szemrevételezés alapján a legjobb eloszlású állomások minden évszakhoz:

#újra a szezonális benchmarkok eloszlásainak ábrái:

plot_ecdf_minden_allomas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "tavasz"
)

plot_ecdf_minden_allomas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "nyár"
)

plot_ecdf_minden_allomas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "ősz"
)

plot_ecdf_minden_allomas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "tél"
)

#ezek alpján a következőket választottam:

#tavasz

ref_vizual_tavasz <- tibble(
  azonosito = 116004,
  nev = "Tiszafüred",
  evszak = "tavasz"
)

#nyár

ref_vizual_nyar <- tibble(
  azonosito = 116004,
  nev = "Tiszafüred",
  evszak = "nyár"
)

#ősz

ref_vizual_osz <- tibble(
  azonosito = 116004,
  nev = "Tiszafüred",
  evszak = "ősz"
)

#tél

ref_vizual_tel <- tibble(
  azonosito = 108001,
  nev = "Fertőszéplak, Soproni u.",
  evszak = "tél"
)

#egyben:
ref_vizual <- rbind(ref_vizual_tavasz, ref_vizual_nyar, ref_vizual_osz, ref_vizual_tel)

### kiválasztott állomások vs többi állomás a D-stat és rendállás függvényében ####

#függvény, ami végigcsinálja ugyanezt a folyamatot (mint Tiszafüreddel fentebb)

osszehasonlitas_rendallas_plot <- function(
    allomas_szuro_df,
    rendallas_df,
    target_azonosito,
    plot_method = c("lm", "loess")
) {
  
  plot_method <- match.arg(plot_method)
  
  # 1. Célállomás kiszűrése
  osszehasonlitas <- allomas_szuro_df %>%
    dplyr::filter(
      allomas_A == target_azonosito |
        allomas_B == target_azonosito
    )
  
  # 2. Állomások "rendezése", hogy a target mindig A oldalon legyen
  rendezett_df <- osszehasonlitas %>%
    dplyr::mutate(
      csere = allomas_B == target_azonosito,
      uj_allomas_A      = dplyr::if_else(csere, allomas_B, allomas_A),
      uj_allomas_A_nev  = dplyr::if_else(csere, allomas_B_nev, allomas_A_nev),
      uj_allomas_A_elem = dplyr::if_else(csere, allomas_B_elem, allomas_A_elem),
      uj_allomas_B      = dplyr::if_else(csere, allomas_A, allomas_B),
      uj_allomas_B_nev  = dplyr::if_else(csere, allomas_A_nev, allomas_B_nev),
      uj_allomas_B_elem = dplyr::if_else(csere, allomas_A_elem, allomas_B_elem)
    ) %>%
    dplyr::transmute(
      allomas_A      = uj_allomas_A,
      allomas_B      = uj_allomas_B,
      allomas_A_nev  = uj_allomas_A_nev,
      allomas_B_nev  = uj_allomas_B_nev,
      allomas_A_elem = uj_allomas_A_elem,
      allomas_B_elem = uj_allomas_B_elem,
      dplyr::across(
        c(evszak, statistic, p_value, p_adjusted_BH, `Szignifikáns-e az eltérés?`)
      )
    )
  
  # 3. Rendelkezésre állás hozzájoinolása
  rendezett_df <- rendezett_df %>%
    dplyr::left_join(
      rendallas_df %>%
        dplyr::select(azonosito, evszak, sum_rend_allas),
      by = c("allomas_B" = "azonosito", "evszak" = "evszak")
    )
  
  # 4. Korrelációs adatkeret
  korr_df <- rendezett_df %>%
    dplyr::select(statistic, sum_rend_allas) %>%
    tidyr::drop_na()
  
  # 5. Címhez szükséges metaadatok
  allomas_nev <- unique(rendezett_df$allomas_A_nev)[1]
  evszak_nev  <- unique(rendezett_df$evszak)[1]
  
  if (length(unique(rendezett_df$allomas_A_nev)) > 1) {
    warning("Több különböző állomásnév található az adatokban!")
  }
  
  # 6. Plot
  ggplot(korr_df, aes(x = sum_rend_allas, y = statistic)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = plot_method, se = TRUE) +
    labs(
      x = "Rendelkezésre állás",
      y = "D-statisztika értéke",
      title = paste0(
        allomas_nev, " (", evszak_nev,
        "): D-statisztika és rendelkezésre állás kapcsolata",
        " (", plot_method, " illesztés)"
      )
    ) +
    theme_minimal()
}

#nézzük meg előbb a rendelkezésre állások alapján

#itt használható az aggregált rendállás df (df_rend_allas_agg) és a Levi által 
#0.25 fölöttre szűrt df_rend_allas_agg_szurt df is, de az általam megnézett
#plotokban nincsen különbség a kettő között (nem néztem végig az összes lehetséges
#kombinációt)

#TAVASZRA + LM

ref_rendall_tavasz

osszehasonlito_plot_tavasz_rendall <- osszehasonlitas_rendallas_plot(
  allomas_szuro_df = ks_tavasz_teljes_rel,
  rendallas_df     = df_rend_allas_agg,
  target_azonosito = "119004",
  plot_method      = "lm")

osszehasonlito_plot_tavasz_rendall

#NYÁRRA + LOESS

ref_rendall_nyar

osszehasonlito_plot_nyar_rendall <- osszehasonlitas_rendallas_plot(
  allomas_szuro_df = ks_nyár_teljes_rel,
  rendallas_df     = df_rend_allas_agg,
  target_azonosito = "119006",
  plot_method      = "loess")

osszehasonlito_plot_nyar_rendall

#itt érdemes lenne megnézni, hogy melyik állomás viszi el a LOESS-t ennyire ott
#a ~0.6-os D-stat és 2.1-es rendállásnál

#ŐSZRE + LM

ref_rendall_osz

osszehasonlito_plot_osz_rendall <- osszehasonlitas_rendallas_plot(
  allomas_szuro_df = ks_ősz_teljes_rel,
  rendallas_df     = df_rend_allas_agg,
  target_azonosito = "119003",
  plot_method      = "lm")

osszehasonlito_plot_osz_rendall

#TÉLRE + LOESS

ref_rendall_tel

osszehasonlito_plot_tel_rendall <- osszehasonlitas_rendallas_plot(
  allomas_szuro_df = ks_tél_teljes_rel,
  rendallas_df     = df_rend_allas_agg,
  target_azonosito = "105003",
  plot_method      = "loess")

osszehasonlito_plot_tel_rendall

#a szemteszt alapján is nézzük meg őket (egyik évszak esetében sem a legmagasabb
#rendelkezésre állású állomás lett kiválasztva):

ref_vizual

#TAVASZRA + LM

osszehasonlito_plot_tavasz_vizual <- osszehasonlitas_rendallas_plot(
  allomas_szuro_df = ks_tavasz_teljes_rel,
  rendallas_df     = df_rend_allas_agg,
  target_azonosito = "116004",
  plot_method      = "lm")

osszehasonlito_plot_tavasz_vizual

#NYÁRRA + LOESS

osszehasonlito_plot_nyar_vizual <- osszehasonlitas_rendallas_plot(
  allomas_szuro_df = ks_nyár_teljes_rel,
  rendallas_df     = df_rend_allas_agg,
  target_azonosito = "116004",
  plot_method      = "loess")

osszehasonlito_plot_nyar_vizual

#ŐSZRE + LM

osszehasonlito_plot_osz_vizual <- osszehasonlitas_rendallas_plot(
  allomas_szuro_df = ks_ősz_teljes_rel,
  rendallas_df     = df_rend_allas_agg,
  target_azonosito = "116004",
  plot_method      = "lm")

osszehasonlito_plot_osz_vizual

#TÉLRE + LOESS

osszehasonlito_plot_tel_vizual <- osszehasonlitas_rendallas_plot(
  allomas_szuro_df = ks_tél_teljes_rel,
  rendallas_df     = df_rend_allas_agg,
  target_azonosito = "108001",
  plot_method      = "loess")

osszehasonlito_plot_tel_vizual

#a két megközelítés összehasonlítása egy ábrán:

#tavasz

osszehasonlito_plot_tavasz_rendall / osszehasonlito_plot_tavasz_vizual

#nyár

osszehasonlito_plot_nyar_rendall / osszehasonlito_plot_nyar_vizual

#ősz

osszehasonlito_plot_osz_rendall / osszehasonlito_plot_osz_vizual

#tél

osszehasonlito_plot_tel_rendall / osszehasonlito_plot_tel_vizual

########################## outlierek vizsgálata ################################

#például ősznél látni egy kb 1-es D-stat értéket, ami azt jelenti, hogy a legnagyobb
#a két eo függvényben az majdnem az egész intervallum

ks_osz_balatonedericsre_szurt <- ks_ősz_teljes_rel %>%
  filter(allomas_A == "119003" | allomas_B == "119003") %>%
  filter(statistic > 0.9)

#ez a Nyírtelek 115004 állomás

ecdf_bederics_nyirtelek <- plot_ecdf_par(napi_adatok = napi_szurt_rendall,
              target_evszak = "ősz",
              allomas_A = "119003",
              allomas_B = "115004",
              valtozo = "Relatív")
ecdf_bederics_nyirtelek

nyirtelek_relativ_ertekek <- napi_szurt_rendall %>%
  filter(azonosito == "115004")

hist(nyirtelek_relativ_ertekek$Relatív)

nrow(nyirtelek_relativ_ertekek)

#vagyis itt egy darab mérés volt 2022 11. 30-án, kemény 2 biciklist számolt, a
#többi nap végig 0, így a szezon átlag 0.01, a relatívja pedig ennél fogva 182 lett.
#érdekes még, hogy elfelejtettem az őszre szűrni, mégis csak 2022 őszi megfigyelé-
#sek vannak.



#ezek az outlierek akkor is bent maradnak, ha a df_rend_allas_szurt-et használom


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
######## 14.3. TESZTSTATISZTIKA ÉS RENDÁLLÁSOK KAPCSOLATÁNAK VIZSGÁLATA #########
########                         SZUPERELOSZLÁS                         #########             
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

#azt kéne csinálni, hogy ha megvan az aggregált optimalizált szupereloszlás, akkor
#ennek az átlagos ecdf eltérését különböző d stat szinteken ábrázolni -->  azt
#feltételezem, hogy minél nagyobb a rendállás a másik eloszlás esetében, annál
#kisebb a d-stat, azaz annál kisebb az átlagos eltérés a két ecdf között

#A Marci által említett "szupereloszlást" úgy fogom elkészíteni, hogy
  # 1. minden évszakhoz kiválasztok egy referencia értéket (az eddig láttottak alap-
#ján érdemes a legmagasabb rendállású állomást választani a benchmarkok közül)
  # 2. mivel mások a skálák, ezeket egyesíteni kell (percentilis skálázással)
  # 3. a többi állomáshoz kiszámolom a skálázó konstanst (OLS módszerrel)
  # 4. létrehozok egy új eloszlást, az közelített eloszlások precentiliseinek
#az átlagával számol (azaz percentilis értékeink lesznek)

#újra a szezonális benchmarkok eloszlásainak ábrái:

plot_ecdf_minden_allomas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "tavasz"
)

plot_ecdf_minden_allomas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "nyár"
)

plot_ecdf_minden_allomas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "ősz"
)

plot_ecdf_minden_allomas(
  napi_adatok = napi_adatok_benchmark_norm,
  valtozo = "Relatív",
  target_evszak = "tél"
)

#minden évszak legmagasabb rendállású állomása:

#tavasz

tavasz_ref_rendall <- napi_szurt_rendall %>%
  filter(evszak == "tavasz") %>%
  filter(sum_rend_allas == max(sum_rend_allas, na.rm = TRUE)) %>%
  dplyr::select(azonosito, nev) %>%
  slice_head(n = 1)
tavasz_ref_rendall #Zánka

#nyár

nyar_ref_rendall <- napi_szurt_rendall %>%
  filter(evszak == "nyár") %>%
  filter(sum_rend_allas == max(sum_rend_allas, na.rm = TRUE)) %>%
  dplyr::select(azonosito, nev) %>%
  slice_head(n = 1)
nyar_ref_rendall #Örvényes

#ősz

osz_ref_rendall <- napi_szurt_rendall %>%
  filter(evszak == "ősz") %>%
  filter(sum_rend_allas == max(sum_rend_allas, na.rm = TRUE)) %>%
  dplyr::select(azonosito, nev) %>%
  slice_head(n = 1)
osz_ref_rendall #Ederics

#tél

tel_ref_rendall <- napi_szurt_rendall %>%
  filter(evszak == "tél") %>%
  filter(sum_rend_allas == max(sum_rend_allas, na.rm = TRUE)) %>%
  dplyr::select(azonosito, nev) %>%
  slice_head(n = 1)
tel_ref_rendall #Tarcal

#egyben:
tavasz_ref_rendall$szezon <- "tavasz"
nyar_ref_rendall$szezon <- "nyár"
osz_ref_rendall$szezon <- "ősz"
tel_ref_rendall$szezon <- "tél"

ref_rendall <- rbind(tavasz_ref_rendall, nyar_ref_rendall, 
                     osz_ref_rendall, tel_ref_rendall)

#meg lehet határozni pusztán vizuális úton is a referenciákat, itt a tapeo fgny
#képe alapján próbáltam meg a legszebbet kiválasztani (viszonylag stabilan nőjjön,
#ne legyenek benne nagy ugrások, képe alapján nézzen ki úgy, mintha az lenne a többi
#etalonja). Ez alapján:

#tavasz

tavasz_ref_vizual <- tibble(
  azonosito = 116004,
  nev = "Tiszafüred",
  evszak = "tavasz"
)

#nyár

nyar_ref_vizual <- tibble(
  azonosito = 116004,
  nev = "Tiszafüred",
  evszak = "nyár"
)

#ősz

osz_ref_vizual <- tibble(
  azonosito = 116004,
  nev = "Tiszafüred",
  evszak = "ősz"
)

#tél

tel_ref_vizual <- tibble(
  azonosito = 108001,
  nev = "Fertőszéplak, Soproni u.",
  evszak = "tél"
)

#egyben:
ref_vizual <- rbind(tavasz_ref_vizual, nyar_ref_vizual, osz_ref_vizual, tel_ref_vizual)

######################## függvény definiálása ##################################

standard_eloszlas <- function(df,
                              target_evszak,
                              ref_azonosito,
                              nem_ref_azonositok,
                              kvantilis_felbontas = 100,
                              ertek_valtozo) {
  
  # 1. Szűrés
  df_filt <- df[df$evszak == target_evszak &
                  df$azonosito %in% c(ref_azonosito, nem_ref_azonositok), ]
  
  if (nrow(df_filt) == 0) {
    stop("A szűrt adatkeret üres. Ellenőrizd az évszakot és az azonosítókat.")
  }
  
  # 2. Kvantilis rács
  probs <- seq(0, 1, length.out = kvantilis_felbontas)
  
  # 3. Referencia eloszlás kvantilisek 
  ref_values <- df_filt[df_filt$azonosito == ref_azonosito, ertek_valtozo]
  ref_q <- quantile(ref_values, probs = probs, na.rm = TRUE)
  
  # 4. Benchmark eloszlások kvantilisek + OLS skálázás
  scaled_bench_list <- list()
  
  for (b in nem_ref_azonositok) {
    bench_values <- df_filt[df_filt$azonosito == b, ertek_valtozo]
    bench_q <- quantile(bench_values, probs = probs, na.rm = TRUE)
    
    # OLS regresszió az origón át
    c_i <- sum(bench_q * ref_q) / sum(bench_q^2)
    
    scaled_bench_list[[b]] <- c_i * bench_q
  }
  
  # 5. Egyesítés (Vincentizáció)
  if (length(scaled_bench_list) > 0) {
    bench_mat <- do.call(cbind, scaled_bench_list)
    combined_q <- rowMeans(cbind(ref_q, bench_mat))
  } else {
    combined_q <- ref_q
  }
  
  # 6. Eredmény DataFrame
  result <- data.frame(
    azonosito = 1,
    percentilis = probs,
    ertek = as.numeric(combined_q)
  )
  
  return(result)
}

#és függvény az ábrázolásához

plot_standard_eloszlas <- function(df, evszak, x = c("percentilis", "ertek")) {
  x <- match.arg(x)
  
  if (!all(c("percentilis", "ertek") %in% names(df))) {
    stop("A DataFrame-nek tartalmaznia kell a 'percentilis' és 'ertek' oszlopokat.")
  }
  
  if (x == "percentilis") {
    p <- ggplot(df, aes(x = percentilis, y = ertek)) +
      geom_line(color = "steelblue", linewidth = 1) +
      ggtitle(paste0("Standard eloszlásfüggvény: ", evszak)) +
      xlab("Percentilis (0–1)") +
      ylab("Érték")
    
  } else {
    p <- ggplot(df, aes(x = ertek, y = percentilis)) +
      geom_line(color = "darkorange", linewidth = 1) +
      ggtitle(paste0("Standard eloszlásfüggvény: ", evszak)) +
      xlab("Érték") +
      ylab("Percentilis (0–1)")
  }
  
  p + theme_minimal(base_size = 14)
}


###################### standard eloszlások évszakonként ########################

#TAVASZRA (Zánka 119004 referenciával)

tavasz_nem_ref_azonositok <- napi_adatok_benchmark_norm %>%
  filter(evszak == "tavasz", azonosito != "119004") %>%
  pull(azonosito) %>%
  unique()
tavasz_nem_ref_azonositok

#tavaszi standard elo 
tavasz_std_eo <- standard_eloszlas(napi_adatok_benchmark_norm, "tavasz",
                                   ref_azonosito = "119004",
                                   nem_ref_azonositok = tavasz_nem_ref_azonositok,
                                   ertek_valtozo = "Relatív")

tavasz_std_plot <- plot_standard_eloszlas(tavasz_std_eo, evszak = "tavasz", x= "ertek")
tavasz_std_plot

#NYÁRRA (örvényes 119006 referenciával)

nyar_nem_ref_azonositok <- napi_adatok_benchmark_norm %>%
  filter(evszak == "nyár", azonosito != "119006") %>%
  pull(azonosito) %>%
  unique()
nyar_nem_ref_azonositok

#nyári standard elo
nyar_std_eo <- standard_eloszlas(napi_adatok_benchmark_norm, "nyár",
                                   ref_azonosito = "119006",
                                   nem_ref_azonositok = nyar_nem_ref_azonositok,
                                   ertek_valtozo = "Relatív")

nyar_std_plot <- plot_standard_eloszlas(nyar_std_eo, evszak = "nyár", x = "ertek")
nyar_std_plot

#ŐSZRE (B.ederics 119003 referenciával)

osz_nem_ref_azonositok <- napi_adatok_benchmark_norm %>%
  filter(evszak == "ősz", azonosito != "119003") %>%
  pull(azonosito) %>%
  unique()
osz_nem_ref_azonositok

#őszi standard elo
osz_std_eo <- standard_eloszlas(napi_adatok_benchmark_norm, "ősz",
                                 ref_azonosito = "119003",
                                 nem_ref_azonositok = osz_nem_ref_azonositok,
                                 ertek_valtozo = "Relatív")

osz_std_plot <- plot_standard_eloszlas(osz_std_eo, evszak = "ősz", x = "ertek")
osz_std_plot

#TÉLRE (tarcal 105003 referenciával)

tel_nem_ref_azonositok <- napi_adatok_benchmark_norm %>%
  filter(evszak == "tél", azonosito != "105003") %>%
  pull(azonosito) %>%
  unique()
tel_nem_ref_azonositok

#őszi standard elo
tel_std_eo <- standard_eloszlas(napi_adatok_benchmark_norm, "tél",
                                ref_azonosito = "105003",
                                nem_ref_azonositok = tel_nem_ref_azonositok,
                                ertek_valtozo = "Relatív")

tel_std_plot <- plot_standard_eloszlas(tel_std_eo, evszak = "ősz", x = "ertek")
tel_std_plot

################# a standard összehasonlítása a többivel #######################

#itt a többin is végre kell hajtani a kvantilis illesztést, különben a skálák olyan
#mértékben különböznének, hogy a tesztnek semmi relevanciája nem lenne

#függvény definiálása

standard_osszehasonlito <- function(df,
                                evszak,
                                standard_df,
                                ertek_valtozo = c("Relatív", "Nyers"),
                                kvantilis_felbontas = 100) {
  
  ertek_valtozo <- match.arg(ertek_valtozo)
  
  # 1. Standard eloszlás kvantilisei
  std_q <- standard_df$ertek
  probs <- standard_df$percentilis
  
  # 2. Adatok szűrése az adott évszakhoz
  df_filt <- df %>%
    filter(evszak == evszak)
  
  azon_list <- unique(df_filt$azonosito)
  
  # Tárolók
  results <- vector("list", length(azon_list))
  
  # 3. Végigmegyünk minden azonosítón
  for (i in seq_along(azon_list)) {
    az <- azon_list[i]
    
    # Az adott állomás adatai
    subset_df <- df_filt %>% filter(azonosito == az)
    
    # név kinyerése
    nev_i <- subset_df %>% pull(nev) %>% unique() %>% first()
    
    vals <- subset_df %>% pull(ertek_valtozo)
    
    # Ha nincs elég adat
    if (length(vals) < 5) {
      results[[i]] <- tibble(
        azonosito = az,
        nev = nev_i,
        c_i = NA_real_,
        p_value = NA_real_,
        p_adj = NA_real_,
        signif = NA
      )
      next
    }
    
    # Kvantilisek
    vals_q <- quantile(vals, probs = probs, na.rm = TRUE)
    
    # 4. OLS skálázás
    c_i <- sum(vals_q * std_q) / sum(vals_q^2)
    
    scaled_vals <- c_i * vals_q
    
    # 5. KS-teszt
    ks <- ks.test(scaled_vals, std_q)
    
    results[[i]] <- tibble(
      azonosito = az,
      nev = nev_i,
      c_i = c_i,
      D_stat = ks$statistic,
      p_value = ks$p.value
    )
  }
  
  # 6. Eredmények összefűzése
  res <- bind_rows(results)
  
  # 7. BH-korrekció
  res <- res %>%
    mutate(
      p_adj = p.adjust(p_value, method = "BH"),
      signif = p_adj < 0.05
    )
  
  return(res)
}


tavasz_osszehasonlitas <- standard_osszehasonlito(
  df = napi_adatok_benchmark_norm,
  evszak = "tavasz",
  standard_df = nyar_std_eo,
  ertek_valtozo = "Relatív"
)

tavasz_osszehasonlitas



####### VÉGE : tesztstatisztika és rendállások kapcsolatának vizsgálata ########










## Függvény, amely egy adott állomásra ábrázolja a 3 év összegzett napi forgalmát
## egy adott évszakban:

plot_szintetikus_szezon <- function(napi_adatok, allomas_id, target_evszak) {
  plot_data <- napi_adatok %>%
    filter(
      azonosito == allomas_id,
      evszak == target_evszak)
  # Mivel a tábla csak hónapot és napot tartalmaz, hozzárendeljük a 2024-es
  # évet, de ez nem lesz rajta az ábrán, csak a számoláshoz kell
  plot_data <- plot_data %>%
    mutate(
      # Ha tél van és december, akkor legyen az év 2023,
      # minden más esetben maradjon 2024, így fog jól kinézni az ábra
      plot_year = if_else(evszak == "tél" & honap == 12, 2023, 2024),
      plot_date = make_date(year = plot_year, month = honap, day = nap)
    )
  # Állomás nevének kinyerése az első sorból (mivel az ID alapján szűrtünk)
  aktualis_nev <- plot_data$nev[1]
  ## Ábrázolás
  gg <- ggplot(plot_data, aes(x = plot_date, y = atlagolt_napi_forgalom)) +
    # Trendvonal (LOESS) - segít látni az ívet a napi ingadozások mögött
    geom_smooth(se = TRUE, color = "red", fill = "red", alpha = 0.15, size = 0.8) +
    # Az átlagos értékek vonala
    geom_line(color = "darkblue", size = 1, alpha = 0.8) +
    # Pontok
    geom_point(color = "darkblue", size = 2, fill = "white", shape = 21) +

    labs(
      title = paste0(target_evszak, ": szezonális lefolyás (3 év átlaga)"),
      subtitle = paste0(aktualis_nev, " (", allomas_id, ")"),
      x = NULL,
      y = "Átlagos napi kerékpáros forgalom (db)"
    ) +
    scale_x_date(date_labels = "%b %d.", date_breaks = "2 weeks") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )
  return(gg)
}

table(napi_osszesito_benchmark$azonosito, napi_osszesito_benchmark$nev)

# ~~~~~~~~~~~~~~~~~~~~                Próba               ~~~~~~~~~~~~~~~~~~~~ #

# Példa 1a: Balatonederics, nyár
print(plot_szintetikus_szezon(
  napi_adatok = napi_osszesito_benchmark,
  allomas_id = "119003",
  target_evszak = "nyár"
))

# Példa 1b: Örvényes, nyár
print(plot_szintetikus_szezon(
  napi_adatok = napi_osszesito_benchmark,
  allomas_id = "119006",
  target_evszak = "nyár"
))

# Példa 2a: Baja, tavasz
print(plot_szintetikus_szezon(
  napi_adatok = napi_osszesito_benchmark,
  allomas_id = "103009",
  target_evszak = "tavasz"
))

# Példa 2b: Dunakeszi, tavasz
print(plot_szintetikus_szezon(
  napi_adatok = napi_osszesito_benchmark,
  allomas_id = "113006",
  target_evszak = "tavasz"
))

# Példa 3a: Balatonederics, tél
print(plot_szintetikus_szezon(
  napi_adatok = napi_osszesito_benchmark,
  allomas_id = "119003",
  target_evszak = "tél"
))

# Példa 3b: Füle, tél
print(plot_szintetikus_szezon(
  napi_adatok = napi_osszesito_benchmark,
  allomas_id = "107006",
  target_evszak = "tél"
))

# ...

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Készítünk egy olyan adatbázist, ahol az egyes állomások napi forgalmát az
## adott évszakban megfigyelhető átlagos forgalomhoz viszonyítjuk, ezzel
## az eltérő forgalmú állomások összehasonlíthatóvá válnak:

napi_adatok_norm <- napi_adatok %>%
  group_by(azonosito, nev, evszak) %>%
  mutate(
    szezon_atlag = mean(atlagos_napi_forgalom, na.rm = TRUE),
    relativ_forg = atlagos_napi_forgalom / szezon_atlag
  ) %>%
  ungroup()

plot_szezon_norm <- function(napi_adatok, allomas_id, target_evszak) {
  plot_data <- napi_adatok %>%
    filter(
      azonosito == allomas_id,
      evszak == target_evszak)
  # Mivel a tábla csak hónapot és napot tartalmaz, hozzárendeljük a 2024-es
  # évet, de ez nem lesz rajta az ábrán, csak a számoláshoz kell
  plot_data <- plot_data %>%
    mutate(
      # Ha tél van és december, akkor legyen az év 2023,
      # minden más esetben maradjon 2024, így fog jól kinézni az ábra
      plot_year = if_else(evszak == "tél" & honap == 12, 2023, 2024),
      plot_date = make_date(year = plot_year, month = honap, day = nap)
    )
  # Állomás nevének kinyerése az első sorból (mivel az ID alapján szűrtünk)
  aktualis_nev <- plot_data$nev[1]
  ## Ábrázolás
  gg <- ggplot(plot_data, aes(x = plot_date, y = relativ_forg)) +
    # Trendvonal (LOESS) - segít látni az ívet a napi ingadozások mögött
    geom_smooth(se = TRUE, color = "red", fill = "red", alpha = 0.15, size = 0.8) +
    # Az átlagos értékek vonala
    geom_line(color = "darkblue", size = 1, alpha = 0.8) +
    # Pontok
    geom_point(color = "darkblue", size = 2, fill = "white", shape = 21) +
    
    labs(
      title = paste0(target_evszak, ": szezonális lefolyás (3 év átlaga)"),
      subtitle = paste0(aktualis_nev, " (", allomas_id, ")"),
      x = NULL,
      y = "Évszakos átlaghoz viszonyított napi kerékpárforgalom"
    ) +
    scale_x_date(date_labels = "%b %d.", date_breaks = "2 weeks") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )
  return(gg)
}

# ~~~~~~~~~~~~~~~~~~~~               Próba                ~~~~~~~~~~~~~~~~~~~~ #

# Példa 1a: Tiszafüred, nyár
print(plot_szezon_norm(
  napi_adatok = napi_adatok_norm,
  allomas_id = "116004",
  target_evszak = "nyár"
))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Készítünk olyan ábrát, ahol az adott évszakra vonatkozóan az összes
## benchmark állomás LOESS görbéjét kirajzoljuk, hogy megnézzük, mennyire
## hasonlóak a szezonális mintázatok:

plot_szezon_osszes_allomas <- function(napi_adatok_norm, target_evszak) {
  # Szűrés az évszakra
  plot_data <- napi_adatok_norm %>%
    filter(evszak == target_evszak)
  # Dátum manipuláció (téli dátumok javítása) - ugyanaz a logika
  plot_data <- plot_data %>%
    mutate(
      plot_year = if_else(evszak == "tél" & honap == 12, 2023, 2024),
      plot_date = make_date(year = plot_year, month = honap, day = nap)
    )
  
  gg <- ggplot(plot_data, aes(x = plot_date, y = relativ_forg, color = nev, group = nev)) +
    # Referenciavonal (1.0 = átlag)
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", size = 0.8) +
    # A. Opció: Simított trendvonalak (ez a legszebb sok állomásnál)
    # A 'se = FALSE' kikapcsolja a szürke árnyékolást, hogy ne legyen káosz
    geom_smooth(se = FALSE, size = 1.2, alpha = 0.8, method = "loess", span = 0.3) +
    labs(
      title = paste0(target_evszak, ": szezonális trendek összehasonlítása"),
      subtitle = "Relatív forgalom (1.0 = saját szezonális átlag)",
      x = NULL,
      y = "Relatív forgalom (szorzószám)",
      color = "Állomás  "
    ) +
    scale_x_date(date_labels = "%b %d.", date_breaks = "2 weeks") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",      # Jelmagyarázat alulra
      legend.direction = "horizontal", # Vízszintes elrendezés
      legend.text = element_text(size = 9),
      legend.margin = margin(t = 5)    # Kis távolság a grafikon aljától
    ) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
  return(gg)
}

# ~~~~~~~~~~~~~~~~~~~~           Ábrák generálása         ~~~~~~~~~~~~~~~~~~~~ #

# 1. TAVASZ 
print(plot_szezon_osszes_allomas(napi_adatok_norm, "tavasz"))

# 2. NYÁR
print(plot_szezon_osszes_allomas(napi_adatok_norm, "nyár"))

# 3. ŐSZ
print(plot_szezon_osszes_allomas(napi_adatok_norm, "ősz"))

# 4. TÉL
print(plot_szezon_osszes_allomas(napi_adatok_norm, "tél"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Készítünk olyan ábrát is, ahol az adott évszakra vonatkozóan az összes
## benchmark állomás tényleges napi adatát is kirajzoljuk:

plot_szezon_osszes_allomas_napi <- function(napi_adatok_norm, target_evszak) {
  
  plot_data <- napi_adatok_norm %>%
    filter(evszak == target_evszak) %>%
    mutate(
      plot_year = if_else(evszak == "tél" & honap == 12, 2023, 2024),
      plot_date = make_date(year = plot_year, month = honap, day = nap)
    )
  
  gg <- ggplot(plot_data, aes(x = plot_date, y = relativ_forg, color = nev, group = nev)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", size = 0.8) +
    geom_line(size = 0.8, alpha = 0.7) + 
    labs(
      title = paste0(target_evszak, ": napi lefolyás (tényleges adatok)"),
      subtitle = "Relatív forgalom (1.0 = saját szezonális átlag)",
      x = NULL,
      y = "Relatív forgalom (szorzószám)",
      color = "Állomás  "
    ) +
    scale_x_date(date_labels = "%b %d.", date_breaks = "2 weeks") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",   
      legend.direction = "horizontal", 
      legend.text = element_text(size = 9),
      legend.margin = margin(t = 5)
    ) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
  return(gg)
}

# ~~~~~~~~~~~~~~~~~~~~           Ábrák generálása         ~~~~~~~~~~~~~~~~~~~~ #

# 1. TAVASZ 
print(plot_szezon_osszes_allomas_napi(napi_adatok_norm, "tavasz"))

# 2. NYÁR
print(plot_szezon_osszes_allomas_napi(napi_adatok_norm, "nyár"))

# 3. ŐSZ
print(plot_szezon_osszes_allomas_napi(napi_adatok_norm, "ősz"))

# 4. TÉL
print(plot_szezon_osszes_allomas_napi(napi_adatok_norm, "tél"))




# ~~~~~~~~~~~~~~~~~~~~        EDDIG CSINÁLTAM MEG         ~~~~~~~~~~~~~~~~~~~~ #




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####                8. TÖBBI ÁLLOMÁS ELOSZLÁSÁNAK BECSLÉSE                   ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #





