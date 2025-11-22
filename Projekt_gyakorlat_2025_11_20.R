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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####                       2. ADATOK IMPORTÁLÁSA                             ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

rm(list = ls())

getwd()

# A zárójelen belül írd át a saját, adatokat tartalmazó mappádra az elérési utat
setwd("C:/Users/Asus/Documents/ELTE-TÁTK/3. félév (2025-26-1)/Projekt gyakorlat")

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####                   3. ADATTISZTÍTÁS ÉS -FELDOLGOZÁS                      ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####                          4. LEÍRÓ STATISZTIKÁK                          ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Óránkénti frekvencia ábrája
df %>%
  ggplot(aes(x = kerekp_forg)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7) +
  labs(title = "Óránkénti kerékpáros forgalom eloszlása",
       x = "Óránkénti kerékpárosok száma", y = "Megfigyelések száma") +
  coord_cartesian(xlim = c(NA, 250), ylim = c(NA, 500000))

## Napi frekvencia ábrája
df_napi <- df %>%
  group_by(azonosito, irany, ev, honap, nap) %>%
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
####                 6. BENCHMARK-ÁLLOMÁSOK MEGHATÁROZÁSA                    ####
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

# A szűrést minden esetben csak 2022, 2023 és a 2024 évszakaira alapoztuk. 
# Ezen három év esetében minden évszakban történt adatfelvétel, illetve 
# 2022-től azon feltételezéssel éltünk, hogy a COVID hatása is már 
# marginálisnak tekinthető. 

evek <- c(2022, 2023, 2024)

# Szűrjünk le ezekre az évekre, és adjuk hozzá az állomásnevet oszlopként:
df_benchmark <- df %>%
  left_join(metadata %>% select(azonosito, nev), by = "azonosito") %>%
  filter(ev %in% evek) # Csak a vizsgált 3 év

# Csak a listában szereplő állomásokat tartjuk meg és csak a hozzájuk rendelt 
# évszakban!
df_benchmark <- df_benchmark %>%
  inner_join(benchmark_allomasok, by = c("evszak", "nev"))

## Benchmark állomások átnevezése rövidebb nevekre:
df_benchmark <- df_benchmark %>%
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
####           7. BENCHMARK-ÁLLOMÁSOK ELOSZLÁSÁNAK VIZSGÁLATA                ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Napi szummák kiszámítása a konkrét dátumokra
napi_osszesito <- df_benchmark %>%
  group_by(azonosito, nev, evszak, ev, honap, nap) %>%
  summarise(napi_forgalom = sum(kerekp_forg, na.rm = TRUE), .groups = "drop")

## Évek "összemosása"
# Csoportosítunk hónap-nap szerint, és vesszük a 3 év átlagát
napi_adatok <- napi_osszesito %>%
  group_by(azonosito, nev, evszak, honap, nap) %>%
  summarise(
    atlagos_napi_forgalom = mean(napi_forgalom, na.rm = TRUE),
    megfigyelesek_szama = n(), # Hány évből jött össze (max 3)
    .groups = "drop"
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Függvény, amely egy adott állomásra ábrázolja a 3 év átlagos napi forgalmát
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
  gg <- ggplot(plot_data, aes(x = plot_date, y = atlagos_napi_forgalom)) +
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

# ~~~~~~~~~~~~~~~~~~~~                Próba               ~~~~~~~~~~~~~~~~~~~~ #

# Példa 1a: Tiszafüred, nyár
print(plot_szintetikus_szezon(
  napi_adatok = napi_adatok,
  allomas_id = "116004",
  target_evszak = "nyár"
))

# Példa 1b: Balatonederics, nyár
print(plot_szintetikus_szezon(
  napi_adatok = napi_adatok,
  allomas_id = "119003",
  target_evszak = "nyár"
))

# Példa 2a: Baja, tavasz
print(plot_szintetikus_szezon(
  napi_adatok = napi_adatok,
  allomas_id = "103009",
  target_evszak = "tavasz"
))

# Példa 2b: Dunakeszi, tavasz
print(plot_szintetikus_szezon(
  napi_adatok = napi_adatok,
  allomas_id = "113006",
  target_evszak = "tavasz"
))

# Példa 3a: Balatonederics, tél
print(plot_szintetikus_szezon(
  napi_adatok = napi_adatok,
  allomas_id = "119003",
  target_evszak = "tél"
))

# Példa 3b: Füle, tél
print(plot_szintetikus_szezon(
  napi_adatok = napi_adatok,
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





