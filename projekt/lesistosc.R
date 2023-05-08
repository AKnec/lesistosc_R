library(terra)
library(sf)
library(raster)
library(tmap)
library(units)
library(dplyr)

# Wczytanie warstwy z gminami w Polsce i wyodrębnienie granic gminy Wojciechów
gminy = read_sf("gminy/gminy.shp")
borders = filter(gminy, gminy$JPT_NAZWA_ == "Wojciechów")

# Wyświetlenie granic gminy
tm_shape(borders) +
  tm_graticules(lwd = 0.3, col = "gray50") +
  tm_borders(lwd = 4, col = "darkgreen") +
  tm_layout(main.title = "Granice gminy Wojciechów",
            inner.margins = c(0.15, 0.15, 0.15, 0.15),
            main.title.position = "center") +
  tm_scale_bar(breaks = c(0, 2, 4, 6),
               text.size = 0.5) +
  tm_credits("Agata Kneć, Karol Trojanowski, 2023",
             position = c("left", "bottom")) +
  tm_compass(position = c("right", "top"), 
             type = "rose", 
             size = 1.5)

# Ścieżka do folderu ze zdjęciami satelitarnymi
folder_path15 = "lato_15"
folder_path22 = "lato_22"

# Wczytanie wszystkich plików .TIF z folderu do listy
raster_list15 = list.files(path = folder_path15, pattern = "\\.TIF$", full.names = TRUE)
raster_list22 = list.files(path = folder_path22, pattern = "\\.TIF$", full.names = TRUE)

# Wczytanie plików jako obiekt rastrowy
rasters15 = rast(raster_list15)
rasters22 = rast(raster_list22)

# Połączenie warstw rastrowych w stos
rasters_stack15 = rast(stack(rasters15))
rasters_stack22 = rast(stack(rasters22))

# Wyświetlenie stosu
plot(rasters_stack15)
plot(rasters_stack22)

# Sprowadzenie do wspólnego układu współrzędnych granic gminy

# Nazwa układu w rastrach
crs(rasters15, describe = TRUE)
crs(rasters22, describe = TRUE)

# Nazwa układu w wektorze
st_crs(borders)[1]

# Ustawienie układu współrzędnych dla warstwy wektorowej na taki jaki mają rastry
borders = st_transform(borders, crs = crs(rasters15))

# Zmieniony układ współrzędnych wektora 
st_crs(borders)[1]

# Przycięcie rastrów do obszaru zainteresowania
rasters_crop15 = crop(rasters_stack15, borders, mask = TRUE)
rasters_crop22 = crop(rasters_stack22, borders, mask = TRUE)

plot(rasters_crop15)
plot(rasters_crop22)

#Analiza dla roku 2015

# Wyświetlenie wartości rastrów
rasters_crop_mat15 = values(rasters_crop15)

# Lokalizacja danych NA w rastrach
i15 = noNA(rasters_crop15, FNA = TRUE)
i15 = values(i15)

# Pozbycie się wartości NA w rastrach
rasters_crop_mat15 = rasters_crop_mat15[!is.na(i15), ]

# Grupowanie za pomocą metody kmeans
set.seed(1)
km15 = kmeans(rasters_crop_mat15, centers = 15)

i15[!is.na(i15)] = km15$cluster
rasters_crop_km15 = rasters_crop15
rasters_crop_km15[["kmeans"]] = i15
rasters_crop_km15

plotRGB(rasters_crop_km15, r = 3, g = 2, b = 1, 
        stretch = TRUE)

plot(rasters_crop_km15[["kmeans"]], type = "classes", main = "Wizualizacja zgrupowania obszarów za pomocą metody kmeans")

# Klasyfikacja obszarów na pozostałe (1) i lasy (2)
rcl15 = matrix(c(1, 9, 1, 9, 10, 2, 10, 15, 1),
             ncol = 3, byrow = TRUE)

rasters_recl15 = classify(rasters_crop_km15[["kmeans"]], rcl = rcl15)
plot(rasters_recl15, main = "Wizualizacja reklasyfikacji na lasy i obiekty pozostałe")

# Zamiana rastra na wektor
rasters_r_poly15 = as.polygons(rasters_recl15)
rasters_r_poly_sf15 = st_as_sf(rasters_r_poly15)

# Rozbicie na pojedyncze poligony 
polygon15 = st_cast(rasters_r_poly_sf15, "POLYGON")

#Selekcja lasów
lasy15 = subset(polygon15, polygon15$kmeans == 2)

# Obliczenie powierzchni lasów i dodanie jej do nowej kolumny area
area = st_area(lasy15)
lasy15$area = area

# Wybór lasów o powierzchni większej niż 0.1ha
min_area = 1000 
min_area = set_units(min_area, m^2)
lasy_wieksze15 = subset(lasy15, (lasy15$area > min_area))

# Połączenie poligonów lasów w jeden multipoligon
lasy_wieksze_union15 = st_union(lasy_wieksze15)

# Obliczenie pola powierzchni lasów i dodanie wartości do nowej kolumny
area_lasy15 = st_area(lasy_wieksze_union15)
lasy_wieksze_union15$calkowita = area_lasy15

# Obliczenie lesistości i dodanie wartości do nowej kolumny
pow_gminy = st_area(borders)
borders$powierzchnia = pow_gminy

lesistosc15 = (lasy_wieksze_union15$calkowita / borders$powierzchnia) * 100
lesistosc15

#-------------------------------------------------------------------------------

#Analiza dla roku 2022

# Wyświetlenie wartości rastrów
rasters_crop_mat22 = values(rasters_crop22)

# Lokalizacja danych NA w rastrach
i22 = noNA(rasters_crop22, FNA = TRUE)
i22 = values(i22)

# Pozbycie się wartości NA w rastrach
rasters_crop_mat22 = rasters_crop_mat22[!is.na(i22), ]

# Grupowanie za pomocą metody kmeans
set.seed(1)
km22 = kmeans(rasters_crop_mat22, centers = 15)

i22[!is.na(i22)] = km22$cluster
rasters_crop_km22 = rasters_crop22
rasters_crop_km22[["kmeans"]] = i22
rasters_crop_km22

plotRGB(rasters_crop_km22, r = 3, g = 2, b = 1, 
        stretch = TRUE)

plot(rasters_crop_km22[["kmeans"]], type = "classes", main = "Wizualizacja zgrupowania obszarów za pomocą metody kmeans")

# Klasyfikacja obszarów na pozostałe (1) i lasy (2)
rcl22 = matrix(c(1, 11, 1, 11, 13, 2, 13, 15, 1),
               ncol = 3, byrow = TRUE)

rasters_recl22 = classify(rasters_crop_km22[["kmeans"]], rcl = rcl22)
plot(rasters_recl22, main = "Wizualizacja reklasyfikacji na lasy i obiekty pozostałe")

# Zamiana rastra na wektor
rasters_r_poly22 = as.polygons(rasters_recl22)
rasters_r_poly_sf22 = st_as_sf(rasters_r_poly22)

# Rozbicie na pojedyncze poligony 
polygon22 = st_cast(rasters_r_poly_sf22, "POLYGON")

#Selekcja lasów
lasy22 = subset(polygon22, polygon22$kmeans == 2)

# Obliczenie powierzchni lasów i dodanie jej do nowej kolumny area
area = st_area(lasy22)
lasy22$area = area

# Wybór lasów o powierzchni większej niż 0.1ha
min_area = 1000 
min_area = set_units(min_area, m^2)
lasy_wieksze22 = subset(lasy22, (lasy22$area > min_area))

# Połączenie poligonów lasów w jeden multipoligon
lasy_wieksze_union22 = st_union(lasy_wieksze22)

# Obliczenie pola powierzchni lasów i dodanie wartości do nowej kolumny
area_lasy22 = st_area(lasy_wieksze_union22)
lasy_wieksze_union22$calkowita = area_lasy22

# Obliczenie lesistości i dodanie wartości do nowej kolumny
pow_gminy = st_area(borders)
borders$powierzchnia = pow_gminy

lesistosc22 = (lasy_wieksze_union22$calkowita / borders$powierzchnia) * 100

# Wyniki analizy dla 2015 i 2022 roku

tm_shape(lasy_wieksze15["area"]) +
  tm_graticules(lwd = 0.3, col = "gray50") +
  tm_borders(lwd = 1.5, col = "darkgreen") +
  tm_layout(main.title = "Granice lasów 2015",
            inner.margins = c(0.15, 0.15, 0.15, 0.15),
            main.title.position = "center") +
  tm_scale_bar(breaks = c(0, 2, 4, 6),
               text.size = 0.5) +
  tm_credits("Agata Kneć, Karol Trojanowski, 2023",
             position = c("left", "bottom")) +
  tm_compass(position = c("right", "top"), 
             type = "rose", 
             size = 1.5)

tm_shape(lasy_wieksze22["area"]) +
  tm_graticules(lwd = 0.3, col = "gray50") +
  tm_borders(lwd = 1.5, col = "darkgreen") +
  tm_layout(main.title = "Granice lasów 2022",
            inner.margins = c(0.15, 0.15, 0.15, 0.15),
            main.title.position = "center") +
  tm_scale_bar(breaks = c(0, 2, 4, 6),
               text.size = 0.5) +
  tm_credits("Agata Kneć, Karol Trojanowski, 2023",
             position = c("left", "bottom")) +
  tm_compass(position = c("right", "top"), 
             type = "rose", 
             size = 1.5)

lesistosc15
lesistosc22

