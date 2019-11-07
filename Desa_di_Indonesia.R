### Web Scraping and Making Data Visualization###
### Ide dari script ini adalah memanen data dari web yakni data wikipedia 
### Data tentang jumlah desa dan kelurahan di Indonesia dari tahun ke tahun
### Kemudian membuat visualisasi datanya 
### Visuali data terinspirasi oleh github JuanmaMN 
### link : https://github.com/JuanmaMN/TidyTuesday/blob/master/TidyTuesday%2017-9-2019.R#L37

#Required Packages

library(rvest)
library(tidyverse)
library(showtext)
library(extrafont)
library(ggthemes)
library(awtools)

#Read the html from the URL, specify which areas to read, and read in as a table
### menentukan alamat url yang akan di scrape
url1 <- 'https://id.wikipedia.org/wiki/Daftar_kecamatan_dan_kelurahan_di_Indonesia'

### baca html
url2 <- read_html(url1)

## melihat struktur html
str(url2)  

### membaca tabel
table1 <- url1 %>% 
  read_html(url1) %>% 
  html_nodes('div table') %>%
  html_table(fill = TRUE) ## diisi fill = TRUE  karena jumlah kolom dari masing2 tabel tdk sama

### table1 adalah list dengan jumlah total tabel sebanyak 37
### memilih tabel pertama
data_desa <- table1[[1]]

##Meremove nama kolom, karena nama kolom tidak valid
colnames(data_desa) <- as.character(unlist(data_desa[1,]))

## Menghilangkan beberapa baris yang tidak relevan ##
## remove row 1 dan 2
data_desa <- data_desa[-c(1:2),]
## remove column 17 dan 18
data_desa <- data_desa[,-c(17:18)]
## remove comlumn 18
data_desa <- data_desa[,-c(18)]

### masih terdapat dot pada data ###
## menghilangkan dot yang ada di data_desa untuk menghindarkan salah penghitungan ###
### mengekspor ke format csv
write.csv(data_desa, file = "data_desa.csv")

### lalu mengimpornya lagi ke RStudio

### Loading packade untuk data visualization ###
library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(scales)
library(tidyverse)
library(streamgraph)
library(viridis)
library(plotly)

## memilih data desa di tahun 2004, 2013, dan 2019, menamakannya dengan desa
desa <- data_desa[,c(1,2,11,17)]
glimpse(desa)

### wrangling###
## merename variabel, dan mengatur data berdasarkan jumlah desa yang paling banyak pada 2019
desa1 <- desa %>%
  rename( T2004 = X2004, T2013 = X2013, T2019 = X2019) %>%
  arrange(desc(T2019))
  
### Mengurukan variabel provinsi dengan cara menjadikannya faktor terlebih dahulu
desa1$Provinsi <- factor(desa1$Provinsi, levels = c("Jawa Tengah",
                                                    "Jawa Timur", "Aceh", "Sumatra Utara",
                                                    "Jawa Barat", "Papua", "Nusa Tenggara Timur",
                                                    "Sumatra Selatan", "Sulawesi Selatan", "Lampung",
                                                    "Sulawesi Tenggara", "Kalimantan Barat", "Sulawesi Tengah",
                                                    "Kalimantan Selatan", "Riau", "Papua Barat", "Sulawesi Utara",
                                                    "Kalimantan Tengah", "Jambi", "Banten", "Bengkulu",
                                                    "Maluku", "Maluku Utara", "Sumatra Barat",
                                                    "Nusa Tenggara Barat", "Kalimantan Timur",
                                                    "Gorontalo", "Bali", "Sulawesi Barat",
                                                    "Kalimantan Utara", "DI Yogyakarta", "KepRiau",
                                                    "KepBangka Belitung", "DKI Jakarta"))
### merename variabel tahun
names(desa1)[2]<-"2004"
names(desa1)[3]<-"2013"
names(desa1)[4]<-"2019"

#### Plotting ###

ggplot() +
  geom_segment(
    data = gather(desa1, measure, val, -Provinsi) %>% 
      group_by(Provinsi) %>% 
      top_n(-1) %>% 
      slice(1) %>%
      ungroup(),
    aes(x = 200, xend = 9000, y = Provinsi, yend = Provinsi),
    linetype = "blank", size = 0.3, color = "gray80"
  ) +
  geom_segment(
    data = gather(desa1, measure, val, -Provinsi) %>% 
      group_by(Provinsi) %>% 
      summarise(start = range(val)[1], end = range(val)[2]) %>% 
      ungroup(),
    aes(x = start, xend = end, y = Provinsi, yend = Provinsi),
    color = "gray80", size = 2
  ) +
  # reshape the data frame & plot the points
  geom_point(
    data = gather(desa1, measure, value, -Provinsi),
    aes(value, Provinsi, color = measure), 
    size = 2
  ) + 
  geom_text(data = filter(desa1, Provinsi == "Jawa Barat"),
            aes(x = 1250, y = Provinsi),
            label = "2004", fontface = "bold",
            color = "#33FF33") +
  geom_text(data = filter(desa1, Provinsi == "Jawa Barat"),
            aes(x = 4003, y = Provinsi),
            label = "2013", fontface = "bold",
            color = "#33FF33")  +
  geom_text(data = filter(desa1, Provinsi == "Nusa Tenggara Timur"),
            aes(x = 5525, y = Provinsi),
            label = "2019", fontface = "bold",
            color = "#33FF33") + 
  theme_dark() +
    labs(
      title = "Desa & Kelurahan di Indonesia",
      subtitle = "Perkembangan jumlah desa dan kelurahan di Indonesia dalam tiga periode waktu",
      caption = "Graphic by Retno Novvitasari (Twitter: @retnovvita)",
      x = "Jumlah Desa & Kelurahan",
      y = "Provinsi") + theme(legend.position="none") 


### Twitter @retnovvita
