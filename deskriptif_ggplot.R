## Deskriptif Statistik
library(tidyverse)
data_latihan <- read.csv("G:/Mandat/data_latihan.csv", sep=";")
mydata <- data_latihan %>% na.omit()
head(mydata)

### Deskriptif Statistik (mean, median, std.dev, variance,
### min, max, range, n)
mydata %>% group_by(jk) %>% summarise(
  n = n()
)
# Deskriptif Variabel Tinggi Badan
mydata %>% summarise(
  rata_rata = mean(tinggi),
  median    = median(tinggi),
  std.dev   = sd(tinggi),
  minimum   = min(tinggi),
  maksimum  = max(tinggi), 
  jarak     = range(tinggi),
  n         = n()
)
# Deskriptif Variabel Berat Badan
mydata %>% summarise(
  rata_rata = mean(berat),
  median    = median(berat),
  std.dev   = sd(berat),
  minimum   = min(berat),
  maksimum  = max(berat), 
  jarak     = range(berat),
  n         = n()
)

### Deskriptif Statistik berdasarkan kelompok
mydata %>% group_by(jk) %>% summarise(
  rata_rata = mean(tinggi),
  median    = median(tinggi),
  std.dev   = sd(tinggi),
  minimum   = min(tinggi),
  maksimum  = max(tinggi), 
  jarak     = max(tinggi) - min(tinggi),
  n         = n()
)

## Grafik
library(ggplot2)
### 1. data
### 2. mapping
### 3. grafik
### 4. judul, subjudul, dll.
### 5. tema


### Diagram Batang
ggplot(data = mydata,
       mapping = aes(x = jk, fill = jk)) + geom_bar() + 
  labs(title = "Jumlah Laki-laki dan Perempuan",
       subtitle = "Latihan Membuat Grafik dengan R") +
  xlab("Jenis Kelamin") + 
  ylab("Jumlah Responden") +
  theme_classic() + 
  theme(legend.position = "none")


### Diagram Garis
data_penjualan = data.frame(bulan = c(1:6),
                            jumlah_penjualan = c(12, 26, 17, 50, 70, 126))

ggplot(data = data_penjualan,
       mapping = aes(x = bulan,
                     y = jumlah_penjualan)) +
  geom_line(size = 1.3)


### Diagram Pencar
mydata = mydata %>% filter(berat < 1000)

ggplot(data = mydata,
       mapping = aes(x = tinggi, y = berat, color = jk)) +
  geom_point(size = 1.8) + theme_classic()


### Histogram
ggplot(mydata, aes(x = berat)) + 
  geom_histogram(binwidth = 20)


### Boxplot
ggplot(mydata, aes(x = jk, y = berat, color = jk)) + 
  geom_boxplot(size = 1) + 
  theme(legend.position = "none")


library(ggpubr)
ggboxplot(mydata, x="jk", y="berat")
