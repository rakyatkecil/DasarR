library(dplyr)

mydata <- read.csv("G:/Mandat/data_latihan.csv", sep=";")

## Menghilangkan Missing Values
mydata2 <- mydata %>% na.omit()

## Membuat Variabel Baru
mydata2 <- mydata2 %>% 
  mutate(tinggiM = tinggi / 100)
View(mydata2)

## Transform - compute
mydata2 <- mydata2 %>% 
  mutate(IMT = berat / (tinggiM^2))


## Recode
library(tidyverse)
mydata2 <- mydata2 %>% 
  mutate(IMTkat = case_when(
    IMT < 18 ~ "Kurus",
    IMT > 25 ~ "Gemuk",
    IMT >= 18 & IMT <= 25 ~ "Normal"
  )
  )


## Filter 
mydata3 <- mydata2 %>% filter(IMTkat == "Gemuk")
View(mydata3)


## Latihan
mydata4 <- mydata2 %>% filter(jk == "male" | jk == "female")
View(mydata4)

mydata4 <- mydata4 %>% mutate(
  JK = case_when(
    jk == "male" ~ "M",
    jk == "female" ~ "F"
  )
)

## Merge


## Frequency, tabulasi
mydata2 %>% table()
