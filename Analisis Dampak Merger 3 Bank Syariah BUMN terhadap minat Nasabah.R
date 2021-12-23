library(tidyverse)
library(epiDisplay)
library(psych)
library(car)
library(lessR)
library(readr)
Kuesioner_Mujiono <- read_csv("DATA SCIENCE APP/Project Dwnld/DATA ANALYST/R/Project/Oji/Kuesioner_Mujiono.csv", 
                              col_types = cols(Jenis_Kelamin = col_factor(levels = c()), 
                                               Usia = col_number(), Kualitas_Pelayanan_Setelah_Merger = col_factor(levels = c()), 
                                               Prinsip_Syariah_Sebelum_Merger = col_factor(levels = c()), 
                                               Produk_Setelah_merger = col_factor(levels = c()), 
                                               Kredibilitas_BSI = col_factor(levels = c()), 
                                               Kemudahan_layanan = col_factor(levels = c()), 
                                               Migrasi_bank = col_factor(levels = c()), 
                                               Kemudahan_migrasi_rekening_bank = col_factor(levels = c()), 
                                               Pengguna_bank_BSI = col_factor(levels = c()), 
                                               Lokasi_BSI = col_factor(levels = c()), 
                                               Keyakinan_nasabah_agar_dana_dikelolaBSI = col_factor(levels = c()), 
                                               Ketertarikan_nasabah_menabung = col_factor(levels = c()), 
                                               Ketertarikan_menabung_karena_Kemudahan_Akses = col_factor(levels = c()), 
                                               Ketertarikan_menabung_karena_kepuasan_pelayanan = col_factor(levels = c()), 
                                               Ketertarikan_karena_prinsipSyariah_dalam_praktik_transaksi = col_factor(levels = c()), 
                                               Ketertarikan_bertransaksi_Karena_kebutuhan_dan_tujuan = col_factor(levels = c()), 
                                               Ketertarikan_menggunakan_karena_pelayanan = col_factor(levels = c()), 
                                               Ketertarikan_karena_mudah_dalam_transaksi = col_factor(levels = c())))
View(Kuesioner_Mujiono)

DataQuest <- Kuesioner_Mujiono
str(DataQuest)
head(DataQuest)

#-- deskripsi Data --#
summary(DataQuest)
describe(DataQuest)
describe.by(DataQuest, group = DataQuest$Jenis_Kelamin)

#-- grafik sementara --#
tab1(DataQuest$Jenis_Kelamin, sort.group = "decreasing", cum.percent = T, 
     col = c("red", "pink"), main = "Data Berdasarkan Jenis Kelamin")

BarChart(Produk_Setelah_merger, data = DataQuest, by= Jenis_Kelamin,
         main = "Data Berdasarkan Produk di BSI")

BarChart(Ketertarikan_karena_prinsipSyariah_dalam_praktik_transaksi, 
         data = DataQuest, by= Jenis_Kelamin,
         main = "Data Berdasarkan Ketertarikan Nasabah Menabung Karena Prinsip 
         Syariah dalam Praktik Transaksi di BSI")

#-- Uji Persyaratan Analisis --#

## Uji Normalitas Data ##

length(DataQuest$Jenis_Kelamin)
table(DataQuest$Jenis_Kelamin)
DataQuest$Jenis_Kelamin <- as.numeric(DataQuest$Jenis_Kelamin)

ks.test(DataQuest$Jenis_Kelamin, "pnorm", mean = mean(DataQuest$Jenis_Kelamin),
        sd = sd(DataQuest$Jenis_Kelamin))

shapiro.test(DataQuest$Jenis_Kelamin)

## Uji Homogenitas Data ##

leveneTest(DataQuest$Jenis_Kelamin ~ DataQuest$Produk_Setelah_merger,
           data = DataQuest)

leveneTest(DataQuest$Jenis_Kelamin ~ 
              DataQuest$Ketertarikan_karena_prinsipSyariah_dalam_praktik_transaksi,
           data = DataQuest)

## Linear Models ##

lm(formula = Jenis_Kelamin ~ Produk_Setelah_merger 
   + Ketertarikan_karena_prinsipSyariah_dalam_praktik_transaksi 
   , data = DataQuest)

lm(formula = Usia ~ Produk_Setelah_merger 
   + Ketertarikan_karena_prinsipSyariah_dalam_praktik_transaksi 
   , data = DataQuest)

