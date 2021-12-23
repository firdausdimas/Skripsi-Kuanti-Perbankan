library(tidyverse)
library(epiDisplay)
library(psych)
library(car)
library(lessR)
library(readr)
Kuesioner_Muji <- read_csv("DATA SCIENCE APP/Project Dwnld/DATA ANALYST/R/Project/Oji/Kuesioner_Muji.csv", 
                           col_types = cols(Jenis_Kelamin = col_factor(levels = c()),
                                            Pekerjaan = col_factor(levels = c()),
                                            Usia = col_number(), Kualitas_Pelayanan_Setelah_Merger = col_number(), 
                                            Prinsip_Syariah_Sebelum_Merger = col_number(), 
                                            Produk_Setelah_merger = col_number(), 
                                            Kredibilitas_BSI = col_number(), 
                                            Kemudahan_layanan = col_number(), 
                                            Migrasi_bank = col_number(), Kemudahan_migrasi_rekening_bank = col_number(), 
                                            Pengguna_bank_BSI = col_number(), 
                                            Lokasi_BSI = col_number(), Keyakinan_nasabah_agar_dana_dikelolaBSI = col_number(), 
                                            Ketertarikan_nasabah_menabung = col_number(), 
                                            Ketertarikan_menabung_karena_Kemudahan_Akses = col_number(), 
                                            Ketertarikan_menabung_karena_kepuasan_pelayanan = col_number(), 
                                            Ketertarikan_karena_prinsipSyariah_dalam_praktik_transaksi = col_number(), 
                                            Ketertarikan_bertransaksi_Karena_kebutuhan_dan_tujuan = col_number(), 
                                            Ketertarikan_menggunakan_karena_pelayanan = col_number(), 
                                            Ketertarikan_karena_mudah_dalam_transaksi = col_number()))
View(Kuesioner_Muji)


str(Kuesioner_Muji)
head(Kuesioner_Muji)
Kuesioner <- Kuesioner_Muji

#-- deskripsi Data --#

summary(Kuesioner_Muji)
describe(Kuesioner_Muji)
describe.by(Kuesioner_Muji, group = Kuesioner_Muji$Jenis_Kelamin)

#-- grafik sementara --#

tab1(Kuesioner_Muji$Jenis_Kelamin, sort.group = "decreasing", cum.percent = T, 
     col = c("red", "pink"), main = "Data Berdasarkan Jenis Kelamin")

BarChart(Produk_Setelah_merger, data = Kuesioner_Muji, by= Jenis_Kelamin,
         main = "Data Berdasarkan Produk di BSI")

BarChart(Ketertarikan_karena_prinsipSyariah_dalam_praktik_transaksi, 
         data = Kuesioner_Muji, by= Jenis_Kelamin,
         main = "Data Berdasarkan Ketertarikan Nasabah Menabung Karena Prinsip 
         Syariah dalam Praktik Transaksi di BSI")
#-- Uji Persyaratan Analisis --#

## Uji Normalitas Data ##

length(Kuesioner_Muji$Jenis_Kelamin)
table(Kuesioner_Muji$Jenis_Kelamin)
Kuesioner_Muji$Jenis_Kelamin <- as.numeric(Kuesioner_Muji$Jenis_Kelamin)

ks.test(Kuesioner_Muji$Jenis_Kelamin, "pnorm", mean = mean(Kuesioner_Muji$Jenis_Kelamin),
        sd = sd(Kuesioner_Muji$Jenis_Kelamin))

shapiro.test(Kuesioner_Muji$Jenis_Kelamin)

## Linear Models ##

lm(formula = Jenis_Kelamin ~ Produk_Setelah_merger 
   + Ketertarikan_karena_prinsipSyariah_dalam_praktik_transaksi 
   , data = Kuesioner_Muji)

lm(formula = Usia ~ Produk_Setelah_merger 
   + Ketertarikan_karena_prinsipSyariah_dalam_praktik_transaksi 
   , data = Kuesioner_Muji)

## Uji Homogenitas Data ##

leveneTest(Kuesioner_Muji$Jenis_Kelamin ~ Kuesioner_Muji$Produk_Setelah_merger,
           data = Kuesioner_Muji)

leveneTest(Kuesioner_Muji$Jenis_Kelamin ~ 
             Kuesioner_Muji$Ketertarikan_karena_prinsipSyariah_dalam_praktik_transaksi,
           data = Kuesioner_Muji)

## Uji Hipotesis Data ##

#-- Uji T --#

t.test(Kuesioner_Muji$Produk_Setelah_merger, mu = 2)

t.test(Kuesioner_Muji$Produk_Setelah_merger ~ Kuesioner_Muji$Jenis_Kelamin)

t.test(Kuesioner_Muji$Ketertarikan_karena_prinsipSyariah_dalam_praktik_transaksi 
       ~ Kuesioner_Muji$Jenis_Kelamin)

t.test(Kuesioner_Muji$Produk_Setelah_merger,
         Kuesioner_Muji$Produk_Setelah_merger, paired = T)

#-- Uji Mann W --#

wilcox.test(Kuesioner_Muji$Produk_Setelah_merger ~ Kuesioner_Muji$Jenis_Kelamin)

#-- Uji Wilcoxon --#

wilcox.test(Kuesioner_Muji$Produk_Setelah_merger ~ 
              Kuesioner_Muji$Ketertarikan_karena_prinsipSyariah_dalam_praktik_transaksi
            , paired = T)
