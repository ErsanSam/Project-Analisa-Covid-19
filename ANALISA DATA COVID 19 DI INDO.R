#Mengakses API covid.go.id
library(httr)
resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
#Status Code
#Ada beberapa jenis status code yang umumnya dijumpai, antara lain:

# 200, Artinya permintaan sukses dipenuhi.
# 404, Artinya berkas yang diminta tidak dapat ditemukan.
#403, Artinya akses permintaan ditolak.
#500, Artinya terjadi kesalahan pada server.
library(httr)
resp <- GET("https://data.covid19.go.id/public/api/update.json")
status_code(resp)
#STATUS CODE 2
resp$status_code
identical(resp$status_code, status_code(resp))
#HEADERS 
#Selamat status permintaan Anda melalui API sukses dipenuhi! Sekarang cobalah Anda jalankan fungsi headers() pada resp untuk mengetahui metadata apa saja yang tersimpan. Apakah isi dari elemen content-type? Kapan terakhir kali informasi diperbaharui?
headers(resp)
#MENGEKSTRAK ISI RESPOWN
cov_id_raw <- content(resp, as = "parsed", simplifyVector = TRUE)
#MENGEKSTRAK ISI RESPOWN PART 2
length(cov_id_raw)
names(cov_id_raw)
cov_id_update <- cov_id_raw$update
#ANALISA DATA
  #Kapan tanggal pembaharuan data penambahan kasus?
  #Berapa jumlah penambahan kasus sembuh?
  #Berapa jumlah penambahan kasus meninggal?
  #Berapa jumlah total kasus positif hingga saat ini?
  #Berapa jumlah total kasus meninggal hingga saat ini?
lapply(cov_id_update,names)
cov_id_update$penambahan$tanggal
cov_id_update$penambahan$jumlah_sembuh
cov_id_update$penambahan$jumlah_meninggal
cov_id_update$total$jumlah_positif
cov_id_update$total$jumlah_meninggal
#_____________________________________________________________________
#APA KABAR JAWA BARAT?
  #Berapa jumlah total kasus COVID-19 di Jawa Barat?
  #Berapa persentase kematian akibat COVID-19 di Jawa Barat?
  #Berapa persentase tingkat kesembuhan dari COVID-19 di Jawa Barat?
library(httr)
resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)


names(cov_jabar_raw)
cov_jabar_raw$last_date
cov_jabar_raw$kasus_total
cov_jabar_raw$meninggal_persen
cov_jabar_raw$sembuh_persen
#Memperoleh Informasi yang Lebih Lengkap
cov_jabar <- cov_jabar_raw$list_perkembangan
str(cov_jabar)
head(cov_jabar)
#Menjinakkan Data
library(dplyr)
new_cov_jabar <-
  cov_jabar %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
str(new_cov_jabar)  
#Menunjukkan Melalui Gambar
library(ggplot2)
library(hrbrthemes)
ggplot(new_cov_jabar, aes(x = tanggal, y = kasus_baru)) +
  geom_col()
#Menunjukkan Melalui Gambar - Pt 2
library(ggplot2)
library(hrbrthemes)
ggplot(new_cov_jabar, aes(tanggal, kasus_baru)) +
  geom_col(fill = "salmon")+
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Positif COVID-19 di Jawa Barat",
    subtitle = "Terjadi pelonjakan kasus akibat pergerakan masyarakat yang terus meningkat sejak awal Ramadhan hingga puncaknya pada tanggal 17 Juli 2021.",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  )+
  theme(plot.title.position = "plot")
#Grafik untuk Kasus Sembuh
library(ggplot2)
library(hrbrthemes)
ggplot(new_cov_jabar, aes(tanggal, sembuh)) +
  geom_col(fill = "olivedrab2") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di Jawa Barat",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
#Grafik untuk Kasus Meninggal
library(ggplot2)
library(hrbrthemes)
ggplot(new_cov_jabar, aes(tanggal, meninggal)) +
  geom_col(fill = "darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di Jawa Barat",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
#Apakah Pekan ini Lebih Baik?
library(dplyr)
library(lubridate)

cov_jabar_pekanan <- new_cov_jabar %>% 
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )

glimpse(cov_jabar_pekanan)
#Menjawab Pertanyaan
  #Membuat kolom baru yang berisi jumlah kasus baru dalam satu pekan sebelumnya. Kolom ini diberi nama “jumlah_pekanlalu”.
  #Mengganti nilai NA pada kolom “jumlah_pekanlalu” dengan nilai 0
  #Melakukan komparasi antara kolom “jumlah” dengan kolom “jumlah_pekanlalu”. Hasil komparasi ini disimpan dalam kolom baru dengan nama “lebih_baik”, isinya adalah TRUE apabila jumlah kasus baru pekan ini lebih rendah dibandingkan jumlah kasus pekan lalu
library(dplyr)
cov_jabar_pekanan <-
  cov_jabar_pekanan %>% 
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah, 1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )
glimpse(cov_jabar_pekanan)
#MEMBUAT BAR CHART
library(ggplot2)
library(hrbrthemes)
ggplot(cov_jabar_pekanan[cov_jabar_pekanan$tahun==2021,], aes(pekan_ke, jumlah, fill = lebih_baik)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = 1:29, expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di Jawa Barat Tahun 2021",
    subtitle = "Kolom hijau menunjukan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
    caption = "Sumber data: covid.19.go.id\n per tanggal 16 Juli 2021"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
#Pola dan Dinamika
library(dplyr)
cov_jabar_akumulasi <- 
  new_cov_jabar %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )

tail(cov_jabar_akumulasi)
#MEMBUAT LINE CHART
library(ggplot2)
ggplot(data = cov_jabar_akumulasi, aes(x = tanggal, y = akumulasi_aktif)) +
  geom_line()
#Transformasi DATA
#dirubah dari yang semula berformat wide menjadi format long.
library(dplyr)
library(tidyr)

dim(cov_jabar_akumulasi)

cov_jabar_akumulasi_pivot <- 
  cov_jabar_akumulasi %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )

dim(cov_jabar_akumulasi_pivot)

glimpse(cov_jabar_akumulasi_pivot)
#pivot longer
#Semenjak tidyr versi 1.0.0, Anda disarankan untuk menggunakan fungsi pivot_longer() sebagai pengganti gather() dan pivot_wider() sebagai pengganti spread(). pivot_longer() dan pivot_wider() memiliki fitur yang lebih lengkap dibandingkan gather() dan spread(). Proses transformasi cov_jabar_akumulasi menjadi cov_jabar_akumulasi_pivot dapat dikerjakan dengan menggunakan pivot_longer() sebagai berikut:
cov_jabar_akumulasi_pivot <-
  cov_jabar_akumulasi %>%
  pivot_longer(
    cols=-tanggal,
    names_to = "kategori",
    names_prefix = "akumulasi_",
    values_to = "jumlah"
  )
cov_jabar_akumulasi_pivot
#Tahap Terakhir
#Anda hampir selesai! Pada tahap terakhir ini Anda cukup salin dan jalankan baris kode berikut di konsol untuk membuat grafik komparasi antara akumulasi kasus aktif, kasus sembuh dan kasus meninggal:
library(ggplot2)
library(hrbrthemes)
ggplot(cov_jabar_akumulasi_pivot, aes(tanggal, jumlah/2e+3, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah Kasus Akumulasi (ribuan)",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Jawa Barat",
    caption = "Sumber data: covid.19.go.id\n per tanggal 16 Juli 2021"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    plot.xy=element_text(vjust = 0.5)
  )