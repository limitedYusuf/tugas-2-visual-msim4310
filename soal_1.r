# Yusuf

# Import library dplyr
library(dplyr)
library(kableExtra)

# Simpan data kunjungan dari tabel soal dalam bentuk frame data
data_kunjungan <- data.frame(
  Bulan = c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus", "September", "Oktober", "November", "Desember"), # nolint: line_length_linter.
  Jumlah_Pengunjung = c(90, 86, 97, 83, 85, 85, 65, 58, 60, 93, 94, 54)
)

# Proses Transformasi Akar
data_kunjungan <- data_kunjungan %>%
  mutate(Akar_Jumlah_Pengunjung = sqrt(Jumlah_Pengunjung))

# Simpan hasil transformasi akar dalam bentuk tabel
data_kunjungan_akar <- data_kunjungan[, c("Bulan", "Akar_Jumlah_Pengunjung")]
kable(data_kunjungan_akar, format = "html", caption = "Data Transformasi Akar") %>% # nolint
  kable_styling(full_width = FALSE) %>%
  as.character() %>%
  writeLines("data_transformasi_akar.html")

# Proses Transformasi Logaritma
data_kunjungan <- data_kunjungan %>%
  mutate(Log_Jumlah_Pengunjung = log(Jumlah_Pengunjung))

# Simpan hasil transformasi logaritma dalam bentuk tabel
data_kunjungan_log <- data_kunjungan[, c("Bulan", "Log_Jumlah_Pengunjung")]
kable(data_kunjungan_log, format = "html", caption = "Data Transformasi Logaritma") %>% # nolint
  kable_styling(full_width = FALSE) %>%
  as.character() %>%
  writeLines("data_transformasi_logaritma.html")

# Proses Transformasi Tukey
data_kunjungan <- data_kunjungan %>%
  mutate(Tukey_Transform = sqrt(Jumlah_Pengunjung + 3/8)) # nolint: infix_spaces_linter, line_length_linter.
# Kenapa 3/8?
# Karena tukey pada dasarnya adalah transformasi akar kuadrat dari nilai yang ditambahkan dengan suatu konstanta. Pada umum nya yang digunakan adalah 3/8 atau 0.375 # nolint: line_length_linter.

# Simpan hasil transformasi tukey dalam bentuk tabel
data_kunjungan_tukey <- data_kunjungan[, c("Bulan", "Tukey_Transform")]
kable(data_kunjungan_tukey, format = "html", caption = "Data Transformasi Tukey") %>% # nolint
  kable_styling(full_width = FALSE) %>%
  as.character() %>%
  writeLines("data_transformasi_tukey.html")

# Tampilkan 3 plot dalam 1 frame
par(mfrow = c(1, 3))

# Boxplot transformasi akar
boxplot(data_kunjungan$Akar_Jumlah_Pengunjung,
        main = "Boxplot Transformasi Akar",
        ylab = "Transformasi Akar",
        col = "lightblue")

# Boxplot transformasi logaritma
boxplot(data_kunjungan$Log_Jumlah_Pengunjung,
        main = "Boxplot Transformasi Logaritma",
        ylab = "Transformasi Logaritma",
        col = "lightgreen")

# Boxplot transformasi Tukey
boxplot(data_kunjungan$Tukey_Transform,
        main = "Boxplot Transformasi Tukey",
        ylab = "Transformasi Tukey",
        col = "lightcoral")