library(tidyverse)
library(stringr)
library(writexl)
#touch data data
names(Magang)
count(Magang, Kelamin)
  JK <- table(Magang$Kelamin)

Magang <- rename(magang, 
       Name = "Nama Lengkap",
       Kelamin = "Jenis Kelamin",
       Kuliah = "Asal Perguruan Tinggi",
       Provinsi = "Asal Provinsi",
       Departemen_choice = "Departemen apa yang kamu inginkan?",
       Alasan_D = "Apa alasan kamu menginginkan departemen tersebut",
       Skilled_at = "Skill apa yang kamu miliki saat ini?",
       Learning_RN  = "Skill apa yang sedang kamu pelajari saat ini?",
       Bersedia = "apakah kamu bersedia untuk melakuan Internship Program di Ousean Group selama 3 bulan?")

nrow(Magang)

length(unique(Magang$Kelamin))
length(unique(Magang$Provinsi))
length(unique(Magang$Kuliah))
length(unique(Magang$Departemen_choice))

table(Magang$Provinsi)
sort(table(Magang$Kuliah))

#code lebih lanjut
jawa <- Magang %>%
  count(Kuliah) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

Provinsi <- Magang %>%
  count(Provinsi) %>%
  arrange(Provinsi)
  
  
  
DP <- Magang %>%
  count(Departemen_choice, Kelamin)

# disini letak ggplot grafis untuk setiap variable yang ada

ggplot(Magang, aes(x = Kelamin, fill = Kelamin)) +
  geom_bar()

ggplot(jawa, aes(x = reorder(Kuliah, n), y = n, fill = Kuliah)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.5)

ggplot(Provinsi, aes(x = reorder(Provinsi, n), y = n, fill = Provinsi)) +
  geom_col() +
  coord_flip() +
  guides(fill = "none")

ggplot(DP, aes(x = Departemen_choice, y = n, fill = Kelamin)) +
  geom_col() +
  coord_flip()

ggplot(Skill_Total %>% slice_max(n, n = 10), aes(x = reorder(Skilled_at, n), y = n, fill = Skilled_at)) +
  geom_col(show.legend = FALSE) +
  coord_flip()


#Rasio lebih lanjut

DP2 <- data.frame(Magang) %>%
  count(Departemen_choice, Kelamin) %>%
  tidyr::pivot_wider(names_from = Kelamin, values_from = n)

Distribusi2 <- data.frame(Magang) %>%
  tidyr::separate_rows(Skilled_at) %>%
  count(Provinsi, Skilled_at, sort = TRUE) %>%
  mutate(Provinsi = stringr::str_to_title(Provinsi)) %>%
  arrange(Provinsi, desc(n))%>%
  tidyr::pivot_wider(names_from = Provinsi, values_from = n)

Skill_Total <- Magang %>%
  tidyr::separate_rows(Skilled_at, sep = ",") %>%   # pisahkan skill
  mutate(Skilled_at = str_trim(Skilled_at)) %>%     # buang spasi
  count(Skilled_at, sort = TRUE)                    # hitung total tiap skill

head(Skill_Total, 10)   # lihat 10 skill teratas


write_xlsx(Magang, "Data_Magang.xlsx")

arrange(Distribusi2)
