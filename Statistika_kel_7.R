data_nilai <- read.csv(file = 'Data Tinggi Badan Mahasiswa.csv')
print(data_nilai)


#Menghitung banyak data
banyak_data <- nrow(data_nilai)
print(banyak_data)

#Mencari nilai terkecil dan terbesar
min_data <- min(data_nilai)
max_data <- max(data_nilai)
print(min_data)
print(max_data)

#Mencari range
range <- max_data-min_data
print(range)

#Mencari banyak kelas
k <- 1 + (3.3 * log(range,  base = 10))
k <- round(k)
print(k)

#Mencari interval kelas
interval <- range/k
interval <- interval
interval <- round(interval)
print(interval)

#Membuat tabel
batas_bawah <- c(144, 152, 160, 168, 176, 184)
batas_atas <- c(151,159,167,175,183,191) 
frekuensi <- c(8, 50, 53, 54, 30, 5)
table <- data.frame(batas_bawah, batas_atas, frekuensi)
print(table)

# Mencari mean
fixi <- c(
  (batas_atas[1]+batas_bawah[1])/2* frekuensi[1],
  (batas_atas[2]+batas_bawah[2])/2* frekuensi[2],
  (batas_atas[3]+batas_bawah[3])/2* frekuensi[3],
  (batas_atas[4]+batas_bawah[4])/2* frekuensi[4],
  (batas_atas[5]+batas_bawah[5])/2* frekuensi[5],
  (batas_atas[6]+batas_bawah[6])/2* frekuensi[6]
)

table <- data.frame(batas_bawah, batas_atas, fixi, frekuensi)
print(table)
rata2 <- sum(fixi)/banyak_data
print(rata2)

#Mencari median
b <- 160 - 0.5
F <- 50
f <- 53
median <- b + interval * (0.5*banyak_data - F)/f
print(median)

#Mencari modus

b <- 168 -0.5
b1 = frekuensi[4]-frekuensi[3]
b2 = frekuensi[4]-frekuensi[5]

modus <- b + interval * b1/(b1+b2)
print(modus)

#Mencari range
#Cara 1 Menghitung nilai Range menggunakan nilai tengah kelas terakhir dan kelas pertama
range1 = (batas_atas[6]+batas_bawah[6])/2 - (batas_atas[1]+batas_bawah[1])/2
print(range1)

#Cara 2 Menghitung nilai Range menggunakan tepi atas kelas terakhir dan tepi bawah kelas pertama
range2 = (batas_atas[6] + 0.5 )- (batas_bawah[1] - 0.5)
print(range2)

#Simpangan rata-rata
xi <- c(
  (batas_atas[1]+batas_bawah[1])/2,
  (batas_atas[2]+batas_bawah[2])/2,
  (batas_atas[3]+batas_bawah[3])/2,
  (batas_atas[4]+batas_bawah[4])/2,
  (batas_atas[5]+batas_bawah[5])/2,
  (batas_atas[6]+batas_bawah[6])/2
  )

xi_min_mean <- c(
  abs(xi[1] - rata2),
  abs(xi[2] - rata2),
  abs(xi[3] - rata2),
  abs(xi[4] - rata2),
  abs(xi[5] - rata2),
  abs(xi[6] - rata2)
)

fi_xi_xbar <- c(
  frekuensi[1] * xi_min_mean[1], 
  frekuensi[2] * xi_min_mean[2], 
  frekuensi[3] * xi_min_mean[3], 
  frekuensi[4] * xi_min_mean[4], 
  frekuensi[5] * xi_min_mean[5], 
  frekuensi[6] * xi_min_mean[6] 
)

table <- data.frame(batas_bawah, batas_atas, xi, fixi, xi_min_mean, fi_xi_xbar)
print(table)
print(sum(fi_xi_xbar))
SR = sum(fi_xi_xbar)/banyak_data
print(SR)

# Standar deviasi
xi_min_mean_k <- c(
  (xi[1] - rata2)**2,
  (xi[2] - rata2)**2,
  (xi[3] - rata2)**2,
  (xi[4] - rata2)**2,
  (xi[5] - rata2)**2,
  (xi[6] - rata2)**2
)

fi_xi_xbar_k <- c(
  frekuensi[1] * xi_min_mean_k[1], 
  frekuensi[2] * xi_min_mean_k[2], 
  frekuensi[3] * xi_min_mean_k[3], 
  frekuensi[4] * xi_min_mean_k[4], 
  frekuensi[5] * xi_min_mean_k[5], 
  frekuensi[6] * xi_min_mean_k[6] 
)

table <- data.frame(batas_bawah, batas_atas, xi, fixi, xi_min_mean, xi_min_mean_k, fi_xi_xbar_k)
print(table)

S = (sum(fi_xi_xbar_k)/banyak_data)**0.5
print(S)
