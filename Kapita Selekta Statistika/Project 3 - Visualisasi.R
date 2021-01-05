## VISUALISASI ##
library(ggplot2)
#integer
pred <- predict(lm(kakao$hv ~ kakao$blk), data = kakao)
ggplot(data = kakao, mapping = aes(x = blk, y = hv)) +
  geom_point(aes(color = blk), color = "black") +
  geom_line(mapping = aes(y = pred), color = "purple") +
  labs(x = "Jumlah Kerugian Bulanan akibat Black Pod",
       y = "Jumlah Pod buah yang di panen")

pred <- predict(lm(kakao$hv ~ kakao$wilt), data = kakao)
ggplot(data = kakao, mapping = aes(x = wilt, y = hv)) +
  geom_point(aes(color = wilt), color = "black") +
  geom_line(mapping = aes(y = pred), color = "purple") +
  labs(x = "Jumlah Kerugian Bulanan akibat Cherelle Wilt",
       y = "Jumlah Pod buah yang di panen")

pred <- predict(lm(kakao$hv ~ kakao$loss), data = kakao)
ggplot(data = kakao, mapping = aes(x = loss, y = hv)) +
  geom_point(aes(color = loss), color = "black") +
  geom_line(mapping = aes(y = pred), color = "purple") +
  labs(x = "Jumlah Kerugian Bulanan akibat infeksi lainnya",
       y = "Jumlah Pod buah yang di panen")

pred <- predict(lm(kakao$hv ~ kakao$ty), data = kakao)
ggplot(data = kakao, mapping = aes(x = ty, y = hv)) +
  geom_point(aes(color = ty), color = "black") +
  geom_line(mapping = aes(y = pred), color = "purple") +
  labs(x = "Jumlah Pod Kecil pada akhir bulan",
       y = "Jumlah Pod buah yang di panen")

pred <- predict(lm(kakao$hv ~ kakao$sml), data = kakao)
ggplot(data = kakao, mapping = aes(x = sml, y = hv)) +
  geom_point(aes(color = sml), color = "black") +
  geom_line(mapping = aes(y = pred), color = "purple") +
  labs(x = "Jumlah Pod Sedang pada akhir bulan",
       y = "Jumlah Pod buah yang di panen")

pred <- predict(lm(kakao$hv ~ kakao$lrg), data = kakao)
ggplot(data = kakao, mapping = aes(x = lrg, y = hv)) +
  geom_point(aes(color = lrg), color = "black") +
  geom_line(mapping = aes(y = pred), color = "purple") +
  labs(x = "Jumlah Pod Besar pada akhir bulan",
       y = "Jumlah Pod buah yang di panen")

pred <- predict(lm(kakao$hv ~ kakao$pcp), data = kakao)
ggplot(data = kakao, mapping = aes(x = pcp, y = hv)) +
  geom_point(aes(color = pcp), color = "black") +
  geom_line(mapping = aes(y = pred), color = "purple") +
  labs(x = "Rata-rata curah hujan bulanan",
       y = "Jumlah Pod buah yang di panen")

pred <- predict(lm(kakao$hv ~ kakao$maxt), data = kakao)
ggplot(data = kakao, mapping = aes(x = maxt, y = hv)) +
  geom_point(aes(color = maxt), color = "black") +
  geom_line(mapping = aes(y = pred), color = "purple") +
  labs(x = "Rata-rata suhu maksimum bulanan",
       y = "Jumlah Pod buah yang di panen")

pred <- predict(lm(kakao$hv ~ kakao$mint), data = kakao)
ggplot(data = kakao, mapping = aes(x = mint,  y = hv)) +
  geom_point(aes(color = mint), color = "black") +
  geom_line(mapping = aes(y = pred), color = "purple") +
  labs(x = "Rata-rata suhu minimum bulanan",
       y = "Jumlah Pod buah yang di panen")

pred <- predict(lm(kakao$hv ~ kakao$midt), data = kakao)
ggplot(data = kakao, mapping = aes(x = midt, y = hv)) +
  geom_point(aes(color = midt), color = "black") +
  geom_line(mapping = aes(y = pred), color = "purple") +
  labs(x = "Rata-rata suhu siang hari bulanan",
       y = "Jumlah Pod buah yang di panen")

pred <- predict(lm(kakao$hv ~ kakao$smi), data = kakao)
ggplot(data = kakao, mapping = aes(x = smi, y = hv)) +
  geom_point(aes(color = smi), color = "black") +
  geom_line(mapping = aes(y = pred), color = "purple") +
  labs(x = "Rata-rata indeks kelembaban tanah bulanan",
       y = "Jumlah Pod buah yang di panen")

pred <- predict(lm(kakao$hv ~ kakao$lmi), data = kakao)
ggplot(data = kakao, mapping = aes(x = lmi, y = hv)) +
  geom_point(aes(color = lmi), color = "black") +
  geom_line(mapping = aes(y = pred), color = "purple") +
  labs(x = "Rata-rata indeks kelembaban daun bulanan",
       y = "Jumlah Pod buah yang di panen")

#kategorik
ggplot(data = kakao) +
  geom_bar(mapping = aes(x = can, fill = can)) +
  labs(x = "Kondisi Kanopi pada akhir bulan",
       y = "Jumlah Pod buah yang di panen")

ggplot(data = kakao) +
  geom_bar(mapping = aes(x = flu, fill = flu)) +
  labs(x = "Ukuran daun muda (flush) pada akhir bulan",
       y = "Jumlah Pod buah yang di panen")

ggplot(data = kakao) +
  geom_bar(mapping = aes(x = flw, fill = flw)) +
  labs(x = "Banyaknya Bunga pada akhir bulan",
       y = "Jumlah Pod buah yang di panen")
