#Вступне слово: єдина база даних, фільтри, групування даних,
#регулярні вирази для пошуку окремих слів
#Пам’ятайте - повна автоматизація майже недосяжна. Частину роботи
#все-одно доведеться виконувати вручну


#Запускаємо потрібні пакети
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)

#1)#Під’єднуємо базу даних SQLite
trans.sql <- src_sqlite("/home/geka/Documents/Anticor/edata.sqlite",
                create = TRUE)

#2)#Скільки записів у базі даних і яка загальна сума транзакцій
tbl(trans.sql, sql("SELECT * FROM edata")) %>%
  collect(n = Inf) %>%
  summarise(all.suma = sum(amount), all.count = n())

#3)#Скільки транзакцій і на яку суму зробили відділи освіти та Управління освіти
tbl(trans.sql, sql("SELECT * FROM edata")) %>%
  collect(n = Inf) %>%
  filter(payer_edrpou %in% c("02142282","02124781",
        "02142313", "02142325", "04544501", "02142276",
        "02142307", "02142299")) %>%
  group_by(payer_edrpou) %>%
  summarise(all.suma = sum(amount), all.count = n(),
            min.trans = min(amount), max.trans = max(amount),
            med.trans = median(amount))

#4)#Скільки і кому заплатили відділи освіти та Управління освіти
tbl(trans.sql, sql("SELECT * FROM edata")) %>%
  collect(n = Inf) %>%
  filter(payer_edrpou %in% c("02142282","02124781",
        "02142313", "02142325", "04544501", "02142276",
        "02142307", "02142299")) %>%
#але при такому підході - буде проблема із ФОПами
#краще ФОПів взагалі відфільтрувати
  # filter(nchar(recipt_edrpou) != 10) %>%
  group_by(payer_edrpou, recipt_edrpou) %>%
  summarise(all.suma = sum(amount), all.count = n(),
            min.trans = min(amount), max.trans = max(amount),
            med.trans = median(amount)) -> tmp

#5)#Як виокремити платежі, які було спрямовано на ФОПів?
tbl(trans.sql, sql("SELECT * FROM edata")) %>%
  collect(n = Inf) %>%
  filter(payer_edrpou %in% c("02142282","02124781",
        "02142313", "02142325", "04544501", "02142276",
        "02142307", "02142299")) %>%
  filter(nchar(recipt_edrpou) == 10) -> tmp
#Далі треба ці дані обробляти вручну:
write.csv(tmp, "FOP.csv", row.names = FALSE)
#і після обробки - завантажувати в R

#6)#Як виокремити платежі на конкретну особу (ФОП)?
tbl(trans.sql, sql("SELECT * FROM edata")) %>%
  collect(n = Inf) %>%
  filter(payer_edrpou %in% c("02142282","02124781",
        "02142313", "02142325", "04544501", "02142276",
        "02142307", "02142299")) %>%
  mutate(recipt_name2 = gsub("[[:cntrl:][:digit:][:punct:]]",
                            " ", recipt_name)) %>%
  mutate(words = strsplit(as.character(recipt_name2), " ")) %>%
  unnest(words) %>%
  filter(words != "") %>%
  filter(nchar(words) > 3) %>%
  mutate(words = toupper(words)) %>%
  filter(words %in% c("АНГЕЛЕЙКО")) %>%
  distinct(id, .keep_all = TRUE) -> tmp
#Але і тут без ручної обробки - дуже складно!
write.csv(tmp, "Angeleyko.csv", row.names = FALSE)
  
#7)#Скільки і кому освітяни заплатили за електричні плити?
tbl(trans.sql, sql("SELECT * FROM edata")) %>%
  collect(n = Inf) %>%
  filter(payer_edrpou %in% c("02142282","02124781",
        "02142313", "02142325", "04544501", "02142276",
        "02142307", "02142299")) %>%
  mutate(payment_details2 = gsub("[[:cntrl:][:digit:][:punct:]]",
                            " ", payment_details)) %>%
  mutate(words = strsplit(as.character(payment_details), " ")) %>%
  unnest(words) %>%
  filter(words != "") %>%
  filter(nchar(words) > 3) %>%
  mutate(words = toupper(words)) %>%
  filter(words %in% c("ПЛИТА", "ПЛИТИ", "ПЛИТ", "ПЛИТУ")) %>%
  select(id, amount, payer_edrpou, payer_name, recipt_name,
         recipt_edrpou, trans_date, payment_details) %>%
  distinct(id, .keep_all = TRUE) -> tmp
  
#8)#Візуалізація транзакцій
#Скільки загалом заплатили відділи освіти та Управління освіти
#Верхня частина скрипту - із прикладу №3
png("Osvita_platezhi.png", width = 297, 
    height = 210, units = "mm", res = 150)
tbl(trans.sql, sql("SELECT * FROM edata")) %>%
  collect(n = Inf) %>%
  filter(payer_edrpou %in% c("02142282","02124781",
        "02142313", "02142325", "04544501", "02142276",
        "02142307", "02142299")) %>%
  group_by(payer_edrpou) %>%
  summarise(all.suma = sum(amount)/1e+6, all.count = n(),
            min.trans = min(amount), max.trans = max(amount),
            med.trans = median(amount)) %>%
  # mutate(ustanova = ordered(ifelse(
  #   (payer_edrpou == "02142299"), "Управління освіти і науки",
  #   ifelse((payer_edrpou == "04544501"), "Довгинцівський відділ освіти",
  #   ifelse((payer_edrpou == "02142276"), "Інгулецький відділ освіти",
  #   ifelse((payer_edrpou == "02142313"), "Металургійний відділ освіти",
  #   ifelse((payer_edrpou == "02142282"), "Покровський відділ освіти",
  #   ifelse((payer_edrpou == "02124781"), "Саксаганський відділ освіти",
  #   ifelse((payer_edrpou == "02142307"), "Тернівський відділ освіти",
  #   ifelse((payer_edrpou == "02142325"), "Центрально-Міський відділ освіти", "ERROR"
  # )))))))), levels = rev(c("Управління освіти і науки",
  #           "Довгинцівський відділ освіти",
  #           "Інгулецький відділ освіти",
  #           "Металургійний відділ освіти",
  #           "Покровський відділ освіти",
  #           "Саксаганський відділ освіти",
  #           "Тернівський відділ освіти",
  #           "Центрально-Міський відділ освіти")))) %>%
ggplot(aes(x = payer_edrpou, y = all.suma, fill = "")) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  # geom_label(aes(label = paste(round(all.suma, 2),
  #  "млн.грн.", sep = " ")), nudge_y = 1,
  #  vjust = 0.5, hjust = 0.0, family = "Roboto",
  #  size = 5.0, colour = "white", fontface = "bold", parse = FALSE) +
  # scale_y_continuous(breaks=seq(0, 650, 50),
  #     limits = c(0, 780)) +
  labs(y = "\nзагальні суми платежів, млн.грн.",
       x = "",
       title = "Суми платежів районних відділів освіти\nта Управління освіти за період 15.09.2015 до 20.11.2016\n(дані порталу www.spending.gov.ua)")+
  scale_fill_manual(values=c("gold2")) +
  theme(plot.title = element_text(hjust = 0.5),
   text = element_text(family = "Roboto",
   face = "italic", size = 12, colour = "firebrick4", lineheight = 0.9),
   legend.position = "none",
   legend.text = element_text(family = "Roboto",
   face = "italic", size = 12, colour = "firebrick4", lineheight = 0.8, angle = 0),
   axis.text.x = element_text(colour = "gray15", face="bold",
      size= 12, angle = 0, vjust = 1, hjust = 0.5),
   axis.text.y = element_text(colour = "gray15", face="bold",
      size= 12, angle = 0, vjust = 0.5, hjust = 1),
   axis.ticks = element_line(colour = "orange", size = 0.2),
   plot.background = element_rect(fill = "gray95"),
   panel.grid.major = element_line(colour = "orange", size = 0.2),
   panel.grid.minor = element_line(colour = "gray90"),
   panel.background = element_rect(fill="gray95"))
dev.off()


#Додатково: Робота з календарними датами - скільки платили щомісяця?
tbl(trans.sql, sql("SELECT * FROM edata")) %>%
  collect(n = Inf) %>%
  filter(payer_edrpou %in% c("02142282","02124781",
        "02142313", "02142325", "04544501", "02142276",
        "02142307", "02142299")) %>%
  mutate(date.pos = as.Date(trans_date,"%Y-%m-%d")) %>%
  filter(date.pos >= "2016-01-01", date.pos <= "2016-12-31") %>%
  mutate(month.order = factor(format(date.pos, "%B"),
    levels = c("січень", "лютий", "березень", "квітень", "травень", 
    "червень", "липень", "серпень", "вересень", 
    "жовтень",  "листопад", "грудень"))) %>%
  mutate(weekday.order = factor(format(date.pos, "%a"),
    levels = c("пн", "вт", "ср", "чт", "пт", "сб", "нд"))) %>%
  group_by(month.order) %>%
  summarise(all.suma = sum(amount, na.rm = TRUE)/1e+6,
            all.count = n()) -> tmp
