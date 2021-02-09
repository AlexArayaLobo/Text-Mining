###########################################
### Text Mining - Periódicos Costa Rica ###
###########################################

###Objetivos
#Visualizar de diferentes fomas las palabras más frecuentes en los cuerpos de las noticias y sus titulares.
#Comparar frecuencias de palabras entre dos condiciones previamente establecidas (Libertad vs Prisión).
#Comparar frecuencias de palabras entre dos periódicos muestreads (CrHoy vs Diario Extra).
#Realizar Análisis de sentimientos para estudiar la negatividad de los titulares de las noticias vs los cuerpos de estas.


###LO UNICO QUE FALTA ES SUBIR LAS IMAGENES.

# Librerias usadas
library(openxlsx) # Abrir base
library(tidyverse)
library(dplyr)
library(tidytext)
library(tm)
library(stopwords)
library(wordcloud)
library(ggplot2)
library(ggpubr)
library(reshape2) # Para acast en wordcloud.comparison
library(scales) # Para grafico de frecuencias
library(igraph) # Para graficar bigramas
library(ggraph) # Para graficar bigramas
library(topicmodels) #Para Topic Modeling


### Base de datos
Abogado2 <- read.xlsx("BaseAbogado2.xlsx")


### Diccionario de StopWords
custom_stop_words <- bind_rows(stop_words,
                               data_frame(
                                 word = tm::stopwords("spanish"),
                                 lexicon = "custom"))


#Generando una nueva base de datos mediante Token + antijoin a stopwords.
#Contenido: Texto general (Excluye titulos y subtitulos)
BaseToken2 <- 
  Abogado2 %>%
  select(Texto, Periodico, Condicion) %>%
  unnest_tokens(word, Texto) %>%
  anti_join(custom_stop_words)

### Analisis para TEXTO GENERAL (excluyendo titulos y subtitulos de noticias) ###

# WordCloud para las palabras mas frecuentes del texto general (Top 250)
WordCloudEdu1 <- 
  BaseToken2 %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 250)) # 250 palabras
dev.new() #Abrir nueva ventana para un mejor visualizado.

# Top 20 de palabras mas frecuentes del texto general (Top 20)
BaseToken2 %>%
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  geom_label(aes(label = n), fontface = "bold", size = 3) +
  labs(y = "Frecuencia de palabras", 
       x = "Palabras", 
       caption = "Fuente: Diario Extra y CrHoy") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Top 20 de palabras más frecuentes en las noticias recopiladas")

### CONDICIONES: LIBERTAD VS PRISION ###

# Top 20 Palabras más frecuentes. Libertad vs Prisión
BaseToken2 %>%
  count(Condicion,word, sort = TRUE) %>%
  group_by(Condicion) %>%
  top_n(20, n) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, Condicion)) %>%
  ggplot(aes(word, n, fill = Condicion)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~Condicion, scales = "free_y") +
  geom_label(aes(label = n), 
             size = 2.5, 
             fontface = "bold", 
             position=position_stack(0.96),
             show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Frecuencia de palabras", x = "Palabras",
       title = "Top 20 palabras más frecuentes en texto",
       subtitle = "Condiciones: Libre vs Prisión") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# WordCloud comparando frecuencia de palabras Prision vs Libertad en texto.
WordCloudEdu2 <- 
  BaseToken2 %>%
  count(Condicion, word) %>%
  acast(word ~ Condicion, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("tomato", "darkturquoise"),
    max.words = 100, random.order = FALSE)
dev.new() #Palabras repetidas para ambas condiciones no son mostradas en el wordcloud.

### Análisis para TÍTULOS de noticias ###

#Contenido de nueva base: Texto de Títulos
BaseTokenTitulos <- 
  Abogado2 %>%
  select(Titulo, Periodico, Condicion) %>%
  unnest_tokens(word, Titulo) %>%
  anti_join(custom_stop_words)

# Top 20 de Palabras más frecuentes en titulos
BaseTokenTitulos %>%
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(alpha = 0.8) +
  geom_label(aes(label = n), fontface = "bold", size = 3) +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Frecuencia de palabras", 
       x = "Palabras",
       caption = "Fuente: Diario Extra y CrHoy") +
  ggtitle("Top 20 de palabras más frecuentes en titulares de noticias") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

# Top 20 palabras más frecuentes en Titulos de noticias
# Libertad vs Prisión
BaseTokenTitulos %>%
  count(Condicion,word, sort = TRUE) %>%
  group_by(Condicion) %>%
  top_n(20, n) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, Condicion)) %>%
  ggplot(aes(word, n, fill = Condicion)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~Condicion, scales = "free_y") +
  geom_label(aes(label = n), 
             size = 2.5, 
             fontface = "bold", 
             position=position_stack(0.96),
             show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Frecuencia de palabras", x = "Palabras",
       title = "Top 20 palabras más frecuentes en titulares de noticias",
       subtitle = "Condiciones: Libre vs Prisión") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# WordCloud Comparando frecuencia de palabras en títulos.
# Condiciones: Prision vs Libre.
WordCloudEduTitulos <- 
  BaseTokenTitulos %>%
  count(Condicion, word) %>%
  acast(word ~ Condicion, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("tomato", "darkturquoise"),
    max.words = 100, random.order = FALSE)
dev.new()



### PERIODICOS: CrHoy vs Diario Extra ###

#TEXTO GENERAL

# Top 20 de Palabras más frecuentes en Texto general para ambos periódicos.
BaseToken2 %>%
  count(Periodico,word, sort = TRUE) %>%
  group_by(Periodico) %>%
  top_n(20, n) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, Periodico)) %>%
  ggplot(aes(word, n, fill = Periodico)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~Periodico, scales = "free_y") +
  geom_label(aes(label = n), 
             size = 2.5, 
             fontface = "bold", 
             position=position_stack(0.96),
             show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Frecuencia de palabras", x = "Palabras",
       title = "Top 20 palabras más frecuentes en texto",
       subtitle = "Periódicos: CrHoy vs Diario Extra") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# WordCloud Comparando Frecuencia de palabras Diario Extra vs CrHoy en Texto
WordCloudEdu3 <- BaseToken2 %>%
  count(Periodico, word) %>%
  acast(word ~ Periodico, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("tomato", "darkturquoise"),
    max.words = 100)
dev.new()

#TÍTULOS EN PERIÓDICOS
BaseTokenTitulos %>%
  count(Periodico,word, sort = TRUE) %>%
  group_by(Periodico) %>%
  top_n(20, n) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, Periodico)) %>%
  ggplot(aes(word, n, fill = Periodico)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~Periodico, scales = "free_y") +
  geom_label(aes(label = n), 
             size = 2.5, 
             fontface = "bold", 
             position=position_stack(0.96),
             show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Frecuencia de palabras", x = "Palabras",
       title = "Top 20 palabras más frecuentes en titulares de noticias",
       subtitle = "Periódicos: CrHoy vs Diario Extra") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# WordCloud Comparando Frecuencia de palabras en títulos.
# Periódicos: Diario Extra vs CrHoy
WordCloudPeriodicosTitulos <- 
  BaseTokenTitulos %>%
  count(Periodico, word) %>%
  acast(word ~ Periodico, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("tomato", "darkturquoise"),
    max.words = 100)
dev.new()


### SENTIMENT ANALYSIS ###

# Usando diccionario AFINN
download.file(
  "https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
  "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>%
  tbl_df()

# Análisis de Sentimientos de Prision vs Libertad en Texto General de las noticias colectadas.
Sentiment_Prision_Libre <- afinn %>%
  left_join(BaseToken2, by = c("Palabra" = "word")) %>%
  mutate(Sentimiento = ifelse(Puntuacion > 0, "Positivo", "Negativo")) %>%
  filter(!is.na(Condicion))

Sent_Text_Cond <-
  ggplot(Sentiment_Prision_Libre, aes(x = Condicion, fill = Sentimiento)) +
  geom_bar(position = "fill", width = 0.6, alpha = 0.8) +
  scale_fill_manual(values=c("#d1495b", "#66a182")) +
  ylab("Proporcion") +
  geom_hline(
    yintercept = 0.195, linetype = "dashed",
    color = "black", size = 0.2) +
  labs(y = "Proporción", x = "Condición", subtitle = "Cuerpo de noticia") +
  ggtitle("Análisis de sentimientos entre las condiciones de los implicados") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# Análisis de Sentimientos de Diario Extra vs CrHoy en Texto General de las noticias colectadas.
Sentiment_Periodicos <- afinn %>%
  left_join(BaseToken2, by = c("Palabra" = "word")) %>%
  mutate(Sentimiento = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>%
  filter(!is.na(Periodico))

Sent_Text_Per <- 
  ggplot(Sentiment_Periodicos, aes(x = Periodico, fill = Sentimiento)) +
  geom_bar(position = "fill", width = 0.6, alpha = 0.8) +
  scale_fill_manual(values=c("#d1495b", "#66a182")) +
  ylab("Proporcion") +
  geom_hline(
    yintercept = 0.195, linetype = "dashed",
    color = "black", size = 0.2) +
  labs(y = "Proporción", x = "Periódico", subtitle = "Cuerpo de noticia") +
  ggtitle("Análisis de sentimientos entre los periódicos muestreados") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# Análisis de Sentimientos en Títulos entre Condiciones

# Nuevos Tokens para Titulos
BaseTokenTitulos <- Abogado2 %>%
  select(Titulo, Periodico, Condicion) %>%
  unnest_tokens(word, Titulo) %>%
  anti_join(custom_stop_words)

Sentiment_Titulos <- afinn %>%
  left_join(BaseTokenTitulos, by = c("Palabra" = "word")) %>%
  mutate(Sentimiento = ifelse(Puntuacion > 0, "Positivo", "Negativo")) %>%
  filter(!is.na(Condicion))

Sent_Tit_Cond <-
  ggplot(Sentiment_Titulos, aes(x = Condicion, fill = Sentimiento)) +
  geom_bar(position = "fill", width = 0.6, alpha = 0.8) +
  scale_fill_manual(values=c("#d1495b", "#66a182")) +
  ylab("Proporcion") +
  geom_hline(
    yintercept = 0.055, linetype = "dashed",
    color = "black", size = 0.2) +
  labs(
    title = "Análisis de sentimientos entre las condiciones de los implicados",
    subtitle = "Títulos de noticias", y = "Proporción", x = "Condición") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



# Sentiment Analysis Titulos entre Periodicos
Sentiment_Titulos <- afinn %>%
  left_join(BaseTokenTitulos, by = c("Palabra" = "word")) %>%
  mutate(Sentimiento = ifelse(Puntuacion > 0, "Positivo", "Negativo")) %>%
  filter(!is.na(Periodico))

Sent_Tit_Per <- 
  ggplot(Sentiment_Titulos, aes(x = Periodico, fill = Sentimiento)) +
  geom_bar(position = "fill", width = 0.6, alpha = 0.8) +
  scale_fill_manual(values=c("#d1495b", "#66a182")) +
  geom_hline(
    yintercept = 0.047, linetype = "dashed",
    color = "black", size = 0.2) +
  labs(
    title = "Análisis de sentimientos entre los periódicos muestreados",
    subtitle = "Titulares de noticias", y = "Proporción", x = "Periódico") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Gráficos conjuntos

#Gráfico de Sentiment Analysis en Texto general y titulares según las condiciones de los implicados.
ggarrange(Sent_Text_Cond, 
          Sent_Tit_Cond,
          ncol = 2, nrow = 1,
          labels = c("A","B"))

#Gráfico de Sentiment Analysis en Texto general y titulares según los periódicos muestreados.
ggarrange(Sent_Text_Per, 
          Sent_Tit_Per,
          ncol = 2, nrow = 1,
          labels = c("A","B"))

#Hay una tendencia a mayor negatividad en los titulares de las noticias.



### BIGRAMAS ###

# Bigramas para Texto general
bigramsTexto <- Abogado2 %>%
  unnest_tokens(bigram, Texto, token = "ngrams", n = 2)

bigram_texto <- bigramsTexto %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word) %>%
  filter(n > 15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 point.padding = unit(0.2, "lines")
  ) +
  theme_void() + labs(title = "Bigramas para texto general")
bigram_texto


# Bigrama para títulos
bigramsTitulo <- Abogado2 %>%
  unnest_tokens(bigram, Titulo, token = "ngrams", n = 2)

bigram_titulo <- bigramsTitulo %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word) %>%
  filter(n > 2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 point.padding = unit(0.2, "lines")
  ) +
  theme_void() + labs(title = "Bigramas para títulos")
bigram_titulo


### Comparando frecuencias de palabras entre CRHoy and DiarioExtra ###
Frequency <- 
  Abogado2 %>% 
  select(Periodico,Texto) %>%
  unnest_tokens(word, Texto) %>%
  anti_join(custom_stop_words) %>%
  group_by(Periodico) %>%
  count(word, sort = TRUE) %>%
  left_join(Abogado2 %>%
              group_by(Periodico) %>%
              summarise(total = n())) %>%
  mutate(freq = n/total) %>%
  select(Periodico, word, freq) %>%
  spread(Periodico, freq) %>%
  arrange(Diario_Extra, CrHoy)

ggplot(Frequency, aes(Diario_Extra, CrHoy)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

### TOPIC MODEL ###

#Generando DocumentTermMatrix
Base_Topic <- 
  BaseToken2 %>% 
  mutate(ID = row_number()) %>%
  count(ID, word, sort = TRUE) %>%
  ungroup() %>%
  cast_dtm(ID, word, n)

Ab_LDA <- LDA(Base_Topic, k = 4, control = list(seed = 1234)) #4 Topics
Ab_LDA

Ab_Topics <- tidy(Ab_LDA, matrix = "beta")

Ab_Top_Terms <- 
  Ab_Topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Beta", x = "Palabras",
       title = "Topic Model (k = 4)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))
Ab_Top_Terms #Al ser el texto tan similar, no se percibe una esperada separación de los temas.


