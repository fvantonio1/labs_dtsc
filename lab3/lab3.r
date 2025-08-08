library(ggplot2)
library(dplyr)
library(readr)

dados <- read.csv('cogumelos_dataset.csv')

summary(dados)
str(dados)

dados <- dados |>
  mutate(across(where(is.character), as.factor))

sum(is.na(dados)) # sem dados faltantes

plot_count <- function(data, x_var){
  p <- ggplot(data, aes(x={{x_var}}, fill=classe)) 
  p <- p + geom_bar()
  
  return(p)
}

plot_count(dados, cor_chapeu)
plot_count(dados, forma_chapeu)
plot_count(dados, anel_caule)
plot_count(dados, base_caule)
plot_count(dados, tipo_lamelas)
plot_count(dados, cor_lamelas)
plot_count(dados, textura_superficie)
plot_count(dados, habitat)
plot_count(dados, sazonalidade)

plot_count(dados |>
  filter(cor_chapeu %in% c('amarelo', 'branco', 'vermelho')) |>
  filter(anel_caule == 'presente') |>
  filter(tipo_lamelas %in% c('adnatas', 'decorrentes')) |>
  filter(textura_superficie %in% c('escamosa', 'lisa')) |>
  filter(cor_lamelas %in% c('branca', 'marrom')) |>
  filter(base_caule == 'normal') |>
  filter(habitat != 'tronco_morto'), forma_chapeu) +
  labs(title='Para chapeus amarelos, brancos e vermelhos')

plot_count(dados |>
  filter(cor_chapeu %in% c('amarelo', 'branco', 'vermelho')) |>
  filter(anel_caule == 'ausente') |> 
  filter(habitat = 'solo') |>
  filter(tipo_lamelas != 'decorrentes') |>
  filter(base_caule != 'bulbosa') |> 
  filter(cor_lamelas %in% c('branca', 'marrom')) |>
  filter(forma_chapeu == 'plano') |>
  filter(sazonalidade == 'primavera'), textura_superficie) +
  labs(title='Para chapeus amarelos, brancos e vermelhos')

plot_count(dados |>
  filter(cor_chapeu %in% c('amarelo', 'branco', 'vermelho')) |>
  filter(anel_caule == 'ausente') |> 
  filter(habitat %in% c('madeira', 'tronco_morto')) |>
  filter(base_caule != 'com_volva') |> 
  filter(forma_chapeu %in% c('conico', 'campanulado')) |>
  filter(cor_lamelas == 'creme') |> 
  filter(tipo_lamelas != 'decorrentes'), sazonalidade) +
  labs(title='Para chapeus amarelos, brancos e vermelhos')

plot_count(dados |>
  filter(cor_chapeu %in% c('marrom', 'cinza', 'bege')) |>
  filter(anel_caule == 'ausente') |> 
  filter(tipo_lamelas != 'decorrentes') |>
  filter(textura_superficie == 'lisa') |>
  filter(sazonalidade == 'outono') |> 
  filter(cor_lamelas == 'branca'), base_caule) +
  labs(title='Para chapeus marrom, cinza e bege')

plot_count(dados |>
  filter(cor_chapeu %in% c('marrom', 'cinza', 'bege')) |>
  filter(anel_caule == 'ausente') |> 
  filter(tipo_lamelas != 'decorrentes') |>
  filter(textura_superficie != 'lisa') |>
  filter(forma_chapeu != 'conico') |>
  filter(cor_lamelas != 'marrom') |>
  filter(habitat != 'tronco_morto') |>
  filter(base_caule != 'normal'), sazonalidade) +
  labs(title='Para chapeus marrom, cinza e bege')

plot_count(dados |>
  filter(cor_chapeu %in% c('marrom', 'cinza', 'bege')) |>
  filter(anel_caule == 'presente') |>
  filter(forma_chapeu %in% c('plano', 'convexo')) |>
  filter(sazonalidade != 'verao') |>
  filter(cor_lamelas != 'marrom') |>
  filter(base_caule != 'normal') |>
  filter(sazonalidade != 'primavera') |>
  filter(textura_superficie != 'viscosa') |>
  filter(habitat != 'madeira'), tipo_lamelas) +
  labs(title='Para chapeus marrom, cinza e bege')

plot_count(dados |>
  filter(cor_chapeu %in% c('marrom', 'cinza', 'bege')) |>
  filter(anel_caule == 'presente') |>
  filter(forma_chapeu %in% c('campanulado', 'conico')) |> 
  filter(tipo_lamelas != 'livres') |>
  filter(base_caule != 'bulbosa') |>
  filter(sazonalidade != 'inverno') |>
  filter(habitat != 'solo') |>
  filter(cor_lamelas == 'branca') |>
  filter(textura_superficie == 'lisa'), textura_superficie) +
  labs(title='Para chapeus marrom, cinza e bege')

arvore <- function(data) {
  n <- nrow(data)
  predictions <- logical(n)
  
  for (i in 1:n) {
    row <- data[i, ]
    
    cor_chapeu <- as.character(row$cor_chapeu)
    anel_caule <- as.character(row$anel_caule)
    tipo_lamelas <- as.character(row$tipo_lamelas)
    textura_superficie <- as.character(row$textura_superficie)
    cor_lamelas <- as.character(row$cor_lamelas)
    base_caule <- as.character(row$base_caule)
    habitat <- as.character(row$habitat)
    forma_chapeu <- as.character(row$forma_chapeu)
    sazonalidade <- as.character(row$sazonalidade)
    
    if (cor_chapeu %in% c('amarelo', 'branco', 'vermelho')) {
      if (anel_caule == 'presente') {
        if (tipo_lamelas == 'livres') {
          predictions[i] <- FALSE # venenoso
        } 
        else { # tipo_lamelas == ('adnatas', 'decorrentes')
          if (textura_superficie == 'viscosa') {
            predictions[i] <- FALSE # venenoso
          } 
          else { # textura_superficie == {'escamosa', 'lisa'}
            if (cor_lamelas == 'rosa') {
              predictions[i] <- FALSE
            }
            else if (cor_lamelas == 'creme') {
              predictions[i] <- TRUE
            }
            else { # cor_lamelas == ('branca', 'marrom')
              if (base_caule %in% c('bulbosa', 'com_volva')) {
                predictions[i] <- FALSE
              }
              else { # base_caule == 'normal'
                if (habitat == 'tronco_morto') {
                  predictions[i] <- TRUE
                }
                else { # habitat == ('solo', 'madeira')
                  if (forma_chapeu == 'conico') {
                    predictions[i] <- FALSE
                  }
                  else {
                    predictions[i] <- TRUE
                  }
                }
              }
            }
          }
        }
      } 
      else { # anel_caule == 'ausente'
        if (habitat == 'solo') {
          if (tipo_lamelas == 'decorrentes') {
            predictions[i] <- FALSE
          }
          else { # tipo_lamelas == ('adnatas', 'livres')
            if (base_caule == 'bulbosa') {
              predictions[i] <- FALSE
            }
            else { # base_caule == ('com_volva', 'normal')
              if (cor_lamelas %in% c('creme', 'rosa')) {
                predictions[i] <- FALSE
              }
              else { # cor_lamelas
                if (forma_chapeu %in% c('campanulado', 'conico')) {
                  predictions[i] <- FALSE
                }
                else if (forma_chapeu == 'convexo') {
                  predictions[i] <- TRUE
                }
                else { # forma_chapeu == plano
                  if (sazonalidade == 'inverno') {
                    predictions[i] <- TRUE
                  }
                  else if (sazonalidade == 'outono') {
                    predictions[i] <- FALSE
                  }
                  else { # sazonalidade == 'primavera'
                    if (textura_superficie == 'lisa')
                      predictions[i] <- TRUE
                    else {
                      predictions[i] <- FALSE
                    }
                  }
                }
              }
            }
          }
        }
        else { # habitat == ('madeira', 'tronco_morto')
          if (base_caule == 'com_volva') {
            if (textura_superficie == 'escamosa') {
              if (forma_chapeu == 'conico') {
                predictions[i] <- FALSE
              }
              else { # forma_chapeu == campanulado
                if (sazonalidade == 'verao') {
                  predictions[i] <- FALSE
                }
                else {
                  predictions[i] <- TRUE
                }
              }
            }
            else {
              predictions[i] <- FALSE
            }
          }
          else { # base_caule == ('bulbosa', 'normal')
            if (forma_chapeu %in% c('convexo', 'plano')) {
              predictions[i] <- TRUE
            }
            else { # forma_chapeu == ('conico', 'campanulado')
              if (cor_lamelas == 'creme') {
                if (tipo_lamelas == 'decorrentes') {
                  predictions[i] <- TRUE
                }
                else {
                  predictions[i] <- FALSE
                }
              }
              else {
                predictions[i] <- FALSE
              }
            }
          }
        }
      }
    } 
    else { # cor == ('marrom', 'cinza', 'bege')
      if (anel_caule == 'ausente') {
        if (tipo_lamelas == 'decorrentes') {
          predictions[i] <- TRUE
        }
        else { 
          if (textura_superficie == 'lisa') {
            if (sazonalidade == 'outono') {
              if (cor_lamelas == 'branca') {
                if (base_caule == 'normal') {
                  predictions[i] <- TRUE
                }
                else {
                  predictions[i] <- FALSE
                }
              }
              else { # cor_lamelas != branca
                predictions[i] <- TRUE
              }
            }
            else { # sazonalidade != 'outono'
              predictions[i] <- TRUE
            }
            
          }
          else { # textura_superficie == ('escamosa', 'viscosa')
            if (forma_chapeu == 'conico') {
              predictions[i] <- FALSE
            }
            else {
              if (cor_lamelas == 'marrom') {
                predictions[i] <- TRUE
              } 
              else { # cor_lamelas != marrom
                if (habitat == 'tronco_morto') {
                  predictions[i] <- TRUE
                }
                else {
                  if (base_caule == 'normal') {
                    predictions[i] <- TRUE
                  }
                  else {
                    if (sazonalidade == 'outono') {
                      predictions[i] <- TRUE
                    }
                    else {
                      predictions[i] <- FALSE
                    }
                  }
                }
              }
            }
          }
        }
      }
      else { # anel_caule == 'presente'
        if (forma_chapeu %in% c('plano', 'convexo')) {
          if (sazonalidade == 'verao') {
            predictions[i] <- TRUE
          }
          else {
            if (cor_lamelas == 'marrom') {
              predictions[i] <- TRUE
            }
            else {
              if (base_caule == 'normal') {
                predictions[i] <- TRUE
              }
              else {
                if (sazonalidade == 'primavera') {
                  predictions[i] <- FALSE
                }
                else {
                  if (textura_superficie == 'viscosa') {
                    predictions[i] <- FALSE
                  }
                  else {
                    if (habitat == 'madeira') {
                      predictions[i] <- TRUE
                    }
                    else {
                      if (tipo_lamelas == 'adnatas') {
                        predictions[i] <- TRUE
                      }
                      else {
                        predictions[i] <- FALSE
                      }
                    }
                  }
                }
              }
            }
          }
        }
        else { # forma_chapeu == (campanulado, conico)
          if (tipo_lamelas == 'livres') {
            if (textura_superficie == 'lisa') {
              if (base_caule == 'bulbosa') {
                predictions[i] <- FALSE
              }
              else {
                predictions[i] <- TRUE
              }
            }
            else {
              predictions[i] <- FALSE
            }
          }
          else { # tipo_lamelas == 'decorrentes', 'adnatas'
            if (base_caule == 'bulbosa') {
              predictions[i] <- FALSE
            }
            else {
              if (sazonalidade == 'inverno') {
                predictions[i] <- TRUE
              }
              else {
                if (habitat == 'solo') {
                  if (textura_superficie == 'viscosa') {
                    predictions[i] <- FALSE
                  }
                  else if (textura_superficie == 'lisa') {
                    predictions[i] <- TRUE
                  }
                  else { # textura_superficie == 'escamosa'
                    predictions[i] <- FALSE
                  }
                }
                else { # habitat != 'solo'
                  if (cor_lamelas == 'branca') {
                    if (textura_superficie == 'escamosa') {
                      predictions[i] <- TRUE
                    }
                    else  {
                      predictions[i] <- FALSE
                    }
                  }
                  else {
                    predictions[i] <- TRUE
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  return(predictions)
}

y <- dados$classe == 'comestivel'
# previsao dos dados
yhat <- arvore(dados)

# acuracia
acuracia <- sum(y == yhat) / length(y)

# matrix de confusão
conf_matrix <- table(Actual=y, Predicted=yhat)
conf_matrix

# adotarei a classe comestível como positiva e a venenoso como negativa
TN <- conf_matrix['FALSE', 'FALSE']
TP <- conf_matrix['TRUE', 'TRUE']
FN <- conf_matrix['TRUE', 'FALSE']
FP <- conf_matrix['FALSE', 'TRUE']

precisao <- TP / (TP + FP)
recall <- TP / (TP + FN)

erros_idx <- y != yhat
erros <- dados[erros_idx, ]
