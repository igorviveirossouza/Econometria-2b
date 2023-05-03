
test_unit_root <- function(data) {
  library(urca)
    # Passo 1: Teste de raiz unitária com trend
  adf_test_trend <- ur.df(data, type = "trend", selectlags = "AIC")

  # Passo 2: Verificar se há raiz unitária
  if (adf_test_trend@teststat[1] <=  adf_test_trend@cval['tau3','5pct']) {
    # Não há raiz unitária
    cat("Não há raiz unitária.\n")
  } else {
    # Passo 3: Teste do parâmetro associado trend
    if (adf_test_trend@teststat[,'phi3']>=adf_test_trend@cval['phi3','5pct']) {
      if(adf_test_trend@testreg$coefficients['tt','Pr(>|t|)']<=.05){
        # Não há raiz unitária
        cat("Não há raiz unitária e há tendência .\n")
      } else{
        # Há há raiz unitária
        cat("Há raiz unitária.\n")
      }
    } else{
      # Passo 4: Teste de raiz unitária com drift
      adf_test_drift <- ur.df(data, type = "drift", selectlags = "AIC")

      # Passo 5: Verificar se há raiz unitária
      if (adf_test_drift@teststat[1] <=  adf_test_drift@cval['tau2','5pct']) {
        # Não há raiz unitária
        cat("Não há raiz unitária.\n")
      } else {
        # Passo 6: Teste do parâmetro associado à raiz unitária
        if (adf_test_drift@teststat[,'phi1']>=adf_test_drift@cval['phi1','5pct']) {
          if(adf_test_drift@testreg$coefficients['z.lag.1','Pr(>|t|)']<=.05){
            # Não há raiz unitária
            cat("Não há raiz unitária e há tendência determinística .\n")
          } else{
            # Há há raiz unitária
            cat("Há raiz unitária com drift .\n")
          }
        } else{
          # Passo 7: Teste de raiz unitária sem trend e drift
          adf_test_none <- ur.df(data, type = "none", selectlags = "AIC")

          # Verificar se há raiz unitária
          if (adf_test_none@teststat[1] <=  adf_test_none@cval['tau1','5pct']) {
            # Não há raiz unitária
            cat("Não há raiz unitária.\n")
          } else {
            # Há raiz unitária
            cat("Pure Random Walk.\n")
          }
        }
        }
      }
    }
  }




# Dados de exemplo
set.seed(123)
data <- ( 0.2*seq(1:100)+  rnorm(100))
data|>plot(t="l")
# Executar o algoritmo
test_unit_root(data)
