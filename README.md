# MANS-Clustering-Imobiliario
Este projeto foi desenvolvido no âmbito da unidade curricular de Métodos de Aprendizagem Não Supervisionada, com o objetivo de explorar padrões no setor imobiliário usando técnicas de análise multivariada e clustering.

### Objetivo
O objetivo principal foi **agrupar imóveis com características semelhantes,** de forma a identificar padrões e possíveis perfis de propriedades (ex.: imóveis de luxo, imóveis urbanos compactos, etc.).

Foram aplicadas técnicas de redução de dimensionalidade e de clustering para interpretar relações entre as variáveis e observar a segmentação natural dos dados.

### Processo/metodologia

1. Compreensão e limpeza dos dados
2. Redução de dimensionalidade
3. Aplicação de métodos de clustering
4. Avaliação e comparação

### Ferramentas utilizadas
R (Psych, Corrplot, Cluster, Mclust, ggplot2)

### Resultados
- Os vários algoritmos formaram clusters com características distintas, mas a separação entre grupos não foi forte.
- Ward e PAM foram os métodos com maior concordância durante a comparação.
- O dataset não apresentava grupos naturais bem definidos — sugerindo **fraca segmentação real no mercado** com base apenas nas variáveis intrínsecas.
- Conclusão: a segmentação imobiliária provavelmente requer variáveis adicionais (ex.: localização, preço, fatores socioeconómicos).
