# BEIB <img src="man/figures/logo.png" align="right" height="139" alt="Hex Sticker Placeholder" />

<!-- badges: start -->
<!-- badges: end -->

O **BEIB** (Biblioteca de Indicadores da Educação Básica) é um pacote em R desenvolvido com o objetivo de facilitar o acesso e resgatar os dados dos indicadores educacionais calculados e disponibilizados pelo [INEP](https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/indicadores-educacionais) sobre a Educação Básica no Brasil.

## Instalação

Você pode instalar a versão de desenvolvimento do BEIB do [GitHub](https://github.com/) executando:

```r
# install.packages("devtools")
devtools::install_github("seu-usuario/BEIB")
```

## Indicadores Disponíveis

O pacote fornece funções para acessar facilmente os seguintes conjuntos de dados e indicadores da Educação Básica:

* Adequação da Formação Docente
* Complexidade de Gestão da Escola
* Esforço Docente
* Ideb (Índice de Desenvolvimento da Educação Básica)
* Indicadores Financeiros Educacionais
* Média de Alunos por Turma
* Média de Horas-aula diária
* Nível Socioeconômico (Inse)
* Percentual de Docentes com Curso Superior
* Regularidade do Corpo Docente
* Remuneração Média dos Docentes
* Taxas de Distorção Idade-série
* Taxas de Não-resposta (TNR)
* Taxas de Transição
* Taxas de Rendimento

## Exemplo de Uso

Este é um exemplo básico de como utilizar o pacote para importar os dados de um dos indicadores:

```r
library(BEIB)

# Exemplo hipotético: Baixando os dados de Média de Alunos por Turma para um ano específico
dados_turma <- get_media_alunos_turma(ano = 2023)

head(dados_turma)
```

## Fonte dos Dados

Todos os dados acessados por este pacote provêm do portal de Dados Abertos do Governo Federal e do Instituto Nacional de Estudos e Pesquisas Educacionais Anísio Teixeira (Inep).

Para mais detalhes sobre as metodologias de cálculo de cada indicador, visite:
[Indicadores Educacionais - INEP](https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/indicadores-educacionais)

---

**Nota:** Este pacote está em desenvolvimento. Contribuições, sugestões e relatos de bugs são muito bem-vindos!
