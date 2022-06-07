# monografia-estatistica
Repositório para disponibilizar os principais arquivos relacionados ao meu projeto de Monografia para a Graduação em Estatística pela UFMG.

### Modelagem Linear Generalizada com Efeito Espacial: um estudo com dados educacionais do estado de Minas Gerais

- Maria Luisa Gomes dos Reis
- Orientador: Professor Vinícius Diniz Mayrink
- Belo Horizonte, 2022

## Resumo

**Palavras-chave:** Modelos Lineares Generalizados; Estatística Bayesiana; Estatística Espacial; Taxa de abandono escolar.

<div style="text-align: right">
Em casos em que os dados sob estudo não se adequam à distribuição Normal e a transformação dos valores traz complexidade para a interpretação dos resultados, é adequado o uso de Modelos Lineares Generalizados (MLG). Os dados estudados neste trabalho dizem respeito às taxas de abandono escolar para os municípios do estado de Minas Gerais nos anos de 2010, 2015 e 2020. O objetivo principal foi avaliar quais fatores são capazes de explicar a taxa de abandono escolar neste estado, além de verificar se estes fatores mudaram ao longo dos anos. A análise descritiva e visual dos dados indicou forte assimetria à esquerda, com muitos valores próximos de zero. O uso do MLG Gama se mostrou propício, mas, por se tratar de uma taxa que varia de 0% a 100%, o MLG Beta seria mais apropriado. Como as informações das taxas de abandono são por município, foi possível trabalhar com a Estatística Espacial, adicionando ao modelo uma associação espacial entre municípios vizinhos. Uma vez que a estrutura espacial foi vastamente explorada neste estudo, optou-se por trabalhar com a Estatística Bayesiana. Antes de modelar os dados reais das taxas foi conduzido um estudo simulado para avaliar qual o melhor modelo para ajustá-las. Após este estudo, concluiu-se que as taxas de abandono para os municípios de Minas Gerais poderiam ser ajustadas assumindo efeito espacial tanto na estrutura da média quanto na do parâmetro de dispersão. Dentre os blocos de ensino, o Ensino Médio foi o que apresentou menor concentração de valores próximos de zero e por isso analisou-se apenas as taxas para este bloco. Apesar disso, a frequência de valores iguais a zero na variável resposta poderia impactar negativamente no ajuste e por isso foram obtidas mais 3 amostras, uma para cada ano sob estudo, que excluíam os municípios com taxa igual a zero, a fim de avaliar se haveria grandes diferenças nos resultados de um grupo ou outro de amostras. No geral, os fatores capazes de explicar significativamente as taxas de abandono do Ensino Médio dos municípios de Minas Gerais foram a taxa de escolas na área urbana que, quanto maior, tende a diminuir a taxa de abandono; e a taxa de escolas públicas no município que, quanto maior, tende a aumentar a taxa de abandono escolar. Os resultados se apresentaram consistentes ao longo dos anos e para as amostras com e sem as taxas iguais a zero, indicando que as causas por trás das taxas de abandono do Ensino Médio não tenham mudado ao longo da última década. 
</div>
