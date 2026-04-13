# Kōnane Board Game (Scala)

Projeto da UC **Programação Multiparadigma (2025/2026)** para implementação do jogo Kōnane, seguindo o enunciado em duas partes:

- **Parte 1**: camada de negócio (lógica do jogo) em Scala.
- **Parte 2**: camada de apresentação (TUI + GUI).

## Estado Atual do Projeto

### Funcionalidades implementadas

- Inicialização do tabuleiro com tamanho variável (`4x4`, `6x6`, `8x8`, `10x10`) com padrão alternado de peças.
- Remoção inicial de duas peças adjacentes (centro ou cantos), conforme seleção do utilizador.
- Cálculo de jogadas válidas por peça (`getValidMovesForPiece`) com suporte a capturas em cadeia na mesma direção.
- Validação e aplicação de jogadas (`play`), incluindo remoção das peças capturadas e atualização das coordenadas livres.
- Jogada aleatória pura com gerador pseudo-aleatório próprio (`MyRandom`) e jogada automática (`playRandomly`).
- Interface textual (TUI) com:
  - menu principal;
  - escolha de tamanho do tabuleiro;
  - escolha da posição inicial removida;
  - menu de jogo;
  - visualização do tabuleiro e jogadas possíveis.

### Funcionalidades pendentes/parciais

- `Load game` ainda não implementado (aparece como “under development” no menu).
- Verificação explícita de vencedor/fim de jogo (tarefa dedicada) não está completa.
- Temporizador e funcionalidade de `undo` não implementados.
- GUI (`JavaFX`) ainda não implementada.
- Persistência de estado entre execuções (guardar/carregar jogos) não implementada.

## Mapeamento das Tarefas do Enunciado (T1..T8)

- **T1**: implementada (`randomMove`).
- **T2**: implementada (`play` + auxiliares).
- **T3**: implementada (`playRandomly`).
- **T4**: implementada (representação visual do tabuleiro/jogadas em linha de comando via TUI).
- **T5**: pendente.
- **T6**: pendente.
- **T7**: parcialmente implementada (TUI funcional, mas sem todos os requisitos avançados do enunciado).
- **T8**: pendente (GUI).

## Como Executar

Pré-requisitos:

- JDK compatível com Scala 3.
- `sbt` instalado.

Executar:

```bash
sbt run
```

## Como Jogar (estado atual)

No menu principal:

- `N` -> novo jogo
- `L` -> carregar jogo (placeholder)
- `Q` -> sair

No menu de jogo:

- `P` -> jogar manualmente
- `M` -> jogada aleatória automática
- `C` -> consultar jogadas válidas de uma coordenada
- `R` -> teste de seleção aleatória (T1)
- `Q` -> voltar ao menu principal

Notas:

- Coordenadas são introduzidas por **linha** e **coluna** (índices numéricos).
- No tabuleiro: `B` = preta, `W` = branca, `.` = vazio.

## Estrutura do Código

- `src/main/scala/Main.scala` -> ciclo principal e menus.
- `src/main/scala/GameLogic.scala` -> regras e lógica de jogo.
- `src/main/scala/TUI.scala` -> interface textual.
- `src/main/scala/GUI.scala` -> interface gráfica (ainda por implementar).
- `src/main/scala/Types.scala` -> tipos e enums (`Board`, `Coord2D`, `Stone`, `HolePosition`).
- `src/main/scala/MyRandom.scala` -> gerador pseudo-aleatório puro.

## Observações

- A `scalaVersion` no `build.sbt` está definida para `3.8.2`.
- Não existem, neste momento, testes automatizados no repositório.
 
