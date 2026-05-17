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
- AI para jogar contra o computador, com três níveis de dificuldade:
  - fácil: jogada aleatória;
  - intermédio: preferência por jogadas com mais capturas;
  - avançado: preferência por jogadas que melhoram a mobilidade futura.
- Interface textual (TUI) com:
  - menu principal;
  - escolha do modo de jogo;
  - escolha da dificuldade da máquina;
  - escolha de tamanho do tabuleiro;
  - escolha da posição inicial removida;
  - menu de jogo;
  - visualização do tabuleiro e jogadas possíveis.
- Interface gráfica (GUI) em JavaFX.
- Verificação de fim de jogo quando um jogador fica sem jogadas válidas.
- Temporizador por jogada.
- Funcionalidade de `undo`.
- Guardar e carregar jogo através do ficheiro `savegame.txt`.
- O ficheiro de save guarda também o histórico das jogadas, permitindo fazer `undo` depois de carregar um jogo.

## Mapeamento das Tarefas do Enunciado (T1..T8)

- **T1**: implementada (`randomMove`).
- **T2**: implementada (`play` + auxiliares).
- **T3**: implementada (`playRandomly`).
- **T4**: implementada (representação visual do tabuleiro/jogadas em linha de comando via TUI).
- **T5**: implementada (`hasValidMoves`).
- **T6**: implementada (guardar/carregar jogo).
- **T7**: implementada na TUI.
- **T8**: implementada com GUI em JavaFX.

## Como Executar

Pré-requisitos:

- JDK compatível com Scala 3.
- `sbt` instalado.

Executar:

```bash
sbt run
```

O projeto tem duas classes principais. Ao executar, o `sbt` pode pedir para escolher:

- `GUIApp` -> interface gráfica.
- `Main` -> interface textual no terminal.

## Como Jogar (estado atual)

No menu principal:

- `N` -> novo jogo
- `L` -> carregar jogo
- `Q` -> sair

No menu de jogo:

- `P` -> jogar manualmente
- `M` -> jogada aleatória automática
- `C` -> consultar jogadas válidas de uma coordenada
- `U` -> fazer undo
- `S` -> guardar jogo
- `Q` -> voltar ao menu principal

Notas:

- Coordenadas são introduzidas por **linha** e **coluna** (índices numéricos).
- No tabuleiro: `B` = preta, `W` = branca, `.` = vazio.
- O jogo é guardado em `savegame.txt`.
- Ao carregar um jogo guardado, o histórico de jogadas também é recuperado, por isso o `undo` continua disponível.

## Estrutura do Código

- `src/main/scala/Main.scala` -> ciclo principal e menus.
- `src/main/scala/GameLogic.scala` -> regras e lógica de jogo.
- `src/main/scala/AILogic.scala` -> lógica da máquina e níveis de dificuldade.
- `src/main/scala/TUI.scala` -> interface textual.
- `src/main/scala/GUI.scala` -> arranque da interface gráfica.
- `src/main/scala/GameController.scala` -> controlo da interface gráfica do jogo.
- `src/main/scala/MenuController.scala` -> controlo do menu gráfico.
- `src/main/scala/SaveLoadLogic.scala` -> guardar e carregar jogos.
- `src/main/scala/Types.scala` -> tipos e enums (`Board`, `Coord2D`, `Stone`, `HolePosition`).
- `src/main/scala/MyRandom.scala` -> gerador pseudo-aleatório puro.

## Observações

- A `scalaVersion` no `build.sbt` está definida para `3.8.2`.
