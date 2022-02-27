# Pokédex - PFPA

Repositório: https://github.com/Misterio77/pokedex-pfpa
Versão live (em um servidor nosso): https://pokedex.misterio.me

## Sobre
Essa é uma aplicação web (feita em Haskell, com Yesod).

As funcionalidades implementadas são:
- Autenticação (login, registro, logout) e autorização (apenas o admin pode
- adicionar e remover pokémons)
- Adição, remoção, e listagem de pokémons no Pokédex
- Adição, remoção, e listagem de pokémons no seu perfil (team builder)

## Como executar
Primeiro, configure as credenciais do seu banco em `config/settings.yml`.

Você pode compilar e executar com o `stack`, `cabal`, ou `nix`:

```
stack build && stack exec pokedex-pfpa
```

```
cabal run
```

```
nix run
```

As migrações são executadas automaticamente.

O yesod gerará automaticamente uma chave `session.aes`, para assinar e
encriptar os cookies de sessão.

O CSS, templates, e configurações do `config/settings.yml` são embedados no
executável em tempo de compilação, então você pode levar o executável para
outro lugar e executar normalmente (só precisa levar a `session.aes`, ou deixar
o yesod gerar outro).

Você pode sobreescrever as configurações em tempo de execução usando variáveis
de ambiente (veja elas em `config/settings.yml`), ou especificando um caminho
para outro `.yml` especificando o caminho p/ ele como argumento ao executar.
