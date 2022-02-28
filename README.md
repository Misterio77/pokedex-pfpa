# Projeto Yesod - Programação Funcional Pura com Aplicações - EaD - Curso de verão IME

## Integrantes do grupo
- 10856803 Gabriel Fontes
- 11816122 João Vitor Diógenes
- 6476509 Euclides Torres Ometto Stolf

## Projeto escolhido: 1 - Crie um site utilizando o framework Yesod.

- **Versão live**: https://pokedex.misterio.me (hospedado em um servidor nosso, basta abrir e navegar o/). Você pode acessar o usuário `admin` com a senha `admin`.

- Repositório: https://github.com/Misterio77/pokedex-pfpa

## Funcionalidades resumidas

- Autenticação (login, registro, logout) e autorização (o primeiro usuário registrado é considerado o admin)
- Adição, remoção, e listagem de pokémons no Pokédex (apenas admin)
- Adição, remoção, e listagem de pokémons no seu perfil

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
o yesod gerar outra).

Você pode sobreescrever as configurações em tempo de execução usando variáveis
de ambiente (veja elas em `config/settings.yml`), ou especificando um outro
`.yml` como argumento ao executar.

## Descrição detalhada

Nosso projeto possui o intuito de ser um organizador de equipes para mestres de Pokémons. Cada treinador deve se registrar para ter acesso à funcionalidade de montar a sua própria equipe, e para tal, existem pokémons pré-cadastrados disponíveis, com as devidas informações de nome, tipo e respectiva imagem.

O objetivo final é ser um sistema simples, mas de usabilidade agradável e que poderia ser útil em mostrar quais pokémons fazem parte do seu time, como um mestre pokémon.

### Banco de dados
Nosso banco de dados está estruturado de acordo com este diagrama Crows Foots:

![diagrama](https://lh5.googleusercontent.com/4rOFJAhgNe0gVwuRhkXH08o-apQyspBvPBR_AHZf8xGi9OeLeqWIxqTyPfK9h6Ym8nti25-7JnUD62lpYpwYyBa66lY5h5eizFJ7R3SrfajPj0ZmuHRX_pnWNY3NzhjXZdE9LRDa)

### Estilização
Utilizamos a ferramenta SCSS para estilização. Aplicamos uma estilização padrão (para cada elemento, independente da página), de forma que tenhamos padronização em todo o sistema, e também praticidade e agilidade no desenvolvimento.

_PS: Sim, o tema é adaptável de acordo com o seu navegador e sistema operacional. Sim, o preto e amarelo no tema escuro é #CAASO. **Voa ICMC.**_
