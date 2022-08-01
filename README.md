# sgm-api

This is a simple webserver application where, after registration, one can create events
other users can attend at.

## Quick start

This app is written using [stack](https://docs.haskellstack.org/en/stable/README/) as a build tool
and [servant](https://docs.servant.dev/en/stable/index.html) as a framework.
You can build the binary using

```bash
stack build
```

## Development

Before starting your application, make sure that all the required environment variables are set.
In order to do that you can rename `.env.example` to `.env`, that would be sufficient for local
development, dummy values will be read by [dotenv](https://hackage.haskell.org/package/dotenv)
library and used by app.

If you use vscode, you can simply open this repo in [devcontainer](https://code.visualstudio.com/docs/remote/containers).
This will take care of installing all the required dependencies and starting a database instance.
As soon as container is ready you can spawn a terminal and run

```bash
stack run
```

If you want to run the app using your local environment, you can use a `docker-compose.yaml` file
from `.devcontainer` folder

```bash
(cd ./.devcontainer && docker-compose up -d)
stack run
```

## Running tests

Tests are written using [Hspec](https://hackage.haskell.org/package/hspec) library.

To run the tests simply call

```bash
stack test
```

Keep in mind, that in order for integration tests to succeed, testing database should be accessible,
the same `docker-compose.yaml` file can be used

```bash
(cd ./.devcontainer && docker-compose up -d)
stack test
```
