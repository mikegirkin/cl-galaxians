# AGENTS

## Project structure

- All the sprites are located in the `./resources/images` folder
- Generic geometry-related things are located in a separate `geometry` package in `./src/geometry`
- Tests are located in `./src/test` folder and should be located at the path named same as in `src`

## Hidden knowledge and assumptions

World coordinate system it Y directed up, X directed right

## Work ethics

- Do not add any new sprites, images, or any other resources
- Always run `make test` after making changes to make sure that project can pass all the defined tests
- After implementing changes check `README.md` and make relevant updates if necessary
- All functions that mutate the state should have `!` suffix in their name
