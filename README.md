### What is this?

These are CLI tools, to find patterns (tokens) in lead sheets in Jazz music.
This is an attempt at finding underlying structure of Jazz music and using it for
mapping other Jazz songs.

### CLI tools?

The first one named `raw-songs-handler` reads provided Jazz sheets and outputs
JSON with parsed, annotated songs and a txt file with songs in "relative notation".
Instead of looking at song as a series of chords, we convert them in chords separated by their
half-tone differences.

For example : `Dm7 G7 CM7 CM7` -> `m7 (+5) 7 (+5) M7 (0) M7`

The second is `token-creator` takes the relative notation as input and
constructs and outputs a given number of tokens. Additionally you can request
a report with more information about the tokens for further analysis.

The third is `songs-tokenizer` takes the JSON file with songs and tokens,
and outputs a JSON with tokenized songs. The report can be also requested.
Also, the same tool can be used for visualization of tokenized JSON.

More on running and usage below.

### Prerequisites

The project is written in Haskell, using cabal.
I recommend installing [GHCup](https://www.haskell.org/ghcup/install/)
to handle these dependencies.

### How to build and run the tools?

For building and running, we will use `cabal run exe:<my-target> -- <arguments>` command.
For help message run the following:
```
cabal run exe:raw-songs-handler -- --help
cabal run exe:token-creator -- --help
cabal run exe:songs-tokenizer -- --help
```


Make JSON and relative notation files:
```
cabal run exe:raw-songs-handler -- -r rel-notation.txt -o parsed-songs.json
```

Make tokens and report files (this may take about 2 minutes, depending on amount of tokens):
```
cabal run exe:token-creator -- -n rel-notation.txt -m 500 -r tokens-report-500.txt -t tokens-500.txt
```

Make a tokenized JSON file with the report (this may take about 7 minutes, depending on amount of tokens):
```
cabal run exe:songs-tokenizer -- -i parsed-songs.json -t tokens-500.txt -o tokenized-songs-500.json -r songs-report.txt
```
