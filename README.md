## Usage
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

Make a tokenized JSON file with the report (this may take about 5 minutes, depending on amount of tokens):
```
cabal run exe:songs-tokenizer -- -i parsed-songs.json -t tokens-500.txt -o tokenized-songs-500.json -r songs-report.txt
```

View the resulting tokenization in the terminal (you may use any other file pager of your liking instead of `less`):
```
cabal run exe:songs-tokenizer -- -i tokenized-songs-500.json --colored-view | less 
```

## How it works?

### What do we want?

There are a lot of well known patterns in Jazz songs.
For example, `Dm7 G7 	Cmaj7 Cmaj7` is a very popular piece.

We would like to locate them automatically, but there are several obstacles:

1. There are a lot of different seventh chords notations, and we have to unify them.

For example, we discard altered bases (`Dm7/G` -> `Dm7`), and convert all possible seventh chords to these
(their forms in the program and in the "wild"):
```
   mb7b5  | o
   m7b5   | ø
   mM7    | mΔ
   M7     | Δ
   m7     | m7
   7      | 7
```

2. Patterns are relative. For example, `C7 D7` and `D7 E7` are identical patterns, because the halftone difference in them is the same.
That's why we convert all songs to the "relative notation", where it will be easier to look for them.

An example of conversion:
`Em7b5 A7 Dm7 Em7b5 A7 Dm7 Dm7` -> `m7b5 (+5) 7 (+5) m7 (+2) m7b5 (+5) 7 (+5) m7 (0) m7`

Seventh notation is preserved, while the notes are converted to the distance in halftones.
As you can tell, this also complicates the search for patterns, since we want them to be convertible back to chords.
For example, `m7b5 (+5) 7` clearly means something like `Dm7b5 G7`, while `7 (+5)` makes no sense for humans.
This will be addressed later.

3. As humans, we have a "feel" for how patterns should look like, and they have to be respected.
For example, humans like when patterns end with two of the same chords, and don't like when such pairs are present in
the middle of the pattern. Also, humans prefer patterns with lengths 3, 4 and 8 of chords.

### How the creation of tokens works?

We use a modified BPE algorithm on the songs in relative notation, to find the most popular tuples in the data, and create the tokens by them.
To make them look more like the desired chord series we make tuples of three ((`M7`, `(0)`, `M7`) -> `M7(0)M7`)
There we also boost the chances of tokens we find more "human".
Also, we don't join the songs in one stream of data, and make tokens across them in parallel.

### What are the reports about?

They present all information found about tokens while creating and applying them to songs.
While creating we find the standings of popularity, their respective amounts and percentages, together
with their representations as tokens (in relative notation) and blocks (as regular chords).

While creating tokens, we also compare the found tokens/blocks with existing in the music literature blocks.
In the last columns are presented the names of the closest blocks (with numbers of chords that were altered in minor) and how similar are they.
The closer the number to 1 is, the closer the found token and existing block are.
 
