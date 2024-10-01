## The readme is outdated

### What is this?

This is a CLI tool, that helps create text tokens using a given text file.
It uses the Byte Pair Encoder algorithm, and can make a specified amount of extra tokens
(to the addition of the letters used in the text)

### Building

To build run `stack build` in the `/byte-pair-encoder` directory.
If you want to copy the exec file to local bin,
run `stack build --copy-bin`

### Running

The help message:

`byte-pair-encoder-exe --help`

```
The bpe program

bpe [OPTIONS]

Common flags:
  -t --text-path=FILE         Get data for tokenization from file as a text
  -c --config-path=FILE       Specify non-default config file (ignore other
                              flags if using this one)
  -l --load-state-path=FILE   Specify a tokenizer save file to continue work
                              (loads it if specified)
  -m --make-tokens=INT        How many tokens should it create
  -s --save-state-path=FILE   If specified, saves the internal state in the
                              path
  -g --give-top-n-tokens=INT  Gives top n tokens as an output
  -? --help                   Display help message
  -V --version                Print version information
```

Some examples of usage:

Make 256 extra tokens and give top 10 by popularity.
`byte-pair-encoder-exe --text-path ./mine/text_example.txt --make-tokens 256 --give-top-n-tokens 10`

Make 256 extra tokens and save the tokenizator state.
`byte-pair-encoder-exe --text-path ./mine/text_example.txt --make-tokens 256 --save-state-path ./save_file.txt`

Load state, make 1024 tokens and give top 50 of the tokens
`byte-pair-encoder-exe --load-state-path ./save_file.txt --make-tokens 1024 --give-top-n-tokens 50`