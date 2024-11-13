## TODO

- [x] Dictionary symbols parser
- [x] Multi line BPE mode
- [x] Fix for tone range to `[-5, 6]`
- [x] Merge everyting to M, M7, 7
- [x] Test a merger with more sept chords
- [x] Test a merger with repeating chords
- [x] Leave no more than 2 consecutive chords

- [x] Table analysis output
- [x] Token to Blocks converter (assume that the first M7 is CM7)
- [x] Priority for tokens that start and end with a sept accord
- [x] Split song by tokens ???
- [x] Visualize song split (use default pagenizer)

- [x] Refactoring
- [] Stats after creating tokens
    - [x] Place by addition, amount of tokens, probability of a token, token itself and a block representation
    - [x] Precent covered by meaningful tokens
    - [x] Entropy by Shannon
    - [x] The longest, the most popular n tokens
    - [x] Is token an existing token?
    - [] A token from a website?
    - [x] What is the total number of found tokens
    - [] Change found tokens to fuzzy find of tokens.
- [] Stats after tokenization ?
    - [x] Precent covered by meaningful tokens
    - [x] Entropy by Shannon


- [x] Get blocks from literature
- [x] Get blocks with minor alterations
- [x] Get blocks with stress alterations
- [x] Rewrite readme

- [] Tests!
    - [x] Is chords reading is adequate
    - [x] Songs before tokenization and after are equivalent

    - [] For raw-songs-handler
    - [] For token-creator
    - [] For songs-tokenizer


- [x] Tokenizer State
- [x] Save state to a file
- [x] Retrive state from a file
