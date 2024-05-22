# Development report

### What the task was.

The task was to create an CLI app, that implements the Byte Pair Encoding algoritm.

### The architecture of your solution.

I have decided to create a `TokenizerState` datatype, that stores all the information needed for the algorithm to work.
The algorithm works as follows: 
 - Make a list of all tokens
 - Make a frequencies map of all pairs of tokens (pairs like this: [1,2,3,4] -> [(1,2), (2,3), (3,4)]) 
 - Take the most frequent pair and make another token out of it
 - Replace the token pair with a new token

To convert from list of token to text and back, we also save a lookup table.

To make a CLI interface I used the `CmdArgs` package.

### Why certain architecture dessisions were done.

I changed the program to global datatype because it was hard to follow the conversions and changes
in the pipline of the program. Also, it will be possible to make the `TokenizerState` an instance of
some classes.

Other desiciions in the `Tokenizer` file are driven by the optimization attempts.

All handeling if the CLI interface is stored in the `Main` file, and divided in a semi-logical chunks.
It's still hard to express the CLI interface handeling in the functional style, but I think I did a relatively
good job.

### Why certain libraries were chosen.

The `CmdArgs` package is the first package that was working and had a good documentation.
No other not-standard packages were used. (and needed, to be honest)

### Investigation of the performance.

The development the majority of the time was driven by the performance.
I used the ghc profiler to understand the bottelneks in the performance,
 and I was focusing my attention at them.
The most time consuming part of the algorithm is the `mostFrequentTokenPair` function.
It looks for the token pair to merge. It used to be extremely slow, but with usage of `Map`
 and Map functions like `Map.foldrWithKey` and `Map.insertWith` managed to make this part more optimized.
I think, that the better optimizations are possible, but only with using multiple cores for this part.
(it seems to be trivialy parallelable) but I didn't have time to implement this.
Plus, the perfirmance is decent as is as well.
There was an option to rewrite the decodeTable to the `BiMap`, but I have decided, since it takes not as long
as I expected, this is not a high priority change.
