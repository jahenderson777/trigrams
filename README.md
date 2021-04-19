# HealthUnlocked technical test - trigrams

A trigram is sequence of 3 words in any given text. Let's take an example sentence: "the cat sat on the other cat on the mat on the floor" - its trigram map will look like this:
```clj
{["the" "cat"]   ["sat"]
 ["cat" "sat"]   ["on"]
 ["sat" "on"]    ["the"]
 ["on" "the"]    ["other" "mat" "floor"]
 ["the" "other"] ["cat"]
 ... etc.
 }
 ```

To generate a new sequence of text, start with a key at random, say `["on" "the"]`, and then pick a value at random, say `"other"`, now you have a trigram `["on" "the" "other"]`. For the next trigram, you look up `["the" "other"]` and get a value at random - in this case there will only be one, `"cat"`. Continue ad infinitum.

Using a book of your choosing from [Project Gutenberg](https://www.gutenberg.org/) and the process described above, please complete the following:

1. Ignoring punctuation marks, generate a sequence of 50 words.
2. Generate 10 full sentences (sentences start with a capital letter and end in a punctuation mark).
3. Given a word from the text, generate a sentence which contains it anywhere inside it (i.e. not just the start).

## Chosen book

The Hound of the Baskervilles by Arthur Conan Doyle

https://www.gutenberg.org/ebooks/2852

## Usage

Run the project directly:

    $ clojure -M:run-m


Run the project's tests:

    $ clojure -M:test:runner

## Example output
```
Task 1: generate a sequence of 50 words
Lord clarendon I most earnestly commend to your severely practical mind the work of the wall under which I may not have helped us to the contrary we shall meet at the silver-tipped bank in front of the prehistoric people where they like with their profiles towards me and stooped.


Task 2: Generate 10 full sentences
And fine above us get together and sparkling brightly from behind a pair of gold-rimmed glasses in. Long and left of the business ready to swear you are not content with cultivating the lady’s. Another of those miserable ponies wander into the hut and waited his chance of the butler had. Lives so long and weary labours to so piteous an end of our going to honour us. Out by the cabman’s reply than anywhere else until the instant when we both saw him rejoin. In orchids on the night of what we heard we cannot even swear to one thing and. Sorry madam but I can still remember your complete indifference as to tell him that you should. Changes in your line of portraits which covered the lower curve of the kind nor could any. She raised one hand as if seeking approbation for what he regarded as our own trouble came. Earnest purpose can he see the waste-paper of yesterday his Clarendon I most earnestly commend to your.


Task 2: Given a word from the text, generate a sentence which contains it anywhere inside it (using example of 'great')
Of events as nearly as i then you may expect great changes to begin here soon their skins turned cold for there is.
```

## License

Copyright © 2021 Alex Jonathan Henderson

_EPLv1.0 is just the default for projects generated by `clj-new`: you are not_
_required to open source this project, nor are you required to use EPLv1.0!_
_Feel free to remove or change the `LICENSE` file and remove or update this_
_section of the `README.md` file!_

Distributed under the Eclipse Public License version 1.0.
