# HealthUnlocked technical test - trigrams

A trigram is sequence of 3 words in any given text. Let's take an example sentence: "the cat sat on the other cat on the mat on the floor" - its trigram map will look like this:

{["the" "cat"]   ["sat"]
 ["cat" "sat"]   ["on"]
 ["sat" "on"]    ["the"]
 ["on" "the"]    ["other" "mat" "floor"]
 ["the" "other"] ["cat"]
 ... etc.
 }
To generate a new sequence of text, start with a key at random, say ["on" "the"], and then pick a value at random, say "other", now you have a trigram ["on" "the" "other"]. For the next trigram, you look up ["the" "other"] and get a value at random - in this case there will only be one, "cat". Continue ad infinitum.

Using a book of your choosing from Project Gutenberg and the process described above, please complete the following:

Ignoring punctuation marks, generate a sequence of 50 words.
Generate 10 full sentences (sentences start with a capital letter and end in a punctuation mark).
Given a word from the text, generate a sentence which contains it anywhere inside it (i.e. not just the start).

## Usage

Run the project directly, via `:exec-fn`:

    $ clojure -X:run-x


Run the project's tests:

    $ clojure -M:test:runner

## Example output
```
Task 1: generate a sequence of 50 words
Who lived in a few minutes later we shall have a case where everything goes against you my word that I covet your skull sir until the early hours of intense mental concentration during which I could see that I could still cling to the top of my conversation with Barrymore just


Task 2: Generate 10 full sentences
Fellow shall be in the collection of butterflies and moths the formation of which the boy. Fellow shall be interpreted to make her the direct accessory to murder time it up to. Fellow shall be to descend to the ground if there is the _Devon County Chronicle_ of. Fellow shall be to show us the spot where the Fernworthy people will burn me in. Fellow shall be in such states who approach us with offers to donate royalties under this. Fellow shall be to show the dreary curves of the house have you believe my sons. Fellow shall be in a tight place had indeed been used to carry it out with. Fellow shall be ready for him in the bottle out him after when to that deadly. Fellow shall be to show you up to his feet in his Yorkshire days been the. Fellow shall be found in the States and in his frankest and most unconcerned manner silent


Task 2: Given a word from the text, generate a sentence which contains it anywhere inside it (using example of 'great')
Middleton s park slap across it sir and that when i was too great misfortune will befall you
```

## License

Copyright © 2021 Alex Jonathan Henderson

_EPLv1.0 is just the default for projects generated by `clj-new`: you are not_
_required to open source this project, nor are you required to use EPLv1.0!_
_Feel free to remove or change the `LICENSE` file and remove or update this_
_section of the `README.md` file!_

Distributed under the Eclipse Public License version 1.0.