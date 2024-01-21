# Advent of Code 2023 Solutions in [Common Lisp](https://lisp-lang.org/)

Advent of Code 2023 link: https://adventofcode.com/2023

## Why Common Lisp?

I enjoy coding in the LISP family of languages and having read the Practical Common Lisp book _a while ago_, was keen to find a reason to use the language.

Advent of Code doesn't lend itself well to exploring the more interesting features of CL (such as the condition/restart system), but nonetheless it was a good opportunity to write code in the language. That said, the code is pretty awful not helped by the nature of advent of code :joy: In the first few days, I didn't even how to do basic things such as how to split strings or create hash maps.

## Note

1. The solutions do not follow any particular structure (e.g. some have `part1` and `part2` functions, and in others I've simply changed a function called `main` or ran an expression ad-hoc) - it was my first Advent of Code (attempting 2020 in gForth for all of about 3 days doesn't count), so I wasn't sure how best to structure things to start with.
2. There are bits of code that reference hardcoded paths. I really should clean those up but notably there are also references to shared libraries (e.g. `libblas.so`) that need to be fixed. 
