# Advent of Code 2023 Solutions in [Common Lisp](https://lisp-lang.org/)

Advent of Code 2023 link: https://adventofcode.com/2023

## Why Common Lisp?

I enjoy coding in the LISP family of languages and having read the Practical Common Lisp book _a while ago_, was keen to find a reason to use the language.

Advent of Code doesn't lend itself well to exploring the more interesting features of CL (such as the condition/restart system), but nonetheless it was a good opportunity to write code in the language. That said, the code is pretty awful not helped by the nature of advent of code :joy: In the first few days, I didn't even how to do basic things such as how to split strings or create hash maps.

## Note

1. The solutions do not follow any particular structure (e.g. some have `part1` and `part2` functions, and in others I've simply changed a function called `main` or ran an expression ad-hoc) - it was my first Advent of Code (attempting 2020 in gForth for all of about 3 days doesn't count), so I wasn't sure how best to structure things to start with.
2. There are bits of code that reference hardcoded paths. I really should clean those up but notably there are also references to shared libraries (e.g. `libblas.so`) that need to be fixed. 

## So how was Common Lisp?

Common Lisp was really fun. Probably the most fun language I've used. 

It reminds me more of Smalltalk than any other language I've used to date, notably the interactive debugger (the ability to click through a stack trace and see the local variables at each frame.. the ability to evaluate arbitrary expressions in the context of the frame... the ability to load code changes into the running environment and then restart from a given frame and continue execution of the program).

I focused a bit too much on trying to complete the tasks with my (very) limited knowledge in the beginning. And so it took me _far too long_ to start using some of the better debugging capabilities in SLY/sbcl such as SLY stickers, even though I was aware of it before starting out. :unamused: 

I do think there's still lots of scope for better debugging capabilities in completing _these kinds of tasks_, e.g. visualizations. Other languages such as Python are better suited to completing the challenges for that reason (i.e. there are better or more readily available libraries as you can see in the advent of code reddit; likely due to larger community and it's not hard to do in CL, so everything rolls their own solution ala [The Lisp Curse](http://www.winestockwebdesign.com/Essays/Lisp_Curse.html)). Note: there's some Sketch code in the repo for drawing 2d mazes and visualising a variety of algorithms (still a WIP) for the longest path task.

Also I avoided the Common Lisp Object System (CLOS) too much in the beginning. It's very convenient, powerful, and rather nice to use. And as I found out later, can be a nice stepping stone to a purely functional solution as it helps manage the state and spot patterns whilst keeping the code easier to read and well organised; at least, that was my experience.
