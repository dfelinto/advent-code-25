# Advent of code 25

I will try to use the [advent of code](https://adventofcode.com/2025) to better learn Closure Script.


## Installation

Follow the guide here: https://clojure.org/guides/install_clojure

Copy your own input data to /inputs/
(day1.txt, day2.txt, ...)

If you don't want to do this, just use the test file as teh main file:
`cp inputs/day1-test.txt inputs/day1.txt`.

## Running

Day 1-7.1
```bash
clj -M -m cljs.main --target node --output-to out.js -c day1.core  && node out.js
```

Day 7.2- onwards
```bash
bb src/day7_2/core.cljs
```

I was so stuck on the time Day 7 part 2 was taken to process that I switched to Babashka.
As it turned out, the issue wasn't of performance, but of algorithm :)

## My Rules (aka AI?)

This is the first time I take part on the advent of code. I wanted to do a more serious attempt at learning Clojure Script and this fit right in.

I've been using AI to navigate my way around the language, mostly for syntax (less so every day). And after I'm done with a challenge I do a pass on ChatGPT to know a more idiomatic way of writing the same code (or a faster one). I've been uploading some of those attempts (look for ai.cljs in some of the folders).

More recently I found an even better resource: [Christian Mangelsdorf's X account](https://x.com/ch_mangelsdorf).

The guy is brilliant, and his one-page solutions are so elegant. So now, after I'm done with a challenge, I go to his feed to learn how to do it "the right way".

I'll save his screenshots here later so they don't get lost. Thank you random generous internet stranger :)

## Progress so far

Days 1-6 were quite within my comfort zone. While I reached this mid-point I noticed already how much more comfortable I'm at reading Clojure code.

My initial goal was to be better at analysing Penpot code-base. I went back to it this week and it is day and night. Before that I watched a one-hour video on Clojure and risked a few simple patches. At the moment I feel finally comfortable understanding whateheck I did back then hehe.

I'm also reading the Brave and Bold Clojure book and it seems great so far. I can highly recommend it already.

## Challenges

- [x] [Day 1.1](https://adventofcode.com/2025/day/1)
- [x] Day 1.2
- [x] [Day 2.1](https://adventofcode.com/2025/day/2)
- [x] Day 2.2
- [x] [Day 3](https://adventofcode.com/2025/day/3)
- [x] Day 3.2
- [x] [Day 4](https://adventofcode.com/2025/day/4)
- [x] Day 4.2
- [x] [Day 5](https://adventofcode.com/2025/day/5)
- [x] Day 5.2
- [x] [Day 6](https://adventofcode.com/2025/day/6)
- [x] Day 6.2
- [x] [Day 7](https://adventofcode.com/2025/day/7)
- [x] Day 7.2
- [x] [Day 8](https://adventofcode.com/2025/day/8)
- [x] Day 8.2
- [x] [Day 9](https://adventofcode.com/2025/day/9)
- [x] Day 9.2
- [x] [Day 10](https://adventofcode.com/2025/day/10)
- [x] Day 10.2
