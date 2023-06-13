## Background

This is a dysfunctional [Piet](https://www.dangermouse.net/esoteric/piet.html) intepreter in Clojure designed for:

- the code to be pretty comprehensible
- the corners softened with decent errors and forgiving treatment of edge cases
- runtime verbosity that is comprehensive but not overwhelming.

[clj-piet](https://github.com/ljos/clj-piet) exists, check my fork, but it's over a decade old and doesn't traverse whitespace as per the updated spec. This project is inspired and takes influence from that one but is just my updated personal rewrite.

Initial commit: All this does is traverse codel blocks. It doesn't execute anything, chiefly, commands that adjust the pointer.

Boo-hoo.. I tried to get fancy with the DP/CC and it's not happy. It picks the wrong exit points on large blocks by always checking column then row.

### To run

```sh
clj -X:run '{:file "programs/file.png"}'
```

`:codel-size` and `verbose?` default to `1` and `true`.

### Program status

- [factorial](./programs/factorial.png) ❌ no pointer manipulation
- [hello-world-1](./programs/hello-world-1.png) ❌ no pointer manipulation
- [hello-world-2](./programs/hello-world-2.png) ❌ bad traversal algorithm
- [hello-world-3](./programs/hello-world-3.png) ✅
- [pi](./programs/pi.png) ✅
- [sample-1](./programs/sample-1.png) ✅

Most of the programs are reused or slightly edited from [Piet sample programs](https://www.dangermouse.net/esoteric/piet/samples.html).

## Appendix

### A. Language overview

Piet scripts are images, or, 2D arrays of pixels representing a predetermined set of color values.

Execution happens "between" tiles, in the movement from one color tile to the next.

The color of a pixel doesn't contain any data about what to run, only the difference between two colors.

What is this, a programming language for ants?! Where a textual language parses files one-dimensionally character by character, the layout of colors causes the interpreter to crawl around the program as it would on a plane. It looks forward, zigzagging and looping around based on boundaries between plots of color.

Piet has 18 program colors and two utility shades (walls and open space), see below.

### B. Valid colors and their codes

![Table of piet colors, hex codes, java rbg ints, and rgb trios](./resources/table.svg)

Hex, the RGB ints that `.getRGB` returns, and RGB.

### Gotchas

**White is kind of strange**

On white blocks the pointer acts differently from colorful spaces. Instead of a rigid 'flip-check-turn-check' cycle, it acts identically to blocks in icy zelda dungeons or slippery sokoban puzzles.

Instead of terminating after 8 unsuccessful checks in each possible block outlet, a program caught in whitespace terminates once its path of checks loops back onto itself.

Two termination cases adds some unpleasant complexity, but whitespace handling is more intuitive.

### Questions

**Why Clytemnestra?**

Just descriptive...like mathematicians use Greek letters to extend the alphabet.

**This language is so impossible to write**

Yeah...

If you want to change one piece in the middle of the program you probably have to rewrite the whole thing.

With an abstract colorful visual language I'd rather have the pointer jumping around than doing an ant walk.

Each codel block has 4 different "easy"-to-spot properties: area, R, G, and B. In Piet the RGB values are reduced to one property, ∆color, so each unit of language only encodes two pieces of data.

I imagine a language equally difficult to deal with for different reasons, where, for instance,

- area: int value
- ∆R: command to execute
- ∆G: relative X offset to next codel
- ∆B: relative Y offset to next codel

That would be chaos...
