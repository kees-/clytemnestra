## Background

It's alive! This is a [Piet](https://www.dangermouse.net/esoteric/piet.html) intepreter in Clojure with design goals of:

- the code to be pretty comprehensible
- the corners softened with decent errors and forgiving treatment of edge cases
- runtime verbosity that is comprehensive but not overwhelming.

![Example running the program from the command line](./resources/sample.png)

[clj-piet](https://github.com/ljos/clj-piet) exists, check my fork, but it's over a decade old and doesn't traverse whitespace as per the updated spec. This project is inspired by and takes influence from that one but is just my updated personal rewrite.

### To run

```sh
clj -X:run '{:file "programs/file.png"}'
```

`:codel-size`, `:verbose?`, and `:limit` default to `1`, `true`, and `0` (unlimited).

### Program status

- [corner-test](./programs/corner-test.png) ✅
- [factorial](./programs/factorial.png) ✅
- [hello-world-1](./programs/hello-world-1.png) ✅
- [hello-world-2](./programs/hello-world-2.png) ✅
- [hello-world-3](./programs/hello-world-3.png) ✅
- [pi](./programs/pi.png) ❌ Int madness
- [tetris](./programs/tetris.png) ✅
- [whitespace-test](./programs/corner-test.png) ✅

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

### C. Available commands

⬇️ hue shift ➡️ tone shift

|     | +0        | +1      | +2       |
| --- | --------- | ------- | -------- |
| +0  |           | push    | pop      |
| +1  | +         | -       | *        |
| +2  | /         | mod     | not      |
| +3  | >         | pointer | switch   |
| +4  | duplicate | roll    | int in   |
| +5  | char in   | int out | char out |

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
- G: relative X offset to next codel
- B: relative Y offset to next codel

That would be chaos...
