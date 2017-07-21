# Cannery


An simple CLI-app that outputs text based on canned snippets that are read from a yaml file.
Why:
* I got tired of writing the same kinds of stock e-Mails with minimal changes
* I needed a pet project to play around with [shapeless](https://github.com/milessabin/shapeless)
* For fun and profit

To run:
```
sbt run "example_snippets.yml output.txt"
```

**Disclaimer:**
This is mostly for my own amusement at the moment and mostly serves as a testbed for experimentation, so it's not exactly well-tested.

---
### TODOs / Plans

#### Cleanup
- [ ] We can parse StringTemplates directly, since we have `.map` now.
- [ ] Rename Yaml-related type classes
- [ ] Add tests for the interesting bits.
    - [ ] Fix encoding issues for string case
    - [X] YAML-related type class derivation
- [ ] Consider extracting YAML-related stuff into separate project
- [ ] See how useful cats could be
    - [ ] Show typeclass
    - [ ] Some of the folds during parsing
    - [ ] ValidatedNel would capture all the errors instead of failing at the first
- [X] Use shapeless for deep type checks and extract a better helper for YamlReads

#### Features
- [ ] Multiple mentions of the same variable in different snippets should only ask me for the value once.
- [ ] Allow both arbitrary number of snippets and keywords.
- [ ] Add option parser and enable other outputs, such as stdout, clipboard
- [ ] Support Co-Product in YamlReads type class
    * try implementation for Left, fall back to the Reads for the right side of the co-product maybe use recoverWith
    * for CNil we have failed, since none of the cases matched

#### Other

- [x] Rename to cannery
- [x] Add example .yml
- [x] Write Readme with disclaimer
- [x] Push to Github
