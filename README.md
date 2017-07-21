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
This is mostly for my own amusement at the moment and mostly serves as a playground for experimentation, so it's not exactly well-tested.

---
### TODOs / Plans

#### Cleanup
- [ ] Add tests for the interesting bits.
    - [ ] Fix encoding issues for string case
    - [X] YAML-related type class derivation
- [ ] Consider renaming Yaml-related type classes
- [ ] See how useful cats could be
    - [ ] Show typeclass
    - [ ] Some of the folds during parsing
    - [ ] ValidatedNel would capture all the errors instead of failing at the first
- [ ] Consider extracting YAML-related stuff into separate project
- [X] Use shapeless for deep type checks and extract a better helper for YamlReads
- [X] We can parse StringTemplates directly, since we have `.map` now.

#### Features
- [ ] Support `Option[T]` fields in YamlReads
- [ ] Add option parser and enable other outputs, such as:
    - [ ] stdout
    - [ ] clipboard
    - [ ] file (implemented, but need abstracting from)
- [X] Support simple Co-Product in YamlReads type class
- [X] Multiple mentions of the same variable in different snippets should only ask me for the value once.
- [X] Allow both arbitrary number of snippets and keywords.

#### Other

- [x] Rename to cannery
- [x] Add example .yml
- [x] Write Readme with disclaimer
- [x] Push to Github
