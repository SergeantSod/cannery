# Cannery


An simple CLI-app that outputs text based on canned snippets that are read from a yaml file.
Why:
* I got tired of writing the same kinds of stock e-Mails with minimal changes
* I needed a pet project to play around with [shapeless](https://github.com/milessabin/shapeless)
* For fun and profit

**Disclaimer:**
This is mostly for my own amusement at the moment and mostly serves as a testbed for experimentation, so it's not exactly well-tested.

---
### TODOs

- [x] Rename to cannery
- [ ] Add example .yml
- [ ] Add option parser and enable other outputs, such as stdout, clipboard
- [ ] Support Co-Product in YamlReads type class
    * in case of Left, fall back to the Reads for the right side of the co-product, use flatmap
    * for CNil we have failed, since none of the cases matched
- [ ] Rename Yaml-related type classes
- [x] Write Readme with disclaimer
- [x] Push to Github
- [ ] Add tests for the interesting bits.
    - [ ] YAML-related type class derivation
    - [ ] Consider extracting it into separate project
