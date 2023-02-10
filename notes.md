Serve only .../get.{html,xml}

Create only via .../post.{html,xml}

TODO:

- expressions
  - formatting?
  - sorting? filtering?
    - should this go on for instead?
  - membership tests?
  - xpath-like selectors
    - mandatory for expressions
      - /a/b/c - child selectors\
    - optional for future?
      - //a//b//c - descendent selectors
      - /a/(b,c) - conjunction selecting both b and c
      - a[...] - boolean constraint
        - a[binop <expr> logop ...] - a binop b holds
        - a[<path> logop ...] - there is such a path below a