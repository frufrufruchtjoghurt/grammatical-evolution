
# Table of Contents

1.  [Grammatical Evolution](#orgd76bed3)
    1.  [Structure of regular grammars](#org6f4730a)
        1.  [A very simple example](#org5ee6051)
        2.  [Circular rules](#orgea3620c)
        3.  [Exit conditions](#orgd1ab6d4)
    2.  [Generating an initial population](#orge593dd8)
        1.  [Generating regular grammar trees](#orgbcbeb6d)
        2.  [grow](#org24d3060)
        3.  [full](#org4e13e3d)
        4.  [ramped half-and-half](#orgfd75843)



<a id="orgd76bed3"></a>

# Grammatical Evolution


<a id="org6f4730a"></a>

## Structure of regular grammars

This small section should evaluate regular grammars of varying complexity and evaluate different representations in Clojure.


<a id="org5ee6051"></a>

### A very simple example

Consider a regular grammar which should only produce words containing the letter `a`. The only restriction is, that the word has to contain at least one &rsquo;a&rsquo;.
Sample words could look like this: `aaaaaa`, `a`, `aa` etc.

The following rules could represent this regular grammar:

V<sub>T</sub> = {<sub>a</sub>\_}
V<sub>N</sub> = {S, A}

S = A
A = <span class="underline">a</span> A
A = &epsilon;

How can we now represent this rules as a tree-like structure? If we chose a very simplistic approach, we could simply nest rules if they reference each other like this:

    (S
      (A "a"
         (A "\epsilon")))

For a different approach, let&rsquo;s analyse the textual description of this grammar again: *Words that contain `a` n times, but at least once*. If we wanted to specify this rule like a regular expression, we could simply write `a+` to get at least one `a`. If we apply this to our ruleset, it could look like this:

V<sub>N</sub> = {<sub>a</sub>\_}
V<sub>T</sub> = {S, A}

S = A
A = <span class="underline">a</span> +

By writing the grammar like this, we can express the same rules as before, but without the need of an additional rule for &epsilon;.
The tree in our code would look like this:

    (S
      (A "a" "+"))


<a id="orgea3620c"></a>

### Circular rules

Let&rsquo;s look at a more complex ruleset. This time, we want to create words that alternate between `a` and `b`. Some example words could look like this: `aba`, `ababab`, `ab` etc.

Following rules could now represent our grammar:

V<sub>N</sub> = {<sub>a</sub>\_, <span class="underline">b</span>}
V<sub>T</sub> = {S, A, B}

S = A
A = aB
A = &epsilon;
B = bA
B = &epsilon;

We are not able to simplify these rules by using regular expression syntax. Hence, our tree could look like this:

    (S
      (A "a"
         (B "b"
            (A "\epsilon"))))

We now have the problem, that we are unable to express `B = \epsilon` this way, since `B` is only referenced once, but `A` is referenced twice.
We could enhance our syntax with regular expression symbols, in order to convey different rules.

    (S
      (A "a" "*" "A"))

This would be equivalent to following ruleset:

V<sub>T</sub> = {<sub>a</sub>\_}
V<sub>N</sub> = {S, A}

S = A
A = <span class="underline">a</span> A
A = &epsilon;

Similarly this:

    (S
      (A "a" "+" "A"))

would result in:

V<sub>T</sub> = {<sub>a</sub>\_}
V<sub>N</sub> = {S, A}

S = A
A = <span class="underline">a</span> A
A = <span class="underline">a</span>

If we now apply this to our more complex example from the beginning, following tree might be created:

    (S
      (A "a" "*"
         (B "b" "*"
            (A "\epsilon"))))

Upon evaluation of this tree, we can remove redundant modifiers.

    (S
      (A "a"
         (B "b" "*"
            (A "\epsilon"))))

For each inserted terminal symbol, either `*`, `+` or no modifier are added. This creates a more equal way to create rules,


<a id="orgd1ab6d4"></a>

### Exit conditions

If we reconsider the example above, the grammar also allows an empty word &epsilon; as valid. Following grammar defines the case, when at least one pair `ab` should be produced.

V<sub>N</sub> = {<sub>a</sub>\_, <span class="underline">b</span>}
V<sub>T</sub> = {S, A, B, C}

S = A
A = aB
A = aC
B = bA
C = b

We could represent those rules in a tree like this:

    (S
      (A "a"
         (B "b"
            (A "a"
               (C "b")))))

We now can decode each level of the tree into a rule, whilst also taking nested rules into account. This notation should be sufficient to depict any regular grammar.

Following rules are applied when creating a new rule tree:

1.  All *terminal symbols* are treated as variables.
2.  All *non-terminal symbols* are treated as functions.
3.  A terminal symbol can get a *modifier* `*` or `+`.


<a id="orge593dd8"></a>

## Generating an initial population


<a id="orgbcbeb6d"></a>

### Generating regular grammar trees

A very simplistic approach to generating trees would be to restrict generation by inherent principles of regular grammars. Therefore, for each rule, one terminal symbol with a modifier will be selected. Then, a non-terminal symbol might be selected and a new rule for this non-terminal symbol will be created.

We start with the rule S.

    (S)

S will point to our first rule, so another rule will be immediatly created.

    ;; select from V_N = {A, B, C, 0}
    ;; 0 marks the possibility that no additional rule might be created as well
    (S
      (A))

Rule A needs a terminal symbol, to be valid in terms of regular grammars.

    ;; select from V_T = {"a", "b", "\epsilon"}
    (S
      (A "a"))

An optional modifier can be selected as well.

    ;; select from V_mod = {"", "*", "+"}
    (S
      (A "a" "+"))

We now randomly decide, if we want to add another rule.

    ;; select from V_N = {A, B, C, 0}
    (S
      (A "a" "+"
         (C)))

We can now complete this rule.

    (S
      (A "a" "+"
         (C "b" "" 0)))

In this case, 0 was selected as the next rule, so further rule will be created. Following final ruleset is being created:

V<sub>T</sub> = {<sub>a</sub>\_, <span class="underline">b</span>}
V<sub>N</sub> = {S, A, C}
V<sub>mod</sub> = {&ldquo;&rdquo;, +}

S &rarr; A
A &rarr; <span class="underline">a</span> C
A &rarr; <span class="underline">a</span>
C &rarr; <span class="underline">b</span>


<a id="org24d3060"></a>

### grow


<a id="org4e13e3d"></a>

### full


<a id="orgfd75843"></a>

### ramped half-and-half

