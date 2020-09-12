+++
date = "2014-08-21T16:28:49+02:00"
draft = false
title = "Binary diffing"
slug = "binary-diffing"
aliases = [
	"binary-diffing"
]
+++
Yesterday, a new feature was [pushed]( https://github.com/radare/radare2/commit/9f18c219a6ecc9952130a6c265461f652cb6cd55) to radare2: offset-based function diffing. We'd like to take this opportunity to write a bit about radare2's diffing features before showing the shiny new one.

Let's take a copy of a cracked crackme as an example, and the `true` and `false` binaries.

Without parameter, `radiff2` will by default show what bytes changed, and the corresponding offsets.
```
$ radiff2 genuine cracked
0x000081e0 85c00f94c0 => 9090909090 0x000081e0
0x0007c805 85c00f84c0 => 9090909090 0x0007c805

$ rasm2 -d 85c00f94c0
test eax, eax
sete al

```
Notice how the two jumps are noped.

For bulk processing, you may want to have a higher-overview of the differences. This is why radare2 is able to compute the [distance]( https://en.wikipedia.org/wiki/Levenshtein_distance ) and the percentage of similarity between two files with the `-s` option:
```
$ radiff2 -s /bin/true /bin/false
similarity: 0.97
distance: 743
```
If you want more concrete data, it's also possible to *count* the differences, with the `-c` option:
```
$ radiff2 -c genuine cracked
2
```

If you're unsure about the fact that you're dealing with similar binaries, you can check if some functions are matching with the `-C` option. The columns being: "First file offset", "Percentage of matching" and "Second file offset".

```
$ radiff2 -C /bin/false /bin/true 
                   entry0  0x4013e8 |   MATCH  (0.904762) | 0x4013e2  entry0
sym.imp.__libc_start_main  0x401190 |   MATCH  (1.000000) | 0x401190  sym.imp.__libc_start_main
             fcn.00401196  0x401196 |   MATCH  (1.000000) | 0x401196  fcn.00401196
             fcn.0040103c  0x40103c |   MATCH  (1.000000) | 0x40103c  fcn.0040103c
             fcn.00401046  0x401046 |   MATCH  (1.000000) | 0x401046  fcn.00401046
             [...]
```

And now the cool feature : radare2 supports graph-diffing, à la [DarunGrim]( http://www.darungrim.org/ ), with the `-g` option. You can either give a symbol name, of specify two offsets in case the function you want to diff doesn't have the same name in both file.

For example, `radiff2 -g main /bin/true  /bin/false | xdot -` will show the differences between the main function of `true` and `false`. You can compare it to `radiff2 -g main /bin/false /bin/true` (Notice the order of the arguments) to get the two versions.

This is the result:
![/bin/true and /bin/false graph diff](/blog/images/true_false.png)

The parts in yellow are indicating that some offsets are not matching, the grey one is a perfect match, while the red one highlight a strong difference. If you look closely, you'll see that the left one is `mov edi, 0x1; call sym.imp.exit`, while the right one is `xor edi, edi; call sym.imp.exit`.

Binary diffing is an important feature for reverse engineering. It can be used to analyze [security updates]( https://en.wikipedia.org/wiki/Patch_Tuesday ), infected binaries, firmware changes and more..

We have only shown the code analysis diffing functionality, but radare2 supports more sort of diffing between two binaries at byte level, deltified similarities and more to come.

We have plans to implement more kinds of bindiffing functionalities into r2, and why not, add support for ascii art graph diffing and better integration with the rest of the toolkit.