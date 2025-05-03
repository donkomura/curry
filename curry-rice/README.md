## Curry-rice program

```bash
$ runcurry curry.curry > rice.curry
$ diff <(runcurry rice.curry) curry.curry
$ echo $?
0
```